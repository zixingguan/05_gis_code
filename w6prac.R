library(spatstat)
library(here)
library(sp)
library(tmap)
library(sf)
library(tmaptools)

##First, get the London Borough Boundaries
LondonBoroughs <- st_read("E:/aterm1/05_gis/github/05_gis_code_w5_prac/w5_prac/ESRI/London_Borough_Excluding_MHW.shp")

# 制作地图
library(stringr)
BoroughMap <- LondonBoroughs %>%
  dplyr::filter(str_detect(GSS_CODE, "^E09"))%>%
  st_transform(., 27700)

qtm(BoroughMap)

##Now get the location of all Blue Plaques in the City
BluePlaques <- st_read("https://s3.eu-west-2.amazonaws.com/openplaques/open-plaques-london-2018-04-08.geojson")%>%
  st_transform(.,27700)

##plot the blue plaques in the city
tmap_mode("plot")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaques) +
  tm_dots(col = "blue")

#现在，您可能已经注意到，至少有一块蓝色牌匾位于自治市边界之外。
#错误的牌匾会给我们的分析带来问题，所以我们需要将牌匾剪裁到边界上……
#首先，我们将移除所有具有相同网格参考的牌匾，因为这会在以后的分析中造成问题。
#remove duplicates
library(tidyverse)

library(sf)
BluePlaques <- distinct(BluePlaques)
#使用 dplyr::distinct() 去重，移除 BluePlaques 数据集中重复的行。

#现在，只需选择伦敦境内的积分
#这里，第二个运算符是空的，它控制着保留哪些属性，
#尽管我宁愿保留所有属性并使用tidyverse进行操作。
BluePlaquesSub <- BluePlaques[BoroughMap,]
#BluePlaques：一个 sf 对象，表示点、线或面数据。
#BoroughMap：另一个 sf 对象，通常是多边形或面数据。
#筛选逻辑：BluePlaques[BoroughMap, ] 会返回 BluePlaques 中与 BoroughMap 相交的几何数据

#check to see that they've been removed
tmap_mode("plot")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")

#首先，我们需要对我们的 sf 对象进行子集化，以提取我们感兴趣的行政区。
#我将选择哈罗，因为我知道那里的点数足够少，可以确保分析成功。
#extract the borough
# select by attribute
Harrow <- BoroughMap %>%
  filter(., NAME=="Harrow")

#Check to see that the correct borough has been pulled out
tm_shape(Harrow) +
  tm_polygons(col = NA, alpha = 0.5)

#接下来，我们需要剪辑我们的蓝色牌匾，以便我们有一个子集
#只包含那些属于行政区或兴趣的牌匾
#clip the data to our single borough
BluePlaquesSub <- BluePlaques[Harrow,]
#check that it's worked
tmap_mode("plot")

tm_shape(Harrow) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")

#now set a window as the borough boundary
window <- as.owin(Harrow)
plot(window)

#对于点模式分析，我们需要创建一个点模式（ppp）对象。
#create a sp object
BluePlaquesSub<- BluePlaquesSub %>%
  as(., 'Spatial')
#使用了管道操作符（%>%）将 BluePlaquesSub 数据转换为一个 Spatial 对象。
#Spatial 是 sp 包中的一个类，用于表示空间数据（比如地理位置、坐标等）。

#create a ppp object
BluePlaquesSub.ppp <- ppp(x=BluePlaquesSub@coords[,1],
                          y=BluePlaquesSub@coords[,2],
                          window=window)
#将 BluePlaquesSub 中的坐标数据（BluePlaquesSub@coords）转换为一个 ppp（点过程）对象。
#ppp 是 spatstat 包中表示空间点过程的类，用于处理点的分布情况。
#x 和 y 是表示点的坐标，window 是定义观察区域的窗口。
#x=BluePlaquesSub@coords[,1]：提取 BluePlaquesSub 对象中的坐标数据，并取第一列的值作为点的 x 坐标。
#y=BluePlaquesSub@coords[,2]：取第二列的值作为点的 y 坐标。

#查看ppp对象
BluePlaquesSub.ppp %>%
  plot(.,pch=16,cex=0.5, 
       main="Blue Plaques Harrow")


#核密度分析
#总结点数据的一种方法是在一个称为“内核”的窗口下绘制点的密度。
#内核的大小和形状会影响生成的密度模式，
#但使用density（）函数从ppp对象生成内核密度估计（KDE）映射非常容易。
BluePlaquesSub.ppp %>%
  density(., sigma=500) %>%
  plot()
#sigma 值设置了内核的直径，直径越大，点越大


#样方分析
#CSR最基本的测试是二次分析。
#我们可以使用spatstat中的平方计数函数对我们的数据进行简单的平方分析。
#请注意，我不建议在您进行的任何实际分析中进行平方分析，但这对于开始理解泊松分布很有用…
#First plot the points
plot(BluePlaquesSub.ppp,
     pch=16,
     cex=0.5, 
     main="Blue Plaques in Harrow")

#now count the points in that fall in a 6 x 6
#grid overlaid across the windowBluePlaquesSub.ppp2<-BluePlaquesSub.ppp %>%
BluePlaquesSub.ppp %>%
  quadratcount(.,nx = 6, ny = 6)%>%
  plot(., add=T, col="orange")

#，想知道伦敦地区的蓝色牌匾是否存在任何类型的空间模式。
#这意味着将我们观察到的点分布与基于泊松分布的统计可能性（完全空间随机）分布进行比较
#再次使用相同的quadratcount()函数（对于相同大小的网格），我们可以将结果保存到表中
#run the quadrat count
Qcount <- BluePlaquesSub.ppp %>%
  quadratcount(.,nx = 6, ny = 6) %>%
  as.data.frame() %>%
  dplyr::count(Var1=Freq)%>%
  dplyr::rename(Freqquadratcount=n)







