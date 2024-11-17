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

#检查第一列的数据类型——如果它是factor，我们需要将其转换为数字

Qcount %>% 
  summarise_all(class)

#接下来我们需要计算我们的预期值，基于泊松分布计算预期概率的公式
sums <- Qcount %>%
  #calculate the total blue plaques (Var * Freq)
  mutate(total = Var1 * Freqquadratcount) %>%
  dplyr::summarise(across(everything(), sum))%>%
  dplyr::select(-Var1) 

lambda<- Qcount%>%
  #calculate lambda
  mutate(total = Var1 * Freqquadratcount)%>%
  dplyr::summarise(across(everything(), sum)) %>%
  mutate(lambda=total/Freqquadratcount) %>%
  dplyr::select(lambda)%>%
  pull(lambda)

#使用泊松公式计算预期值。k是正方形中计数的蓝色斑块的数量，位于我们表格的第一列…
QCountTable <- Qcount %>%
  mutate(Pr=((lambda^Var1)*exp(-lambda))/factorial(Var1))%>%
  #now calculate the expected counts based on our total number of plaques
  #and save them to the table 现在根据斑块总数计算预期计数#并将其保存到mutate表中
  mutate(Expected= (round(Pr * sums$Freqquadratcount, 0)))

#Compare the frequency distributions of the observed and expected point patterns
#比较观察到的和预期的点模式的频率分布
plot(c(1,5),c(0,14), type="n",
     xlab="Number of Blue Plaques (Red=Observed,Blue=Expected)", 
     ylab="Frequency of Occurances")
points(QCountTable$Freqquadratcount, 
       col="Red", 
       type="o", 
       lwd=3)
points(QCountTable$Expected, col="Blue", 
       type="o", 
       lwd=3)

#我们可以观察到，它们在下端都有更高的频率计数，这让人想起泊松分布。
#这可能表明，对于这组特定的象限，我们的模式接近完全空间随机性（即没有点的聚类或分散）。

#但我们如何确认这一点？
#为了确认，我们可以使用内置于spatstat中的quadrat.test（）函数。
#这使用卡方检验来比较每个象限的观察频率和预期频率（而不是我们上面计算的象限箱）。
#卡方检验确定两个分类变量之间是否存在关联。卡方值越高，差异越大。
#如果我们的卡方检验的p值<0.05，那么我们可以拒绝一个零假设，
#即“我们的数据中没有模式，即完全的空间随机性”
#（将零假设视为与我们的数据显示模式的假设相反）。

#我们需要寻找的是p>0.05的值。如果我们的p值>0.05，那么这表明我们有CSR，
#我们的点没有模式。如果它小于0.05，则表明我们的点确实有聚类。
teststats <- quadrat.test(BluePlaquesSub.ppp, nx = 6, ny = 6)
#会有一个警告：— 一些观察到的计数非常小（0），这可能会影响象限检验的准确性。
#nx和ny可以改变，是规定n*n个网格分析

plot(BluePlaquesSub.ppp,pch=16,cex=0.5, main="Blue Plaques in Harrow")
plot(teststats, add=T, col = "purple")
#在新的图中，我们可以看到每个象限的三个数字。左上角的数字是观察到的点数；
#右上角是泊松期望点数；
#底部的值是残差值（也称为皮尔逊残差值），或 (观察值 - 预期值) / Sqrt(预期值)。


# Ripley’s K
#解决二次分析局限性的一种方法是将观察到的点分布与一系列不同距离半径的泊松随机模
#型进行比较。这就是 Ripley 的 K 函数所计算的。
K <- BluePlaquesSub.ppp %>%
  Kest(., correction="border") %>%
  plot()

Kval <- as.data.frame(Kest(BluePlaquesSub.ppp, correction = "Ripley"))
#当 K 值高于该线时，数据似乎在该距离处聚集。当 K 值低于该线时，数据分散。

#基于密度的噪声应用空间聚类：DBSCAN
#Quadrat 和 Ripley 的 K 分析是有用的探索性技术，可以告诉我们点数据中是否存在空间聚类，
#但它们无法告诉我们聚类发生在我们感兴趣的区域中的哪个位置。
#要发现这一点，我们需要使用替代技术。
#一种用于在空间（无论是物理空间还是变量空间）中发现聚类的流行技术是 DBSCAN
library(fpc)


#first check the coordinate reference system of the Harrow spatial polygon:
st_geometry(BoroughMap)

#DBSCAN 要求您输入两个参数：
#1. Epsilon - 这是算法搜索聚类的半径 2. MinPts - 这是应被视为聚类的最小点数

#根据之前 Ripley's K 分析的结果，我们可以看到，我们得到的聚类半径约为 1200 米，
#图中最大的凸起处约为 700 米。
#因此，700 米可能是一个不错的起点，我们将从搜索至少 4 个点的聚类开始……
#first extract the points from the spatial points data frame 首先从空间点数据帧中提取点
BluePlaquesSubPoints <- BluePlaquesSub %>%
  coordinates(.)%>%
  as.data.frame()

#now run the dbscan analysis
db <- BluePlaquesSubPoints %>%
  fpc::dbscan(.,eps = 700, MinPts = 4)

#now plot the results
plot(db, BluePlaquesSubPoints, main = "DBSCAN Output", frame = F)
plot(BoroughMap$geometry, add=T)

# used to find suitable eps value based on the knee in plot
# k is no of nearest neighbours used, use min points
#用于根据图k中的knee找到合适的eps值。如果没有使用最近邻，请使用最小值
library(dbscan)

BluePlaquesSubPoints%>%
  dbscan::kNNdistplot(.,k=4)
#此图显示每个点到 k 个邻居的平均距离，然后按升序绘制。
#拐点是此值（到邻居的距离）增加的地方。
#DBSCAN 分析表明，对于这些 eps 和 MinPts 值，我正在分析的区域中有三个聚类。



#上面的图有点基础，看起来不太美观。用ggplot2来生成更酷的地图……
library(ggplot2)
#我们的新 db 对象包含大量信息，包括每组点坐标所属的群集，
#该点是种子点还是边界点等。我们只需调用对象即可获得摘要
db
#如果你在 RStudio 的环境窗口中打开该对象，你还将看到对象中的各种插槽，包括集群
db$cluster
#将这个集群成员信息添加回我们的数据框中
BluePlaquesSubPoints<- BluePlaquesSubPoints %>%
  mutate(dbcluster=db$cluster)
#创建一些凸包多边形来包裹我们聚类中的点。
chulls <- BluePlaquesSubPoints %>%
  group_by(dbcluster) %>%
  dplyr::mutate(hull = 1:n(),
                hull = factor(hull, chull(coords.x1, coords.x2)))%>%
  arrange(hull)
#chulls2 <- ddply(BluePlaquesSubPoints, .(dbcluster), 
#  function(df) df[chull(df$coords.x1, df$coords.x2), ])

#由于 0 实际上不是一个聚类（它是所有不属于聚类的点），因此将其从数据框中删除
chulls <- chulls %>%
  filter(dbcluster >=1)

# ggplot2根据我们的数据创建一个对象
dbplot <- ggplot(data=BluePlaquesSubPoints, 
                 aes(coords.x1,coords.x2, colour=dbcluster, fill=dbcluster)) 
#add the points in
dbplot <- dbplot + geom_point()
#now the convex hulls
dbplot <- dbplot + geom_polygon(data = chulls, 
                                aes(coords.x1,coords.x2, group=dbcluster), 
                                alpha = 0.5) 
#now plot, setting the coordinates to scale correctly and as a black and white plot 
#(just for the hell of it)...
dbplot + theme_bw() + coord_equal()


#添加底图
##First get the bbox in lat long for Harrow
HarrowWGSbb <- Harrow %>%
  st_transform(., 4326)%>%
  st_bbox()

#将底图转换为英国国家网格
library(OpenStreetMap)

basemap <- OpenStreetMap::openmap(c(51.5549876,-0.4040502),c(51.6405356,-0.2671315),
                                  zoom=NULL,
                                  "osm")

# convert the basemap to British National Grid
basemap_bng <- openproj(basemap, projection="+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +nadgrids=OSTN15_NTv2_OSGBtoETRS.gsb +units=m +no_defs +type=crs")
#请注意，这个长字符串是英国国家电网（EPSG 27700）的PROJ4。
#在过去，我们可以指定“+init=epsg:27700”。然而，现在我们必须在EPSG网站上找到PROJ4
#网址：https://epsg.io/27700

#绘制带有聚类的精美地图
#autoplot(basemap_bng) sometimes works
autoplot.OpenStreetMap(basemap_bng)+ 
  geom_point(data=BluePlaquesSubPoints, 
             aes(coords.x1,coords.x2, 
                 colour=dbcluster, 
                 fill=dbcluster)) + 
  geom_polygon(data = chulls, 
               aes(coords.x1,coords.x2, 
                   group=dbcluster,
                   fill=dbcluster), 
               alpha = 0.5)  





