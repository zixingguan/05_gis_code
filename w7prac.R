library(here)
library(janitor)
library(sf)
library(tidyverse)

#read the ward data in
LondonWards <- st_read(here::here("statistical-gis-boundaries-london","ESRI","London_Ward.shp"))%>%
  st_transform(.,27700)

LondonWardsMerged <- st_read(here::here("statistical-gis-boundaries-london", 
                                        "ESRI",
                                        "London_Ward_CityMerged.shp"))%>%
  st_transform(.,27700)

WardData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                     locale = locale(encoding = "latin1"),
                     na = c("NA", "n/a")) %>% 
  clean_names()

LondonWardsMerged <- LondonWardsMerged %>% 
  left_join(WardData, 
            by = c("GSS_CODE" = "new_code"))%>%
  dplyr::distinct(GSS_CODE, .keep_all = T)%>%
  dplyr::select(GSS_CODE, ward_name, average_gcse_capped_point_scores_2014)


#have a look to check that it's 
#in the right projection
st_crs(LondonWardsMerged)


library(tmap)
BluePlaques <- st_read("https://s3.eu-west-2.amazonaws.com/openplaques/open-plaques-london-2018-04-08.geojson")%>%
  st_transform(.,27700)

tmap_mode("plot")
tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaques) +
  tm_dots(col = "blue")

#数据清理
#去掉伦敦城外的蓝色牌匾
summary(BluePlaques)

BluePlaquesSub <- BluePlaques[LondonWardsMerged,]

tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")


#数据处理
example<-st_intersects(LondonWardsMerged, BluePlaquesSub)

example

check_example <- LondonWardsMerged%>%
  st_join(BluePlaquesSub)%>%
  filter(ward_name=="Kingston upon Thames - Coombe Hill")


#现在我们只需取每个多边形的每个列表的长度并将其添加为新列
#就可以知道每个ward里有多少个蓝色牌匾
library(sf)
points_sf_joined <- LondonWardsMerged%>%
  mutate(n = lengths(st_intersects(., BluePlaquesSub)))%>%
  janitor::clean_names()%>%
  
  #calculate area
  mutate(area=st_area(.))%>%
  
  #then density of the points per ward
  mutate(density=n/area)%>%
  
  #select density and some other variables 
  dplyr::select(density, ward_name, gss_code, n, average_gcse_capped_point_scores_2014)

#绘制一张分级统计地图
points_sf_joined<- points_sf_joined %>%                    
  group_by(gss_code) %>%         
  summarise(density = first(density),
            wardname= first(ward_name),
            plaquecount= first(n))

tm_shape(points_sf_joined) +
  tm_polygons("density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("wardname", "density"),
              title="Blue Plaque Density")

#从地图上看，伦敦市中心似乎有一些蓝色斑块聚集，
#所以让我们用 Moran's I 和其他一些统计数据来检查一下
#为了能够计算Moran's I和任何类似的统计量，我们需要首先定义一个Wij空间权重矩阵
library(spdep)
#首先计算伦敦所有区的质心
coordsW <- points_sf_joined%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)

#create a neighbours list
library(spdep)
LWard_nb <- points_sf_joined %>%
  poly2nb(., queen=T)
summary(LWard_nb)
#这里告诉我们邻居的平均数量是 5.88

#绘制邻居
#plot them
plot(LWard_nb, st_geometry(coordsW), col="orange")
#map下面添加一个贴图：points_sf_joined
plot(points_sf_joined$geometry, add=T)

#空间权重矩阵
#create a spatial weights matrix from these weights
Lward.lw <- LWard_nb %>%
  nb2mat(., style="B")

sum(Lward.lw)
sum(Lward.lw[1,])


#自相关
#莫兰的I需要一个空间权重列表类型的对象，而不是矩阵。
#所以要将矩阵变成列表
Lward.lw <- LWard_nb %>%
  nb2listw(., style="C")

#Moran 的 I
#Moran 的 I 检验告诉我们我们是否有聚类值（接近于 1）或分散值（接近于 -1）
#我们将计算密度而不是原始值，因为不同区域的面积大小不一样
I_LWard_Global_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%  #提取数据框中名为 density 的列，作为一个向量
  moran.test(., Lward.lw)

I_LWard_Global_Density

#Geary 的 C
C_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  geary.test(., Lward.lw)

C_LWard_Global_Density

# Getis Ord
G_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  globalG.test(., Lward.lw)

G_LWard_Global_Density

#Moran's I 统计量 = 0.67（记住 1 = 聚类，0 = 无模式，-1 = 分散）
#这表明我们有一些独特的聚类

#Geary 的 C 统计量 = 0.41（记住 Geary 的 C 介于 0 和 2 之间；1 表示没有空间自相关，
#<1 - 正空间自相关或相似值聚类，>1 - 负空间自相关或不相似值聚类）
#表明相似值是聚类

#一般 G 统计量 = G > 预期，
#因此高值趋于聚类。
#因此，全球统计数据表明，伦敦的蓝牌匾存在空间自相关性


#use the localmoran 局部莫兰 function to generate I for each ward in the city

I_LWard_Local_count <- points_sf_joined %>%
  pull(plaquecount) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

I_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

#what does the output (the localMoran object) look like?
slice_head(I_LWard_Local_Density, n=5)

#我们想将一些列（I 分数（第 1 列）和 z 分数标准差（第 4 列））复制回
#LondonWards 空间多边形数据框
points_sf_joined <- points_sf_joined %>%
  mutate(plaque_count_I = as.numeric(I_LWard_Local_count$Ii))%>%
  mutate(plaque_count_Iz =as.numeric(I_LWard_Local_count$Z.Ii))%>%
  mutate(density_I =as.numeric(I_LWard_Local_Density$Ii))%>%
  mutate(density_Iz =as.numeric(I_LWard_Local_Density$Z.Ii))

#mapping
#绘制局部 Moran's I 输出的地图……

#我们将根据以下规则手动设置断点：
#距离平均值 2.58 或 <-2.58 个标准差的数据点在 99% 的水平上显著
#（自相关的不存在概率 <1%）；
#>1.96 - <2.58 或 <-1.96 至 >-2.58 个标准差在 95% 的水平上显著
#>#（自相关的不存在概率 <5%）。>1.65 = 90% 等等。

breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
#现在创建一个新的发散颜色酿造调色板，并使用（反向）反转顺序rev()，
#以便更高的值对应于红色

library(RColorBrewer)
MoranColours<- rev(brewer.pal(8, "RdGy"))

#在交互式地图上绘制
tm_shape(points_sf_joined) +
  tm_polygons("plaque_count_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Blue Plaques in London")


#Local Getis Ord  
Gi_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localG(., Lward.lw)

head(Gi_LWard_Local_Density)

#将G添加到sf的数据框中
points_sf_joined <- points_sf_joined %>%
  mutate(density_G = as.numeric(Gi_LWard_Local_Density))

#映射
library(RColorBrewer)

GIColours<- rev(brewer.pal(8, "RdBu"))

#now plot on an interactive map
tm_shape(points_sf_joined) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, Blue Plaques in London")


#看看平均 GSCE 分数……
#use head to see what other variables are in the data file
slice_head(points_sf_joined, n=2)

#or（这个方法需要删掉几何类型
Datatypelist <- LondonWardsMerged %>% 
  st_drop_geometry()%>%
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

I_LWard_Local_GCSE <- LondonWardsMerged %>%
  arrange(GSS_CODE)%>%
  pull(average_gcse_capped_point_scores_2014) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

points_sf_joined <- points_sf_joined %>%
  arrange(gss_code)%>%
  mutate(GCSE_LocIz = as.numeric(I_LWard_Local_GCSE$Z.Ii))


tm_shape(points_sf_joined) +
  tm_polygons("GCSE_LocIz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, GCSE Scores")

#Gi* 统计数据用于查看高分和低分的聚类
G_LWard_Local_GCSE <- LondonWardsMerged %>%
  dplyr::arrange(GSS_CODE)%>%
  dplyr::pull(average_gcse_capped_point_scores_2014) %>%
  as.vector()%>%
  localG(., Lward.lw)

points_sf_joined <- points_sf_joined %>%
  dplyr::arrange(gss_code)%>%
  dplyr::mutate(GCSE_LocGiz = as.numeric(G_LWard_Local_GCSE))

tm_shape(points_sf_joined) +
  tm_polygons("GCSE_LocGiz",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, GCSE Scores")


