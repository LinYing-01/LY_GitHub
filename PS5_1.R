install.packages("sp")
install.packages("rgdal")
install.packages("sf")
install.packages("raster")
library("sp")
library("rgdal")
library("sf")
library("raster")


#1.1
#见文件夹截图
#1.2
China_map <- readOGR("F:/R/Assignment/A5/China_map", "bou2_4p")
#六月处于夏季，一般降雨等条件较丰富，所以以六月为例
Myraster <- raster('F:/R/Assignment/A5/wc2.1_2.5m_wind/wc2.1_2.5m_wind_06.tif')
myraster_prec <- raster("F:/R/Assignment/A5/wc2.1_2.5m_prec/wc2.1_2.5m_prec_06.tif")
myraster_srad <- raster("F:/R/Assignment/A5/wc2.1_2.5m_srad/wc2.1_2.5m_srad_06.tif")
plot(Myraster,main="Wind in Jun")
R_crop <- crop(Myraster,China_map)
R_mask <- mask(R_crop,China_map)
crop_prec <- crop(myraster_prec,China_map)
crop_srad <- crop(myraster_srad,China_map)
mask_prec <- mask(crop_prec,China_map)
mask_srad <- mask(crop_srad,China_map)
#mask()参考了“R语言论坛”的帖子
plot(R_mask, main="China_wind in Jun")
plot(mask_prec, main="China_prec in Jun")
plot(mask_srad, main="China_srad in Jun")

#1.3
R_mask
#image
#set color
col <- terrain.colors(30)
image(R_mask, col=col, main="China_wind contour in Jun")
contour(R_mask, add=T, level=5, col="black")
#查看max最大为9.844，level高于5以后画图基本看不出来，所以选择5
#选址位于世界“第三极”——青藏高原地区、高原以北等地（建起来难度
#可能有点大，但是我很喜欢favorite）

#1.4
myraster_prec
#最小值为0，最大值为2226，经过调试，选址介于50-100之间
plot(mask_prec, col=col, main="China_prec contour in Jun")
contour(mask_prec, add=T, col="blue", level=100)
contour(mask_prec, add=T, col="blue", level=50)

myraster_srad
#values : 0, 30552(min, max), 选址在20000和22000之间
plot(mask_srad, col=col, main="China_srad contour in Jun")
contour(mask_srad, add=T, level=20000, col="red")
contour(mask_srad, add=T, level=22000, col="red")
#如图，两者交叉区域可作为PV farms选址
