library(rgdal)
library(ggplot2)
source('../helper-functions.r', chdir=T)

nz <- readOGR('shapefiles', 'bigislands', p4s="+init=epsg:4326")
nz <- spTransform(nz, CRS("+init=epsg:2193"))


## All traffic
traff0 <- readOGR('shapefiles/Traffic Volumes', 'Traffic_All_Volumes_2012_WFS')
traff0 <- spTransform(traff0, CRS("+init=epsg:2193"))
traff0$traffic_ba <- factor(traff0$traffic_ba,
                            levels=c("< 1000", "1000 - 4000", "4000 - 10000", "10000 - 20000", "> 20000"))
traff0$traffic_ba <- rename.levels(traff0$traffic_ba,
                 c("< 1000",     "1000 - 4000",    "4000 - 10000",    "10000 - 20000",    "> 20000"),
                 c("0 to 1,000", "1,000 to 4,000", "4,000 to 10,000", "More than 10,000", "More than 10,000"))
levels(traff0$traffic_ba) <- c("0 to 1,000", "1,000 to 4,000", "4,000 to 10,000", "More than 10,000")

hightraff0 <- subset(traff0, adt >= 35000 | loading_he >= 1200)


## Turn to data frames for ggplot
nz <- fortify(nz)
traff <- fortify(traff0, region='traffic_ba')
traff$traff <- traff0$traffic_ba[match(traff$id, rownames(traff0@data))]
hightraff <- fortify(hightraff0, region='traffic_ba')
hightraff$traff <- hightraff0$traffic_ba[match(hightraff$id, rownames(hightraff0@data))]


save(nz, traff, hightraff, file='fortified-shapefiles.rdata')
