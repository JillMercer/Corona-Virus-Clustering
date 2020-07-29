#CPCS 
#Creates clusters based on death rates geographically with Corona Virus

corona=read.csv("corona.csv")
fixed=read.csv("corona.csv")
deaths=read.csv("deaths.csv")
testData=corona
testData$class=NULL
testData[1:7]=NULL
city=testData[5]
testData[5]=NULL
cluster=kmeans(na.omit(testData), 5)
cluster$size
plot(testData$Confirmed,testData$Deaths, col=cluster$cluster)
par(mfrow=c(1,1))
plot(corona$Country_Region,corona$Deaths, col=corona$Country_Region)
View(corona) 
install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

world=ne_countries(scale="medium", returnclass="sf")
class(world)


site=data.frame(longitude=c(fixed$Long_),latitude=c(fixed$Lat))
site
#mainland
ggplot(data = world) +
  geom_sf() +
  geom_point(data = site, aes(x = longitude, y = latitude), size = 0.1, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-155, -66), ylim = c(24, 50), expand = FALSE)
#hawaii
ggplot(data = world) +
  geom_sf() +
  geom_point(data = site, aes(x = longitude, y = latitude), size = 1, 
             shape = 23, fill = "green") +
  coord_sf(xlim = c(-160, -154.5), ylim = c(18.5, 22.5), expand = FALSE)
#alaska
ggplot(data = world) +
  geom_sf() +
  geom_point(data = site, aes(x = longitude, y = latitude), size = 1, 
             shape = 23, fill = "green") +
  coord_sf(xlim = c(-180, -140), ylim = c(50, 72), expand = FALSE)

cluster=kmeans(na.omit(deaths[, 3:5]),5)
cluster$size

dd=cbind(na.omit(deaths),cluster=cluster$cluster)

attach(dd)
ggplot(dd, aes(Long_,Lat, color = cluster)) + geom_point(shape=dd$cluster)
ggplot()
dd
plot(cluster, data=deaths[, 3:5])
cluster
deaths[, 3]


