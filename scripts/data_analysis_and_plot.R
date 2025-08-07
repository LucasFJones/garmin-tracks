library(ggplot2)
library(raster)
library(sp)
library(XML)
library(OpenStreetMap)
library(lubridate)
library(ggmap)
library(gridExtra)
library(grid)
library(ggplotify)
library(vcd)
library(gridGraphics)


## PROCESSING
shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }

col1 <- seq(0,100,5)

col2 <- seq(200, 100, -5)

my_df <- data.frame(c1= col1, c2= col2)

my_df
my_df$nc1 <- shift.vec(my_df$c1, -1)
my_df$nc2 <- shift.vec(my_df$c2, -1)
my_df

pfile <- htmlTreeParse(file = "your/data", error = function(...) {
}, useInternalNodes = T)

elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
times <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)

str(coords)
lats <- as.numeric(coords["lat",])
lons <- as.numeric(coords["lon",])
lons
lats
elevations
geodf <- data.frame(lat = lats, lon = lons, time = times)

rm(list=c( "lats", "lons", "pfile", "times", "coords"))
head(geodf)
geodf$lat.p1 <- shift.vec(geodf$lat, -1)
geodf$lon.p1 <- shift.vec(geodf$lon, -1)
head(geodf)

geodf$dist.to.prev <- apply(geodf, 1, FUN = function (row) {
  pointDistance(c(as.numeric(row["lat.p1"]),
                  as.numeric(row["lon.p1"])),
                c(as.numeric(row["lat"]), as.numeric(row["lon"])),
                lonlat = T)
})

head(geodf$dist.to.prev)
td <- sum(geodf$dist.to.prev, na.rm=TRUE)
print(paste("The distance run was ", td, " meters"))
# Transform the column ‘time’ so that R knows how to interpret it.
geodf$time <- strptime(geodf$time, format = "%Y-%m-%dT%H:%M:%OS")
# Shift the time vector, too.
geodf$time.p1 <- shift.vec(geodf$time, -1)
# Calculate the number of seconds between two positions.
geodf$time.diff.to.prev <- as.numeric(difftime(geodf$time.p1, geodf$time))

head(geodf$time.diff.to.prev, n=15) 

geodf$speed.m.per.sec <- geodf$dist.to.prev / geodf$time.diff.to.prev
geodf$speed.km.per.h <- geodf$speed.m.per.sec * 3.6
geodf$speed.knots <- geodf$speed.km.per.h * 0.54
geodf$speed.knots <- geodf$speed.m.per.sec *1.94384
geodf$speed.km.per.h <- ifelse(is.na(geodf$speed.km.per.h), 0, geodf$speed.km.per.h)
geodf$lowess.speed <- lowess(geodf$speed.knots, f = 0.2)$y
geodf$lowess.ele <- lowess(geodf$ele, f = 0.2)$y

grab_grob <- function(){
  grid.echo()
  grid.grab()
}


## Major plot
geodf2 <- geodf[-c(1:8131),]
geodf2
plot(geodf2$speed.knots, type = "l", bty = "n", xaxt = "n", ylab = "Speed (knots)", xlab = "",
     col = "grey40")
lines(geodf2$lowess.speed, col = "blue", lwd = 3)
legend(x="bottom", legend = c("GPS speed", "LOWESS speed"),
       col = c("grey40", "blue"), lwd = c(1,3), bty = "n")
abline(h = mean(geodf2$speed.km.per.h), lty = 2, col = "blue")

plot(rev(geodf2$lon), rev(geodf2$lat), type = "l", col = "red", lwd = 3, bty = "n", ylab = "Latitude", xlab = "Longitude")
lat <- c((min(geodf2$lat)-0.1), max(geodf2$lat))
lon <- c(min(geodf2$lon), (max(geodf2$lon)+0.1))
bbox <- make_bbox(lon,lat)
b1<- get_map(bbox,maptype="watercolor", source="stamen")
ggmap(b1) + geom_point(data = geodf2, 
                       aes(lon,lat), size=1, alpha=0.7) +
  labs(x = "Longitude", y = "Latitude",
       title="Figawi 2023 trip")

## DAY 1 - May 23
geodf3 <- geodf[c(8131:8525),]
geodf3
av3 <- mean(geodf3$speed.knots)
av3
par(xpd=TRUE)
plot(geodf3$speed.knots, type = "l", bty = "n", xaxt = "n", ylab = "Speed (knots)", xlab = "",
     col = "grey40",lwd=c(2))
lines(geodf3$lowess.speed, col = "blue", lwd = 3)
legend(80,1,c("GPS speed", "LOWESS speed"),
       col = c("grey40", "blue"), lwd = c(2,3), bty = "n")
mtext("May 23, Average Speed = 6.21 kn", side=3)

plot(rev(geodf3$lon), rev(geodf3$lat), type = "l", col = "red", lwd = 3, bty = "n", ylab = "Latitude", xlab = "Longitude")
#lat <- c(min(geodf3$lat), max(geodf3$lat))
#lon <- c(min(geodf3$lon), max(geodf3$lon))
bbox <- make_bbox(lon,lat)
b1<- get_map(bbox,maptype="watercolor", source="stamen")
ggmap(b1) + geom_point(data = geodf3, 
                       aes(lon,lat), size=1, alpha=0.7) +
  labs(x = "Longitude", y = "Latitude",
       title="Stonington to Point Judith")
map
#g <- grab_grob()
#grid.arrange(
#  map,g,
#  ncol=1
#)

## DAY 2 - May 24
geodf4 <- geodf[c(8526:9119),]
geodf4
av4 <- mean(geodf4$speed.knots)
av4
par(xpd=TRUE)
plot(geodf4$speed.knots, type = "l", bty = "n", xaxt = "n", ylab = "Speed (knots)", xlab = "",
     col = "grey40",lwd=c(2))
lines(geodf4$lowess.speed, col = "blue", lwd = 3)
legend(80,1,c("GPS speed", "LOWESS speed"),
       col = c("grey40", "blue"), lwd = c(2,3), bty = "n")
mtext("May 24, Average Speed = 6.86 kn", side=3)

plot(rev(geodf4$lon), rev(geodf4$lat), type = "l", col = "red", lwd = 3, bty = "n", ylab = "Latitude", xlab = "Longitude")
#lat <- c(min(geodf4$lat), max(geodf4$lat))
#lon <- c(min(geodf4$lon), max(geodf4$lon))
bbox <- make_bbox(lon,lat)
b1<- get_map(bbox,maptype="watercolor", source="stamen")
ggmap(b1) + geom_point(data = geodf4, 
                       aes(lon,lat), size=1, alpha=0.7) +
  labs(x = "Longitude", y = "Latitude",
       title="Point Judith to Menemsha")

## DAY 3 - May 25
geodf5 <- geodf[c(9120:9553),]
geodf5
av5 <- mean(geodf5$speed.knots)
av5
par(xpd=TRUE)
plot(geodf5$speed.knots, type = "l", bty = "n", xaxt = "n", ylab = "Speed (knots)", xlab = "",
     col = "grey40",lwd=c(2))
lines(geodf5$lowess.speed, col = "blue", lwd = 3)
legend(80,1,c("GPS speed", "LOWESS speed"),
       col = c("grey40", "blue"), lwd = c(2,3), bty = "n")
mtext("May 25, Average Speed = 5.11 kn", side=3)

plot(rev(geodf5$lon), rev(geodf5$lat), type = "l", col = "red", lwd = 3, bty = "n", ylab = "Latitude", xlab = "Longitude")
lat <- c(min(geodf5$lat), max(geodf5$lat))
lon <- c(min(geodf5$lon), max(geodf5$lon))
bbox <- make_bbox(lon,lat)
b1<- get_map(bbox,maptype="watercolor", source="stamen")
ggmap(b1) + geom_point(data = geodf5, 
                       aes(lon,lat), size=1, alpha=0.7) +
  labs(x = "Longitude", y = "Latitude",
       title="Menemsha to Edgartown")
## DAY 4 - May 26
geodf6 <- geodf[c(9554:9887),]
geodf6
av6 <- mean(geodf6$speed.knots)
av6
par(xpd=TRUE)
plot(geodf6$speed.knots, type = "l", bty = "n", xaxt = "n", ylab = "Speed (knots)", xlab = "",
     col = "grey40",lwd=c(2))
lines(geodf6$lowess.speed, col = "blue", lwd = 3)
legend(80,1,c("GPS speed", "LOWESS speed"),
       col = c("grey40", "blue"), lwd = c(2,3), bty = "n")
mtext("May 26, Average Speed = 4.03 kn", side=3)

plot(rev(geodf6$lon), rev(geodf6$lat), type = "l", col = "red", lwd = 3, bty = "n", ylab = "Latitude", xlab = "Longitude")
lat <- c(min(geodf6$lat), max(geodf6$lat))
lon <- c(min(geodf6$lon), max(geodf6$lon))
bbox <- make_bbox(lon,lat)
b1<- get_map(bbox,maptype="watercolor", source="stamen")
ggmap(b1) + geom_point(data = geodf6, 
                       aes(lon,lat), size=1, alpha=0.7) +
  labs(x = "Longitude", y = "Latitude",
       title="Edgartown to Hyannis")
## DAY 5 - May 27 -- RACE
geodf7 <- geodf[c(9888:10510),]
geodf7
av7 <- mean(geodf7$speed.knots)
av7
par(xpd=TRUE)
plot(geodf7$speed.knots, type = "l", bty = "n", xaxt = "n", ylab = "Speed (knots)", xlab = "",
     col = "grey40",lwd=c(2))
lines(geodf7$lowess.speed, col = "blue", lwd = 3)
legend(80,1,c("GPS speed", "LOWESS speed"),
       col = c("grey40", "blue"), lwd = c(2,3), bty = "n")
mtext("May 27 , Average Speed = 4.47 kn", side=3)

plot(rev(geodf7$lon), rev(geodf7$lat), type = "l", col = "red", lwd = 3, bty = "n", ylab = "Latitude", xlab = "Longitude")
lat <- c(min(geodf7$lat), max(geodf7$lat))
lon <- c(min(geodf7$lon), max(geodf7$lon))
bbox <- make_bbox(lon,lat)
b1<- get_map(bbox,maptype="watercolor", source="stamen")
ggmap(b1) + geom_point(data = geodf7, 
                       aes(lon,lat), size=1, alpha=0.7) +
  labs(x = "Longitude", y = "Latitude",
       title="Hyannis to Nantucket -- Figawi")
## DAY 6 - May 28
geodf8 <- geodf[c(10572:11173),]
geodf8
av8 <- mean(geodf8$speed.knots)
av8
par(xpd=TRUE)
plot(geodf8$speed.knots, type = "l", bty = "n", xaxt = "n", ylab = "Speed (knots)", xlab = "",
     col = "grey40",lwd=c(2))
lines(geodf8$lowess.speed, col = "blue", lwd = 3)
legend(80,1,c("GPS speed", "LOWESS speed"),
       col = c("grey40", "blue"), lwd = c(2,3), bty = "n")
mtext("May 28, Average Speed = 4.94 kn", side=3)

plot(rev(geodf8$lon), rev(geodf8$lat), type = "l", col = "red", lwd = 3, bty = "n", ylab = "Latitude", xlab = "Longitude")
lat <- c(min(geodf8$lat), max(geodf8$lat))
lon <- c(min(geodf8$lon), max(geodf8$lon))
bbox <- make_bbox(lon,lat)
b1<- get_map(bbox,maptype="watercolor", source="stamen")
ggmap(b1) + geom_point(data = geodf8, 
                       aes(lon,lat), size=1, alpha=0.7) +
  labs(x = "Longitude", y = "Latitude",
       title="Nantucket to Edgartown")
## DAY 7 - May 29 -- STORM
## DAY 8 - May 30 -- HOME
geodf9 <- geodf[c(11177:12711),]
geodf9
av9 <- mean(geodf9$speed.knots)
av9
par(xpd=TRUE)
plot(geodf9$speed.knots, type = "l", bty = "n", xaxt = "n", ylab = "Speed (knots)", xlab = "",
     col = "grey40",lwd=c(2))
lines(mean(geodf9$lowess.speed), col = "blue", lwd = 3)
legend(80,1,c("GPS speed", "LOWESS speed"),
       col = c("grey40", "blue"), lwd = c(2,3), bty = "n")
mtext("May 30, Average Speed = 8.3 kn", side=3)

plot(rev(geodf9$lon), rev(geodf9$lat), type = "l", col = "red", lwd = 3, bty = "n", ylab = "Latitude", xlab = "Longitude")
lat <- c(min(geodf9$lat), max(geodf9$lat))
lon <- c(min(geodf9$lon), max(geodf9$lon))
bbox <- make_bbox(lon,lat)
b1<- get_map(bbox,maptype="watercolor", source="stamen")
ggmap(b1) + geom_point(data = geodf9, 
                       aes(lon,lat), size=1, alpha=0.7) +
  labs(x = "Longitude", y = "Latitude",
       title="Edgartown to Stonington")
