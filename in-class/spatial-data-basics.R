#
# Spatial Data with Terra
# https://rspatial.org/spatial/2-spatialdata.html

# # # # #
# Simple representation of spatial data
name <- LETTERS[1:10]
longitude <- c(-116.7, -120.4, -116.7, -113.5, -115.5,
               -120.8, -119.5, -113.7, -113.7, -110.7)
latitude <- c(45.3, 42.6, 38.9, 42.1, 35.7, 38.9,
              36.2, 39, 41.6, 36.9)
stations <- cbind(longitude, latitude)
# Simulated rainfall data
set.seed(0)
precip <- round((runif(length(latitude))*10)^3)

# Plot
psize <- 1 + precip/500
plot(stations, cex=psize, pch=20, col='red', main='Precipitation')
# add names to plot
text(stations, name, pos=4)
# add a legend
breaks <- c(100, 250, 500, 1000)
legend.psize <- 1+breaks/500
legend("topright", legend=breaks, pch=20, pt.cex=legend.psize, col='red', bg='gray')

# Lines and polygons
lon <- c(-116.8, -114.2, -112.9, -111.9, -114.2, -115.4, -117.7)
lat <- c(41.3, 42.9, 42.4, 39.8, 37.6, 38.3, 37.6)
x <- cbind(lon, lat)
plot(stations, main='Precipitation')
polygon(x, col='blue', border='light blue')
lines(stations, lwd=3, col='red')
points(x, cex=2, pch=20)
points(stations, cex=psize, pch=20, col='red', main='Precipitation')

wst <- data.frame(longitude, latitude, name, precip)
wst

# # # # #
# Vector 
# https://rspatial.org/spatial/3-vectordata.html
# 

longitude <- c(-116.7, -120.4, -116.7, -113.5, -115.5, -120.8, -119.5, -113.7, -113.7, -110.7)
latitude <- c(45.3, 42.6, 38.9, 42.1, 35.7, 38.9, 36.2, 39, 41.6, 36.9)
lonlat <- cbind(longitude, latitude)

library(terra)
pts <- vect(lonlat)

# data structure
class(pts)
pts
geom(pts)

# crs
crdref <- "+proj=longlat +datum=WGS84"
pts <- vect(lonlat, crs=crdref)
pts
cat(crs(pts))

# Generate random precipitation values, same quantity as points
precipvalue <- runif(nrow(lonlat), min=0, max=100)
df <- data.frame(ID=1:nrow(lonlat), precip=precipvalue)

# combine spatvect with df
ptv <- vect(lonlat, atts=df, crs=crdref)
ptv

## Lines and Polygons
lon <- c(-116.8, -114.2, -112.9, -111.9, -114.2, -115.4, -117.7)
lat <- c(41.3, 42.9, 42.4, 39.8, 37.6, 38.3, 37.6)
lonlat <- cbind(id=1, part=1, lon, lat)
lonlat

lns <- vect(lonlat, type="lines", crs=crdref)
lns

pols <- vect(lonlat, type="polygons", crs=crdref)
pols

# plot
plot(pols, las=1)
plot(pols, border='blue', col='yellow', lwd=3, add=TRUE)
points(pts, col='red', pch=20, cex=3)

# saved shape
# Diekirch is a commune with town status in north-eastern Luxembourg
f <- system.file("ex/lux.shp", package="terra")
f
v <- vect(f)
v

# # # # # #
# Raster
# https://rspatial.org/spatial/4-rasterdata.html

library(terra) ## terra 1.7.62
# only geometry
r <- rast(ncol=10, nrow=10, xmin=-150, xmax=-80, ymin=20, ymax=60)
r

# assign random values
values(r) <- runif(ncell(r))
r
plot(r)

# reassign by cell numbers
values(r) <- 1:ncell(r)
r
# add polygon and points
plot(r)
lon <- c(-116.8, -114.2, -112.9, -111.9, -114.2, -115.4, -117.7)
lat <- c(41.3, 42.9, 42.4, 39.8, 37.6, 38.3, 37.6)
lonlat <- cbind(id=1, part=1, lon, lat)
pts <- vect(lonlat)
pols <- vect(lonlat, type="polygons", crs="+proj=longlat +datum=WGS84")
points(pts, col="red", pch=20, cex=3)
lines(pols, col="blue", lwd=2)

# create multilayer object
r2 <- r * r
r3  <- sqrt(r)
s <- c(r, r2, r3)
s
plot(s)

# # # # #
# Coordinate reference system
library(terra)
## terra 1.7.62
f <- system.file("ex/lux.shp", package="terra")
p <- vect(f)
p
crs(p)
plot(p, "NAME_2")
plot(p, "POP")

# assigning
pp <- p
crs(pp) <- ""
crs(pp)
## [1] ""
crs(pp) <- "+proj=longlat +datum=WGS84"
crs(pp)

# transforming
newcrs <- "+proj=robin +datum=WGS84"
rob <- terra::project(p, newcrs)
rob
# and back
p2 <- terra::project(rob, "+proj=longlat +datum=WGS84")
plot(rob)

# transforming raster
r <- rast(xmin=-110, xmax=-90, ymin=40, ymax=60, ncols=40, nrows=40)
values(r) <- 1:ncell(r)
r
plot(r)

newcrs
## [1] "+proj=robin +datum=WGS84"
pr1 <- terra::project(r, newcrs)
crs(pr1)
plot(pr1)

# # # # #
# vector data manipulation
f <- system.file("ex/lux.shp", package="terra")
p <- vect(f)
p
crs(p)
plot(p, "NAME_2")
plot(p, "POP")

# extract attributes
d <- as.data.frame(p)
head(d)

# extract geometry
g <- geom(p)
head(g)

# extract well-known-text wkt
g <- geom(p, wkt=TRUE)
substr(g, 1, 50)

# extract variable informatio
p$NAME_2
p[, "NAME_2"]

# add new variables
set.seed(0)
p$lets <- sample(letters, nrow(p))
p

# get lengths
perim(p)

# reassign variable
p$lets <- sample(LETTERS, nrow(p))
head(p)

# get rid of variable
p$lets <- NULL


# # # # # #
# merge
dfr <- data.frame(District=p$NAME_1, Canton=p$NAME_2, Value=round(runif(length(p), 100, 1000)))
dfr <- dfr[order(dfr$Canton), ]
pm <- merge(p, dfr, by.x=c('NAME_1', 'NAME_2'), by.y=c('District', 'Canton'))
pm
head(pm)

# extract single record
i <- which(p$NAME_1 == 'Grevenmacher')
g <- p[i,]
g

# change polygon to rast and subset a single polygon
# append
z <- rast(p)
dim(z) <- c(2,2)
values(z) <- 1:4
names(z) <- 'Zone'
# coerce SpatRaster to SpatVector polygons
z <- as.polygons(z)
z
#
z2 <- z[2,]
plot(p)
plot(z, add=TRUE, border='blue', lwd=5)
plot(z2, add=TRUE, border='red', lwd=2, col='red')

# append
b <- rbind(p, z)
# with older versions
# b <- c(p, z)
head(b)
tail(b)

# Aggregate/dissolve
pa <- aggregate(p, by='NAME_1')
za <- aggregate(z)
plot(za, col='light gray', border='light gray', lwd=5)
plot(pa, add=TRUE, col=rainbow(3), lwd=3, border='white')

# aggregate without dissolving borders
zag <- aggregate(z, dissolve=FALSE)
zag
plot(zag, col="light gray")

# split back apart
zd <- disagg(zag)
zd

# # # # #
# Overalay

# erase
e <- erase(p, z2)
plot(e)

# intersect
i <- intersect(p, z2)
plot(i)

# crop
e <- ext(6, 6.4, 49.7, 50)
pe <- crop(p, e)
plot(p)
plot(e, add=TRUE, lwd=3, col="red")
plot(pe, col='light blue', add=TRUE)
plot(e, add=TRUE, lwd=3, border="blue")

# Union
u <- terra::union(p, z)
u
set.seed(5)
plot(u, col=sample(rainbow(length(u))))

# Cover
cov <- cover(p, z[c(1,4),])
cov
plot(cov)
