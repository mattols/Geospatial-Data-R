
library(terra)

# DEM
dem = rast("data/KarakoramSRTM/SRTM_Karakoram_reproj.tif")
# dem = rast("data/KarakoramSRTM/kara_srtm_mosaic.tif")
dim(dem)
plot(dem)
plot(dem, type='interval', breaks=4)


kshp = vect("data/shapefiles/karakoram.shp")
writeVector(k, "data/shapefiles/karakoram_outline.geojson", filetype="GeoJSON")
plot(kshp,add=T,border='blue')

# mosaic files
# project image

# glacier data?
load("data/glaciers.RData")
writeVector(g, "data/shapefiles/kara.geojson", filetype="GeoJSON")
g = vect("data/shapefiles/kara.geojson")
head(g)
dim(g)

library(dplyr);library(ggplot2)
as.data.frame(g) %>% ggplot() +
  geom_point(aes(x = Lmax, y = Area, colour = Slope))

# glaciers - which biggest?
# relationship between Lmax and Area (estimate)

as.data.frame(g) %>% arrange(desc(Area)) %>% 
  select(Name, Area, Zmin, Zmax, Zmed, Slope, Aspect, Lmax) %>% head(10)

g2 = g[order(g$Area, decreasing = TRUE)[1:10],]
g2

g3 = g[order(g$Area, decreasing = TRUE)[1:100],]
g3

g4 = g3[order(g3$Aspect)]

# separate north vs south facing glaciers
fn = list.files("data/MODSCAG_2014", pattern=".tif", full.names = TRUE)

sca = rast(fn)

# regex
strsplit(fn[1], "\\.")
strsplit(fn[1], "\\.")[[1]][3]
gsub(".*(h.*).006.*$","\\1", fn[1])

unique(gsub(".*(h.*).006.*$","\\1", fn))

mtiles = gsub(".*(h.*).006.*$","\\1", fn)
mdates = gsub(".*GA.A(.*).h.*$","\\1", fn)
mdates2 = as.Date(mdates, "%Y%j")
mmo = as.numeric(format(mdates2, "%m"))

fnspr = fn[mmo >= 4 & mmo <= 6]

# fraction of cloud cover
vals = global(rast(fnspr[c(TRUE,FALSE)]) == 250,"sum", na.rm=T)$sum / ncell(rast(fnspr[1]))
plot(vals~as.Date(gsub(".*GA.A(.*).h.*$","\\1", fnspr),
                  "%Y%j")[c(TRUE,FALSE)], xlab="date")

spring_fn = fnspr[c(TRUE,FALSE)][which(vals < 0.05)][seq(1,18, 3)]
spmdates = gsub(".*GA.A(.*).h.*$","\\1", spring_fn)
match(spmdates, mdates)

# final mostly cloud-free indices
fix = sort(c(match(spmdates, mdates), match(spmdates, mdates) + 1) )

sp2 = paste0("2014", c("100","118", "130", "149", "157", "166", "178"))
fix = sort(c(match(sp2, mdates), match(sp2, mdates) + 1) )

# rst1 = rast(fnspr[c(TRUE,FALSE)])
# rst1[rst1>100] = NA
# vals = global(rst1,"mean", na.rm=T)$mean / ncell(rast(fnspr[1]))

# h23v05 | h24v05 |

# fn2
fn23 = list.files("data/MODSCAG_2014", pattern="h23v05.*.tif", full.names = TRUE)
fn24 = list.files("data/MODSCAG_2014", pattern="h24v05.*.tif", full.names = TRUE)

sca23 = rast(fn23)
sca24 = rast(fn24)

plot(mosaic(sca23[[110]], sca24[[110]]))

# test plot
proj_test = project(mosaic(sca23[[110]], sca24[[110]]), crs(g2))
plot(proj_test)
plot(centroids(g2), "Name", add=T)
bbox24 = as.polygons(ext(sca24[[110]]))
crs(bbox24) <- crs(sca24)
plot(project(bbox24 , crs(g2)), add=T, border='red')

s23mean = global(sca23,"mean", na.rm=T)

plot(s23mean$mean[s23mean$mean<100])
lines(s23mean$mean[s23mean$mean<100])

grepl("h23v05", fn)

# x <- do.call(merge, fn) 


fnn = fn[fix]

# fun
stk.mosaic = function(ls){
  project(mosaic(ls[[1]], ls[[2]]), "EPSG:4326")
}

rls = lapply(fnn, rast)
mv = lapply(seq(1,(length(rls)-1), 2), function(x) stk.mosaic(rls[x:(x+1)]))
mv
plot(mv[[1]])

mvs = crop(rast(mv), kshp, mask=T)
mvs[mvs>100]=NA

md = mvs[[7]] - mvs[[1]]
plot(md)
rdem = resample(dem, md)
plot(values(md)~values(rdem))
plot(mvs[[c(1,7)]])
bb = as.polygons(ext(rast(fnn[2])))
crs(bb) <- crs(rast(fnn[2]))
plot(project(bb , crs(mvs)), add=T, border='red')
# ave values by date
plot(global(mvs, "mean", na.rm=T)$mean)

m <- c(1000, 3000, 1, # min, max, new value
       3000, 5000, 2,
       5000, 9000, 3)
rcl_mat <- matrix(m, ncol=3, byrow=TRUE) # create matrix
dcl <-  classify(rdem, rcl_mat)
plot(dcl)

z = zonal(md, dcl, fun="mean",na.rm=T)
z

# Snow cover in the karakoram
# Question: how does snow cover change from spring to summer across different elevations in the Karakoram region
# which zones lose snow most quickly in spring (3-5 zones)

# outline is region of interest
# format of image
# mosaic, crop, mask,
# key - clouds etc.
# sca values > 20
# calculate total sca by elevation zones (percentage)
# which area is losing fastest - linear model? - spring melt rate? (basin loses X % of snow cover per day or how much is lost between each date)
# share regex for dates

# what percentage or area of the basin melts per day - crude estimate (can use full range of days)

mvs2 = mvs >20
plot(app(mvs2, "sum"))
zonal(app(mvs2, "sum"), dcl, fun="mean",na.rm=T)

dft = zonal(mvs2, dcl, fun="sum",na.rm=T)
dtable = table(values(dcl))
dtable
dim(dft)

colnames(dft) <-  c("Zones",gsub(".*GA.A(.*).h.*$","\\1", colnames(dft)[2:8]))
library(tidyr)
dfp = pivot_longer(dft, cols = !Zones, names_to = "date",values_to = "values")
head(dfp)
dfp %>% mutate(date = as.Date(date, "%Y%j")) %>% 
  mutate(perc = ifelse(Zones==1, values/dtable[1],
                       ifelse(Zones==2, values/dtable[2],
                              ifelse(Zones==3, values/dtable[3], NA)))) %>% 
  ggplot(aes(x=date,y=perc, colour=Zones)) + geom_point()

mk = crop(mvs, k, mask=TRUE)
plot(mk[[2]])
# raster results
# cloud free days each year, each month, by pixel, for each glacier
# monthly and annual SCA composite (mean) for each glacier

project(mosaic(ls[[1]], ls[[2]]), "EPSG:4326")

# KEY
# 0-100 Fraction of pixel covered in snow
# 230 Pixel off grid
# 235 Pixel not run
# 250 Clouds
# 253 Water Masked
# 255 No Data

kp = project(k, crs(rast(fn[2])))
stk.mosaic.plot = function(ls){
  dddt = gsub(".*GA.A(.*).h.*$","\\1", names(ls[[1]]))
  p = mosaic(ls[[1]], ls[[2]])
  plot(crop(p, kp, mask= T), main = dddt)
}

rls = lapply(fnspr, rast)
lapply(seq(1,(length(rls)-1), 2), function(x) stk.mosaic.plot(rls[x:(x+1)]))




# 
fnn
for (i in 1:length(fnn)){
  r = rast(fnn[i])
  writeRaster(r, file.path("data/modscag_tiles_2014", basename(fnn[i])))
}
