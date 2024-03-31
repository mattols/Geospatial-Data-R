#
# Avalanche analysis
#
#
library(terra);library(dplyr);library(ggplot2);library(gridExtra)
# avy.csv <- read.csv("data/avalanche/avalanches_slc.csv")

# avy.csv <- avy.csv %>% mutate(Date = as.Date(Date, "%Y-%m-%d")) %>% 
#   filter(as.Date("10/01/2011", "%Y-%m-%d") < Date)
write.csv(avy.csv[,1:18], "data/avalanche/avalanches_2012_2024.csv", row.names = F)

avy.csv <- read.csv("data/avalanche/avalanches_2012_2024.csv")
summary(avy.csv)
head(avy.csv)
dim(avy.csv)

## PART 1
# PLOTS
avy.csv %>% mutate(Date = as.Date(Date, "%Y-%m-%d")) %>% 
  # filter(as.Date("01/01/2012", "%Y-%m-%d") < Date) %>% 
  ggplot(aes(x=Date) ) + geom_bar()

# 2021 season
avy.csv %>% mutate(Date = as.Date(Date, "%Y-%m-%d")) %>% 
  filter(as.Date("2020-10-01", "%Y-%m-%d") < Date &
           as.Date("2021-10-01", "%Y-%m-%d") > Date)  %>% 
  ggplot(aes(x=Date) ) + geom_bar()

# years
avy.csv %>% mutate(Date = as.Date(Date, "%Y-%m-%d")) %>% 
  mutate(year = format(Date, "%Y")) %>% group_by(year) %>% 
  summarise(n = n())   %>% 
  ggplot(aes(x=year, y=n) ) + geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5, size=8))

# months
avy.csv %>% mutate(Date = as.Date(Date, "%Y-%m-%d")) %>% 
  mutate(month = format(Date, "%m")) %>% group_by(month) %>% 
  summarise(n = n())   %>% 
  ggplot(aes(x=month, y=n) ) + geom_bar(stat='identity')

# total number of avalanches by region
avy.csv %>% mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
  group_by(Region) %>% summarize(n = n()) %>% 
  ggplot() +
  geom_bar(aes(x=Region,y=n), stat="identity")

# death by year
avy.csv %>% mutate(Date = as.Date(Date, "%Y-%m-%d")) %>% 
  mutate(year = format(Date, "%Y")) %>% group_by(year, Killed) %>% 
  summarise(n = n())   %>% 
  ggplot(aes(x=year, y=Killed) ) + geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5, size=8))


# Aspect
avy.csv %>% mutate(Date = as.Date(Date, "%Y-%m-%d")) %>% 
  group_by(Aspect) %>% 
  summarise(n = n())   %>% 
  ggplot(aes(x=Aspect, y=n) ) + geom_bar(stat='identity')


# seasons analysis
# years list
# yrs <- avy.csv %>% mutate(Date = as.Date(Date, "%Y-%m-%d")) %>% 
#   mutate(year = format(Date, "%Y")) %>% group_by(year) %>% 
#   summarise(n = n()) %>% ungroup() %>% select(year) %>% mutate(year = as.numeric(year))
# 
# avy.csv %>% mutate(Date = as.Date(Date, "%Y-%m-%d")) %>% 
#   cut(Date, breaks =  as.Date(paste0(yrs$year[-1],"-10-01")),
#       labels = paste0("season_", yrs$year[-1])) %>% 
#   head()
# paste0(yrs$year,"-10-01")
# ideas 
# - use the cut function to create seasons and observe which season had the most reported avalanches
# - filter for 2020-21 which months had the most avalanches

# PArt 2
# Create plot width/depth etc
avy.csv %>% mutate(Date = as.Date(Date, "%Y-%m-%d")) %>% 
  # filter(Region=="Salt Lake") %>% 
  # filter(as.Date("2020-10-01", "%Y-%m-%d") < Date &
  #          as.Date("2021-10-01", "%Y-%m-%d") > Date)  %>% 
  mutate(long = as.numeric(gsub("^.*,\\s","", Coordinates) ) )  %>% 
  mutate(lat = as.numeric(gsub(",\\s.*$","", Coordinates) ) )  %>% 
  mutate(Width = as.numeric(gsub("[[:punct:]]","", Width) ) )  %>% 
    mutate(Vertical = as.numeric(gsub("[[:punct:]]","", Vertical) ) )  %>% 
    mutate(Depth = as.numeric(gsub("[[:punct:]]","", Depth) ) )  %>% 
  ggplot(aes(x=Depth, y = Vertical, col=Region)) + geom_point()
 
# create vect
av <- avy.csv %>% mutate(Date = as.Date(Date, "%Y-%m-%d")) %>% 
  filter(Region=="Salt Lake") %>%
  filter(as.Date("2020-10-01", "%Y-%m-%d") < Date &
           as.Date("2021-10-01", "%Y-%m-%d") > Date)  %>%
  mutate(lon = as.numeric(gsub("^.*,\\s","", Coordinates) ) )  %>% 
  mutate(lat = as.numeric(gsub(",\\s.*$","", Coordinates) ) )  %>% 
  mutate(Width = as.numeric(gsub("[[:punct:]]","", Width) ) )  %>% 
  mutate(Vertical = as.numeric(gsub("[[:punct:]]","", Vertical) ) )  %>% 
  mutate(Depth = as.numeric(gsub("[[:punct:]]","", Depth) ) ) %>% 
  mutate(Elevation = as.numeric(gsub("[[:punct:]]","", Elevation) ) ) %>% 
  # filter(!is.na(lon) & !is.na(lat)) %>%
  vect(., geom=c("lon", "lat"), crs="+proj=longlat +datum=WGS84")
plot(av,"Vertical")
map::maps("state","Utah",add=T)  
#
# maps
maps::map("county","Utah")
maps::map("county",c("utah,salt lake","utah,utah","utah,summit", "utah,wasatch")  )


# ASPECT
# Aspect
avy.csv %>% mutate(Date = as.Date(Date, "%Y-%m-%d")) %>% 
  group_by(Aspect) %>% 
  summarise(n = n())   %>% 
  ggplot(aes(x=Aspect, y=n) ) + geom_bar(stat='identity')
# month with highest
avy.csv %>% mutate(Date = as.Date(Date, "%Y-%m-%d")) %>% 
  mutate(month = format(Date, "%m")) %>% group_by(month) %>% 
  summarise(n = n())   %>% 
  ggplot(aes(x=month, y=n) ) + geom_bar(stat='identity')
# FINAL FILTER
# aspect for salt lake during highest month (FINAL)
avy.csv %>% mutate(Date = as.Date(Date, "%Y-%m-%d")) %>% 
filter(Region=="Salt Lake") %>%
  filter(as.Date("2020-10-01", "%Y-%m-%d") < Date &
           as.Date("2021-10-01", "%Y-%m-%d") > Date)  %>%
  mutate(month = format(Date, "%m")) %>% 
  filter(month=="01") %>% 
  group_by(Aspect) %>% 
  summarise(n = n())   %>% 
  ggplot(aes(x=Aspect, y=n) ) + geom_bar(stat='identity')


# aspect and month!
avy.csv %>% mutate(Date = as.Date(Date, "%Y-%m-%d")) %>% 
  filter(Region=="Salt Lake") %>%
  filter(as.Date("2020-10-01", "%Y-%m-%d") < Date &
           as.Date("2021-10-01", "%Y-%m-%d") > Date)  %>%
  mutate(month = format(Date, "%m")) %>% 
  # filter(month=="01") %>% 
  group_by(Aspect) %>% 
  ggplot(aes(x=Aspect, fill=month) ) + geom_bar()
  
  
avy.csv %>% mutate(Date = as.Date(Date, "%Y-%m-%d")) %>% 
  filter(Region=="Salt Lake") %>%
  filter(as.Date("2020-10-01", "%Y-%m-%d") < Date &
           as.Date("2021-10-01", "%Y-%m-%d") > Date)  %>%
  mutate(month = format(Date, "%m")) %>%  group_by(month, Killed) %>% 
  summarise(n = n())   %>% 
  ggplot(aes(x=month, y=Killed) ) + geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5, size=8))


# # # # # # # # # # #

### Terrain analysis
d0 <- rast("data/UT_elev/ASTGTM2_N40W112/ASTGTM2_N40W112_dem.tif")
dim(d0)
plot(d0)
plot(av,"Vertical", add=T, legend="topright", col=blues9)

# crop
d = crop(d0, ext(-111.85,-111.45,40.48,40.75))

# 
dp = project(d, "EPSG:32612")
res(dp)
plot(dp)

# terrain
slope <- terrain(d, v="slope", unit="degrees")
aspect <- terrain(d, v="aspect", unit="degrees")
hill <- shade(slope*pi/180, aspect*pi/180, angle = 40, direction = 270)

# 
plot_mar = c(2,2,3,4)
plot(hill, col=grey(0:100/100), legend=FALSE, mar=plot_mar)
plot(av,"Aspect", legend="topright", add=T)

# d <- dots(av, "Aspect", 1000, col="red", cex=.75)
# does not work

# SLOPE
plot(slope, col=rev(colorspace::heat_hcl(10, alpha=0.6)))
ss <- (slope >=30 & slope <=45)
# ss[ss==0] = NA
plot(hill, col=grey(0:100/100), legend=FALSE, mar=plot_mar)
plot(slope, col=alpha(c(NA, "red"),0.5), add=T)

# Elevation
dft = d*3.28084
dft
m <- c(5000, 8000, 1,
       8000, 9500, 2,
       9500, 12000, 3)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
ezones = classify(dft, rclmat, othersNA=TRUE)
plot(hill, col=grey(0:100/100), legend=FALSE, mar=plot_mar)
plot(ezones, col=alpha(c("green", "yellow", "orange"), 0.5), add=T)
fq = freq(ezones)*(30*30)/(1000*1000)
fq[,3]
area_km = values(ezones) %>% table() *(30*30)/(1000*1000)

# ASPECT
seq(0,360,45) # 8 classifications (must change firts and last) 9 total
cl_ranges <- c(0, rep(seq(0,360-(45/2),45)+(45/2),each=2),360)
asp_mat <- cbind(matrix(cl_ranges, ncol=2, byrow = TRUE),c(1:8,1))
aspr = classify(aspect, asp_mat, othersNA=TRUE)
aspr[dft<5500] = NA
#
plot(hill, col=grey(0:100/100), legend=FALSE, mar=plot_mar, axes=FALSE); axis(side = 1, cex = 1.5)
plot(aspr, col=rev(alpha(c("#3288BD",RColorBrewer::brewer.pal(7, "Spectral")), 0.8)), add=T)

#
# avalanche paths & resorts
ski <- vect("data/ski_ut/SkiAreaBoundaries_-2905529762139020469/SkiAreaBoundaries.shp")
apath <- vect("data/ski_ut/Utah_Avalanche_Paths/AvalanchePaths.shp")

head(ski)
ski
plot(ski,"NAME")
ski2 <- crop(ski, project(d, crs(ski))) 

ski2
# writeVector(ski2, "data/ski_ut/skiresorts_cottonwoods.geojson", filetype = "GeoJSON")
s = project(ski2, d)
plot(s,add=T)
#

# zonal stats
s_rast <- rasterize(x = s, y = ss, field="NAME")
zslope = zonal(ss, s_rast, fun="sum", na.rm=T)
s$ssum <- round(zslope$slope * (30*30)/(1000*1000), 2)[match(s$NAME,zslope$NAME)]
zarea = zonal(!is.na(s_rast), s_rast, fun="sum", na.rm=T)
s$tot_area <- round(zarea[,2] * (30*30)/(1000*1000), 2)[match(s$NAME,zarea[,1])]
s$slope_ratio = round((s$ssum/s$tot_area)*100, 2) 
s$ssum_km <- paste(s$ssum,"km^2")

# plot area of slope in km
plot(crop(hill,s), col=grey(0:100/100), legend=FALSE, mar=plot_mar, main="Ski resort - Percent of slopes in avalanche terrain")
plot(crop(ss,s), col=alpha(c(NA, "red"),0.5), add=T)
plot(s, border='blue',lwd=1.5,add=T)
text(s, "NAME", adj=c(1,2), cex=0.6, halo=TRUE,col='blue')
text(s, "slope_ratio", halo=TRUE)
#
plot(crop(hill,s), col=grey(0:100/100), legend=FALSE, mar=plot_mar, main="Ski resort - km2 of slope hazard")
plot(crop(ss,s), col=alpha(c(NA, "red"),0.5), add=T)
plot(s, border='blue',lwd=1.5,add=T)
text(s, "NAME", adj=c(1,-2), cex=0.6, halo=TRUE,col='blue')
text(s, "ssum_km", halo=TRUE)


### PART 3
# median elevaion
zelv = zonal(dft, s_rast, fun="mean", na.rm=T)
s$med_e<- round(zelv$ASTGTM2_N40W112_dem , 2)[match(s$NAME,zelv$NAME)]
plot(crop(hill,s), col=grey(0:100/100), legend=FALSE, mar=plot_mar, main="Ski Resort Elevations")
plot(s,"med_e",add=T, col=alpha(blues9, 0.6),legend="topright",plg=list(title="Mean Elevation",bty="o"))
text(s, "med_e", halo=TRUE)
plot(crop(av,s),"Elevation", add=T,legend="topleft",plg=list(title="Obs Elevation",bty="o"),col=blues9)
#


# Avalanche paths
plot(crop(hill,apath), col=grey(0:100/100), legend=FALSE, mar=plot_mar, axes=FALSE)
plot(apath,add=T)
#
apth_rast <- rasterize(x = apath, y = ss, field="NAME")
zslope_a = zonal(ss, apth_rast, fun="sum", na.rm=T)
apath$ssum <- round(zslope_a$slope * (30*30)/(1000*1000), 2)[match(apath$NAME,zslope_a$NAME)]
zarea = zonal(!is.na(apth_rast), apth_rast, fun="sum", na.rm=T)
apath$tot_area <- round(zarea[,2] * (30*30)/(1000*1000), 2)[match(apath$NAME,zarea[,1])]
apath$slope_ratio = round((apath$ssum/apath$tot_area)*100, 2) 
#
plot(crop(hill,apath), col=grey(0:100/100), legend=FALSE, mar=plot_mar, axes=FALSE)
plot(apath, "slope_ratio", add=T)

#
zelv = zonal(dft, apth_rast, fun="mean", na.rm=T)
apath$med_e<- round(zelv$ASTGTM2_N40W112_dem , 2)[match(apath$NAME,zelv$NAME)]
plot(crop(hill,apath), col=grey(0:100/100), legend=FALSE, mar=plot_mar, main="Ski resort - km2 of slope hazard")
plot(apath,"med_e",add=T, col=alpha(blues9, 0.6))
plot(crop(av,apath),add=T)
#

# ASP zone
#
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
zasp = zonal(aspr, apth_rast, fun="Mode")
apath$asp <- zasp$aspect[match(apath$NAME,zasp$NAME)]
plot(crop(hill,apath), col=grey(0:100/100), legend=FALSE, mar=plot_mar, main="Ski resort - km2 of slope hazard")
plot(apath,"asp",add=T, col=alpha(blues9, 0.6))
plot(crop(av, apath),add=T)
#
# assessing
aspsub <- crop(av,aspr)
aspex <- terra::extract(aspr,aspsub)
boxplot(aspex$aspect~aspsub$Aspect)
#
# aspsub %>% recode(Aspect, ) %>% 
dsub <- crop(av,dft)
dex <- terra::extract(dft,dsub)
boxplot(dex$ASTGTM2_N40W112_dem~dsub$Elevation)
plot(dex$ASTGTM2_N40W112_dem~dsub$Elevation)
abline(a=0,b=1)
sqrt(mean((dex$ASTGTM2_N40W112_dem-dsub$Elevation)^2, na.rm=T))

# 
terra::extract(dft, dsub[1,])
terra::extract(dft, buffer(dsub[1,], 50))



### MODEL FUNCITON
##### 
avy.csv %>% mutate(Date = as.Date(Date, "%Y-%m-%d")) %>% 
  filter(Region=="Salt Lake") %>%
  filter(as.Date("2021-02-01", "%Y-%m-%d") < Date &
           as.Date("2021-03-01", "%Y-%m-%d") > Date)  %>%
  ggplot(aes(x=Date) ) + geom_bar()
# 14th 16th?
avy.csv %>% mutate(Date = as.Date(Date, "%Y-%m-%d")) %>% 
  filter(Region=="Salt Lake") %>%
  filter(as.Date("2021-02-01", "%Y-%m-%d") < Date &
           as.Date("2021-03-01", "%Y-%m-%d") > Date)  %>%
  select(Date) %>% table()










# basemap
bbox_maptiles <- ext(-112.6, -111.5, 40.4, 41.0)
library(leaflet)
m <- leaflet() %>% setView(lng = -111.8, lat = 40.45, zoom = 10)
m %>% addTiles()
plot(av,"Width", add=T)

m %>% addProviderTiles(providers$Esri.NatGeoWorldMap)






# CREATE OCTOGON
# https://www.reddit.com/r/QGIS/comments/1949fqf/help_make_an_octagon_with_a_geometry_generator/
pt	x	y
1	-1.2071	-0.5
2	-1.2071	0.5
3	-0.5	1.2071
4	0.5	1.2071
5	1.2071	0.5
6	1.2071	-0.5
7	0.5	-1.2071
8	-0.5	-1.2071
9	-1.2071	-0.5




##### histogram colors
# r_crop <- crop(x = r, y = p_sub) # crop r to the extent of p_sub
# r_mask <- mask(x = r_crop, mask = p_sub, touches = FALSE) # mask values outside of polygon
# par(mfrow=c(1,2))
# h = hist(r_mask, breaks=12, plot=FALSE) # create histogram data
# hcols = c(terrain.colors(length(h$counts)))[as.factor(h$counts)] # determine colors
# plot(h, col=hcols, main="Elevational distribution", xlab="Elevation (m)")
# plot(r_mask, col=terrain.colors(12), axes=FALSE, mar=c(0,0,0,0)) # plot subset of r
# plot(p_sub, border='firebrick', lty=2, lwd=2, add=T)
# text(p_sub, "NAME_2", cex=1.2)

# ERROR with Breaks argument
