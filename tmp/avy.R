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
d = crop(d0, ext(-111.85,-111.50,40.50,40.75))

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


plot(slope, col=rev(colorspace::heat_hcl(10, alpha=0.6)), add=T, axes=FALSE, plg=list(title="Slope", cex=0.8, bty="o"))
plot(hill, col=grey(0:100/100), legend=FALSE, mar=plot_mar, axes=FALSE); axis(side = 1, cex = 1.5)
plot(aspect, col=c(colorspace::diverge_hcl(12, alpha=0.45),rev(colorspace::diverge_hcl(12, alpha=0.35))), add=T, axes=FALSE, plg=list(title="Aspect", cex=0.8, bty="o"))
plot(hill, col=grey(0:100/100), legend=FALSE, mar=plot_mar, axes=FALSE); axis(side = 1, cex = 1.5)
plot(alt, col=terrain.colors(25, alpha=0.35), add=TRUE, axes=FALSE, plg=list(title="Elevation", cex=0.8, bty="o"))

# avalanche paths & resorts




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
