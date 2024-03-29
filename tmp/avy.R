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
  group_by(Aspect) %>% 
  summarise(n = n())   %>% 
  ggplot(aes(x=Aspect, y=n) ) + geom_bar(stat='identity')



### Terrain analysis
d <- rast("data/UT_elev/ASTGTM2_N40W112/ASTGTM2_N40W112_dem.tif")
dim(d)
plot(d)
plot(av,"Vertical", add=T, legend="topright", col=blues9)


# basemap
bbox_maptiles <- ext(-112.6, -111.5, 40.4, 41.0)
library(leaflet)
m <- leaflet() %>% setView(lng = -111.8, lat = 40.45, zoom = 10)
m %>% addTiles()
plot(av,"Width", add=T)

m %>% addProviderTiles(providers$Esri.NatGeoWorldMap)
