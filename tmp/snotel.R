#
# snotel - possible lab 3
#

# install.packages("snotelr")
# library(snotelr)
# only a GUI - need newer version of R - v2.4
library(sf)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
# library(mapview)

# csv
snowpath <- "tmp/tmp_data/UTSNTL_ALL_2020_2022_Daily_Long.csv"
# read.table(snowpath)
dfs <- read.csv(snowpath)
head(dfs)
dim(dfs)

#### watersheds
ws <- read_sf("~/Downloads/Utah_HUC_Boundaries/HUC.shp")
plot(ws[,1])

# make geospatial
dfs_meta <- dfs %>%
  distinct(Station.Name, .keep_all=TRUE) %>% 
  select(Longitude,Latitude,Station.Name,Elevation..ft.) # x,y for long/lat
snopts <- st_as_sf(dfs_meta, coords = 1:2, crs = st_crs(4326) )
# st_crs(snopts) <- 4326
snopts
plot(snopts[,2])
# transform to same crs
sno <- st_transform(snopts, crs = st_crs(ws) )
sno
# plot(ws[,2], col=NA)
# axis(1);axis(2)
# plot(sno[,2],add=T)

# plot extent
# plot(st_geometry(bb_sol), axes = TRUE, graticule = TRUE, pch = '.')
# plot the multipolygon
plot(st_geometry(ws), axes = TRUE, graticule = TRUE, add = TRUE)
# plot the points
plot(sno['Elevation..ft.'], axes = TRUE, graticule = TRUE, pch = 16, key.pos = NULL, 
     add = TRUE)

# extract 
snoe <- st_intersection(sno, ws)
snoe
snosv=snoe %>% select(Station.Name,Elevation..ft.) %>% 
  st_drop_geometry() 
names(snosv) <- c("names","elevation_ft")
row.names(snosv) <- NULL
write.csv(snosv, "tmp/tmp_data/UTSNTL_ELEV.csv",row.names = FALSE)
#
snoe %>% arrange(Station.Name)
#
sno_match <- snoe %>% group_by(HUC) %>%
  summarise(mean_elev = mean(Elevation..ft.), n = n())
ws2 = ws
ws2$mean_elev <- NA
match(ws2$HUC, sno_match$HUC)
ws2$mean_elev <- sno_match$mean_elev[match(ws2$HUC,sno_match$HUC)]
# ws2$mean_elev[match(sno_match$HUC,ws2$HUC)] <- sno_match$mean_elev[match(ws2$HUC,sno_match$HUC)]
# ws2$mean_elev[match(sno_match$HUC,ws2$HUC)] <- sno_match$mean_elev[match(sno_match$HUC,ws2$HUC)]
# ws2$mean_elev[match(sno_match$HUC,ws2$HUC)] <- sno_match$mean_elev[match(sno_match$HUC, ws2$HUC)]
ws2$mean_elev
plot(ws2["mean_elev"])
plot(st_geometry(snoe),add=T,pch=20,col='black',cex=2)
#
#
#
ws[ws$HUC=="16010101",2] %>% plot()
snoe[snoe$HUC=="16010101",1] %>% plot(add=T, col='black')
# does not work

# one basin
plot(st_geometry(ws[ws$HUC=="16010101",2]), col = sf.colors(12, categorical = TRUE), border = 'grey', 
     axes = TRUE)
plot(st_geometry(snoe[snoe$HUC=="16010101",1]), 
     pch = 3, col = 'red', add = TRUE)
# st_centroid

# all
plot(st_geometry(ws), col = sf.colors(12, categorical = TRUE), border = 'grey', 
     axes = TRUE)
plot(st_geometry(snoe), 
     pch = 3, col = 'red', add = TRUE)

# plot variables https://r-spatial.github.io/sf/articles/sf5.html 
plot(ws["HUC"], key.pos = 1, axes = TRUE, key.width = lcm(1.3), key.length = 1.0)
plot(snoe["HUC"], axes=TRUE)
#

# with ggplot
ws %>% mutate(HUC = as.factor(HUC)) %>% ggplot() + 
  geom_sf(aes(fill = HUC)) 

(ws$pt_count <- lengths(st_intersects(ws, sno)))


# # # # # # # # #
## PIVOT WIDER
# wide
snowide <- "tmp/tmp_data/UTSNTL_ALL_2020_2022_Daily_Wide.csv"
dfw <- read.csv(snowide)
dim(dfw)
# fix dates
as.Date(dfw$Date[1],format="%m/%d/%y")
#
# basic swe plot of one site
dfw %>% mutate(Date=as.Date(Date,format="%m/%d/%y")) %>%
  ggplot(aes(x=Date,y=Agua.Canyon..907..Snow.Water.Equivalent..in..Start.of.Day.Values)) +
  geom_line(col='blue') + labs(y="SWE (inches)",title="Agua Canyon Snotel 2020-2023 Seasons") +
  theme_classic()
#
# plot swe and temperature of one site
scal = 4
dfw %>% mutate(Date=as.Date(Date,format="%m/%d/%y")) %>%
  mutate(t_color = if_else(Agua.Canyon..907..Air.Temperature.Average..degF. > 32, "No", "Yes")) %>% 
  ggplot(aes(x=Date)) +
  geom_bar(aes(y = Agua.Canyon..907..Snow.Water.Equivalent..in..Start.of.Day.Values),
           col='lightblue', stat='identity') +
  geom_line(aes(y = Agua.Canyon..907..Air.Temperature.Average..degF. / scal, color=t_color, group=1 ), size=0.5) +
  scale_y_continuous(name = "SWE (inches)",
    sec.axis = sec_axis( trans=~.*scal, name="Temperature (degF)")) +
  scale_color_manual(values=c("firebrick","deepskyblue3"),name="Freezing") +
  theme_bw() +
  theme(
    axis.title.y = element_text(color = "lightblue4"),
    axis.text.y = element_text(color = "lightblue4"),
    axis.title.y.right = element_text(color = "firebrick"),
    axis.text.y.right = element_text(color = "firebrick")
  ) +
  labs(title="Agua Canyon Snotel 2020-2023")

# Add another station 
glimpse(dfw[,1:13]) # 6 variables for each station

#
dfw %>% pivot_longer(cols=contains("Air.Temperature.Average"),
                     names_to = "site", values_to = "t_ave") %>%
  dim()

# snow depth
dfw %>% select(Date, contains("Snow.Depth")) %>% 
  pivot_longer(cols=!Date,
               names_to = "site", values_to = "depth") %>%
  filter(depth>2) %>%
  head()

# deepest day of each year
dfw %>%  mutate(Date=as.Date(Date,format="%m/%d/%y")) %>%
  select(Date, contains("Snow.Depth")) %>% 
  pivot_longer(cols=!Date,
               names_to = "site", values_to = "depth") %>%
  filter(depth>0) %>%
  # mutate(year = format(Date, "%Y")) %>%
  mutate(season = cut(Date, breaks =  c(as.Date(c("2020-10-01", "2021-10-01", "2022-10-01")), Inf), 
                      labels = paste0("season_", c(1, 2, 3)))) %>%
  mutate(site = gsub("\\...*$","",site)) %>% 
  group_by(site, season) %>%
  summarise(snow_ave = mean(depth, na.rm=T), snow_max = max(depth, na.rm=T),
            max_day = Date[which.max(depth)]) %>%
  group_by(season) %>%
  slice_max(order_by = snow_max, n = 5)
  # arrange(desc(snow_max))

# by season ???
period = seq(min(dfw2$Date), max(dfw2$Date), by = "1 year")
seq_along(period)
dfw2$season <- cut(dfw2$Date, breaks =  c(as.Date("2020-10-01", "2021-10-01", "2022-10-01"), Inf), 
                   labels = paste0("season_", c(1, 2, 3)))

cut(dfw2$Date, breaks =  c(as.Date(c("2020-10-01", "2021-10-01", "2022-10-01")), Inf), 
    labels = paste0("season_", c(1, 2, 3)))



## station names
# gsub("^([A-z].*)\\..","\\1",names(dfw)[2:13])
gsub("\\.","_", names(dfw)[2:13])
gsub("^(.*)__[0-9]+__(.*)__.*$","\\2", gsub("\\.","_",names(dfw)[2:13]))
names(dfw) <- gsub("\\.","_", names(dfw))

# multiple columns
dfw %>% pivot_longer(
  cols = 2:ncol(dfw),
  names_to = c("station", "variable"),
  names_pattern = "^(.*)__[0-9]+__(.*)__.*$",
  values_to = "values"
) %>% head()

# multiple columns plot
dfw %>% 
  mutate(Date=as.Date(Date,format="%m/%d/%y")) %>%
  pivot_longer(
  cols = !Date,
  names_to = c("station", "variable"),
  names_pattern = "^(.*)__[0-9]+__(.*)__.*$",
  values_to = "values"
) %>%
  mutate(year = format(Date,"%Y")) %>%
  filter( grepl("Snow_Water", variable) ) %>%
  ggplot(aes(x = Date, y = values, colors=station)) +
  geom_line() +
  facet_grid(.~year)


# split out seasons
dfw2 = dfw %>% 
  mutate(Date=as.Date(Date,format="%m/%d/%y"))

period = seq(min(dfw2$Date), max(dfw2$Date), by = "1 year")
seq_along(period)
dfw2$season <- cut(dfw2$Date, breaks =  c(period, Inf), 
                 labels = paste0("season_", seq_along(period)))

dfw2 %>% 
  pivot_longer(
    cols = !c(Date,season),
    names_to = c("station", "variable"),
    names_pattern = "^(.*)__[0-9]+__(.*)__.*$",
    values_to = "values"
  ) %>%
  filter( grepl("Snow_Water", variable) ) %>%
  ggplot(aes(x = Date, y = values, color=station)) +
  geom_line() +
  theme(legend.position="none") +
  facet_wrap(.~season, scales = "free_x")

# investigate number of stations and variables
dfw3 <- dfw2 %>% 
  pivot_longer(
    cols = !c(Date,season),
    names_to = c("station", "variable"),
    names_pattern = "^(.*)__[0-9]+__(.*)__.*$",
    values_to = "values"
  ) 
dim(dfw3)
head(dfw3)
length(unique(dfw3$station))


# median, max SWE for each station
df_med = dfw3 %>%
  filter(variable == "Snow_Water_Equivalent__in") %>%
  filter(values > 0) %>%
  group_by(station, season) %>%
  summarise(swe_med = median(values), swe_max = max(values),
            swe_mean = mean(values)) 
# join
df_elev <- dfs_meta %>% select(Station.Name, Elevation..ft.) %>%
  rename(station = Station.Name) %>% rename(elev = Elevation..ft.) %>% 
  mutate(station = gsub("\\s", "_", station)) %>% 
  mutate(station = gsub("\\.|#|-", "_", station))
df_elev[c(10,71,91),]
sntl = full_join(df_med, df_elev, by = "station")
# sum(is.na(sntl$elev))
# sntl[which(is.na(sntl$elev)),]
sntl
# plot
sntl %>% 
  ggplot(aes(x = elev, y = swe_mean)) +
  geom_point() +
  theme_bw() +
  facet_wrap(.~season)

# swe med predicts max swe?
# seasonality of elevation predicting swe



# last question
quantile(snosv$elevation_ft)



# other datasets
# https://guides.library.yale.edu/c.php?g=296375&p=7352744
# https://datos.gob.es/en/noticia/10-public-data-repositories-related-natural-sciences-and-environment
# https://www.usgs.gov/educational-resources/usgs-geospatial-data-sources
# https://waterdata.usgs.gov/nwis/qw
# 
#





