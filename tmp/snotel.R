#
# snotel - possible lab 3
#

# install.packages("snotelr")
# library(snotelr)

# only a GUI - need newer version of R - v2.4

# csv
snowpath <- "tmp/tmp_data/UTSNTL_ALL_2020_2022_Daily_Long.csv"
# read.table(snowpath)
dfs <- read.csv(snowpath)
head(dfs)
dim(dfs)


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

## station names
# gsub("^([A-z].*)\\..","\\1",names(dfw)[2:13])
gsub("\\.","_", names(dfw)[2:13])
gsub("^(.*)__[0-9]+__(.*)__.*$","\\2", gsub("\\.","_",names(dfw)[2:13]))
names(dfw) <- gsub("\\.","_", names(dfw))

#
dfw %>% pivot_longer(cols=contains("Air.Temperature.Average"),
                     names_to = "site", values_to = "t_ave") %>%
  dim()

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
  ggplot(aes(x = Date, y = values, colors=station)) +
  geom_line() +
  facet_wrap(.~season)


# other datasets
# https://guides.library.yale.edu/c.php?g=296375&p=7352744
# https://datos.gob.es/en/noticia/10-public-data-repositories-related-natural-sciences-and-environment
# https://www.usgs.gov/educational-resources/usgs-geospatial-data-sources
# https://waterdata.usgs.gov/nwis/qw
# 
#





