
# airq
# 2018 - 2022 Ut
# daily pm2.5
# https://www.epa.gov/outdoor-air-quality-data/download-daily-data
# pm2.5 concentration - ug/m3
#

# concepts:
# - date/time
# - time-series
# - data cleaning

# ideas: creating a warning system , worse airq cities
# predict when you might expect bad air quality in 2023?
# can detect change in AQI due to pandemic? (lockdown March 18 - )
# pandemic response more visible in some counties?


# path test
"C:\Users\10504912\OneDrive - Utah Valley University\Projects\src\data\pm25_2018_2022"
x <- readline() # enter path into prompt
x
apth <- gsub("\\\\", "/", x)

# read in and convert data
apth <- "C:\\Users\\10504912\\OneDrive - Utah Valley University\\Projects\\src\\data\\pm25_2018_2022"
list.files(apth)

# one year
df2018 <- read.csv(list.files(apth,full.names = T)[1])
head(df2018)

hist(df2018$Daily.Mean.PM2.5.Concentration, main = "2018 Mean PM 2.5 Concentation - Utah", xlab = "PM2.5 ppm")
unique(df2018$CBSA_NAME)

library(ggplot2);library(dplyr)
df2018 %>% mutate(DATE = as.POSIXct(Date, format="%m/%d/%Y")) %>% 
  select(DATE, Daily.Mean.PM2.5.Concentration, DAILY_AQI_VALUE, CBSA_NAME, COUNTY, STATE, SITE_LATITUDE, SITE_LONGITUDE) %>%
  rename(DAILY_PM2.5_MEAN = Daily.Mean.PM2.5.Concentration) %>%
  filter(CBSA_NAME=="Provo-Orem, UT" | CBSA_NAME=="Salt Lake City, UT") %>%
  ggplot(aes(x=DATE, y=DAILY_AQI_VALUE, colour=CBSA_NAME )) +
  geom_line() +
  ggtitle("Daily AQI for Salt Lake City and Orem, UT - 2018") + ylab("Daily Air Quality Index (AQI)") + xlab("")

df2018 %>% mutate(DATE = as.POSIXct(Date, format="%m/%d/%Y")) %>% 
  select(DATE, Daily.Mean.PM2.5.Concentration, DAILY_AQI_VALUE, CBSA_NAME, COUNTY, STATE, SITE_LATITUDE, SITE_LONGITUDE) %>%
  rename(DAILY_PM2.5_MEAN = Daily.Mean.PM2.5.Concentration) %>%
  filter(CBSA_NAME=="Provo-Orem, UT" | CBSA_NAME=="Salt Lake City, UT" | CBSA_NAME=="St. George, UT" | CBSA_NAME=="Vernal, UT") %>%
  ggplot(aes(x=DATE, y=DAILY_AQI_VALUE, colour=CBSA_NAME )) +
  geom_line() +
  ggtitle("Utah Daily AQI 2018") + ylab("Daily Air Quality Index (AQI)") + xlab("") +
  facet_wrap(~CBSA_NAME,ncol=2)

df2018 %>% mutate(DATE = as.POSIXct(Date, format="%m/%d/%Y")) %>% 
  select(DATE, Daily.Mean.PM2.5.Concentration, DAILY_AQI_VALUE, CBSA_NAME, COUNTY, STATE, SITE_LATITUDE, SITE_LONGITUDE) %>%
  rename(DAILY_PM2.5_MEAN = Daily.Mean.PM2.5.Concentration) %>%
  ggplot(aes(x=DATE, y=DAILY_AQI_VALUE, colour=COUNTY )) +
  geom_line() +
  ggtitle("Utah Daily AQI 2018") + ylab("Daily Air Quality Index (AQI)") + xlab("") +
  facet_wrap(~COUNTY,ncol=3)
  
# which location has worse air?
# month of worst, month of best
# county best, worst?

#######
# combine all years
df25 = do.call('rbind', lapply(list.files(apth,full.names = T),read.csv))
dim(df25)
head(df25)

df25 %>% mutate(DATE = as.POSIXct(Date, format="%m/%d/%Y")) %>% 
  select(DATE, Daily.Mean.PM2.5.Concentration, DAILY_AQI_VALUE, CBSA_NAME, COUNTY, STATE, SITE_LATITUDE, SITE_LONGITUDE) %>%
  rename(DAILY_PM2.5_MEAN = Daily.Mean.PM2.5.Concentration) %>%
  filter(CBSA_NAME=="Provo-Orem, UT" | CBSA_NAME=="Salt Lake City, UT") %>%
  ggplot(aes(x=DATE, y=DAILY_AQI_VALUE, colour=COUNTY )) +
  geom_line() +
  ggtitle("Daily AQI for Salt Lake City and Orem, UT - 2018-2022") + ylab("Daily Air Quality Index (AQI)") + xlab("")

df25 %>% mutate(DATE = as.POSIXct(Date, format="%m/%d/%Y")) %>% 
  select(DATE, Daily.Mean.PM2.5.Concentration, DAILY_AQI_VALUE, CBSA_NAME, COUNTY, STATE, SITE_LATITUDE, SITE_LONGITUDE) %>%
  rename(DAILY_PM2.5_MEAN = Daily.Mean.PM2.5.Concentration) %>%
  filter(CBSA_NAME=="Salt Lake City, UT") %>%
  mutate(YEAR = format(DATE,"%Y")) %>%
  ggplot(aes(x=DATE, y=DAILY_AQI_VALUE, colour=YEAR )) +
  geom_line() +
  ggtitle("Daily AQI for Salt Lake City and Orem, UT - 2018-2022") + ylab("Daily Air Quality Index (AQI)") + xlab("") +
  facet_wrap(~YEAR, scales="free_x")

# over this five year period, is there a significant trend in air quality during any month?
