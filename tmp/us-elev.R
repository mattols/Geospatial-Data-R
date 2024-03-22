#
# Extract / clean USGS elevation data using regex
#
library(rvest)
## USGS - Elevation
url1 = "https://www.usgs.gov/educational-resources/highest-and-lowest-elevations"
page=read_html(url1)
# 
e_table0 = html_node(page, "table") %>% 
  html_table(fill=TRUE)
#cleaning names
# e_table <- e_table0 %>% 
#   clean_names()

# # # # # #
# avoid superscript - https://stackoverflow.com/questions/34833584/extract-html-table-with-superscripts-using-r
tab <- as.character(html_nodes(page, "table"))
# dat <- html_table(read_html(gsub("</sup>", "}", gsub("<sup>", "{", tab) )))[[1]]
gsub("</sup>", "}", gsub("<sup>", "{", tab) )
gsub("\\{.*\\}", "", "Whitney</td>\n<td>Inyo-Tulare</td>\n<td>\n{1}14,494</td>\n<td>Death")
gsub("<sup>.*</sup>", "", "Mount Whitney</td>\n<td>Inyo-Tulare</td>\n<td>\n<sup>1</sup>14,494</td>\n<td>Death Valley")
# fixed_html <- gsub("\\{.*\\}", "", tab)
fixed_html2 <- gsub("<sup>\\d+</sup>", "", tab) # wrong dimensions
fixed_html <- gsub("</sup>", "}", gsub("<sup>", "{", tab) )
#
# 
e_table0 = fixed_html %>% read_html() %>% 
  html_table(fill=TRUE)
#
#cleaning names
e_table <- e_table0[[1]] %>% 
  janitor::clean_names()
e_table
# check
e_table %>% select(state_or_possession, elevation_feet, elevation_feet_2)
# # # # # #

# error with table only 7 x 7 instead of 56 rows
et2 = e_table
# Convert ft to numeric
et2$elevation_feet <- gsub("\\{.*\\}", "", et2$elevation_feet)
et2$elevation_feet <- as.numeric(gsub(",", "", et2$elevation_feet))

# et2$elevation_feet_2[et2$elevation_feet_2=="Sea level" | et2$elevation_feet_2=="Sea Level"] = 0
# or
et2$elevation_feet_2 <- gsub("Sea [Ll]evel", 0, et2$elevation_feet_2)
et2$elevation_feet_2 <- gsub("\\{.*\\}", "", et2$elevation_feet_2)
et2$elevation_feet_2 <- as.numeric(gsub(",", "", et2$elevation_feet_2))

# difference
et2$elevation_feet - et2$elevation_feet_2 

et2

### BRING in Temp and Precip
tmp = list.files("data/us-state-noaa", full.names = T)[3:6]


df1 = read.csv(tmp[2],skip=4)
head(df1)

# lapply all
df_ls = lapply(tmp, function(x) read.csv(x,skip=4))

dim(df_ls[[1]])
dim(et2)
nms = gsub("us_temperature_","", gsub("-","_", (gsub(".csv", "", basename(tmp) ))))

match(df_ls[[1]]$Name, et2$state_or_possession)
df1$tmp_dec22 = NA
df1$tmp_dec22[match(et2$state_or_possession, df_ls[[1]]$Name)] = df_ls[[1]]$Value

et3 = et2
et3 = et3 %>% rename(Name = state_or_possession)
dfx <-  full_join(et3, df_ls[[1]], by="Name")



# make new dataframe with all median values and another with yearly values
# observe relationship with elev and median values - diff in median values
# compare yearly data with mean and report anomalies
df_ls
lapply(1:length(tmp), function(x) dim(df_ls[[x]]))

dftemp <- lapply(1:length(tmp), function(x) df_ls[[x]]$Value) %>% as.data.frame()
dft = cbind(df_ls[[1]]$Name, df_ls[[1]]$X1901.2000.Mean, df_ls[[3]]$X1901.2000.Mean, dftemp)
colnames(dft) <-  c("name", "december_means", "june_means",nms)
head(dft)

et3 = et3 %>% rename(name = state_or_possession)
dfnew <- full_join(dft, et3, by="name")

# cor
dfnew$elev_diff = dfnew$elevation_feet - dfnew$elevation_feet_2 
dfnew$seas_diff = dfnew$june_means - dfnew$december_means
pairs(dfnew[,c(2:7, 10, 13:15)])
# weak

# pop data
dfp <- read.csv("data/us-state-pop/population-change-data-table.csv", skip=4)
head(dfp)
dfp = dfp[,1:9]
colnames(dfp) <- c( "name", gsub(".Census", "", gsub( "Percent.Change.", "perc.chng", gsub("Resident.Population.", "pop", colnames(dfp)) ) )[-1])
#
dfa <- full_join(dfnew, dfp, by="name")
head(dfa)

# replace columns
# et2$elevation_feet_2 <- as.numeric(gsub(",", "", et2$elevation_feet_2))
nm1 = colnames(dfa)[c(16,18,20,22)]
dfa[nm1] <- lapply(dfa[nm1], gsub, pattern = ",", replacement = "") %>% lapply(., as.numeric)

dfa %>% glimpse()

pairs(dfa[,c(2:7, 10, 13:23)])
plot(dfa$perc.chng2020~dfa$elev_diff)
lm(dfa$perc.chng2020~dfa$elev_diff) %>% summary()
text(dfa$elev_diff, dfa$perc.chng2020, dfa$name)
#
dfa2 = dfa %>% filter(!name %in% c("Puerto Rico","Alaska", "West Virginia", "Wyoming", "California", "New Mexico", "Delaware", "North Dakota", "Florida", "District of Columbia", "Hawaii"))
# dfa2 = dfa[1:49,]
plot(dfa2$perc.chng2020~dfa2$elev_diff)
text(dfa2$elev_diff, dfa2$perc.chng2020, dfa2$name, cex= 0.4)
lm(dfa2$perc.chng2020~dfa2$elev_diff) %>% summary()


# Cite: https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/statewide/mapping/110/pcp/202212/1/anomaly

# NOAA National Centers for Environmental information, Climate at a Glance: 
# Statewide Mapping, published January 2024, retrieved on January 30, 2024 
# from https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/statewide/mapping

# # # # DATA FOR ASSIGNMENT

# US temp
us_temp <- read.csv("data/us-state-noaa/us-temp-dec-june.csv")
head(us_temp)
ustm <- us_temp %>%
  mutate(
    JuneAnom22 =june_2022 - june_means,
    JuneAnom23 = june_2023 - june_means,
    DecAnom22 = december_2022 - december_means,
    DecAnom23 = december_2023 - december_means
  )
ustm %>%
  arrange(desc(DecAnom22)) %>% 
  select(!contains("_")) %>% 
  head(5)
ustm %>%
  arrange(desc(DecAnom23)) %>% 
  select(!contains("_")) %>% 
  head(5)
ustm %>% select(!contains("_")) %>% 
  group_by(region) %>% 
  summarize(june_anom22_state = name[which.max(JuneAnom22)],
            june_anom22_val = max(JuneAnom22, na.rm=T),
            june_anom23_state = name[which.max(JuneAnom23)],
            june_anom23_val = max(JuneAnom23, na.rm=T))

# ELEV
state_elev <- read.csv("data/state-elev/messy_state_elev.csv")
head(state_elev)

# messy elev
df_el = et2 %>% rename(Names = state_or_possession) %>% 
  select(Names, elevation_feet, elevation_feet_2) %>% 
  mutate(elev_range = elevation_feet - elevation_feet_2)


pop <- read.csv("data/us-state-pop/us-pop.csv", skip=4)
new_cols <- lapply(pop, gsub, pattern = ",", replacement = "")
# pop[,2:6] = new_cols[[2:6]] %>% as.data.frame()
pop[,2:6] = lapply(2:6, function(x) pop[,x] = as.numeric(new_cols[[x]])) %>% 
  as.data.frame()
colnames(pop) <- c("Names", "pop20", "pop10", "pop00", "pop90", "pop80")
head(pop, 2)
#
dfx = full_join(df_el, pop, by="Names")
head(dfx)
dfx2 <- dfx %>%  
  mutate(
    ChangePercent20 = 100*(`pop20` - `pop10`)/`pop10`,
    ChangePercent10 = 100*(`pop10` - `pop00`)/`pop00`,
    ChangePercent00 = 100*(`pop00` - `pop90`)/`pop90`,
    ChangePercent90 = 100*(`pop90` - `pop80`)/`pop80`
  ) %>% 
  arrange(desc(ChangePercent20))
head(dfx2)
dfx2 %>% select(!c(Names, elevation_feet, elevation_feet_2)) %>% 
  lm(elev_range ~ ., . ) %>%  summary()

#
dflong = dfx2 %>% select(!c(elevation_feet, elevation_feet_2)) %>% 
  select(!contains("pop")) %>% 
  pivot_longer(cols=!c(Names, elev_range) )
head(dflong)
dflong %>% ggplot(aes(x = elev_range, y = value, colour = Names)) +
  geom_point() + facet_wrap(.~name) + theme(legend.position="none")
#
plot(dfa$perc.chng2020~dfa$elev_diff)
lm(dfa$perc.chng2020~dfa$elev_diff) %>% summary()
#
