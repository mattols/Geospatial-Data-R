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






# other datasets
# https://guides.library.yale.edu/c.php?g=296375&p=7352744
# https://datos.gob.es/en/noticia/10-public-data-repositories-related-natural-sciences-and-environment
# https://www.usgs.gov/educational-resources/usgs-geospatial-data-sources
# https://waterdata.usgs.gov/nwis/qw
# 
#



