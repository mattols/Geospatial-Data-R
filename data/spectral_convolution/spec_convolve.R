#
# Andrew - spectral convolution
# paths
oli2_path = "data/spectral_convolution/L9_OLI2_Ball_BA_RSR.v2-1.xlsx"
oli_path = "data/spectral_convolution/Ball_BA_RSR.v1.1-1.xlsx"
oli_fwhm = "data/spectral_convolution/Landsat_OLI_OLI2_FWHM.csv"

# landsat fwhm (nm)
dfls = read.csv(oli_fwhm)
# break by sensor and useable bands
df_8 = dfls %>% filter(Sensor=="OLI") %>% filter(BN %in% 2:7)
df_9 = dfls %>% filter(Sensor=="OLI-2") %>% filter(BN %in% 2:7)
head(df_8)
# plot((Center_wavelength_nm^-4)~Center_wavelength_nm, data= df_8, type='b')

# Convolution
ls8 <- read.csv("data/spectral_convolution/OLI_RSR.csv")
ls9 <- read.csv("data/spectral_convolution/OLI2_RSR.csv")
# hyperspectral
hyper <- read.csv("data/spectral_convolution/Lookup_long_nm.csv")
head(hyper)

# # # # # # #
# Functions
spec_convolve <- function(df_hyp, df_rsr, lower_lim, upper_lim){
  # make subset?
  
  sum(df_hyp*df_rsr)
  
}
# reformat
landsat_convolve <- function(df_hyper, df_rsr, df_oli){
  # 
  # landsat ls_sub is 1nm spectral resoultion
  # hyspec lookup table is 10nm (or other)
  #
  
  # for i in bands
  bnd = 1 # Blue
  df_oli = df_8
  df_hyper = hyper
  df_rsr = ls8
  # VARS
  
  ## TEST
  # subset hyperspectral
  hy_sub = df_hyper %>% filter(
    lambda >= df_oli[bnd,"Lower_edge_nm"] &
      lambda <= df_oli[bnd,"Upper_edge_nm"]
  ) 
  # head(hy_sub, 1500) # by 10nm ... 455, 465, etc.
  #
  # filter for only exiting hyspec
  hy_unique = unique(hy_sub$lambda)
  # subset landsat data
  ls_sub = df_rsr %>% filter(
    Wavelength >= df_oli[bnd,"Lower_edge_nm"] &
      Wavelength <= df_oli[bnd,"Upper_edge_nm"]
  ) %>% filter(Wavelength %in% hy_unique)
  head(ls_sub, 10) # by 1nm (filter)
  #
  # for hy_sub
  # group by grain size 
  # across lambda
  hy_sub %>% group_by(grain_size) %>% 
    summarise(L_y = )
  
  
}


#### !!!!!
# NEED more narrow modeled wavelength
plot(rsr~Wavelength, ls8[ls8$Wavelength >= df_oli[bnd,"Lower_edge_nm"] &
                           ls8$Wavelength <= df_oli[bnd,"Upper_edge_nm"],],
     main="Spectral Response Function w/ resampled wavelengths")
abline(v=hy_unique, col='red')



# # # # # # # #

# PLOT FWHM
# test bandwidth (blue)
plot(rsr~Wavelength, ls8[1:80,], type='b')
abline(v=df_8[1,"Lower_edge_nm"], col='red')
abline(v=df_8[1,"Upper_edge_nm"], col='red')


# # # # # # # # # # # # # # # # # # # # # #
###### WRITE OLI META FILES ##############
# Lookup table
library(tidyr);library(ggplot2)
dfl <- read.csv("data/spectral_convolution/LookupTable2.csv")
head(dfl)
dfl2 = dfl %>%
  pivot_longer(cols=!c(X,lambda),
               names_to = "grain_size",
               values_to = "reflectance") %>% 
  mutate(grain_size = as.numeric(gsub("X","",grain_size))) %>% 
  mutate(lambda = lambda*1000) # convert from microns to nm

head(dfl2)
# plot for every 100
dfl2 %>% filter(grain_size %in% seq(100,1500, 100)) %>% 
  ggplot(aes(x=lambda, y=reflectance, colour=grain_size)) + 
  geom_line()
dfl2 %>% filter(grain_size==100) %>% 
  ggplot(aes(x=lambda, y=reflectance, colour=grain_size)) + 
  geom_line(aes(group=1))
dfl2 %>% filter(grain_size==100 | grain_size==500 | grain_size==1000) %>% 
  ggplot(aes(x=lambda, y=reflectance, colour=grain_size)) + 
  geom_line(aes(group=1))
# WORKS
dfl2 %>% filter(grain_size %in% seq(100,1500,100)) %>% 
  mutate(grain_size=as.character(grain_size)) %>% 
  ggplot(aes(x=lambda, y=reflectance, group=grain_size)) + 
  geom_line(aes(colour=grain_size))
### does not  produce three distinct?
# write.csv(dfl2, "data/spectral_convolution/Lookup_long_nm.csv", row.names = F)
# 305 - 4995 nm (10nm bandwidth)
##############################
# # # # # # # # # # # # # # # 



# # # # # # # # # # # # # # # # # # # # # #
###### WRITE OLI META FILES ##############
# Use landsat rsr files to build SR
library(dplyr)
# OLI - based on names
ls8 <- lapply(df_8$Band, function(x)
       {readxl::read_xlsx(oli_path, sheet = x) %>%
         select(1:3) %>%
         rename(rsr = `BA RSR [watts]`,
                stdev = `stdev(RSR)`) %>% filter(rsr > 0.005)} )
# rbind(ls8[[1]],ls8[[2]]) %>% plot(rsr~Wavelength, ., type='b')
# combine all
ls8 <- do.call("rbind", ls8)
# plot all
ls8 %>% plot(rsr~Wavelength, ., type='l', main="Landsat OLI SRF")
# write.csv(ls8, "data/spectral_convolution/OLI_RSR.csv", row.names = F)
# OLI-2
ls9 <- lapply(df_9$Band, function(x)
{readxl::read_xlsx(oli2_path, sheet = x) %>%
    select(1:3) %>%
    rename(rsr = `BA RSR [watts]`,
           stdev = `stdev(RSR)`) %>% mutate(band = x)} )
ls9 <- do.call("rbind", ls9)
ls9 %>% plot(rsr~Wavelength, ., type='l', main="Landsat OLI-2 SRF")
# write.csv(ls9, "data/spectral_convolution/OLI2_RSR_full.csv", row.names = F)
# ISSUES in that the RSR for each band extends beyond and into the the
# next band. Issues for OOB estimation
plot(ls9$Wavelength,typ='l')
#
# step down
idx_d <- which((ls9$Wavelength < lag(ls9$Wavelength)))
which((lag(ls9$Wavelength)-ls9$Wavelength) > 0)
# for BLUE
plot(rsr~Wavelength,ls9[1:752,])
points(rsr~Wavelength,ls9[1:752,] %>% filter(rsr>0.005),col='red') # 5%
#
# eliminate overlap
ls9 <- lapply(df_9$Band, function(x)
{readxl::read_xlsx(oli2_path, sheet = x) %>%
    select(1:3) %>%
    rename(rsr = `BA RSR [watts]`,
           stdev = `stdev(RSR)`) %>% filter(rsr > 0.005)} )
ls9 <- do.call("rbind", ls9)
ls9 %>% plot(rsr~Wavelength, ., type='l', main="Landsat OLI-2 SRF")
# write.csv(ls9, "data/spectral_convolution/OLI2_RSR.csv", row.names = F)
##############################
# # # # # # # # # # # # # # # 
