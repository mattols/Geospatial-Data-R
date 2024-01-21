# Task 3 - demo
# in-class demonstrations

# 1.Formats

now <- Sys.time() # current date and time
class(now)

now2 <- as.Date(now) # convert to Date class
as.POSIXlt(now2) # back to POSIXlt

today <- Sys.Date()
format(today, "%d %b %Y")  # with month as a word

# 2. Indexing
df <- data.frame(col1 = -9:10, col2 = 11:30, col3 = 91:110)



#####
# CS01
# data generation

# geology
# 3 random numbers that equal 1
# added rounding error
jobFun <- function(n) {
  m <- round(matrix(runif(3*n,0,1), ncol=3), 4)
  m<- round(sweep(m, 1, rowSums(m), FUN="/"), 4)
  m*100
}
volc = jobFun(25)
rowSums(volc)
volc[,1]

# geography
# generate random data about the mean
rnorm2 <- function(n,mean,sd) { mean+sd*scale(rnorm(n)) }
r <- rnorm2(100,4,1)
mean(r)  ## 4
sd(r)    ## 1
# better
# Generate average monthly precipitation 
monthly_avg <- c(5, 7, 10, 15, 20, 25, 20, 15, 10, 5, 3, 2) 

# Initialize empty data frame with 12 rows
precip_data <- data.frame(month = character(12), 
                          year1 = numeric(12),
                          year2 = numeric(12), 
                          year3 = numeric(12),
                          year4 = numeric(12),
                          year5 = numeric(12))

# Populate data frame  
for(i in 1:12){
  precip_data$month[i] <- month.abb[i]
  precip_data$year1[i] <- monthly_avg[i] + rnorm(1,0,2)
  precip_data$year2[i] <- monthly_avg[i] + rnorm(1,0,2)
  precip_data$year3[i] <- monthly_avg[i] + rnorm(1,0,2)
  precip_data$year4[i] <- monthly_avg[i] + rnorm(1,0,2)
  precip_data$year5[i] <- monthly_avg[i] + rnorm(1,0,2)
}

# Make September increase each year
precip_data$year1[9] <- monthly_avg[9]
precip_data$year2[9] <- monthly_avg[9] + 2  
precip_data$year3[9] <- monthly_avg[9] + 3
precip_data$year4[9] <- monthly_avg[9] + 5
precip_data$year5[9] <- monthly_avg[9] + 8

# Print data
precip_data_o = precip_data
precip_data

plot(rowMeans(precip_data[,2:6]),type="l")
points(precip_data[,2]);points(precip_data[,3],col='red');points(precip_data[,4],col='blue');points(precip_data[,5],col='green');points(precip_data[,6],col='orange')


ave1=rowMeans(precip_data_o[,2:6]) - rnorm(1,0,2)
plot(ave1, type = 'l')
precip_data$ave30 = ave1

# march and april below the mean
precip_data[3,2:6] = precip_data[3,7] - abs(precip_data[3,2:6] - precip_data[3,7])
precip_data[4,2:6] = precip_data[4,7] - abs(precip_data[4,2:6] - precip_data[4,7])

# final
plot(precip_data[,7], type="l")
points(precip_data[,2]);points(precip_data[,3],col='red');points(precip_data[,4],col='blue');points(precip_data[,5],col='green');points(precip_data[,6],col='orange')
lines(1:12,rowMeans(precip_data[,2:6]),col='red')


##
# environmental Science
# Generate date range (3 sites)
dates <- seq(as.Date('2023-01-01'), as.Date('2023-12-31'), by = 'week')

# Create site IDs
sites <- c('site1', 'site2', 'site3') 

# Initialize data frame
river_data <- data.frame(site_id = character(),
                         date = dates,
                         nitrate = numeric(), 
                         phosphate = numeric())

# Populate with random values  
set.seed(123)
river_data$site_id <- rep(sites, each = 53)
river_data$nitrate <- runif(159, 0, 5)  
river_data$phosphate <- runif(159, 0, 2)

# Make changes for site1
idx <- river_data$date >= '2023-04-15' & river_data$date <= '2023-05-15' & river_data$site_id == 'site1' 
river_data$nitrate[idx] <- runif(sum(idx), 5, 10)

idx <- river_data$date >= '2023-10-01' & river_data$date <= '2023-10-15' & river_data$site_id == 'site1'
river_data$nitrate[idx] <- runif(sum(idx), 5, 10) 

idx <- river_data$date >= '2023-04-01' & river_data$date <= '2023-05-07' & river_data$site_id == 'site1'
river_data$phosphate[idx] <- runif(sum(idx), 2, 5)

# Make changes for site3
idx <- river_data$date >= '2023-07-01' & river_data$date <= '2023-08-31' & river_data$site_id == 'site3' 
river_data$nitrate[idx] <- runif(sum(idx), 2, 10)

idx <- river_data$date >= '2023-08-01' & river_data$date <= '2023-09-07' & river_data$site_id == 'site3'
river_data$phosphate[idx] <- runif(sum(idx), 1, 5) 

# Print first few rows
head(river_data)

### <><> 4 sites

# Generate dates
dates <- seq(as.Date('2023-01-01'), as.Date('2023-12-31'), by = 'week')

# Create 4 site IDs  
sites <- c('site1', 'site2', 'site3', 'site4')

# Initialize dataframe with 4 rows first 
river_data <- data.frame(site_id = character(4*length(dates)),
                         date = rep(dates, 4), 
                         nitrate = numeric(4*length(dates)),
                         phosphate = numeric(4*length(dates)))  

# Populate data
set.seed(123)
river_data$site_id <- rep(sites, each = length(dates))
river_data$nitrate <- runif(4*length(dates), 0, 5)    
river_data$phosphate <- runif(4*length(dates), 0, 2)

# Make changes for site1  
idx <- river_data$date >= '2023-04-15' & 
  river_data$date <= '2023-05-15' &
  river_data$site_id == 'site1'
river_data$nitrate[idx] <- runif(sum(idx), 5, 10) 

idx <- river_data$date >= '2023-10-01' &  
  river_data$date <= '2023-10-15' &
  river_data$site_id == 'site1' 
river_data$nitrate[idx] <- runif(sum(idx), 5, 10)

idx <- river_data$date >= '2023-04-01' &
  river_data$date <= '2023-05-07' &
  river_data$site_id == 'site1'
river_data$phosphate[idx] <- runif(sum(idx), 2, 5)

# Make changes for site3
idx <- river_data$date >= '2023-07-01' &  
  river_data$date <= '2023-08-31' &
  river_data$site_id == 'site3'
river_data$nitrate[idx] <- runif(sum(idx), 2, 10)

idx <- river_data$date >= '2023-08-01' &  
  river_data$date <= '2023-09-07' &
  river_data$site_id == 'site3'
river_data$phosphate[idx] <- runif(sum(idx), 1, 5)

# Make changes for site4
river_data$nitrate[river_data$site_id == 'site4'] <- runif(length(dates), 2, 8)
river_data$phosphate[river_data$site_id == 'site4'] <- runif(length(dates), 1, 4) 

# Limit repeats above thresholds
nitrate_weeks <- 0 
phosphate_weeks <- 0
for(i in 1:53) {
  
  if(river_data$nitrate[river_data$site_id == 'site4'][i] > 5) {
    nitrate_weeks <- nitrate_weeks + 1   
  } else {
    nitrate_weeks <- 0
  }
  
  if(nitrate_weeks >= 4) {
    river_data$nitrate[river_data$site_id == 'site4'][i] <-  
      runif(1, 2, 5) 
    nitrate_weeks <- 0
  }
  
  # Similarly check and limit phosphate repeats
  if(river_data$phosphate[river_data$site_id == 'site4'][i] > 2) {
    phosphate_weeks <- phosphate_weeks + 1   
  } else {
    phosphate_weeks <- 0
  }
  
  if(phosphate_weeks >= 3) {
    river_data$nitrate[river_data$site_id == 'site4'][i] <-  
      runif(1, 2, 5) 
    phosphate_weeks <- 0
  }
  
}

# Print first rows
head(river_data)


plot(nitrate~date, data=river_data, col=c("red","blue","green","orange")[as.factor(river_data$site_id)])
abline(h=5)

plot(phosphate~date, data=river_data, col=c("red","blue","green","orange")[as.factor(river_data$site_id)])
abline(h=2)

river_data[,3:4] <- round(river_data[,3:4],4)

# save
write.csv(river_data,"data/cs/cs01_river_data.csv", row.names = FALSE)

