#
# Skills 1 answers
library(dplyr):library(ggplot2)

data <- read.csv("https://raw.githubusercontent.com/mattols/geospat_data/main/earth-jobs.csv")
head(data)

# 1. List age and gender of the highest and lowest paid employees
data[which.max(data$salary),c(2:3,7)]
data[which.min(data$salary),c(2:3,7)]
# lowest: 26 female, 40953.3
# highest: 34 female, 110775.59

# 2. Order the dataset by salary in descending order.
head(data[order(data$salary,decreasing = TRUE),])

# 3. (2 points) List the employees with a salary greater than $80,000 whose hire date is before 2020-01-01.
data$hire_date <- as.Date(data$hire_date)
data[data$salary > 80000 & data$hire_date < "2020-01-01",]
# 6 individuals

# 4. (2 points) Calculate the median salary difference between the environmental scientists and geologists in this dataset.
median(data[data$area == "Geology",]$salary, na.rm=TRUE) - 
  median(data[data$area == "Environmental Science",]$salary, na.rm = T)
# = 22,320.76

# 5. (3 points) Create a new column that contains the difference in number of days between today’s date 
#     and each employee’s hire date. Use this column to create another column for the time difference in years.
date_diff = as.Date("2024-03-07") - data$hire_date
data$year_diff <- as.numeric( date_diff/ 365.25)
head(data)

# 6. (3 points) Compare the salaries of employees by gender, area, education, and job type. 
#    You may assess this by creating a summary table or different plots. Provide a statement of which groups have greater salaries based on your assessment.
data %>% 
  ggplot(aes(y=salary, x = age, colour=gender)) + 
  geom_point() + facet_grid(.~gender)
# age + being a woman
# Males make on average $5645.38 more than Females
data %>% 
  ggplot(aes(y=salary, x = education)) + 
  geom_boxplot()
# Master's gets paid more
data %>% 
  ggplot(aes(y=salary, x = job)) + 
  geom_boxplot()
# government pay is lower 


# 7. (3 points) Assess whether any variables in the dataset are useful at predicting salary.
fit1 <- lm(salary ~ education + job + area + gender + age, data)
summary(fit1)
#      quite a negative, significant relationship if the student only has a high school ed
#      positive relationship if individual has a masters
#      those in the Field also seem to significantly make less as well as those in the Gov
#      
# multiple R square is high but is a bit inflated by the number of variables

# 8. (5 points) Insert your custom function for sample variance here. 
#    Use the sample() function to create a random vector of 100 samples between 0-100, and use your function.

vect <- sample(100)

#Creating sample variance function
sam_var <- function(vect) {
  total = 0 #creating total variable
  for (i in 1:length(vect)) { #calcuating the total sum of the vector
    total <- total + vect[i]
  }
  v_mean <- total / length(vect) # calculating the mean
  tot_sq_dist = 0 #creating total square distance variable
  for (i in 1:length(vect)) { #loop to calculate total square distance
    sq_dist <- (vect[i]-v_mean)^2
    tot_sq_dist = tot_sq_dist + sq_dist
  }
  sample_variance <- tot_sq_dist/(length(vect)-1)
  return(sample_variance)
}

sam_var(vect)
sam_var(vect)==var(vect)

# or

othervar <- function(x){
  top <- sum( (x - mean(x, na.rm=T) )**2 )
  y <- top / (length(x) - 1)
  return(y)
}
othervar(vect)
othervar(vect)==sam_var(vect)
