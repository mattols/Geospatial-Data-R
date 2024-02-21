#
# Week 6 - in-class problem sets
# # 

# problem sets

# own your solution try to do this on your own without the help of internet

#
# Create a vector, subset every other variable
x <- 1:100
x[seq(from = 1,to = length(x), by = 2)] # solution 1
x[c(FALSE, TRUE)] # solution 2
# greater than 20 - less than 50
x[x>20 & x<50] 

##
# create a random sample of 20 numbers (1:20)
# use the order function and other techniques to rearrange 
# the values
x=sample(1:10)
x; order(x)
x[order(x)]

##
# remove missing values from a vector
remove_na <- function(x) {
  x[!is.na(x)]
}
remove_na(c(1,4,2,NA,7,9,2,NA,1,2,3))


##
# Write a function that takes a numeric vector as input and returns 
# a logical vector indicating which elements are above the mean.
ab_mean <- function(vect){
  ave <- mean(vect,na.rm=T)
  vect > ave
}
ab_mean(1:10)

##
# Create sample, calculate mean, vector T/F above (T) or below (F) the mean
# determine with boolean values above or below the mean
x <- sample(x = 1:100, size = 50)
above = x > mean(x)
above
which(above) # which
x[which(above)] # indexing
x[above]

##
# Write a function that simulates 10 rolls of a 6-sided die and returns the 
# number of times each value was rolled. 
# tip - use sample
dice <- function(n){
  # roll dice
  roll <- sample(1:6, n, replace = TRUE)
  return(table(roll))
}
dice(10)

# create a DND dice with modifier
# 2d6 + mod = total
dnd_dice <- function(n, dice, mod){
  rolls = sample(1:dice, n, replace = TRUE)
  final = sum(rolls) + mod
  return(final)
}
dnd_dice(2, 6, 3)
dnd_dice(n=2, dice=20, mod=5)

##
# write a function that simulates 10 coin flips and sums up the number of heads
# fun this function n times
# tip use sample
flip <- function(n){
  # coin flip
  final <- NULL
  for (i in 1:n) {
    flip <- sample(0:1, 10, replace = TRUE) # 1 is heads
    tot <- sum(flip)
    final <- c(final,tot)
  }
  return(final)
}
hist(flip(10))
hist(flip(100))
hist(flip(10000))
x <- flip(10000)
d <- density(x, bw=1)
plot(d)
ablines(v=mean(x)+sd(x))


##
# Using a for loop, iterate through the numbers 1 to 100 and 
# print "fizz" if the number is divisible by 3, 
# "buzz" if divisible by 5, and "fizzbuzz" if divisible by both.
for (i in 1:100) {
  if( ((i %% 3) == 0) & ((i %% 5) == 0) ){
    print(paste(i,"fizzbuzz"))
  }else if((i %% 5) == 0){
    print(paste(i,"buzz"))
  } else if( (i %% 3 )== 0 ){
    print(paste(i,"fizz"))
  }
}

##
# Using if/else logic, write a function that takes a numeric 
# score as input and returns a letter grade ("A", "B", "C", "D", or "F")
grade <- function(score) {
  if (score >= 90) {
    return("A")
  } else if (score >= 80) {
    return("B") 
  } else if (score >= 70) {
    return("C")
  } else if (score >= 60) {
    return("D")
  } else {
    return("F")
  }
}

##
# Write a function that takes a vector as input and returns the vector reversed. Use indexing.
x[length(x):1]

## FIBONACCI
# write a function that will solve the nth number of the Fibonacci sequence
# In nature, the numbers and ratios in the sequence can be found in the patterns of petals of flowers, the whorls of a pine cone, and the leaves on stems. As the sequence continues, the ratios of the terms approach a number known as the golden ratio.
# https://planetmath.org/listoffibonaccinumbers
# Fn = Fn-2 + Fn-1
fib <- function(x){
  # start  with 1
  fall = c(1, 1) # really starts at 0
  # solve for nth part of sequence
  for (i in 2:x) {
    f = fall[i - 2] + fall[i - 1]
    fall = c(fall, f)
    # print(f)
  }
  return(fall)
}
# 50th point
f50 <- fib(50)
plot(f50)

##
# reverse text
?strsplit
stx <- strsplit("Matt", NULL)[[1]]
strev <- rev(stx)
paste(strev,collapse = '')

##
# Primality test
# write a function to detemine if any number is prime
# https://en.wikipedia.org/wiki/Primality_test
is_prime <- function(n){
  # 
  
}