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
