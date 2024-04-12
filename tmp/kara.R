# # # #
#
# modis snow
#


#







avy <- read.csv("https://raw.githubusercontent.com/mattols/geospat_data/main/avalanches_2012_2024.csv")

avy$Date = as.Date(avy$Date, "%Y-%m-%d")
avy$month = format(avy$Date, "%B")
monthCount = table(avy$month)
month_reorder = monthCount[match(month.name, names(monthCount))]
barplot(month_reorder, main="Avalanches/ Month", xlab="", 
        ylab="# of Avalanches", names.arg = month.name, las=2)