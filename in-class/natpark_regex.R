#
# NATIONAL PARKS
# NOAH's scrape alongside base R cleaning
#

library(rvest)
## NATIONAL PARK TABLE
# Reading in the table from Wikipedia
page=read_html("https://en.wikipedia.org/wiki/List_of_national_parks_of_the_United_States")
# Obtain the piece of the web page that corresponds to the "wikitable" node
table = html_node(page, ".wikitable")
# Convert the html table element into a data frame
table = html_table(table, fill = TRUE)
#
#
#cleaning names
table <- table %>% 
  clean_names()
#removing words and km from area column values
table_2 <- table %>% 
  separate(col = area_2023_8,
           sep = " ",
           into = "area_acres")
# turning area_acres column into numeric
table_2$area_acres <- as.numeric(gsub(",", "", table_2$area_acres))
# turning visitors column into numeric
table_2$recreation_visitors_2022_11 <- as.numeric(gsub(",", "", table_2$recreation_visitors_2022_11))
#
# # # # # # # # # # # # #
#
#


# BASE R SOLUTIONS
library(rvest)
## NATIONAL PARK TABLE
# Reading in the table from Wikipedia
page=read_html("https://en.wikipedia.org/wiki/List_of_national_parks_of_the_United_States")
# Obtain the piece of the web page that corresponds to the "wikitable" node
table = html_node(page, ".wikitable")
# Convert the html table element into a data frame
table = html_table(table, fill = TRUE)
#
sub("\\s.*$", "", colnames(table)) 

# Fix Names:  
table$Name[grep("[[:punct:]]",table$Name)] # which ones 
gsub("[[:punct:]]", "",table$Name) # replace 
gsub("[[:space:]]*$","", x) 
gsub("[[:space:]]*[[:punct:]]$","",table$Name) # final combination 

# ! Location: some do not have an easy solution 
gsub("[^[A-z]]*.*$", "",table$Location ) # doesn’t work for 55, 56, 58 
sub("[^A-z]+[^:space:]{1}[^A-z]+.*$", "",table$Location ) # fix for North Dakota 
sub("[^A-z\\.]+[^:space:]{1}[^A-z]+.*$", "",table$Location ) # some parks list more than one state and have a . At the beginning – FINAL SOLUTION 
# still need to correct Maine

# Date 
as.Date(table$`Date established as park[12]`, "%B %d, %Y") 

# Area 
strsplit(table$`Area (2023)[8]`," ") # use with for loop or lapply 
gsub("\\s\\(.*$", "", table$`Area (2023)[8]`) # get rid of all parentheses 
gsub("\\sacres.*$", "", table$`Area (2023)[8]`) # delete after area 
gsub( ",", "", gsub("\\sacres.*$", "", table$`Area (2023)[8]`)) # final ready to be as.numeric() 

# match only inside parentheses 
# https://stackoverflow.com/questions/28955367/extract-text-in-parentheses-in-r 



# Convert Recreation of n visitors to numeric:  
# Names get rid of any character 
as.numeric(gsub(",","",table$`Recreation visitors (2022)[11]`)) 



# Match Description:  
# Parks with bears? - 
table$Name[grep("bear",table$Description)] 

# Parks with fish - 
table$Name[grep("fish",table$Description)] 

# Try “Desert” and “desert” 
# case insensative 
table$Name[grep("[Dd]esert",table$Description)]  


#