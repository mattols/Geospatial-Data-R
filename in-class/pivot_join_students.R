library(tidyverse)

# Noah longer solution
billboard %>% pivot_longer(cols=starts_with("wk"),
                           names_to = "week", values_to = "rank") %>%
  mutate(week = as.numeric(gsub("wk","",week))) %>%
  ggplot(aes(x=week,y=rank)) + geom_smooth()

# wider
life_df <- data.frame(
  country = c("Brazil", "Brazil", "Brazil", "China", "China", "China", "United States", "United States", "United States"),
  continent = c("Americas", "Americas", "Americas", "Asia", "Asia", "Asia", "Americas", "Americas", "Americas"),
  year = c(1952, 1977, 2007, 1952, 1977, 2007, 1952, 1977, 2007),
  lifeExp = c(50.9, 61.5, 72.4, 44, 64.0, 73.0, 68.4, 73.4, 78.2)
)
life_df_wide <- life_df %>% pivot_wider(names_from = "year",
                                        values_from = "lifeExp")
# plot 1
us_rent_income %>% 
  ggplot(aes(x = NAME, y = estimate,fill=variable)) +
  geom_bar(stat="identity") + facet_grid(.~variable)

# plot 2
us_rent_income %>% select(-moe) %>%
  pivot_wider(names_from=variable,values_from = estimate) %>%
  mutate(rent = rent * 12) %>% 
  pivot_longer(cols=c("income","rent"),
               names_to = "variable", values_to = "estimate") %>%
  ggplot(aes(x = NAME, y = estimate,fill=variable)) +
  geom_bar(stat="identity") + facet_grid(.~variable) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))



# Andrew
state_birds <- data.frame(
  State = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", 
            "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
            "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
            "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
            "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
            "West Virginia", "Wisconsin", "Wyoming"),
  Bird = c("Yellowhammer", "Willow Ptarmigan", "Cactus Wren", "Northern Mockingbird", "California Quail", "Lark Bunting", "American Robin", 
           "Delaware Blue Hen", "Northern Mockingbird", "Brown Thrasher", "Nene (Hawaiian Goose)", "Mountain Bluebird", "Northern Cardinal", 
           "Northern Cardinal", "Eastern Goldfinch", "Eastern Goldfinch", "Western Meadowlark", "Northern Cardinal", "Chickadee", 
           "Chickadee", "Black-Capped Chickadee", "American Robin", "American Robin", "Northern Mockingbird", "Eastern Bluebird", 
           "Western Meadowlark", "Western Meadowlark", "Western Meadowlark", "Mountain Bluebird", "Purple Finch", "Eastern Goldfinch", 
           "Eastern Goldfinch", "Roadrunner", "Eastern Bluebird", "Western Meadowlark", "Cardinal", "Scissor-Tailed Flycatcher", 
           "Western Meadowlark", "Ruffed Grouse", "Rhode Island Red", "Carolina Wren", "Ring-Necked Pheasant", "Northern Mockingbird", 
           "Northern Mockingbird", "Northern Mockingbird", "Hermit Thrush", "Cardinal", "American Robin", "American Robin", "Western Meadowlark" 
  )
)
  
  state_flags <- data.frame(
    State = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", 
              "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
              "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
              "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
              "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
              "West Virginia", "Wisconsin", "Wyoming"),
    Flag_Color = c("Red, White", "Blue, Gold", "Blue, Gold", "Red, White", "Blue, Gold", "Red, White", "Blue, White", "Blue, Yellow", 
                   "Red, White", "Red, White", "Red, White", "Red, Blue", "Blue, White", "Blue, Gold", "Red, White", "Red, White", 
                   "Blue, Yellow", "Blue, Gold", "Blue, White", "Blue, Yellow", "Red, White", "Blue, Gold", "Blue, Gold", "Blue, Yellow", 
                   "Red, White", "Red, White", "Blue, Gold", "Blue, Gold", "Blue, Silver", "Blue, Gold", "Buff, Blue", "Red, Yellow", 
                   "Red, Yellow", "Blue, Gold", "Red, White", "Green, Yellow", "Red, White", "Blue, White", "Blue, Gold", "Red, White", 
                   "White, Blue", "Blue, White", "Blue, Gold", "Red, White", "Red, White", "Red, White", "Red, White", "Red, White", 
                   "Red, White, Blue", "Red, Yellow")
  )
)

# full join
merged_data <- full_join(state_flags, state_birds, by = "State")
print(merged_data)

# now one with cbind()
jazz_2020 <- data.frame(
  Player = c("Player1", "Player2", "Player3"),
  Points = c(20, 15, 18),
  Assists = c(6, 5, 7),
  Rebounds = c(8, 7, 9),
  ThreePointPercentage = c(0.35, 0.42, 0.38)
)

jazz_2022 <- data.frame(
  Player = c("Player1", "Player2", "Player3"),
  Points = c(22, 18, 20),
  Assists = c(7, 6, 8),
  Rebounds = c(9, 8, 10),
  ThreePointPercentage = c(0.37, 0.41, 0.39)
)


combined_jazz <- cbind(jazz_2020, jazz_2022[, -1])

