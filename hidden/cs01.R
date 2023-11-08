# Case Study 01

# Answers

# 1) Mean production is 1496 kWh
# 
# 2) Max production is 1560 kWh
# 
# 3) Standard deviation of irradiation is 0.089 kW/m2
# 
# 4) Linear model: production ~ irradiation + temp 
# R2 = 0.96
# 
# 5) Estimated production for 5C higher temp is 1538 kWh 
# 
# 6) Scatterplot shows linear relationship between production and irradiation
# 
# 7) Average degradation is 11% 
# 
# 8) Mean production of 1496 kWh = 5.38 GJ  
# 
# 9) Total kW over 30 days across 5 km2 area is 
# 1496 kWh x 30 days x (5 km2 x 106 m2/km2) = 224,400,000 kWh

# Code

# 1) Mean production
mean(production)
# 2) Max production
max(production)
# 3) Standard deviation of irradiation
sd(irradiation)
# 4) Linear model 
lm_model <- lm(production ~ irradiation + temp)
summary(lm_model)
# 5) Estimated production for 5C higher 
predict(lm_model, newdata = data.frame(irradiation, temp+5))
# 6) Scatterplot 
plot(production ~ irradiation)
# 7) Average degradation
mean(degrade)
# 8) Convert kWh to GJ
mean(production) * 0.0036 
# 9) Total kW over 30 days and 5 km2
mean(production) * 30 * (5 * 10^6)