install.packages(car)
library(car)
data(Prestige)
help(Prestige)
head(Prestige)
tail(Prestige)
setwd("/Users/isabellameier/Desktop/StatsI_Fall2023")

# Ex 1
# A
# Recoding the variable "type" into "Professional"

Prestige$Professional <- ifelse(Prestige$type == "prof", 1, 0)
Prestige <- subset(Prestige, select = -type)
head(Prestige)
tail(Prestige)

# b
# Running the regression and getting the summary

model <- lm(prestige ~ income + Professional + income*Professional, data = Prestige)
summary(model)
