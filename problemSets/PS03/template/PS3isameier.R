#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("/Users/isabellameier/Desktop/StatsI_Fall2023")
# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")
head(inc.sub)

# Question 1
# 1
model <- lm(voteshare~difflog, data=inc.sub)
summary(model)
# 2
plot(x=inc.sub$difflog, y=inc.sub$voteshare)
abline(model)
# 3
residuals_1 <- residuals(model)
print(residuals_1)


# Question 2 
# 1
model_2 <- lm(presvote~difflog, data=inc.sub)
summary(model_2)
# 2
plot(x=inc.sub$difflog, y= inc.sub$presvote)
abline(model_2)
# 3
residuals_2 <- residuals(model_2)
print(residuals_2)

# Question 3 
# 1 
model_3 <- lm(voteshare~presvote, data=inc.sub)
summary(model_3)
# 2 
plot(x=inc.sub$presvote, y=inc.sub$voteshare)
abline(model_3)
# 3

# Question 4
# 1
model_4 <- lm(residuals_1~residuals_2)
summary(model_4)
# 2
plot(x=residuals_2, y=residuals_1)
abline(model_4)


# Question 5
# 1
model_5 <- lm(voteshare~difflog+presvote,data=inc.sub)
summary(model_5)
