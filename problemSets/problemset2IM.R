# Just getting the table
data <- matrix(c(14, 7, 6, 7, 7, 1), ncol=3)
rownames(data) <- c('Upper Class', 'Lower Class')
colnames(data) <- c('Not Stopped', 'Bribe Requested', 'Stopped/Given warning')
data <- as.table(data)
data
# Doing the sums in order to get to the expected frequencies
sum_of_rows <- rowSums(data)
sum_of_cols <- colSums(data)
table_sum <- addmargins(data, FUN = sum)
data
gp <- ggplot(aes, x=)
# I tried to get the addmargins going but it printed nothing.
# So I had to do all this middle bits to be able to get my totals added to my table
class(data)
str(data)
addmargins(data)
# the next part is to calculate the expected frequencies
expDat <- data.frame() 
for (i in 1:3) {
  expDat[i, 1] <- (sum(data[i,]) * sum
(data[, 1])) / sum(data)
  expDat[i, 2] <- (sum(data[i,]) * sum
(data[, 2])) / sum(data)
  expDat[i, 3] <- (sum(data[i,]) * sum
(data[, 3])) / sum(data)
}
expDat
# will use expDat because it gave me the values in the correct order. 
# ext_freq gave me in the wrong order for some reason. 
# however expDat had an error but when i looked in the global environment the results were correct.
ext_freq <- matrix(
  (sum_of_rows * sum_of_cols) / sum(data),
  nrow = 2, 
  ncol = 3
)
print(ext_freq)
# Calculating chi square without the function, like a formula 
ChiSqr <- sum((data - expDat)^2/expDat)
ChiSqr
# to confirm that my "formula" works the same as the function
xisq <- chisq.test(data)
xisq
# Calculating degree of freedom
# df <- (nrow - 1)(ncol - 1) how do i do this?
# df <- (2-1)(3-1)
# df <- (1)*(2)
# df <- 2
nrow <- 2
ncol <- 3
df <- (nrow-1) * (ncol-1)
df
# B - Calculate the p-value. What is the conclusion if alpha = 0.1?
p_value <- pchisq(3.79, df=2, lower.tail = FALSE)
p_value
# C - standardized residual
# First i couldn't find a way to calculate residuals without doing a linear regression.
# So I named my Y and x to be able to fit it in the function
names(data) <- gsub(" ", "_", names(data))
data
dimnames(data)
names(dimnames(data))
names(dimnames(data)) <- c("Class", "Stopped")      
names(dimnames(data))
data
data <- classbribe
# Doing the linear regression model
class_bribe_model <- lm("Class" ~ "Stopped", data=classbribe)
summary(class_bribe_model)
#
standardised_residuals <- (residual - mean(residuals)) / sd(residuals)
residuals <- df$Class -df$Stopped
# Exercise 2
# item A
# reading the data set to visualise it and so R knows what I will be talking
# about
urlfile="https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv"
datfra <-read_csv(url(urlfile))
spec(datfra)
head(datfra)
print(datfra)
print(datfra, n=400)
#
Y1 <- "reserved"
X1 <- "water"
model1 <- lm(water~reserved, data = datfra)
model1
summary(model1)
res <- resid(model1)
plot(x=datfra$reserved, y=datfra$water, xlim=c(0,2), ylim=c(0,320))
cor.test(datfra$reserved, datfra$water)
