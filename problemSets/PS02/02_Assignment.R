read_excel("/Users/lucykinnear/Desktop/ASDS/Statistics\\02_Stats_Data.xlsx")

getwd()
setwd("//Users/lucykinnear/Desktop/ASDS/Statistics")
mydata = read.csv("Users/lucykinnear/Desktop/ASDS/Statistics\\02_Stats_Data_Orig.csv")
x <- 3.67

#Assign Observed Frequencies
Fo_1 <- 14
Fo_2 <- 6
Fo_3 <- 7
Fo_4 <- 7
Fo_5 <- 7
Fo_6 <- 1

#Assign Expected Frequencies
Fe_1 <- ((27/42) * 21)
Fe_2 <- ((27/42) * 13)
Fe_3 <- ((27/42) * 8)
Fe_4 <- ((15/42) * 21)
Fe_5 <- ((15/42) * 13)
Fe_6 <- ((15/42) * 8)

#Calculate chisq
chisq <- (
  ((Fo_1 - Fe_1)^2/Fe_1) + ((Fo_2 - Fe_2)^2/Fe_2) +
    ((Fo_3 - Fe_3)^2/Fe_3) +  ((Fo_4 - Fe_4)^2/Fe_4) +
    ((Fo_5 - Fe_5)^2/Fe_5) + ((Fo_6 - Fe_6)^2/Fe_6)
)
chisq

pchisq <- pchisq(3.791168, df = 2, lower.tail = FALSE)
pchisq
# Cannot reject the null bc p > alpha !! 

table <- matrix(c(14, 6, 7, 7, 7, 1), ncol=3, byrow=TRUE)
colnames(table) <- c('Not Stopped', 'Bribed', 'Warned')
rownames(table) <- c('Upper Class', 'Lower Class')
table <- as.table(table)
table
chisq_test <- chisq.test(table)
chisq_test
chisq_test$stdres
## interpreting the results 
#compare the standardized residuals in the output table to 
#see which category of variables have the largest difference 
#between the expected counts and the actual counts relative 
#to size, and seem to be dependent
## standardized residuals are the raw residuals (or the
#difference between the observed counts and expected counts),
#divided by the square root of the expected counts.
#positive standardized residuals indicate that there 
#were more occurrences of this outcome than expected. The negative
#standardized residuals indicate that there were less 
#occurrences of this outcome than expected.
#Small standardised residuals tell us that the prediction line 
#is a good fit for the data

#question 2
#null hypothesis: whether or not there is a reservation policy does
#not impact the number of new or repaired drinking water facilities
# in the villages
#alt hypothesis: whether or not there is a reservation policy does
#impact the number of new or repaired drinking water facilities
# in the villages
#Two-Tailed Tests: zα/2 = ±qnorm(1 − α/2)
# part b 

setwd("//Users/lucykinnear/Documents/GitHub/qss/PREDICTION")
Q2_data <- read.csv(file="~/Desktop/women.csv")
Q2_data_of_interest = lm(Q2_data$water ~ Q2_data$reserved)
Q2_data_of_interest
summary(Q2_data_of_interest)
#can reject the null, if alpha is 0.05, significant ! 
#evidence to support the null! tho determinant coefficient (multiple r2) 
#is quite low 
#coefficient is 9.252 ! 
#Changes in the independent variable are associated with changes in the
#dependent variable at the population level. This variable is 
#statistically significant and probably a worthwhile addition to your 
#regression model.
#A positive coefficient indicates that as the value of the independent 
#variable increases, the mean of the dependent variable also tends to increase.
#The coefficient value signifies how much the mean of the dependent variable 
#changes given a one-unit shift in the independent variable while holding other 
#variables in the model constant. 

#question 3
fruitflies <- read.csv(file="~/Desktop/fruitflies.csv")
summary(fruitflies)
hist(fruitflies$longevity.days)
means <- tapply(fruitflies$longevity.days, fruitflies$treatment, mean)
sds <- tapply(fruitflies$treatment, fruitflies$treatment,sd)
## 8 virgin females has shortest ave lifespan, 1 preg female the longest 
boxplot(fruitflies$longevity.days ~ fruitflies$treatment)

# b lifespan v thorax 
q3_lifespan_thorax = lm(fruitflies$longevity.days ~ fruitflies$thorax.mm)
summary(q3_lifespan_thorax)
plot(fruitflies$longevity.days ~ fruitflies$thorax.mm)
abline(lm(fruitflies$longevity.days ~ fruitflies$thorax.mm), col = "red")
library(ggplot2)
ggplot(aes(longevity.days, thorax.mm), data = fruitflies) +
  geom_point(position = "jitter")
abline(lm(fruitflies$longevity.days ~ fruitflies$thorax.mm), col = "red")
###########
#fruitflies$thorax.mm coefficient is 144.33 with tiny p value
#for 1mm change in thorax lifespan incrases 144
ggplot(aes(thorax.mm, longevity.days), data = fruitflies) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)
## p value 1.5e-15 ***

confint(q3_lifespan_thorax, level = 0.9)
## 118.19616 170.4700
######
?predict
class(q3_lifespan_thorax)
str(q3_lifespan_thorax)
new_DF <- q3_lifespan_thorax; new_DF$thorax.mm <- 0
predict(lm(fruitflies$longevity.days ~ fruitflies$thorax.mm), newdata = new_DF, se.fit = T)
