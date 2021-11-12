install.packages(tidyverse)
install.packages("broom")
library(tidyverse)
library(broom)
#loading dataset
incumbents <- read_csv("https://raw.githubusercontent.com/paswami/StatsI_Fall2021/main/datasets/incumbents_subset.csv")

### 1.1 Run a regression where the outcome variable is voteshare and the explanatory 
### variable is difflog.
# visualising some data
summary(incumbents)
boxplot(voteshare ~ difflog, data = incumbents)
hist(incumbents$difflog)

incumbents1 <- lm(voteshare ~ difflog, data = incumbents)
summary(incumbents1)

### 1.2 Make a scatterplot of the two variables and add the regression line
ggplot(incumbents, aes(difflog, voteshare)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

### 1.3 Save the residuals of the model in a seperate object
incumbents1 <- lm(voteshare ~ difflog, data = incumbents)
incumbents1_resid <- resid(incumbents1)
incumbents1_resid

### 1.4 write the prediction equation
variableDifflog <- data.frame(difflog=c(0.5,0.67,0.46,0.6,1.0,0.73))
predict(incumbents1, newdata = variableDifflog, interval = 'confidence')

### 2.1 Run a regression where the outcome variable is presvote and the explanatory 
### variable is difflog.
# visualising some data
summary(incumbents)
boxplot(presvote ~ difflog, data = incumbents)
hist(incumbents$presvote)

incumbents2 <- lm(presvote ~ difflog, data = incumbents)
summary(incumbents2)

### 2.2 Make a scatterplot of the two variables and add the regression line
ggplot(incumbents, aes(difflog, presvote)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

### 2.3 Save the residuals of the model in a seperate object
incumbents2 <- lm(voteshare ~ difflog, data = incumbents)
incumbents2_resid <- resid(incumbents2)
incumbents2_resid
### 2.4 write the prediction equation
variableDifflog <- data.frame(difflog=c(0.5,0.67,0.46,0.6,1.0,0.73))
predict(incumbents2, newdata = variableDifflog, interval = 'confidence')


### 3.1 Run a regression where the outcome variable is voteshare and the explanatory 
### variable is presvote
# visualising some data
summary(incumbents)
boxplot(voteshare ~ presvote, data = incumbents)
hist(incumbents$presvote)

incumbents3 <- lm(voteshare ~ presvote, data = incumbents)
summary(incumbents3)

### 3.2 Make a scatterplot of the two variables and add the regression line
ggplot(incumbents, aes(presvote, voteshare)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

### 3.3 Save the residuals of the model in a seperate object
incumbents3 <- lm(voteshare ~ presvote, data = incumbents)
incumbents3_resid <- resid(incumbents3)

### 3.4 write the prediction equation
variablePresvote <- data.frame(presvote=c(0.5,0.37,0.46,0.32,0.41,0.52))
predict(incumbents3, newdata = variablePresvote, interval = 'confidence')

### 4.1 Run a regression where the outcome variable is the residuals from 
### Question 1 and the explanatory variable is the residuals from Question 2.
incumbents4 <- lm(incumbents1_resid ~ incumbents2_resid)
summary(incumbents4)
### 4.2  Make a scatterplot of the two residuals and add the regression line.
ggplot(incumbents, aes(incumbents2_resid, incumbents1_resid)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

### 4.3  Write the prediction equation
### y = incumbents_resid1, x = incumbents_resid2
### y = -3.490e-33 + 1.000e+00x

### 5.1 Run a regression where the outcome variable is the incumbent’s
### voteshare and the explanatory variables are difflog and presvote
incumbents5 <- lm(voteshare ~ difflog + presvote, data = incumbents)
summary(incumbents5)

### 5.2 Write the prediction equation
## y = vote share, x0 = difflog, x1 = presvote 
# y = 0.449 + 0.0355x0 + 0.257x1
### 5.3 What is it in this output that is identical to the output in Question 4?
### Why do you think this is the case?
