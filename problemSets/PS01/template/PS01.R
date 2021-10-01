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

lapply(c(),  pkgTest)

# set working directory
setwd("/Users/lucykinnear/Documents/GitHub/StatsI_Fall2021/problemSets/PS01/template")

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

t.test(y, conf.level = 0.9)

#the null hypothesis is the opposite --> null is that they are the same or lower --> the alternative is that they are higher 

t.test(y, mu = 100, alternative = "greater")

#we cannot reject the null hypothesis because her students are dumb --> large p-value 
#we made it one sided because default is two sided,by saying that the alternative hypothesis is greater as we are focusing on the mean of her student's being greater  
#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/expenditure.txt", header=T)
str (expenditure)
lines(expenditure$Y)
lines(expenditure$X1)
lines(expenditure$X2)
lines(expenditure$X3)
plot(expenditure, ylim=range(expenditure$Y, expenditure$X1,expenditure$X2,expenditure$X3), col='red', main = "Expenditure of states in US", lower.panel = NULL)

plot(expenditure$Region, expenditure$Y,
     pch = 16, frame = TRUE, 
     xlab = "Region", ylab = "Y", col = "red")

scatter.smooth( expenditure$X1, expenditure$Y, xlab = 'Personal Income', ylab = 'expenditure', main = 'Income and expenditure', col = c("blue", "red", "orange", "black"), pch = c(0,1,2,3))

install.packages("ggplot2")
library(ggplot2)
#change expenditure to data frame
data=as.data.frame(expenditure[,c(2,6)])
data$Region = as.factor(data$Region)
mode(data$Region)

#create a box plot graph comparing expenditure and region, based on advice from a friend who has a statistics degree
#however, I would not have thought of a box plot by myself as this is very new territory for me
ggplot(aes(y = Y, x = Region, fill=Region), data = data)+ geom_boxplot()+ggtitle("Box plots of Expenditure by Region")

#R will read Regions as integers unless you make it into factors because they're labelled 1,2 etc but the numbers are basically just names not actually things you can add
#and then this just creates a data frame out of the two variables you're looking at which is in this case Y(2) and Region(6) based on their position in the list
#mode(data$Region)and then this is to check that you successfully changed the region to factors, if it worked it should give the mode as numeric I think

