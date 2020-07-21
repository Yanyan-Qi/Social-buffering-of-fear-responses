rm(list = ls())
library(psych)
library(tidyr)
library(plotrix)

questionnaire <- read.table("D:/social buffering 2/manuscript/submission/first submission/code and data for updating/male_female_questionnaires.csv",sep = ';',header = TRUE)
names(questionnaire)[1]<- "subject"

questionnaire$sumimpression <- questionnaire$pleasant+questionnaire$easier+questionnaire$helpfu+
  questionnaire$sympathetic


## test whether there is difference between male and female 
# ASI-3
aov1 <- aov(socialconcern ~ gender*condition,  questionnaire) 
summary(aov1)
aggregate(socialconcern ~ gender + condition , data = questionnaire, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

aov2 <- aov(cognitiveconcern ~ gender*condition,  questionnaire) 
summary(aov2)
aggregate(cognitiveconcern ~ gender + condition , data = questionnaire, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

aov3 <- aov(physicalconcern ~ gender*condition,  questionnaire) 
summary(aov3)
aggregate(physicalconcern ~ gender + condition , data = questionnaire, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

# MPSPSS (social support)
aov4 <- aov(family ~ gender*condition,  questionnaire) 
summary(aov4)
aggregate(family ~ gender + condition , data = questionnaire, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

aov5 <- aov(friends ~ gender*condition,  questionnaire) 
summary(aov5)
aggregate(friends ~ gender + condition , data = questionnaire, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

aov6 <- aov(significant.others ~ gender*condition,  questionnaire) 
summary(aov6)
aggregate(significant.others ~ gender + condition , data = questionnaire, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

# ADS
aov7 <- aov(ADS ~ gender*condition,  questionnaire) 
summary(aov7)
aggregate(ADS ~ gender + condition , data = questionnaire, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

# impressions
t.test( sumimpression ~ gender, var.equal = TRUE, questionnaire) 

se.function <- function(x) 
  { 
  sd(x,na.rm=TRUE)/sqrt(length(x[!is.na(x)]))
}
se.function (c(questionnaire[questionnaire$gender == 'female',]$sumimpression))
se.function (c(questionnaire[questionnaire$gender == 'male',]$sumimpression))

