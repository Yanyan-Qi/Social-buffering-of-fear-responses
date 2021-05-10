
## For social buffering VR study
# condition: alone vs. social
# treatment: real person vs. VR 

rm(list = ls())
library(psych)
library(tidyr)
library(plotrix)

questionnaire <- read.table("E:/Germany 2/On lab computer/social buffering-VR/the data/questionnaires/questionnaires_threeconditions.csv",sep = ',',header = TRUE)

questionnaire$pleasant <- 10-questionnaire$pleasant
questionnaire$sympathetic <- 10-questionnaire$sympathetic
questionnaire$helpful <- 10-questionnaire$helpful
questionnaire$easier <- 10-questionnaire$easier

questionnaire$treatment2 <- paste(questionnaire$gender,questionnaire$condition, sep='_')
questionnaire$copresence <- (questionnaire$copresence_other+questionnaire$copresence_self)/8

questionnaire$socialpresence <- (questionnaire$comprehension_other+questionnaire$comprehension_self+
                             questionnaire$emotionalcontagion_other+questionnaire$emotionalcontagion_self+
                               questionnaire$interdependence_self+questionnaire$interdependence_other)/26

questionnaire.gender <- subset(questionnaire, treatment != 'VR')
questionnaire.VR <- subset(questionnaire, gender != 'male')

# gender 
# age
aggregate(age ~ gender + treatment , data = questionnaire.gender, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

# ASI-3
aov1 <- aov(socialconcern ~ gender*treatment,  questionnaire.gender) 
summary(aov1)
aggregate(socialconcern ~ gender + treatment , data = questionnaire.gender, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

aov2 <- aov(cognitiveconcern ~ gender*treatment,  questionnaire.gender) 
summary(aov2)
aggregate(cognitiveconcern ~ gender + treatment , data = questionnaire.gender, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

aov3 <- aov(physicalconcern ~ gender*treatment,  questionnaire.gender) 
summary(aov3)
aggregate(physicalconcern ~ gender + treatment , data = questionnaire.gender, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

# MPSPSS (social support)
aov4 <- aov(family ~ gender*treatment,  questionnaire.gender) 
summary(aov4)
aggregate(family ~ gender + treatment , data = questionnaire.gender, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

aov5 <- aov(friends ~ gender*treatment,  questionnaire.gender) 
summary(aov5)
aggregate(friends ~ gender + treatment , data = questionnaire.gender, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

aov6 <- aov(significant.others ~ gender*treatment,  questionnaire.gender) 
summary(aov6)
aggregate(significant.others ~ gender + treatment , data = questionnaire.gender, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

# ADS
aov7 <- aov(ADS ~ gender*treatment,  questionnaire.gender) 
summary(aov7)
aggregate(ADS ~ gender + treatment , data = questionnaire.gender, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

# prepositive
aov8 <- aov(prepositive ~ gender*treatment,  questionnaire.gender) 
summary(aov8)
aggregate(prepositive ~ gender + treatment , data = questionnaire.gender, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

# prenegative
aov9 <- aov(prenegative ~ gender*treatment,  questionnaire.gender) 
summary(aov9)
aggregate(prenegative ~ gender + treatment , data = questionnaire.gender, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

# preanxiety
aov10 <- aov(preanxiety ~ gender*treatment,  questionnaire.gender) 
summary(aov10)
aggregate(preanxiety ~ gender + treatment , data = questionnaire.gender, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

# trait anxiety
aov11 <- aov(traitanxiety ~ gender*treatment,  questionnaire.gender) 
summary(aov11)
aggregate(traitanxiety ~ gender + treatment , data = questionnaire.gender, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))


# impression - pleasant

t.test( pleasant ~ gender, var.equal = TRUE, questionnaire.gender) 


aggregate(pleasant ~ gender  , data = questionnaire.gender, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

se.function <- function(x) 
  { 
  sd(x,na.rm=TRUE)/sqrt(length(x[!is.na(x)]))
}
se.function (c(questionnaire.gender[questionnaire.gender$gender == 'female',]$pleasant))

# impression - sympathetic
t.test( sympathetic ~ gender, var.equal = TRUE, questionnaire.gender) 


aggregate(sympathetic ~ gender  , data = questionnaire.gender, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

# impression - helpful
t.test( helpful ~ gender, var.equal = TRUE, questionnaire.gender) 


aggregate(helpful ~ gender  , data = questionnaire.gender, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))


# impression - easier
t.test( easier ~ gender, var.equal = TRUE, questionnaire.gender) 


aggregate(easier ~ gender  , data = questionnaire.gender, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))


## test whether there is difference between female_real and female_VR

# age
aggregate(age ~ gender + treatment , data = questionnaire.VR, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))


# ASI-3
aov21 <- aov(socialconcern ~ condition*treatment,  questionnaire.VR) 
summary(aov21)
aggregate(socialconcern ~ condition + treatment , data = questionnaire.VR, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

aov22 <- aov(cognitiveconcern ~ condition*treatment,  questionnaire.VR) 
summary(aov22)
aggregate(cognitiveconcern ~ condition + treatment , data = questionnaire.VR, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

aov23 <- aov(physicalconcern ~ condition*treatment,  questionnaire.VR) 
summary(aov23)
aggregate(physicalconcern ~ condition + treatment , data = questionnaire.VR, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

# MPSPSS (social support)
aov24 <- aov(family ~ condition*treatment,  questionnaire.VR) 
summary(aov24)
aggregate(family ~ condition + treatment , data = questionnaire.VR, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

aov25 <- aov(friends ~ condition*treatment,  questionnaire.VR) 
summary(aov25)
aggregate(friends ~ condition + treatment , data = questionnaire.VR, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

aov26 <- aov(significant.others ~ condition*treatment,  questionnaire.VR) 
summary(aov26)
aggregate(significant.others ~ condition + treatment , data = questionnaire.VR, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

# ADS
aov27 <- aov(ADS ~ condition*treatment,  questionnaire.VR) 
summary(aov27)
aggregate(ADS ~ condition + treatment , data = questionnaire.VR, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

# prepositive
aov28 <- aov(prepositive ~ condition*treatment,  questionnaire.VR) 
summary(aov28)
aggregate(prepositive ~ condition + treatment , data = questionnaire.VR, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

# prenegative
aov29 <- aov(prenegative ~ condition*treatment,  questionnaire.VR) 
summary(aov29)
aggregate(prenegative ~ condition + treatment , data = questionnaire.VR, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

# preanxiety
aov30 <- aov(preanxiety ~ condition*treatment,  questionnaire.VR) 
summary(aov30)
aggregate(preanxiety ~ condition + treatment , data = questionnaire.VR, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

# trait anxiety
aov31 <- aov(traitanxiety ~ condition*treatment,  questionnaire.VR) 
summary(aov31)
aggregate(traitanxiety ~ condition + treatment , data = questionnaire.VR, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

# co-presence
aov31 <- aov(copresence~condition,  questionnaire.VR[questionnaire.VR$treatment =='VR',]) 
summary(aov31)
aggregate(copresence ~  condition , data = questionnaire.VR[questionnaire.VR$treatment =='VR',], 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

# social-presence
aov31 <- aov(socialpresence ~ condition,  questionnaire.VR[questionnaire.VR$treatment =='VR',]) 
summary(aov31)
aggregate(socialpresence ~  condition , data = questionnaire.VR[questionnaire.VR$treatment =='VR',], 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

# impression - pleasant

t.test( pleasant ~ treatment, var.equal = TRUE, questionnaire.VR) 


aggregate(pleasant ~ treatment  , data = questionnaire.VR, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

se.function <- function(x) 
{ 
  sd(x,na.rm=TRUE)/sqrt(length(x[!is.na(x)]))
}
se.function (c(questionnaire.VR[questionnaire.VR$treatment == 'female',]$pleasant))

# impression - sympathetic
t.test( sympathetic ~ treatment, var.equal = TRUE, questionnaire.VR) 


aggregate(sympathetic ~ treatment  , data = questionnaire.VR, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))

# impression - helpful
t.test( helpful ~ treatment, var.equal = TRUE, questionnaire.VR) 


aggregate(helpful ~ treatment  , data = questionnaire.VR, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))


# impression - easier
t.test( easier ~ treatment, var.equal = TRUE, questionnaire.VR) 


aggregate(easier ~ treatment  , data = questionnaire.VR, 
          FUN = function(x) c(mean = mean(x),se = std.error(x)))
