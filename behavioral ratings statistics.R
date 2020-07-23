# script for behavioral data statistics

rm(list = ls())

library(lmerTest)
library(Matrix)
library(car)
library(ggplot2)
library(reshape2)
library(emmeans)
library(MuMIn)
library(effsize)

datapath = "D:/social buffering 2/manuscript/submission/first submission/code and data for updating/behavioral data/"


# read into female data
female_alone <- read.table(paste(datapath,"female_ratings_trial_by_trial_alone.txt",sep = ""))
names(female_alone)<-c("subject","rating","valence","trial","treatment")
female_alone$gender <- 'female'

female_social <-read.table(paste(datapath,"female_ratings_trial_by_trial_social.txt",sep = ""))
names(female_social)<-c("subject","rating","valence","trial","treatment")
female_social$gender <- 'female'

# read into male data
male_alone <- read.table(paste(datapath,"male_ratings_trial_by_trial_alone.txt",sep = ""))
names(male_alone)<-c("rating","valence","trial","treatment","subject")
male_alone$gender <- 'male'

male_social <- read.table(paste(datapath,"male_ratings_trial_by_trial_social.txt",sep = ""))
names(male_social)<-c("rating","valence","trial","treatment","subject")
male_social$gender <- 'male'

# combine groups
mydata <- rbind(female_alone, female_social, male_alone, male_social)


# read into questionnaires
questionnaire <- read.csv("D:/social buffering 2/manuscript/submission/first submission/code and data for updating/questionnaires data.csv",header = TRUE,sep = ';')
names(questionnaire)[1]<- "subject"

mydata_questionnaires <- merge(mydata,questionnaire,by=c('gender','subject'),na.rm=FALSE)

## linear mixed model

# manipulation check in alone treatment group
lmer1 <- lmer(rating ~  valence*gender +scale(ADS)+scale(significant.others)+scale(family)+scale(friends)+ (1|subject), 
              mydata_questionnaires[mydata_questionnaires$treatment=='alone',])
summary(lmer1)
Anova(lmer1,type = 3)
emmeans(lmer1, pairwise ~ valence|gender,pbkrtest.limit = 3510,lmerTest.limit = 3510)
emmeans(lmer1, pairwise ~ gender|valence,pbkrtest.limit = 3510,lmerTest.limit = 3510)
r.squaredGLMM(lmer1)
effsize::cohen.d(rating~gender, mydata_questionnaires[mydata_questionnaires$treatment=='alone'& mydata_questionnaires$valence=='1',]  )
effsize::cohen.d(rating~gender, mydata_questionnaires[mydata_questionnaires$treatment=='alone'& mydata_questionnaires$valence=='2',]  )
effsize::cohen.d(rating~valence, mydata_questionnaires[mydata_questionnaires$treatment=='alone'& mydata_questionnaires$gender=='female',]  )
effsize::cohen.d(rating~valence, mydata_questionnaires[mydata_questionnaires$treatment=='alone'& mydata_questionnaires$gender=='male',]  )
vif(lmer1)

nulllmer0 <-lmer(rating ~ (1|subject), mydata_questionnaires[mydata_questionnaires$treatment=='alone',])
anova(lmer1,nulllmer1)


# social buffering effect 
lmer2 <- lmer(rating ~ treatment * gender+scale(ADS)+scale(significant.others)+scale(family)+scale(friends)+(1|subject), 
              mydata_questionnaires[mydata_questionnaires$valence=="1",])
summary(lmer2)
Anova(lmer2,type = 3)
r.squaredGLMM(lmer2)
vif(lmer2)
nulllmer2 <- lmer(rating ~ (1|subject),mydata_questionnaires[mydata_questionnaires$valence=="1",]  )
anova(lmer2, nulllmer2)


