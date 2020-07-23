###for social buffering2 -- gender differences

## calculate the correlation between trial by trial ratings and SCRs

rm(list = ls())

library(Matrix)
library(plyr)
library(car)
library(stringr)
library(foreign)
library(ggplot2)
library(ggpmisc)
library(psych)
library(lme4)
library(tidyr)
library(gridExtra)
library(Hmisc)
library(plotrix)

# read into the SCRs data (male)
SCRs_path = 'D:social buffering 2/manuscript/submission/first submission/code and data for updating/SCR preprocessed data/'


m_averisve_alone <- read.table(paste(SCRs_path,'male_aloneaversivesounds_trial_by_trial.txt',sep = ""),  sep = '')
m_averisve_social <- read.table(paste(SCRs_path,'male_socialaversivesounds_trial_by_trial.txt',sep = ""),  sep = '')
m_neutral_alone <- read.table(paste(SCRs_path,'male_aloneneutralsounds_trial_by_trial.txt',sep = ""),  sep = '')
m_neutral_social <- read.table(paste(SCRs_path,'male_socialneutralsounds_trial_by_trial.txt',sep = ""),  sep = '')
f_averisve_alone <- read.table(paste(SCRs_path,'female_aloneaversivesounds_trial_by_trial.txt',sep = ""),  sep = '')
f_neutral_alone <- read.table(paste(SCRs_path,'female_aloneneutralsounds_trial_by_trial.txt',sep = ""),  sep = '')
f_averisve_social <- read.table(paste(SCRs_path,'female_socialaversivesounds_trial_by_trial.txt',sep = ""),  sep = '')
f_neutral_social <- read.table(paste(SCRs_path,'female_socialneutralsounds_trial_by_trial.txt',sep = ""),  sep = '')



m_SCRs <- rbind(m_averisve_alone,m_averisve_social,m_neutral_alone,m_neutral_social)
m_SCRs$gender <- 'male'

f_SCRs <- rbind(f_averisve_alone,f_neutral_alone,f_averisve_social,f_neutral_social)
f_SCRs$gender <- 'female'

SCRs <- rbind(m_SCRs, f_SCRs)
colnames(SCRs) <- c("value","treatment","valence","trial",'subject','gender')

SCRs$value <- log10(SCRs$value+1)

# read into behavioral ratings data

behavioralpath = "D:/social buffering 2/manuscript/submission/first submission/code and data for updating/behavioral data/"

# read into female data
female_alone <- read.table(paste(behavioralpath,"female_ratings_trial_by_trial_alone.txt",sep = ""))
names(female_alone)<-c("subject","rating","valence","trial","treatment")
female_alone$gender <- 'female'

female_social <-read.table(paste(behavioralpath,"female_ratings_trial_by_trial_social.txt",sep = ""))
names(female_social)<-c("subject","rating","valence","trial","treatment")
female_social$gender <- 'female'

# read into male data
male_alone <- read.table(paste(behavioralpath,"male_ratings_trial_by_trial_alone.txt",sep = ""))
names(male_alone)<-c("rating","valence","trial","treatment","subject")
male_alone$gender <- 'male'

male_social <- read.table(paste(behavioralpath,"male_ratings_trial_by_trial_social.txt",sep = ""))
names(male_social)<-c("rating","valence","trial","treatment","subject")
male_social$gender <- 'male'

# combine groups
behavioralratings <- rbind(female_alone, female_social, male_alone, male_social)

behavioralratings <- behavioralratings [order(behavioralratings$subject),]
behavioralratings <- behavioralratings [order(behavioralratings$valence),]
behavioralratings <- behavioralratings [order(behavioralratings$subject),]

SCRs <- SCRs [order(SCRs$subject),]

rating_SCRs <- cbind(behavioralratings, SCRs) # combine behavioral ratings and SCRs
rating_SCRs <- rating_SCRs[,c(1,2,4,5,6,7,9)]

# combine the questionnaires
questionnaire <- read.csv("D:/social buffering 2/manuscript/submission/first submission/code and data for updating/questionnaires data.csv",header = TRUE,sep = ';')
names(questionnaire)[1]<- "subject"

rating_SCRs_questionnaire <- merge(rating_SCRs, questionnaire, by= c('subject','gender'), na.rm=TRUE)

# use LMM to investigate the relationship between SCR and behavioral ratings 
lmer1 <- lmer(value ~ scale(rating)*gender +scale(ADS)+scale(significant.others)+scale(family)
              +scale(friends) +(1|subject), 
              rating_SCRs_questionnaire[rating_SCRs_questionnaire$valence == 'aversive',])
summary(lmer1)
Anova(lmer1,type = 3)
vif(lmer1)
r.squaredGLMM(lmer1)

nulllmer1 <- lmer(value ~ (1|subject), 
              rating_SCRs_questionnaire[rating_SCRs_questionnaire$valence == 'aversive',])
anova(lmer1,nulllmer1)

