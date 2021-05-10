## data analysis for social buffering VR study
# behavioral data
# condition: alone vs. social; treatment: real person vs. VR

rm(list = ls())

library(ggplot2)
library(plotrix)
library(Rmisc)
library(lme4)
library(Matrix)
library(car)
library(interplot)
library(emmeans)
setwd('E:/Germany 2/On lab computer/social buffering-VR/the data/behavioral data/')

### compare female_real and male_real condition

# read into real condition data
behavioralpath = "E:/Germany 2/On lab computer/social buffering-VR/the data/behavioral data/"

# read into female_real data
behavior_female_alone <- read.table(paste(behavioralpath,"female_ratings_trial_by_trial_alone.txt",sep = ""))
names(behavior_female_alone)<-c("subject","rating","valence","trial","condition")
behavior_female_alone$gender <- 'female'

behavior_female_social <-read.table(paste(behavioralpath,"female_ratings_trial_by_trial_social.txt",sep = ""))
names(behavior_female_social)<-c("subject","rating","valence","trial","condition")
behavior_female_social$gender <- 'female'

# read into male data
behavior_male_alone <- read.table(paste(behavioralpath,"male_ratings_trial_by_trial_alone.txt",sep = ""))
names(behavior_male_alone)<-c("rating","valence","trial","condition","subject")
behavior_male_alone$subject <- behavior_male_alone$subject+100
behavior_male_alone$gender <- 'male'

behavior_male_social <- read.table(paste(behavioralpath,"male_ratings_trial_by_trial_social.txt",sep = ""))
names(behavior_male_social)<-c("rating","valence","trial","condition","subject")
behavior_male_social$subject <- behavior_male_social$subject+100
behavior_male_social$gender <- 'male'

# read into female_VR data
behavior_alone_VR <- read.table(paste(behavioralpath,"alone_VR.txt",sep = ""),sep = '')
behavior_alone_VR <- behavior_alone_VR[,c(-3,-5:-7,-9,-10)]
names(behavior_alone_VR)<-c("subject","condition","trial","rating","valence")
behavior_alone_VR$gender <- 'female'

behavior_social_VR <- read.table(paste(behavioralpath,"social_VR.txt",sep = ""),sep = '')
behavior_social_VR <- behavior_social_VR[,c(-3,-5:-7,-9,-10)]
names(behavior_social_VR)<-c("subject","condition","trial","rating","valence")
behavior_social_VR$gender <- 'female'

## combine data

behavior.all <- rbind(behavior_female_alone,behavior_female_social,behavior_male_alone,behavior_male_social,behavior_alone_VR,behavior_social_VR)

behavior.all[behavior.all$valence=='2',]$valence <- 'neutral'
behavior.all[behavior.all$valence=='1',]$valence <- 'aversive'


behavior.gender <- rbind(behavior_female_alone,behavior_female_social,behavior_male_alone,behavior_male_social)
behavior.VR <- rbind(behavior_female_alone,behavior_female_social,behavior_alone_VR,behavior_social_VR)


questionnaire.all<- read.table("E:/Germany 2/On lab computer/social buffering-VR/the data/questionnaires/questionnaires_threeconditions.csv",sep = ',',header = TRUE)

## compare female and male
behavior.gender[behavior.gender$valence=='2',]$valence <- 'neutral'
behavior.gender[behavior.gender$valence=='1',]$valence <- 'aversive'

# manipulation check
lmer11 <- lmer(rating ~ valence * gender+(1|subject), 
               behavior.gender[behavior.gender$condition == 'alone',])
summary(lmer11)
Anova(lmer11,type = 3)
emmeans(lmer11, pairwise ~ valence|gender,pbkrtest.limit = 5000,lmerTest.limit = 5000)
effsize::cohen.d(rating~valence, behavior_female_alone)
r.squaredGLMM(lmer11)

behavior.gender.questionnaire <-  merge(behavior.gender,questionnaire.all,by=c('subject','gender','condition'),na.rm=TRUE)
behavior.gender.questionnaire.aversive <- subset(behavior.gender.questionnaire, valence == 'aversive')

# the buffering effect  
lmer13 <- lmer(rating ~ condition*gender+scale(prenegative)+scale(preanxiety)+scale(ADS)+(1|subject), behavior.gender.questionnaire.aversive)
summary(lmer13)
Anova(lmer13,type = 3)
emmeans(lmer13, pairwise ~ condition|gender,pbkrtest.limit = 5000,lmerTest.limit = 5000)
r.squaredGLMM(lmer13)

## compare real person and VR
behavior.VR[behavior.VR$valence=='2',]$valence <- 'neutral'
behavior.VR[behavior.VR$valence=='1',]$valence <- 'aversive'

behavior.VR.questionnaire <-  merge(behavior.VR,questionnaire.all,by=c('subject','gender','condition'),na.rm=TRUE)
behavior.VR.questionnaire.aversive <- subset(behavior.VR.questionnaire, valence == 'aversive')

# manipulation check

lmer12 <- lmer(rating ~ valence * treatment+(1|subject),
               behavior.VR.questionnaire[behavior.VR.questionnaire$condition == 'alone',])
summary(lmer12)
Anova(lmer12,type = 3)
emmeans(lmer12, pairwise ~ valence|treatment,pbkrtest.limit = 5000,lmerTest.limit = 5000)
r.squaredGLMM(lmer12)

# buffering effect
lmer14 <- lmer(rating ~ treatment*condition+scale(prenegative)+scale(preanxiety)+scale(ADS)+(1|subject), behavior.VR.questionnaire.aversive)
summary(lmer14)
Anova(lmer14,type = 3)
emmeans(lmer14, pairwise ~ condition|treatment,pbkrtest.limit = 5000,lmerTest.limit = 5000)
r.squaredGLMM(lmer14)
