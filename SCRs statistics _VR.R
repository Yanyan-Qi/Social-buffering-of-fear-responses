# SCRs data statistics - VR

# condition: alone vs. social; treatment: real-life (real person) vs. VR

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
library(mediation)
library(interactions)
library(lavaan)
library(emmeans)
library(estimability)
library(Rmisc)
library(brms)
library(loo)
library(faint)
library(reshape2)
library(dplyr)
library(MuMIn)
library(effsize)
library(ggeffects)
library(simr)
library(plotrix)

setwd ("E:/Germany 2/On lab computer/social buffering-VR/the data/SCR data/raw data/")

#################### read into female_VR data ##################

# aversive alone
averisve_alone <- read.table('VR_aloneaversivesounds_trial_by_trial.txt',  sep = '')
averisve_alone$V7 <- 'female'
averisve_alone$v8 <- 'VR'

# aversive social 
averisve_social <- read.table('VR_socialaversivesounds_trial_by_trial.txt', sep = '')
averisve_social$V7 <- 'female'
averisve_social$v8 <- 'VR'

# alone neutral 
neutral_alone <- read.table('VR_aloneneutralsounds_trial_by_trial.txt',  sep = '')
neutral_alone$V7 <- 'female'
neutral_alone$v8 <- 'VR'

# social neutral
neutral_social <- read.table('VR_socialneutralsounds_trial_by_trial.txt', sep = '')
neutral_social$V7 <- 'female'
neutral_social$v8 <- 'VR'

female_VR <- rbind(averisve_alone,averisve_social,neutral_alone,neutral_social)
colnames(female_VR) <- c("value","condition","valence","trial",'subject','half','gender','treatment')
female_VR$value <- female_VR$value * -1
female_VR$group <- 'female_VR'

female_VR$SCRs <- log10(female_VR$value +1)
female_VR<- female_VR[,-6]

################## read into female_real_life data ##############

f_averisve_alone <- read.table('female_aloneaversivesounds_trial_by_trial.txt',  sep = '')
f_neutral_alone <- read.table('female_aloneneutralsounds_trial_by_trial.txt',  sep = '')
f_averisve_social <- read.table('female_socialaversivesounds_trial_by_trial.txt',  sep = '')
f_neutral_social <- read.table('female_socialneutralsounds_trial_by_trial.txt',  sep = '')

female_real_life <- rbind(f_averisve_alone,f_neutral_alone,f_averisve_social,f_neutral_social)
colnames(female_real_life) <- c("value","condition","valence","trial",'subject')

female_real_life$gender <- 'female'
female_real_life$treatment <- 'real_life'
female_real_life$group <- 'female_real_life'

female_real_life$SCRs <- log10(female_real_life$value +1)

############### read into male_real_life data   ##################
m_averisve_alone <- read.table('male_aloneaversivesounds_trial_by_trial.txt',  sep = '')
m_neutral_alone <- read.table('male_aloneneutralsounds_trial_by_trial.txt',  sep = '')
m_averisve_social <- read.table('male_socialaversivesounds_trial_by_trial.txt',  sep = '')
m_neutral_social <- read.table('male_socialneutralsounds_trial_by_trial.txt',  sep = '')

male_real_life <- rbind(m_averisve_alone,m_neutral_alone,m_averisve_social,m_neutral_social)
colnames(male_real_life) <- c("value","condition","valence","trial",'subject')

male_real_life$gender <- 'male'
male_real_life$treatment <- 'real_life'
male_real_life$group <- 'male_real_life'

male_real_life$SCRs <- log10(male_real_life$value +1)

# combine female-real_life person, male-real_life person and female-VR

alldata <- rbind(female_real_life,female_VR,male_real_life)


#################  manipulation check   ####################

lmer11 <- lmer(SCRs ~ valence * gender +(1|subject), 
               alldata[alldata$condition == 'alone' & alldata$treatment != 'VR',] )
summary(lmer11)
Anova(lmer11,type = 3)
r.squaredGLMM(lmer11)

lmer12 <- lmer(SCRs ~ valence * treatment +(1|subject), 
               alldata[alldata$condition == 'alone'& alldata$gender != 'male',] )
summary(lmer12)
Anova(lmer12,type = 3)
r.squaredGLMM(lmer12)

#################  buffering effect   #######################


### attach the questionnaires######
questionnaire.all<- read.table("E:/Germany 2/On lab computer/social buffering-VR/the data/questionnaires/questionnaires_threeconditions.csv",sep = ',',header = TRUE)
alldata_questionnaire <-  merge(alldata,questionnaire.all,by=c('subject','gender','condition','treatment'),na.rm=TRUE)

alldata.aversive <- subset(alldata_questionnaire, valence == 'aversive') 

alldata.neutral <- subset(alldata_questionnaire, valence == 'neutral') 
 

############  test the  difference between female_real_life and male_real_life ###########

all.aversive.gender <- alldata.aversive[alldata.aversive$group != 'female_VR',]

## buffering effect

lmer12 <- lmer(SCRs ~ condition*gender+scale(prenegative)+scale(preanxiety)+scale(ADS)+(1|subject), 
               all.aversive.gender)
summary(lmer12)
Anova(lmer12,type = 3)
emmeans(lmer12, pairwise ~ condition|gender,pbkrtest.limit = 5000,lmerTest.limit = 5000)
r.squaredGLMM(lmer12)

## plot

# generate data (a mean for each subject for aversive and neutral sound)
data.mean<- summarySE(alldata_questionnaire[alldata_questionnaire$group != 'female_VR',], 
                      measurevar="SCRs", groupvars=c("subject",'condition','valence', 'gender'),
                      na.rm = TRUE)
data.mean <- data.mean[,c(1:6)]
data.mean$group <- paste(data.mean$gender,'_', data.mean$condition)
plotmean.spaghetti <- summarySE(data.mean,measurevar = 'SCRs',groupvars = c('valence','group'))
data_spaghetti <- merge(data.mean,plotmean.spaghetti,by=c('valence','group'),na.rm=TRUE)


p.bar <- ggplot(data_spaghetti,aes(x= factor(valence), y= SCRs.x, group=subject, color = valence,fill = valence ))

p.bar+geom_point( size=0.9)+ 
  facet_wrap(~group)+
  stat_summary(fun = mean, geom = 'bar',width=0.5,position =position_dodge(), aes(group =1))+
  geom_errorbar(mapping=aes(ymin=SCRs.y-se, ymax=SCRs.y+se), width=.07, size=1, color= 'black')+
  scale_fill_manual(values = c("black", "grey65"))+
  scale_color_manual(values = c("black", "grey65"))+
  theme(legend.position = "bottom",legend.text = element_text(size=16), legend.title = element_blank(),
        axis.text=element_text(size=16,color = 'black'),
        axis.title=element_text(size=18), 
        strip.text = element_text(size = 18))+
  ylab(expression(paste("SCRs (",mu,"S)")))+ xlab("Valence")+
  coord_cartesian(ylim = c(-0.05, 0.25))

## test anxiety sensitivity

# social concern
lmer14 <- lmer(SCRs ~ condition*gender*socialconcern+scale(prenegative)+scale(preanxiety)+scale(ADS)+(1|subject), 
               all.aversive.gender)
summary(lmer14)
Anova(lmer14,type=3)
r.squaredGLMM(lmer14)
interact_plot(lmer14, pred=socialconcern,modx=group,mod2 = gender,interval = TRUE,int.type = c("confidence"),
              int.width = 0.95)

# social concern effect in female_real_life condition
all.aversive.female <- all.aversive.gender[all.aversive.gender$gender == 'female',]
lmer15 <- lmer(SCRs ~ condition*socialconcern+scale(prenegative)+scale(preanxiety)+scale(ADS)+(1|subject), 
               all.aversive.female )
summary(lmer15)
Anova(lmer15,type=3)
r.squaredGLMM(lmer15)
emtrends(lmer15, pairwise ~ 'condition', var = "socialconcern") # simple slope analysis

lmer151 <- lmer(SCRs ~ socialconcern+scale(prenegative)+scale(preanxiety)+scale(ADS)+(1|subject), 
               all.aversive.female[all.aversive.female$condition=='alone',] )
summary(lmer151)
Anova(lmer151,type=3)

lmer152 <- lmer(SCRs ~ socialconcern+scale(prenegative)+scale(preanxiety)+scale(ADS)+(1|subject), 
                all.aversive.female[all.aversive.female$condition=='social',] )
summary(lmer152)
Anova(lmer152,type=3)

#  social concern effect in male_real_life condition
all.aversive.male <- all.aversive.gender[all.aversive.gender$gender =='male',]

lmer16 <- lmer(SCRs ~ condition*socialconcern+scale(prenegative)+scale(preanxiety)+scale(ADS)+ (1|subject), 
               all.aversive.male )

summary(lmer16)
Anova(lmer16,type=3)

lmer161 <- lmer(SCRs ~ socialconcern+scale(prenegative)+scale(preanxiety)+scale(ADS)+ (1|subject), 
               all.aversive.male[all.aversive.male$condition == 'alone',] )
summary(lmer161)
Anova(lmer161,type=3)


lmer162 <- lmer(SCRs ~ socialconcern+scale(prenegative)+scale(preanxiety)+scale(ADS)+ (1|subject), 
               all.aversive.male[all.aversive.male$condition == 'social',] )
summary(lmer162)
Anova(lmer162,type=3)

# physical concern
lmer17 <- lmer(SCRs ~ condition*gender*physicalconcern+scale(prenegative)+scale(preanxiety)+scale(ADS)+(1|subject), 
               all.aversive.gender)
summary(lmer17)
Anova(lmer17,type=3)
r.squaredGLMM(lmer17)

# cognitive concern
lmer18 <- lmer(SCRs ~ condition*gender*cognitiveconcern+scale(prenegative)+scale(preanxiety)+scale(ADS)+(1|subject), 
               all.aversive.gender)
summary(lmer18)
Anova(lmer18,type=3)
r.squaredGLMM(lmer18)

# the effect of impression
all.aversive.gender$sum_impre <- 40-(all.aversive.gender$helpful+ all.aversive.gender$easier +
                                 all.aversive.gender$pleasant + all.aversive.gender$sympathetic)

lmer19 <- lmer(SCRs ~ scale(sum_impre) * gender+scale(prenegative)+scale(preanxiety)+scale(ADS)+(1|subject), 
               all.aversive.gender)
summary(lmer19)
Anova(lmer19,type=3)
r.squaredGLMM(lmer19)


####### plot #########

# social concern
p.socialconcern1 <- ggplot( all.aversive.gender, aes(socialconcern, SCRs,color=condition))+
  facet_wrap(~gender)+
  scale_linetype_manual(values = c(1,2))+
  scale_color_manual(values = c("black", "grey65"))+
  stat_smooth(method = lm) +theme(legend.position = "bottom",legend.text = element_text(size=16), 
                                  legend.title = element_blank(),
                                  axis.text=element_text(size=16,color = 'black'),
                                  axis.title=element_text(size=18), 
                                  strip.text = element_text(size = 18))+ylab("SCRs")+xlab("Social concern") 


p.socialconcern1


# physical concern
p.physicalconcern1 <- ggplot( all.aversive.gender, aes(physicalconcern, SCRs,color=condition))+
  facet_wrap(~gender)+
  scale_linetype_manual(values = c(1,2))+
  scale_color_manual(values = c("black", "grey65"))+
  stat_smooth(method = lm) +theme(legend.position = "bottom",legend.text = element_text(size=16), 
                                  legend.title = element_blank(),
                                  axis.text=element_text(size=16,color = 'black'),
                                  axis.title=element_text(size=18), 
                                  strip.text = element_text(size = 18))+ylab("SCRs")+xlab("Physical concern") 


p.physicalconcern1

# cognitive concern
p.cognitiveconcern1 <- ggplot( all.aversive.gender, aes(cognitiveconcern, SCRs,color=condition))+
  facet_wrap(~gender)+
  scale_linetype_manual(values = c(1,2))+
  scale_color_manual(values = c("black", "grey65"))+
  stat_smooth(method = lm) +theme(legend.position = "bottom",legend.text = element_text(size=16), 
                                  legend.title = element_blank(),
                                  axis.text=element_text(size=16,color = 'black'),
                                  axis.title=element_text(size=18), 
                                  strip.text = element_text(size = 18))+ylab("SCRs")+xlab("Cognitive concern") 


p.cognitiveconcern1

############# test the buffering effect difference between female_real_life and female_VR ################

alldata.aversive.VR <- alldata.aversive[alldata.aversive$group!= 'male_real_life',]

lmer20 <- lmer(SCRs ~ treatment * condition+scale(prenegative)+scale(preanxiety)+scale(ADS)+(1|subject), 
               alldata.aversive.VR)
summary(lmer20)
Anova(lmer20,type = 3)
r.squaredGLMM(lmer20)
cat_plot(lmer20,condition,group,geom = 'bar',int.type = 'confidence',plot.points = FALSE)

# VR condition
lmer21 <- lmer(SCRs ~ condition +scale(prenegative)+scale(preanxiety)+scale(ADS)+(1|subject), 
                        alldata.aversive[alldata.aversive$group == 'female_VR',])
summary(lmer21)
Anova(lmer21,type = 3)
cat_plot(lmer21,group,geom = 'bar',int.type = 'confidence',plot.points = FALSE)
emmeans(lmer21, pairwise ~ condition,pbkrtest.limit = 3486,lmerTest.limit = 3486)
r.squaredGLMM(lmer21)

####### plot #########

# generate data (a mean for each subject for aversive and neutral sound)
data.mean<- summarySE(alldata_questionnaire[alldata_questionnaire$group != 'male_real_life',], measurevar="SCRs", groupvars=c("subject",'group','valence', 'condition','treatment'),
                      na.rm = TRUE)
data.mean <- data.mean[,c(1:7)]
data.mean$group <- paste(data.mean$treatment,'_', data.mean$condition)
plotmean.spaghetti <- summarySE(data.mean,measurevar = 'SCRs',groupvars = c('valence','group'))
data_spaghetti <- merge(data.mean,plotmean.spaghetti,by=c('valence','group'),na.rm=TRUE)

p.bar2 <- ggplot(data_spaghetti,aes(x= factor(valence), y= SCRs.x, group=subject, color = valence,fill = valence ))
p.bar2+geom_point( size=0.9)+ 
  facet_wrap(~group)+
  stat_summary(fun = mean, geom = 'bar',width=0.5,position =position_dodge(), aes(group =1))+
  geom_errorbar(mapping=aes(ymin=SCRs.y-se, ymax=SCRs.y+se), width=.07, size=1, color= 'black')+
  scale_color_manual(values = c("black", "grey65"))+
  scale_fill_manual(values = c("black", "grey65"))+
  theme(legend.position = "bottom",legend.text = element_text(size=16), legend.title = element_blank(),
        axis.text=element_text(size=16,color = 'black'),
        axis.title=element_text(size=18), 
        strip.text = element_text(size = 18))+
  ylab(expression(paste("SCRs (",mu,"S)")))+ xlab("Valence")+
  coord_cartesian(ylim = c(-0.05, 0.25))



# test AS effect  between female_real_life and female_VR

# social concern
lmer23 <- lmer(SCRs ~ treatment*condition*socialconcern +scale(prenegative)+scale(preanxiety)+scale(ADS)+(1|subject), 
               alldata.aversive.VR)
summary(lmer23)
Anova(lmer23,type=3)
r.squaredGLMM(lmer23)

interact_plot(lmer15, pred=socialconcern,modx=group,mod2 = condition,interval = TRUE,int.type = c("confidence"),
              int.width = 0.95)


# social concern effect in female_real_life condition
lmer24 <- lmer(SCRs ~ condition*socialconcern +scale(prenegative)+scale(preanxiety)+scale(ADS)+(1|subject), 
               alldata.aversive.VR[alldata.aversive.VR$treatment == 'real_life',])
summary(lmer24)
Anova(lmer24,type=3)
r.squaredGLMM(lmer24)


emtrends(lmer11, pairwise ~ 'group', var = "socialconcern") # simple slope analysis


# social concern effect in female_VR condition
lmer25 <- lmer(SCRs ~ condition*socialconcern +scale(prenegative)+scale(preanxiety)+scale(ADS)+(1|subject), 
               alldata.aversive.VR[alldata.aversive.VR$treatment == 'VR',])
summary(lmer25)
Anova(lmer25,type=3)
r.squaredGLMM(lmer25)

####### plot #########

p.socialconcern2 <- ggplot(alldata.aversive[alldata.aversive$group != 'male_real_life',], aes(socialconcern, SCRs,color=condition))+
  facet_wrap(~treatment)+
  scale_linetype_manual(values = c(1,2))+
  scale_color_manual(values = c("black", "grey65"))+
  stat_smooth(method = lm) +theme(legend.position = "bottom",legend.text = element_text(size=16), 
                                  legend.title = element_blank(),
                                  axis.text=element_text(size=16,color = 'black'),
                                  axis.title=element_text(size=18), 
                                  strip.text = element_text(size = 18))+
                                  ylab(expression(paste("SCRs (",mu,"S)")))+
                                  xlab("Social concern")


p.socialconcern2


# physical concern
lmer26 <- lmer(SCRs ~ treatment*condition*physicalconcern +scale(prenegative)+scale(preanxiety)+scale(ADS)+(1|subject), 
               alldata.aversive.VR)
summary(lmer26)
Anova(lmer26,type=3)
r.squaredGLMM(lmer26)

interact_plot(lmer26, pred=physicalconcern,modx=condition,mod2 = treatment,interval = TRUE,int.type = c("confidence"),
              int.width = 0.95)


# cognitive concern
lmer27 <- lmer(SCRs ~ treatment*condition*cognitiveconcern +scale(prenegative)+scale(preanxiety)+scale(ADS)+(1|subject), 
               alldata.aversive.VR)
summary(lmer27)
Anova(lmer27,type=3)
r.squaredGLMM(lmer27)

interact_plot(lmer27, pred=cognitiveconcern,modx=condition,mod2 = treatment,interval = TRUE,int.type = c("confidence"),
              int.width = 0.95)

## impression rating

alldata.aversive.VR$sum_impre <- 40-(alldata.aversive.VR$helpful+ alldata.aversive.VR$easier +
                                       alldata.aversive.VR$pleasant+alldata.aversive.VR$sympathetic)

lmer19 <- lmer(SCRs ~ scale(sum_impre) * treatment+scale(prenegative)+scale(preanxiety)+scale(ADS)+(1|subject), 
               alldata.aversive.VR)
summary(lmer19)
Anova(lmer19,type=3)
r.squaredGLMM(lmer19)


#### the correlation between behavioral ratings and SCRs ####

# read into behavioral ratings data
# read into female data

behavioralpath = "E:/Germany 2/On lab computer/social buffering-VR/the data/behavioral data/"

# read into female_real_life data
behavior_female_alone <- read.table(paste(behavioralpath,"female_ratings_trial_by_trial_alone.txt",sep = ""))
names(behavior_female_alone)<-c("subject","rating","valence","trial","condition")

behavior_female_social <-read.table(paste(behavioralpath,"female_ratings_trial_by_trial_social.txt",sep = ""))
names(behavior_female_social)<-c("subject","rating","valence","trial","condition")

# read into male data
behavior_male_alone <- read.table(paste(behavioralpath,"male_ratings_trial_by_trial_alone.txt",sep = ""))
names(behavior_male_alone)<-c("rating","valence","trial","condition","subject")
behavior_male_alone$subject <- behavior_male_alone$subject+100

behavior_male_social <- read.table(paste(behavioralpath,"male_ratings_trial_by_trial_social.txt",sep = ""))
names(behavior_male_social)<-c("rating","valence","trial","condition","subject")
behavior_male_social$subject <- behavior_male_social$subject+100

# read into female_VR data
behavior_alone_VR <- read.table(paste(behavioralpath,"alone_VR.txt",sep = ""),sep = '')
behavior_alone_VR <- behavior_alone_VR[,c(-3,-5:-7,-9,-10)]
names(behavior_alone_VR)<-c("subject","condition","trial","rating","valence")

behavior_social_VR <- read.table(paste(behavioralpath,"social_VR.txt",sep = ""),sep = '')
behavior_social_VR <- behavior_social_VR[,c(-3,-5:-7,-9,-10)]
names(behavior_social_VR)<-c("subject","condition","trial","rating","valence")

## combine data

behavior.all <- rbind(behavior_female_alone,behavior_female_social,behavior_male_alone,behavior_male_social,behavior_alone_VR,behavior_social_VR)

behavior.all[behavior.all$valence=='2',]$valence <- 'neutral'
behavior.all[behavior.all$valence=='1',]$valence <- 'aversive'


## combine behavioral and SCRs data

alldata <- merge(behavior.all,alldata_questionnaire,by = c('subject','valence','trial','condition'))

## test the correlation

# female and male
alldata$scalerating <- scale(alldata$rating)
lmer1 <- lmer(SCRs ~ scalerating * gender +(1|subject), 
              alldata[alldata$group != 'female_VR',])
summary(lmer1)
Anova(lmer1,type = 3)
vif(lmer1)
r.squaredGLMM(lmer1)
emtrends(lmer1, pairwise ~ 'gender', var = "scalerating",pbkrtest.limit = 7257, lmerTest.limit = 7257) # simple slope analysis

# VR 
lmer3 <- lmer(SCRs ~ scalerating*treatment +(1|subject), 
              alldata[alldata$group != 'male_real_life' ,])
summary(lmer3)
Anova(lmer3,type = 3)
vif(lmer3)
r.squaredGLMM(lmer3)
emtrends(lmer3, pairwise ~ 'treatment', var = "scalerating",pbkrtest.limit = 7257, lmerTest.limit = 7257) # simple slope analysis




## test three groups' differences on neutral sound

lmer15 <- lmer(log_value ~ group*group2+scale(ADS)+scale(postanxiety)+scale(significant.others)+scale(family)+scale(friends)+
                 scale(cognitiveconcern)+scale(physicalconcern)+(1|subject), 
                 alldata.neutral.filter)
summary(lmer15)
Anova(lmer15,type = 3)
cat_plot(lmer15,group2,group,geom = 'bar',int.type = 'confidence',plot.points = FALSE)

alldata.meanbysubject <- summarySE(alldata, measurevar='value', 
                                   groupvars=c('subject','valence','gender','group','group2'))

