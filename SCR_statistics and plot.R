
## for social buffering analysis

# install the package
list.of.packages = c("Matrix","plyr","car","stringr","foreign","ggplot2","ggpmisc","psych","lmerTest","tidyr","gridExtra",
                     "mediation","QuantPsyc", "interactions", "lavaan", "emmeans","estimability","optimx","",
                     "MuMIn","sjstats","lme4","simr","pbkrtest","latticeExtra","Hmisc","ggpubr","Rmisc","ggeffects",
                     "dplyr") #  package names
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {install.packages(new.packages)}
lapply(list.of.packages, require, character.only=T)


rm(list = ls())
library(Matrix)
library(plyr)
library(car)
library(stringr)
library(foreign)
library(ggplot2)
library(ggpmisc)
library(psych)
library(lmerTest)
library(tidyr)
library(gridExtra)
library(mediation)
library(QuantPsyc)
library(interactions)
library(lavaan)
library(emmeans)
library(estimability)
library(optimx)
library(MuMIn)
library(sjstats)
library(lme4)
library(simr)
library(pbkrtest)
library(latticeExtra)
library(Hmisc)
library(ggpubr)
library(Rmisc)
library(ggeffects)
library(dplyr)
library(effsize)

data_path = 'D:social buffering 2/manuscript/submission/first submission/code and data for updating/SCR preprocessed data/'

## read into male alone data (aversive) ##

m_averisve_alone <- read.table(paste(data_path,'male_aloneaversivesounds_trial_by_trial.txt',sep = ""),  sep = '')
m_averisve_alone$V6 <- 'male_alone'

# read in male social data (aversive) 
m_averisve_social <- read.table(paste(data_path,'male_socialaversivesounds_trial_by_trial.txt',sep = ""),  sep = '')
m_averisve_social$V6 <- 'male_social'

# read in male alone  data (neutral sound)
m_neutral_alone <- read.table(paste(data_path,'male_aloneneutralsounds_trial_by_trial.txt',sep = ""),  sep = '')
m_neutral_alone$V6 <- 'male_alone'

# read into male social  neutral data
m_neutral_social <- read.table(paste(data_path,'male_socialneutralsounds_trial_by_trial.txt',sep = ""),  sep = '')
m_neutral_social$V6 <- 'male_social'

m_allsound <- rbind(m_averisve_alone,m_averisve_social,m_neutral_alone,m_neutral_social)
colnames(m_allsound) <- c("value","treatment","valence","trial",'subject','type')
m_allsound$gender <- 'male'

## read into female data ##
f_averisve_alone <- read.table(paste(data_path,'female_aloneaversivesounds_trial_by_trial.txt',sep = ""),  sep = '')
f_averisve_alone$V6 <- 'female_alone'

f_neutral_alone <- read.table(paste(data_path,'female_aloneneutralsounds_trial_by_trial.txt',sep = ""),  sep = '')
f_neutral_alone$V6 <- 'female_alone'

f_averisve_social <- read.table(paste(data_path,'female_socialaversivesounds_trial_by_trial.txt',sep = ""),  sep = '')
f_averisve_social$V6 <- 'female_social'

f_neutral_social <- read.table(paste(data_path,'female_socialneutralsounds_trial_by_trial.txt',sep = ""),  sep = '')
f_neutral_social$V6 <- 'female_social'

f_allsound <- rbind(f_averisve_alone,f_averisve_social, f_neutral_alone,f_neutral_social)

colnames(f_allsound) <- c("value","treatment","valence","trial",'subject','type')
f_allsound$gender <- 'female'

## combine male and female data ##
f_m_allsound <- rbind(m_allsound,f_allsound)
f_m_allsound$value <- log10(f_m_allsound$value+1)

# attach the questionnaires
questionnaire <- read.csv("D:/social buffering 2/manuscript/submission/first submission/code and data for updating/questionnaires data.csv",header = TRUE,sep = ';')
names(questionnaire)[1]<- "subject"
questionnaire$sumimpression <- questionnaire$pleasant+questionnaire$easier+questionnaire$helpfu+questionnaire$sympathetic
f_m_allsound_questionnaires <- merge(f_m_allsound,questionnaire,by=c('gender','subject'),na.rm=TRUE)

##################################statistics################################

# manipulation check (alone treatment group)
lmer1 <- lmer(value ~  valence*gender+scale(ADS)+scale(significant.others)+scale(family)+scale(friends)+(1|subject), 
              f_m_allsound_questionnaires[f_m_allsound_questionnaires$treatment == 'alone',])
summary(lmer1)
Anova(lmer1,type = 3)
r.squaredGLMM(lmer1)
emmeans(lmer1, pairwise ~ gender|valence,pbkrtest.limit = 3486,lmerTest.limit = 3486)
emmeans(lmer1, pairwise ~ valence|gender,pbkrtest.limit = 3486,lmerTest.limit = 3486)
vif(lmer1)

nulllmer1 <- lmer(value ~ (1|subject), alone_questionnaires)
anova(lmer1,nulllmer1)

# cohen d
effsize::cohen.d(value ~ gender, f_m_allsound_questionnaires[f_m_allsound_questionnaires$valence == 'aversive' & 
                                                             f_m_allsound_questionnaires$treatment == 'alone',])
effsize::cohen.d(value ~ gender, f_m_allsound_questionnaires[f_m_allsound_questionnaires$valence == 'neutral' & 
                                                             f_m_allsound_questionnaires$treatment == 'alone',])
effsize::cohen.d(value~valence, f_m_allsound_questionnaires[f_m_allsound_questionnaires$gender=='female' &
                                                            f_m_allsound_questionnaires$treatment == 'alone',]  )
effsize::cohen.d(value~valence, f_m_allsound_questionnaires[f_m_allsound_questionnaires$gender=='male' &
                                                            f_m_allsound_questionnaires$treatment == 'alone',]  )



# social buffering effect (all the participants)
f_m_aversivesound_questionnaires <- f_m_allsound_questionnaires[f_m_allsound_questionnaires$valence =='aversive',]

lmer2 <- lmer(value ~ treatment*gender+scale(ADS)+scale(significant.others)+scale(family)+scale(friends)+(1|subject),
              f_m_aversivesound_questionnaires)
summary(lmer2)
Anova(lmer2,type = 3)
vif(lmer2)
r.squaredGLMM(lmer2)
emmeans(lmer2, pairwise ~ treatment|gender,pbkrtest.limit = 3609,lmerTest.limit = 3609)
effsize::cohen.d(value~treatment, f_m_aversivesound_questionnaires[f_m_aversivesound_questionnaires$gender=='female',]  )
effsize::cohen.d(value~treatment, f_m_aversivesound_questionnaires[f_m_aversivesound_questionnaires$gender=='male',]  )

nullmer2 <- lmer(value ~ (1|subject),
                 f_m_aversivesound_questionnaires)
anova(lmer2,nullmer2)

# test the effect of impression ratings
lmer3 <- lmer(value ~ scale(sumimpression)*gender+
                scale(ADS)+scale(significant.others)+scale(family)+scale(friends)+ (1|subject),
              f_m_aversivesound_questionnaires[f_m_aversivesound_questionnaires$treatment=='social',])
summary(lmer3)
vif(lmer3)
Anova(lmer3,type = 3)
r.squaredGLMM(lmer3)

nulllmer3 <- lmer(value ~ (1|subject), f_m_aversivesound_questionnaires[f_m_aversivesound_questionnaires$treatment=='social',])
summary(nulllmer3)

anova(lmer3,nulllmer3)


# test the effect of anxiety sensitivity

# socialconcern
lmer4 <- lmer(value ~ scale(socialconcern)*treatment*gender+scale(ADS)+scale(significant.others)+scale(family)+scale(friends)+
                (1|subject),
              f_m_aversivesound_questionnaires)
summary(lmer4)
Anova(lmer4,type = 3)
vif(lmer4)
r.squaredGLMM(lmer4)
nulllmer4 <- lmer(value ~ (1|subject),
                  f_m_aversivesound_questionnaires)
anova(lmer4,nulllmer4)

# for females
lmer41 <- lmer(value ~ scale(socialconcern)*treatment+scale(ADS)+scale(significant.others)+scale(family)+scale(friends)+
                 (1|subject), f_m_aversivesound_questionnaires[f_m_aversivesound_questionnaires$gender=='female',])
summary(lmer41)
Anova(lmer41,type = 3)
r.squaredGLMM(lmer41)
vif(lmer41)

nulllmer41 <- lmer(value ~  (1|subject), f_m_aversivesound_questionnaires[f_m_aversivesound_questionnaires$gender=='female',])

anova(lmer41, nulllmer41)

# for males
lmer42 <- lmer(value ~ scale(socialconcern)*treatment+scale(ADS)+scale(significant.others)+scale(family)+scale(friends)+
                  (1|subject), f_m_aversivesound_questionnaires[f_m_aversivesound_questionnaires$gender=='male',])
summary(lmer42)
Anova(lmer42,type = 3)
r.squaredGLMM(lmer42)
vif(lmer42)

nulllmer42 <- lmer(value ~ (1|subject), f_m_aversivesound_questionnaires[f_m_aversivesound_questionnaires$gender=='male',])
anova(lmer42, nulllmer42)


# cognitive concern
lmer5 <- lmer(value ~ scale(cognitiveconcern)*treatment*gender+
                +scale(ADS)+scale(significant.others)+scale(family)+scale(friends)+(1|subject),
              f_m_aversivesound_questionnaires)
summary(lmer5)
Anova(lmer5,type = 3)
vif(lmer5)
r.squaredGLMM(lmer5)

nulllmer5 <- lmer(value ~ (1|subject), f_m_aversivesound_questionnaires)
anova(lmer5,nulllmer5)

# physical concern
lmer6 <- lmer(value ~ scale(physicalconcern)*treatment*gender+
                +scale(ADS)+scale(significant.others)+scale(family)+scale(friends)+(1|subject),
              f_m_aversivesound_questionnaires)
summary(lmer6)
Anova(lmer6,type = 3)
vif(lmer6)
r.squaredGLMM(lmer6)

nulllmer6 <- lmer(value ~ (1|subject), f_m_aversivesound_questionnaires)

anova(lmer6,nulllmer6)


# test the buffering effect  for participants in low social concern
lmer7 <- lmer(value ~ treatment*gender+scale(ADS)+scale(significant.others)+scale(family)+scale(friends)+(1|subject),
              f_m_aversivesound_questionnaires[f_m_aversivesound_questionnaires$socialconcernlevel=='low social concern',])
summary(lmer7)
Anova(lmer7,type = 3)
vif(lmer7)
r.squaredGLMM(lmer7)
emmeans(lmer7, pairwise ~ treatment|gender,pbkrtest.limit = 3609,lmerTest.limit = 3609)

effsize::cohen.d(value~treatment, f_m_aversivesound_questionnaires[f_m_aversivesound_questionnaires$gender=='female' & 
                                                                f_m_aversivesound_questionnaires$socialconcernlevel=='low social concern',]  )
effsize::cohen.d(value~treatment, f_m_aversivesound_questionnaires[f_m_aversivesound_questionnaires$gender=='male' &
                                                                  f_m_aversivesound_questionnaires$socialconcernlevel=='low social concern',]  )


nullmer7 <- lmer(value ~ (1|subject),
                 f_m_aversivesound_questionnaires[f_m_aversivesound_questionnaires$socialconcernlevel=='low social concern',])
summary(nullmer7)
anova(lmer7,nullmer7)


# test the buffering effect only for participants in high social concern
lmer8 <- lmer(value ~ treatment*gender+scale(ADS)+scale(significant.others)+scale(family)+scale(friends)+(1|subject),
               f_m_aversivesound_questionnaires[f_m_aversivesound_questionnaires$socialconcernlevel=='high social concern',])
summary(lmer8)
Anova(lmer8,type = 3)
r.squaredGLMM(lmer8)
vif(lmer8)

nullmer8 <- lmer(value ~ (1|subject),
                  f_m_aversivesound_questionnaires[f_m_aversivesound_questionnaires$socialconcernlevel=='high social concern',])
summary(nullmer8)
anova(lmer8,nullmer8)

# test whether there is a difference between high and low social concern in females in alone treatment group
lmer9 <- lmer(value ~ socialconcernlevel+scale(ADS)+scale(significant.others)+scale(family)+scale(friends)+(1|subject),
               f_m_aversivesound_questionnaires[f_m_aversivesound_questionnaires$gender == 'female' & 
                                                  f_m_aversivesound_questionnaires$treatment == 'alone',])
summary(lmer9)
Anova(lmer9,type = 3)
r.squaredGLMM(lmer9)
vif(lmer9)
nulllmer9 <- lmer(value ~ (1|subject),
               f_m_aversivesound_questionnaires[f_m_aversivesound_questionnaires$gender == 'female' & 
                                                  f_m_aversivesound_questionnaires$treatment == 'alone',])

anova(lmer9,nulllmer9)



################################## plot #####################################

####### Figure 1 #########

# generate data (a mean for each subject for aversive and neutral sound)
data.mean<- summarySE(f_m_allsound, measurevar="value", groupvars=c("subject",'gender','type', 'treatment','valence'),
                      na.rm = TRUE)
data.mean <- data.mean[,c(1,2,3,4,5,7)]
plotmean.spaghetti <- summarySE(data.mean,measurevar = 'value',groupvars = c('type','valence'))
data_spaghetti <- merge(data.mean,plotmean.spaghetti,by=c('type','valence'),na.rm=TRUE)

p.spaghetti <- ggplot(data_spaghetti,aes(x= factor(valence), y= value.x, group=subject, color = valence))

p.spaghetti+geom_point( size=0.9)+ geom_line( color = 'grey' )+
  geom_errorbar(mapping=aes(ymin=value.y-se, ymax=value.y+se), width=.07, size=1, color= 'black')+
  stat_summary(fun = mean, geom = 'point',size = 2.5,aes(group =1),color= 'black')+
  stat_summary(fun = mean, geom="line",lwd=1,aes(group=1), color = "black")+ 
  facet_wrap(~type)+
  scale_color_manual(values = c("black", "grey65"))+
  theme(legend.position = "bottom",legend.text = element_text(size=16), legend.title = element_blank(),
        axis.text=element_text(size=16,color = 'black'),
        axis.title=element_text(size=18), 
        strip.text = element_text(size = 18))+
  ylab("SCRs")+ xlab("Valence")


####### Figure 2 #########

p.socialconcern <- ggplot(f_m_aversivesound_questionnaires, aes(socialconcern, value,color=treatment))+
  facet_wrap(~gender)+
  scale_linetype_manual(values = c(1,2))+
  scale_color_manual(values = c("black", "grey65"))+
  stat_smooth(method = lm) +theme(legend.position = "bottom",legend.text = element_text(size=16), 
                                  legend.title = element_blank(),
                                  axis.text=element_text(size=16,color = 'black'),
                                  axis.title=element_text(size=18), 
                                  strip.text = element_text(size = 18))+ylab("SCRs")+xlab("Social concern")


p.socialconcern

####### Figure 3 #########

# order the social concernl level by 'low' and 'high'
f_m_aversivesound_questionnaires$socialconcernlevel = factor(f_m_aversivesound_questionnaires$socialconcernlevel,
                                                              levels = c('low social concern','high social concern'))

p20 <- ggplot(f_m_aversivesound_questionnaires,aes(gender,value))
p20 + geom_boxplot(aes(color = treatment), width = 0.10, position = position_dodge(0.6))+
  scale_color_manual(values = c("black", "grey65"))+
  facet_grid(~socialconcernlevel)+
  labs(x = "Gender", y = "SCRs") +
  theme(legend.position = "bottom",legend.text = element_text(size=16), legend.title = element_blank(),
        axis.text=element_text(size=16,color = 'black'), axis.title=element_text(size=18), 
        strip.text = element_text(size = 18))


####### Figure S1 #########

# plot for cognitive concern (treatment * gender)
p.cognitiveconcern <- ggplot(f_m_aversivesound_questionnaires, aes(cognitiveconcern, value,color=treatment))+
  facet_wrap(~gender)+
  scale_linetype_manual(values = c(1,2))+
  scale_color_manual(values = c("black", "grey65"))+
  stat_smooth(method = lm) +theme(legend.position = "bottom",legend.text = element_text(size=16), 
                                  legend.title = element_blank(),
                                  axis.text=element_text(size=16,color = 'black'),
                                  axis.title=element_text(size=18), 
                                  strip.text = element_text(size = 18))+ylab("SCRs")+xlab("Cognitive concern")


p.cognitiveconcern

# plot for physical concern (treatment * gender)

p.physicalconcern <- ggplot(f_m_aversivesound_questionnaires, aes(physicalconcern, value,color=treatment))+
  facet_wrap(~gender)+
  scale_linetype_manual(values = c(1,2))+
  scale_color_manual(values = c("black", "grey65"))+
  stat_smooth(method = lm) +theme(legend.position = "bottom",legend.text = element_text(size=16), 
                                  legend.title = element_blank(),
                                  axis.text=element_text(size=16,color = 'black'),
                                  axis.title=element_text(size=18), 
                                  strip.text = element_text(size = 18))+ylab("SCRs")+xlab("Physical concern")


p.physicalconcern


