########## LMM with all MMR valies #########

# Behavioral vs neurophysiol
# Dependent variable:
# MMR means based the positive cluster at measurement 1 and 2 + negative cluster at measurement 1 and 2. 
# Alltogehter 8 amplitude values from each participant as dependent variable.
# Means from two separate time-windows: positive cluster 240-290ms and 290-340 ms; negative cluster 390-440 and 440-490ms.
# Electrodes used for means: positive cluster AF3, F3, FC5, F7, Fz, FC1, C3, FC6, F8; negative cluster C4, Cp2,	Pz,	Cz,	F4,	Fc2,	F3,	Fc1
# Independent variables: d' from the first and second behavioral tasks at T1 and T2 

# neccessary packages
library(readxl)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)
library(dplyr)
library(sjPlot)
library(tidyr)
library(car)
library(vcd)
library(lmtest)
library(influence.ME)
library(MuMIn)
library(effectsize)

#####################################################
################ Preparation ########################
#####################################################

library(readxl)
dat <- read_excel("modeling2re.xlsx")

dat$p_no <- as.factor(dat$p_no)
class(dat$p_no)
dat$mmr_all <- as.numeric(dat$mmr_all)
class(dat$mmr_all)
dat$tw <- as.factor(dat$tw)
class(dat$tw)
dat$tw_four <- as.factor(dat$tw_four)
class(dat$tw_four)
dat$time <- as.factor(dat$time)
class(dat$time)
dat$age1 <- as.numeric(dat$age1)
class(dat$age1)
dat$group <- as.factor(dat$group)
class(dat$group)
dat$b1_1 <- as.numeric(dat$b1_1)
class(dat$b1_1)
dat$b1_2 <- as.numeric(dat$b1_2)
class(dat$b1_2)
dat$b2_1 <- as.numeric(dat$b2_1)
class(dat$b2_1)
dat$b2_2 <- as.numeric(dat$b2_2)
class(dat$b2_2)
dat$lang <- as.numeric(dat$lang)
class(dat$lang)


###################################################
################ Modeling ########################
##################################################

library(lme4)
library(lmerTest)

model_0 <- lmer(mmr_all~1 + (time|p_no), data=dat, REML = FALSE)
summary(model_0)

model_1 <- lmer(mmr_all~tw*time + (time|p_no), data=dat, REML = FALSE)
summary(model_1)

#################################################
############## b1_1 #############################
#################################################

# General relationship

model_2 <- lmer(mmr_all~tw*time + b1_1 + (time|p_no), data=dat, REML = FALSE)
summary(model_2) #Not sig


# Is the relationship between behavioral discrimination and MMR amplitude different between groups?

model_22 <- lmer(mmr_all~tw*time + b1_1*group + (time|p_no), data=dat, REML = FALSE)
summary(model_2222) # not sig



#################################################
############## b1_2 #############################
#################################################

# General relationship

model_3 <- lmer(mmr_all~tw*time + b1_2 + (time|p_no), data=dat, REML = FALSE)
summary(model_3) #Not sig


# Is the relationship between behavioral discrimination and MMR amplitude different between groups?

model_33 <- lmer(mmr_all~tw*time + group*b1_2 + (time|p_no), data=dat, REML = FALSE)
summary(model_3333) # not sig


#################################################
############## b2_1 #############################
#################################################


# General relationship

model_4 <- lmer(mmr_all~tw*time + b2_1 + (time|p_no), data=dat, REML = FALSE)
summary(model_4) #Not sig


# Is the relationship between behavioral discrimination and MMR amplitude different between groups?


model_44 <- lmer(mmr_all~tw*time + group*b2_1 + (time|p_no), data=dat, REML = FALSE)
summary(model_4444) #Not sig



#################################################
############## b2_2 #############################
#################################################


# General relationship

model_5 <- lmer(mmr_all~tw*time + b2_2 + (time|p_no), data=dat, REML = FALSE)
summary(model_5) #Not sig

# Is the relationship between behavioral discrimination and MMR amplitude different between groups?

model_55 <- lmer(mmr_all~tw*time + group*b2_2 + (time|p_no), data=dat, REML = FALSE)
summary(model_5555) #Not sig



