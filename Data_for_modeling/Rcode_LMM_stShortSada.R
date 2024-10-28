########## LMM with stShortSada #######

library(readxl)

dat <- read_excel("table_LMM_stShortSada.xlsx")

########################################################
##### Check if every variable in the table is okay #####
#######################################################

summary(dat$p_code)
unique_values <- unique(dat$p_code)
print(unique_values)

# Count of unique values
num_unique_values <- length(unique_values)
print(num_unique_values)

summary(dat$p_no)
nique_values <- unique(dat$p_no)
print(unique_values)

# Count of unique values
num_unique_values <- length(unique_values)
print(num_unique_values)

summary(dat$trial_no)

summary(dat$sex)
unique_values <- unique(dat$sex)
print(unique_values)

# Count of unique values
num_unique_values <- length(unique_values)
print(num_unique_values)

summary(dat$group)
unique_values <- unique(dat$group)
print(unique_values)

summary(dat$mother_edu)
unique_values <- unique(dat$mother_edu)
print(unique_values)

summary(dat$hand)
unique_values <- unique(dat$hand)
print(unique_values)

####################################################
##### Testing model assumptions BEFORE MODELING ####
###################################################

### Multikolineaarsus

cov2cor(cov(dat[,c("age", "lang", "int", "d_1","d_2")]))

#korrelatsioon suurem kui 0.8?

###################################################
################ Modeling ########################
##################################################

library(lme4)
library(lmerTest)



### 0 model and main effects

mudel_0 <- lmer(st64~1 + (1|p_no), data=dat)
summary(mudel_0) 

mudel_1 <- lmer(st64~age + (1|p_no), data=dat)
summary(mudel_1) 

mudel_2 <- lmer(st64~sex + (1|p_no), data=dat)
summary(mudel_2) 

mudel_3 <- lmer(st64~group + (1|p_no), data=dat)
summary(mudel_3)

mudel_4 <- lmer(st64~lang + (1|p_no), data=dat)
summary(mudel_3) 

mudel_5 <- lmer(st64~int + (1|p_no), data=dat)
summary(mudel_5)

mudel_6 <- lmer(st64~d_1 + (1|p_no), data=dat)
summary(mudel_6) 

mudel_7 <- lmer(st64~d_2 + (1|p_no), data=dat)
summary(mudel_7) 

mudel_8 <- lmer(st64~edu + (1|p_no), data=dat)
summary(mudel_8) 


################### interactions with age ##################################################################

mudel_0 <- lmer(st64~1 + (1|p_no), data=dat)
summary(mudel_0) 

mudel_1 <- lmer(st64~age*sex + (1|p_no), data=dat)
summary(mudel_1) 


mudel_3 <- lmer(st64~age*group + (1|p_no), data=dat)
summary(mudel_3)

mudel_4 <- lmer(st64~age*lang + (1|p_no), data=dat)
summary(mudel_3) 

mudel_5 <- lmer(st64~ age*int + (1|p_no), data=dat)
summary(mudel_5) 

mudel_6 <- lmer(st64~age*d_1 + (1|p_no), data=dat)
summary(mudel_6) 

mudel_7 <- lmer(st64~age*d_2 + (1|p_no), data=dat)
summary(mudel_7) 


################### interactions with sex ##################################################################

mudel_0 <- lmer(st64~1t + (1|p_no), data=dat)
summary(mudel_0) 

mudel_3 <- lmer(st64~sex*group + (1|p_no), data=dat)
summary(mudel_3)

mudel_4 <- lmer(st64~sex*lang + (1|p_no), data=dat)
summary(mudel_4) 

mudel_5 <- lmer(st64~sex*int + (1|p_no), data=dat)
summary(mudel_5) 

mudel_6 <- lmer(st64~sex*d_1 + (1|p_no), data=dat)
summary(mudel_6) 

mudel_7 <- lmer(st64~sex*d_2 + (1|p_no), data=dat)
summary(mudel_7) 


################### interactions with group ##################################################################

mudel_0 <- lmer(st64~1 + (1|p_no), data=dat)
summary(mudel_0) 

mudel_4 <- lmer(st64~group*lang + (1|p_no), data=dat)
summary(mudel_4) 

mudel_5 <- lmer(st64~group*int + (1|p_no), data=dat)
summary(mudel_5) 

mudel_6 <- lmer(st64~group*d_1 + (1|p_no), data=dat)
summary(mudel_6) 

mudel_7 <- lmer(st64~group*d_2 + (1|p_no), data=dat)
summary(mudel_7) 


################### interactions with lang ##################################################################

mudel_0 <- lmer(st64~1 + (1|p_no), data=dat)
summary(mudel_0) 

mudel_5 <- lmer(st64~lang*int + (1|p_no), data=dat)
summary(mudel_5) 

mudel_6 <- lmer(stv64~lang*d_1 + (1|p_no), data=dat)
summary(mudel_6) 

mudel_7 <- lmer(st64~lang*d_2 + (1|p_no), data=dat)
summary(mudel_7) 


################### interactions with int ##################################################################

mudel_0 <- lmer(st64~1+ (1|p_no), data=dat)
summary(mudel_0) 


mudel_6 <- lmer(st64~int*d_1 + (1|p_no), data=dat)
summary(mudel_6) 

mudel_7 <- lmer(st64~int*d_2 + (1|p_no), data=dat)
summary(mudel_7) 


################### interactions with d_1 ##################################################################

mudel_0 <- lmer(st64~1 + (1|p_no), data=dat)
summary(mudel_0) 


mudel_7 <- lmer(st64~d_1*d_2 + (1|p_no), data=dat)
summary(mudel_7)

#### No significant predictors found

