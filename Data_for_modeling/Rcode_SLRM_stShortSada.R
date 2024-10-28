##########################################################
############## SLRM with stShortSada ####################
#########################################################


library(readxl)

dat <- read_excel("table_SLRM.xlsx")

# Checking for normal distribution of the predictors

hist(dat$st64)
shapiro.test(dat$st64)
shapiro.test(dat$age)
shapiro.test(dat$lang)
shapiro.test(dat$int)
shapiro.test(dat$d_prime1)
shapiro.test(dat$d_prime2)

################################################
############# Modeling ########################
################################################


# Main Effects

mudel_0 <- lm(st64~1, data=dat)
summary(mudel_0)

mudel_1 <- lm(st64~age, data=dat)
summary(mudel_1)

mudel_2 <- lm(st64~sex, data=dat)
summary(mudel_2)

mudel_3 <- lm(st64~group, data=dat)
summary(mudel_3)

mudel_4 <- lm(st64~lang, data=dat)
summary(mudel_4)

mudel_5 <- lm(st64~int, data=dat)
summary(mudel_5)

mudel_6 <- lm(st64~d_prime1, data=dat)
summary(mudel_6)

mudel_7 <- lm(st64~d_prime2, data=dat)
summary(mudel_7)

mudel_9 <- lm(st64~edu, data=dat)
summary(mudel_9)


############################# int with age #########################################


mudel_0 <- lm(st64~1, data=dat)
summary(mudel_0)

mudel_2 <- lm(st64~age*sex, data=dat)
summary(mudel_2)

mudel_3 <- lm(st64~age*group, data=dat)
summary(mudel_3)

mudel_4 <- lm(st64~age*lang, data=dat)
summary(mudel_4)

mudel_5 <- lm(st64~age*int, data=dat)
summary(mudel_5)

mudel_6 <- lm(st64~age*d_prime1, data=dat)
summary(mudel_6)

mudel_7 <- lm(st64~age*d_prime2, data=dat)
summary(mudel_7)


############################# int with sex #########################################


mudel_0 <- lm(st64~1, data=dat)
summary(mudel_0)

mudel_3 <- lm(st64~sex*group, data=dat)
summary(mudel_3)


mudel_4 <- lm(st64~sex*lang, data=dat)
summary(mudel_4)


mudel_5 <- lm(st64~sex*int, data=dat)
summary(mudel_5)

mudel_6 <- lm(st64~sex*d_prime1, data=dat)
summary(mudel_6)


mudel_7 <- lm(st64~sex*d_prime2, data=dat)
summary(mudel_7)



############################# int with group #########################################


mudel_0 <- lm(st64~1, data=dat)
summary(mudel_0)

mudel_4 <- lm(st64~group*lang, data=dat)
summary(mudel_4)
anova(mudel_0, mudel_4)
AIC(mudel_0, mudel_4) # Smaller is better

mudel_5 <- lm(st64~group*int, data=dat)
summary(mudel_5)

mudel_6 <- lm(st64~group*d_prime1, data=dat)
summary(mudel_6)

mudel_7 <- lm(st64~group*d_prime2, data=dat)
summary(mudel_7)



############################# int with lang #########################################


mudel_0 <- lm(st64~1, data=dat)
summary(mudel_0)

mudel_5 <- lm(st64~lang*int, data=dat)
summary(mudel_5)

mudel_6 <- lm(st64~lang*d_prime1, data=dat)
summary(mudel_6)


mudel_7 <- lm(st64~lang*d_prime2, data=dat)
summary(mudel_7)

mudel_8 <- lm(st64~lang*edu, data=dat)
summary(mudel_8)

############################# int with int #########################################


mudel_0 <- lm(st64~1, data=dat)
summary(mudel_0)

mudel_6 <- lm(st64~int*d_prime1, data=dat)
summary(mudel_6)


mudel_7 <- lm(st64~int*d_prime2, data=dat)
summary(mudel_7)

############################# int with d_1 #########################################


mudel_0 <- lm(st64~1, data=dat)
summary(mudel_0)


mudel_7 <- lm(st64~d_prime1*d_prime2, data=dat)
summary(mudel_7)


### Nothing significant found