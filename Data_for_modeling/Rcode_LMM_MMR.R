########## LMM with MMR #######

#data=tabel_or_st

library(readxl)

dat <- read_excel("table_LMM_MMR.xlsx")

################################################
############# Modeling ########################
################################################

library(lme4)
library(lmerTest)

### 0 model and main effects

mudel_0 <- lmer(mmr~1 + (1|p_no), data=dat)
summary(mudel_0) 

mudel_1 <- lmer(mmr~age + (1|p_no), data=dat)
summary(mudel_1) # trend

mudel_2 <- lmer(mmr~sex + (1|p_no), data=dat)
summary(mudel_2) 

mudel_3 <- lmer(mmr~group + (1|p_no), data=dat)
summary(mudel_3)

mudel_4 <- lmer(mmr~lang + (1|p_no), data=dat)
summary(mudel_4) 

mudel_5 <- lmer(mmr~int + (1|p_no), data=dat)
summary(mudel_5) 

mudel_6 <- lmer(mmr~d_1 + (1|p_no), data=dat)
summary(mudel_6) 

mudel_7 <- lmer(mmr~d_2 + (1|p_no), data=dat)
summary(mudel_7) 


#################################################### Interactions with age ##################################################

mudel_2 <- lmer(mmr~age*sex + (1|p_no), data=dat)
summary(mudel_2) 

mudel_3 <- lmer(mmr~age*group + (1|p_no), data=dat)
summary(mudel_3)
anova(mudel_0, mudel_3) 
AIC(mudel_0, mudel_3) # Zero model is not better

#mudel_4 <- lmer(mmr~age*lang + (1|p_no), data=dat)
#summary(mudel_4)
#anova(mudel_0, mudel_4) 

mudel_5 <- lmer(mmr~age*group + age*int + (1|p_no), data=dat)
summary(mudel_5) 

mudel_6 <- lmer(mmr~age*group + age*d_1 + (1|p_no), data=dat)
summary(mudel_6) 

mudel_7 <- lmer(mmr~age*group +age*d_2 + (1|p_no), data=dat)
summary(mudel_7) 


########################################## Adding main effects to the interaction #################

mudel_0 <- lmer(mmr~1 + (1|p_no), data=dat)
summary(mudel_0)

mudel_1 <- lmer(mmr~age*group + (1|p_no), data=dat)
summary(mudel_1)


# mudel_2 <- lmer(mmr~age*group + lang + (1|p_no), data=dat)
# summary(mudel_2)
# anova(mudel_0, mudel_2) # suurem on erinev
# anova(mudel_1, mudel_2) # suurem on erinev
# library(car)
# vif(mudel_2)


mudel_3 <- lmer(mmr~age*group + sex + (1|p_no), data=dat)
summary(mudel_3)

mudel_4 <- lmer(mmr~age*group + int + (1|p_no), data=dat)
summary(mudel_4)

mudel_5 <- lmer(mmr~age*group + d_1 + (1|p_no), data=dat)
summary(mudel_5)

mudel_6 <- lmer(mmr~age*group + d_2 + (1|p_no), data=dat)
summary(mudel_6)

############################### INteractions with sex #############################################

mudel_3 <- lmer(mmr~ age*group +sex*group + (1|p_no), data=dat)
summary(mudel_3)

mudel_4 <- lmer(mmr~age*group + sex*lang + (1|p_no), data=dat)
summary(mudel_4) 

mudel_5 <- lmer(mmr~age*group + sex*int + (1|p_no), data=dat)
summary(mudel_5) 

mudel_6 <- lmer(mmr~age*group + sex*d_1 + (1|p_no), data=dat)
summary(mudel_6) 

mudel_7 <- lmer(mmr~age*group + sex*d_2 + (1|p_no), data=dat)
summary(mudel_7) 


######################################## INt with group ####################################

mudel_2 <- lmer(mmr~age*group + (1|p_no), data=dat)
summary(mudel_2)

mudel_4 <- lmer(mmr~age*group + group*lang + (1|p_no), data=dat)
summary(mudel_4) 

mudel_5 <- lmer(mmr~age*group + group*int + (1|p_no), data=dat)
summary(mudel_5) 

mudel_6 <- lmer(mmr~age*group + group*d_1 + (1|p_no), data=dat)
summary(mudel_6) 

mudel_7 <- lmer(mmr~age*group + group*d_2 + (1|p_no), data=dat)
summary(mudel_7) 


######################################## Int with int ####################################

mudel_2 <- lmer(mmr~age*group + (1|p_no), data=dat)
summary(mudel_2)

mudel_6 <- lmer(mmr~age*group +int*d_1 + (1|p_no), data=dat)
summary(mudel_6) 

mudel_7 <- lmer(mmr~age*group + int*d_2 + (1|p_no), data=dat)
summary(mudel_7) 

######################################## INt with d_1 ####################################

mudel_2 <- lmer(mmr~age*group + (1|p_no), data=dat)
summary(mudel_2)

mudel_7 <- lmer(mmr~age*group + d_1*d_2 + (1|p_no), data=dat)
summary(mudel_7) #See seos on oluline
anova(mudel_2, mudel_7) # models are not different
AIC(mudel_2, mudel_7) # smaller model is better


####################################################
##### Testing model assumptions AFTER MODELING ####
###################################################

### Normal distribution of variables

hist(dat$age)
shapiro.test(dat$age)

hist(dat$lang)
shapiro.test(dat$lang)


### Normality of residual variance

mudel_2 <- lmer(mmr~age*group + (1|p_no), data=dat)
summary(mudel_2)

shapiro.test(residuals(mudel_2))



### The relationship between the predictors and the dependent variable is assumed to be linear. This can be checked by plotting residuals against fitted values or each predictor.


# Fit a linear mixed model


# Plot residuals against fitted values
plot(fitted(mudel_2), resid(mudel_2), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")  # Adds a horizontal line at zero

# Ideally, no clear patterns should be visible in these plots, indicating a linear relationship.


##### Homoscedacity

# Assuming `model` is your fitted linear model
residuals <- residuals(mudel_2)
fitted_values <- fitted(mudel_2)

# Plot residuals against fitted values
plot(fitted_values, residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted")
abline(h = 0, col = "red")  # Adds a horizontal line at zero for reference

# Adding a lowess line to see trends
lines(lowess(fitted_values, residuals), col = "blue")


##### Distribution of the random effect

# Extract random effects
rand_eff <- ranef(mudel_2)$p_no

rand_eff_numeric <- as.numeric(unlist(rand_eff))


# Plot histogram
hist(rand_eff_numeric, breaks = 30, main = "Histogram of Random Effects")

# Plot Q-Q plot
qqnorm(rand_eff_numeric)
qqline(rand_eff_numeric, col = "red")

shapiro.test(rand_eff_numeric) #on normaaljaotuses


###################################################
########### Final model ##########################
###################################################


mudel_2<- lmer(mmr~age*group + (1|p_no), data=dat)
summary(mudel_2) 

# Check with GAMM

library(mgcv)

model1 <- gamm(mmr ~ age*group+  s(p_no, bs = "re"), data = dat)
summary(model1$gam)


#### Visualizing model effects

library(ggplot2)

mudel <- lmer(mmr ~ age * group + (1 | p_no), data = dat)

# Plot the effects of age and group on mmr
ggplot(dat, aes(x = age, y = mmr, color = group)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = group), se = FALSE) +
  labs(title = "Effect of Age on MMR by Group",
       x = "Age",
       y = "MMR",
       color = "Group") +
  theme_minimal()


