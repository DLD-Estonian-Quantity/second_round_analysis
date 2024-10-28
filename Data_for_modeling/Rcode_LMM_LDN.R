########## LMM with LDN #######

library(readxl)

dat <- read_excel("table_LMM_LDN.xlsx")

################################################
############# Modeling ########################
################################################

library(lme4)
library(lmerTest)

### 0 model and main effects

mudel_0 <- lmer(mmr~1 + (1|p_no), data=dat)
summary(mudel_0) 

mudel_1 <- lmer(mmr~age + (1|p_no), data=dat)
summary(mudel_1)

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
anova(mudel_0, mudel_7) # Different on trend level


#################################################### Interactions with age ##################################################

mudel_0 <- lmer(mmr~1 + (1|p_no), data=dat)
summary(mudel_0) 

mudel_2 <- lmer(mmr~age*sex + (1|p_no), data=dat)
summary(mudel_2) 

mudel_3 <- lmer(mmr~age*group + (1|p_no), data=dat)
summary(mudel_3)

mudel_4 <- lmer(mmr~age*lang + (1|p_no), data=dat)
summary(mudel_4)

mudel_5 <- lmer(mmr~age*int + (1|p_no), data=dat)
summary(mudel_5) 

mudel_6 <- lmer(mmr~age*d_1 + (1|p_no), data=dat)
summary(mudel_6) 

mudel_7 <- lmer(mmr~age*d_2 + (1|p_no), data=dat)
summary(mudel_7) 


############################### INteractions with sex #############################################

mudel_0 <- lmer(mmr~1 + (1|p_no), data=dat)
summary(mudel_0) 

mudel_3 <- lmer(mmr~ sex*group + (1|p_no), data=dat)
summary(mudel_3)

mudel_4 <- lmer(mmr~sex*lang + (1|p_no), data=dat)
summary(mudel_4) 

mudel_5 <- lmer(mmr~sex*int + (1|p_no), data=dat)
summary(mudel_5) 

mudel_6 <- lmer(mmr~sex*d_1 + (1|p_no), data=dat)
summary(mudel_6) 

mudel_7 <- lmer(mmr~sex*d_2 + (1|p_no), data=dat)
summary(mudel_7) 
anova(mudel_0, mudel_7) # Models are different
AIC(mudel_0, mudel_7)# Zero model is not better

######################################## INt with group ####################################

mudel_1 <- lmer(mmr~sex*d_2 + (1|p_no), data=dat)
summary(mudel_1)

mudel_2 <- lmer(mmr~ sex*d_2 + age*group + (1|p_no), data=dat)
summary(mudel_2)

mudel_3 <- lmer(mmr~ sex*d_2 + sex*group + (1|p_no), data=dat)
summary(mudel_3) 


mudel_4 <- lmer(mmr~ sex*d_2 + group*lang + (1|p_no), data=dat)
summary(mudel_4) 

mudel_5 <- lmer(mmr~sex*d_2 + group*int + (1|p_no), data=dat)
summary(mudel_5) 

mudel_6 <- lmer(mmr~sex*d_2 + group*d_1 + (1|p_no), data=dat)
summary(mudel_6) 

mudel_7 <- lmer(mmr~sex*d_2 + group*d_2 + (1|p_no), data=dat)
summary(mudel_7) 


######################################## INt with lang ####################################

mudel_1 <- lmer(mmr~sex*d_2 + (1|p_no), data=dat)
summary(mudel_1)

mudel_5 <- lmer(mmr~sex*d_2 +lang*int + (1|p_no), data=dat)
summary(mudel_5) 

mudel_6 <- lmer(mmr~sex*d_2 + lang*d_1 + (1|p_no), data=dat)
summary(mudel_6) 

mudel_7 <- lmer(mmr~sex*d_2 +lang*d_2 + (1|p_no), data=dat)
summary(mudel_7) 

######################################## INt with int ####################################

mudel_1 <- lmer(mmr~sex*d_2 + (1|p_no), data=dat)
summary(mudel_1)

mudel_6 <- lmer(mmr~sex*d_2 +int*d_1 + (1|p_no), data=dat)
summary(mudel_6) 

mudel_7 <- lmer(mmr~sex*d_2 + int*d_2 + (1|p_no), data=dat)
summary(mudel_7) 


######################################## INt with d_1 ####################################

mudel_1 <- lmer(mmr~sex*d_2 + (1|p_no), data=dat)
summary(mudel_1)

mudel_7 <- lmer(mmr~sex*d_2 + d_1*d_2 + (1|p_no), data=dat)
summary(mudel_7)



####################################################
##### Testing model assumptions AFTER MODELING ####
###################################################


mudel <- lmer(mmr~sex*d_2 + (1|p_no), data=dat)
summary(mudel)


### Normality of residual variance



shapiro.test(residuals(mudel))


#### Multicolinearity

library(car)
vif(mudel)


### The relationship between the predictors and the dependent variable is assumed to be linear. This can be checked by plotting residuals against fitted values or each predictor.


# Fit a linear mixed model


# Plot residuals against fitted values
plot(fitted(mudel), resid(mudel), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")  # Adds a horizontal line at zero

# Ideally, no clear patterns should be visible in these plots, indicating a linear relationship.


##### Homoscedacity


# Assuming `model` is your fitted linear model
residuals <- residuals(mudel)
fitted_values <- fitted(mudel)

# Plot residuals against fitted values
plot(fitted_values, residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted")
abline(h = 0, col = "red")  # Adds a horizontal line at zero for reference

# Adding a lowess line to see trends
lines(lowess(fitted_values, residuals), col = "blue")


##### Distribution of the random effect

# Extract random effects
rand_eff <- ranef(mudel)$p_no

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

mudel<- lmer(mmr~sex*d_2 + (1|p_no), data=dat)
summary(mudel)

# Check with GAMM

library(mgcv)

model <- gamm(mmr ~ sex*d_2 +  s(p_no, bs = "re"), data = dat)

# Summary of the model
summary(model$gam)

