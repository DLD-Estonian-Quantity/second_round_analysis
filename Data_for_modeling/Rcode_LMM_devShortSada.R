########## LMM with devShortSada #######

library(readxl)

dat <- read_excel("table_LMM_devShortSada")

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

### Multicolinearity

cov2cor(cov(dat[,c("age", "lang", "int", "d_1","d_2")]))

###################################################
################ Modeling ########################
##################################################

library(lme4)
library(lmerTest)



### 0 model and main effects

mudel_0 <- lmer(dev64~1 + (1|p_no), data=dat)
summary(mudel_0) 

mudel_1 <- lmer(dev64~age + (1|p_no), data=dat)
summary(mudel_1) 

mudel_2 <- lmer(dev64~sex + (1|p_no), data=dat)
summary(mudel_2) 

mudel_3 <- lmer(dev64~group + (1|p_no), data=dat)
summary(mudel_3)

mudel_4 <- lmer(dev64~lang + (1|p_no), data=dat)
summary(mudel_3) 

mudel_5 <- lmer(dev64~int + (1|p_no), data=dat)
summary(mudel_5) # SIG MAIN EFFECT
anova(mudel_0, mudel_5) #The zero model is not better

mudel_6 <- lmer(dev64~d_1 + (1|p_no), data=dat)
summary(mudel_6) 

mudel_7 <- lmer(dev64~d_2 + (1|p_no), data=dat)
summary(mudel_7) 


################### ADDING MAIN EFFECTS TO INT ##################################################################

mudel_0 <- lmer(dev64~int + (1|p_no), data=dat)
summary(mudel_0) 

mudel_1 <- lmer(dev64~int+age + (1|p_no), data=dat)
summary(mudel_1) 

mudel_2 <- lmer(dev64~int+sex + (1|p_no), data=dat)
summary(mudel_2) 

mudel_3 <- lmer(dev64~int+group + (1|p_no), data=dat)
summary(mudel_3)

mudel_4 <- lmer(dev64~int+lang + (1|p_no), data=dat)
summary(mudel_3) 

#mudel_5 <- lmer(dev64~int + (1|p_no), data=dat)
#summary(mudel_5)

mudel_6 <- lmer(dev64~int+d_1 + (1|p_no), data=dat)
summary(mudel_6) 

mudel_7 <- lmer(dev64~int+d_2 + (1|p_no), data=dat)
summary(mudel_7) 


################### interactions with age ##################################################################

mudel_0 <- lmer(dev64~int + (1|p_no), data=dat)
summary(mudel_0) 

mudel_1 <- lmer(dev64~int+age*sex + (1|p_no), data=dat)
summary(mudel_1) 


mudel_3 <- lmer(dev64~int+age*group + (1|p_no), data=dat)
summary(mudel_3)

mudel_4 <- lmer(dev64~int+age*lang + (1|p_no), data=dat)
summary(mudel_3) 

mudel_5 <- lmer(dev64~int + age*int + (1|p_no), data=dat)
summary(mudel_5) 

mudel_6 <- lmer(dev64~int+age*d_1 + (1|p_no), data=dat)
summary(mudel_6) 

mudel_7 <- lmer(dev64~int+age*d_2 + (1|p_no), data=dat)
summary(mudel_7) 


################### interactions with sex ##################################################################

mudel_0 <- lmer(dev64~int + (1|p_no), data=dat)
summary(mudel_0) 

mudel_3 <- lmer(dev64~int+sex*group + (1|p_no), data=dat)
summary(mudel_3)

mudel_4 <- lmer(dev64~int+sex*lang + (1|p_no), data=dat)
summary(mudel_4) 

mudel_5 <- lmer(dev64~int + sex*int + (1|p_no), data=dat)
summary(mudel_5) 

mudel_6 <- lmer(dev64~int+sex*d_1 + (1|p_no), data=dat)
summary(mudel_6) 

mudel_7 <- lmer(dev64~int+sex*d_2 + (1|p_no), data=dat)
summary(mudel_7) 


################### interactions with group ##################################################################

mudel_0 <- lmer(dev64~int + (1|p_no), data=dat)
summary(mudel_0) 

mudel_4 <- lmer(dev64~int+group*lang + (1|p_no), data=dat)
summary(mudel_4) 

mudel_5 <- lmer(dev64~int + group*int + (1|p_no), data=dat)
summary(mudel_5) 

mudel_6 <- lmer(dev64~int+group*d_1 + (1|p_no), data=dat)
summary(mudel_6) 

mudel_7 <- lmer(dev64~int+group*d_2 + (1|p_no), data=dat)
summary(mudel_7) 


################### interactions with lang ##################################################################

mudel_0 <- lmer(dev64~int + (1|p_no), data=dat)
summary(mudel_0) 

mudel_5 <- lmer(dev64~int + lang*int + (1|p_no), data=dat)
summary(mudel_5) 

mudel_6 <- lmer(dev64~int+lang*d_1 + (1|p_no), data=dat)
summary(mudel_6) 

mudel_7 <- lmer(dev64~int+lang*d_2 + (1|p_no), data=dat)
summary(mudel_7) 


################### interactions with int ##################################################################

mudel_0 <- lmer(dev64~int + (1|p_no), data=dat)
summary(mudel_0) 


mudel_6 <- lmer(dev64~int+int*d_1 + (1|p_no), data=dat)
summary(mudel_6) 

mudel_7 <- lmer(dev64~int+int*d_2 + (1|p_no), data=dat)
summary(mudel_7) 


################### interactions with d_1 ##################################################################

mudel_0 <- lmer(dev64~int + (1|p_no), data=dat)
summary(mudel_0) 


mudel_7 <- lmer(dev64~int+d_1*d_2 + (1|p_no), data=dat)
summary(mudel_7) 


####################################################
##### Testing model assumptions AFTER MODELING ####
###################################################

### Normality of residual variance

shapiro.test(residuals(mudel_0))


### The relationship between the predictors and the dependent variable is assumed to be linear. This can be checked by plotting residuals against fitted values or each predictor.


# Fit a linear mixed model


# Plot residuals against fitted values
plot(fitted(mudel_0), resid(mudel_0), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")  # Adds a horizontal line at zero

# Ideally, no clear patterns should be visible in these plots, indicating a linear relationship.


##### Homoscedacity

# Assuming `model` is your fitted linear model
residuals <- residuals(mudel_0)
fitted_values <- fitted(mudel_0)

# Plot residuals against fitted values
plot(fitted_values, residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted")
abline(h = 0, col = "red")  # Adds a horizontal line at zero for reference

# Adding a lowess line to see trends
lines(lowess(fitted_values, residuals), col = "blue")


### Multicolinearity second time

# VIF should be under 10, mode stringent criteria is under 5

library(car)
vif(mudel_0)

##### Distribution of the random effect

mudel_0 <- lmer(dev64~int + (1|p_no), data=dat)

# Extract random effects
rand_eff <- ranef(mudel_0)$p_no

rand_eff_numeric <- as.numeric(unlist(rand_eff))


# Plot histogram
hist(rand_eff_numeric, breaks = 30, main = "Histogram of Random Effects")

# Plot Q-Q plot
qqnorm(rand_eff_numeric)
qqline(rand_eff_numeric, col = "red")

shapiro.test(rand_eff_numeric) 

##############################################################
################## Final model ##############################
##############################################################


mudel_0 <- lmer(dev64~int + (1|p_no), data=dat)
summary(mudel_0) 

# Check with GAMM
library(mgcv)
model <- gamm(dev64 ~ int +  s(p_no, bs = "re"), data = dat)

# Summary of the model
summary(model$gam)


