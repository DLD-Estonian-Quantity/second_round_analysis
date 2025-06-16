########## LMM with MMR pos and neg cluster FINAL #########

# Dependent variable:
# MMR means based the positive cluster at measurement 1 and negative cluster at measurement 2.
# Means from two separate time-windows 240-290ms and 290-340 ms; 390-440 and 440-490ms.
# Means taken from data at measurment 1 and 2 both
# Electrodes used for means: AF3, F3, FC5, F7, Fz, FC1, C3, FC6, F8 and C4, Cp2,	Pz,	Cz,	F4,	Fc2,	F3,	Fc1

# neccessary packages
library(readxl)
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(sjPlot)
library(tidyr)
library(car)
library(vcd)
library(emmeans)
library(lmtest)
library(influence.ME)
library(MuMIn)
library(effectsize)



#####################################################
################ Preparation ########################
#####################################################

library(readxl)
dat_reduced <- read_excel("modeling_reduced.xlsx")

dat_reduced$mmr <- as.numeric(dat_reduced$mmr)
dat_reduced$mmr_pos <- as.numeric(dat_reduced$mmr_pos)
dat_reduced$mmr_dev32 <- as.numeric(dat_reduced$mmr_dev32)
dat_reduced$mmr_pos_neg <- as.numeric(dat_reduced$mmr_pos_neg)

dat_reduced$tw <- as.factor(dat_reduced$tw)
dat_reduced$time <- as.factor(dat_reduced$time)

dat_reduced$age <- as.numeric(dat_reduced$age)
dat_reduced$age1 <- as.numeric(dat_reduced$age1)
dat_reduced$age2 <- as.numeric(dat_reduced$age2)

dat_reduced$group <- as.factor(dat_reduced$group)

dat_reduced$b1_2 <- as.numeric(dat_reduced$b1_2)
dat_reduced$b2_2 <- as.numeric(dat_reduced$b2_2)
dat_reduced$lang <- as.numeric(dat_reduced$lang)


cov2cor(cov(dat_reduced[,c("age1", "mmr_dev32", "mmr_pos_neg", "time_b", "lang","b1_2","b2_2")]))


###################################################
################ Modeling ########################
##################################################

library(lme4)
library(lmerTest)

### 0 model and main effects

model_0 <- lmer(mmr_pos_neg~1 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_0)

model_1 <- lmer(mmr_pos_neg~tw + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1) #not sig

model_2 <- lmer(mmr_pos_neg~time + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_2) # sig

model_3 <- lmer(mmr_pos_neg~group + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_3) # not sig

model_4 <- lmer(mmr_pos_neg~age1 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_4) # sig; NB soft convergence warning

model_5 <- lmer(mmr_pos_neg~b1_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_5) # not sig

model_6 <- lmer(mmr_pos_neg~b2_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_6) # not sig

model_7 <- lmer(mmr_pos_neg~lang + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_7) # not sig





# Interactions with tw

model_01 <- lmer(mmr_pos_neg~tw*time + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_01) # sig

model_02 <- lmer(mmr_pos_neg~tw*group + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_02) # not sig

model_03 <- lmer(mmr_pos_neg~tw*age1 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_03) # sig

model_04 <- lmer(mmr_pos_neg~tw*b1_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_04) # not sig

model_05 <- lmer(mmr_pos_neg~tw*b2_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_05) # not sig

model_06 <- lmer(mmr_pos_neg~tw*lang + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_06) # not sig






# Interactions with time

model_001 <- lmer(mmr_pos_neg~time*group + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_001) # sig

library(ggplot2)
library(emmeans)

# 1. Get predicted marginal means
emm <- emmeans(model_001, ~ time * group)

# 2. Convert to data frame for plotting
emm_df <- as.data.frame(emm)

# 3. Plot
ggplot(emm_df, aes(x = time, y = emmean, color = group, group = group)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.1) +
  labs(
    title = "Interaction: Time × Group on MMR",
    x = "Time Point",
    y = "Estimated MMR",
    color = "Group"
  ) +
  theme_minimal()

model_002 <- lmer(mmr_pos_neg~time*age1 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_002) # not sig

model_003 <- lmer(mmr_pos_neg~time*b1_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_003) # not sig

model_004 <- lmer(mmr_pos_neg~time*b2_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_004) # not sig

model_005 <- lmer(mmr_pos_neg~time*lang + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_005) # not sig





# Interactions with group

model_0001 <- lmer(mmr_pos_neg~group*age1 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_0001) # not sig

library(ggplot2)

# Add fitted values and raw data to the dataset used in your model
dat_reduced$fitted <- fitted(model_0001)
dat_reduced$residuals <- resid(model_0001)

# Plot: Interaction between age and group on predicted MMR
ggplot(dat_reduced, aes(x = age1, y = fitted, color = group)) +
  geom_point(aes(y = mmr), alpha = 0.5) +  # scatter of actual data
  geom_line(stat = "smooth", method = "lm", se = TRUE, linewidth = 1) +  # trend lines
  labs(
    title = "Interaction: Age × Group on MMR",
    x = "Age (months)",
    y = "Predicted MMR",
    color = "Group"
  ) +
  theme_minimal()

library(car)
vif(model_0001) #Multicolinearity?

model_0002 <- lmer(mmr_pos_neg~group*b1_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_0002) # not sig

model_0003 <- lmer(mmr_pos_neg~group*b2_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_0003) # 

model_0004 <- lmer(mmr_pos_neg~group*lang + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_0004) # not sig






# Interactions with age1

model_00002 <- lmer(mmr_pos_neg~age1*b1_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_00002) # not sig

model_00003 <- lmer(mmr_pos_neg~age1*b2_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_00003) # not sig

model_00004 <- lmer(mmr_pos_neg~age1*lang + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_00004) # not sig






# Interactions with b1_2

model_0000001 <- lmer(mmr_pos_neg~b1_2*b2_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_0000001) # not sig

model_0000002 <- lmer(mmr_pos_neg~b1_2*lang + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_0000002) # not sig






# Interactions with b2_2

model_00000001 <- lmer(mmr_pos_neg~b2_2*lang + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_00000001) # NB multicolinearity

library(car)
vif(model_00000001)






# 0 model + sig effects + interactions with group

model_0 <- lmer(mmr_pos_neg~1 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_0)

model_1 <- lmer(mmr_pos_neg~tw + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1) 

model_2 <- lmer(mmr_pos_neg~time + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_2) 

model_1_1 <- lmer(mmr_pos_neg~tw + time + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_1) 

model_1_2 <- lmer(mmr_pos_neg~tw*time + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_2) # sig

anova(model_0, model_1)
anova(model_0, model_2) # sig
anova(model_0, model_1_1) # sig
anova(model_0, model_1_2) # sig
anova(model_1_1, model_1_2) # sig
AIC(model_0, model_1, model_2, model_1_1, model_1_2) # Smallest AIC model_1_2



model_1_3 <- lmer(mmr_pos_neg~tw*time + group + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_3)

model_1_4 <- lmer(mmr_pos_neg~tw*time + group*time + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_4)

model_1_5 <- lmer(mmr_pos_neg~group + time + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_5) 

model_1_6 <- lmer(mmr_pos_neg~group*time + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_6)

anova(model_1_2, model_1_3)
anova(model_1_2, model_1_4) # sig
anova(model_1_2, model_1_5) # sig

AIC(model_0, model_1_2, model_1_3, model_1_4, model_1_5, model_1_6) # Smallest AIC model_1_4








model_1_7 <- lmer(mmr_pos_neg~tw*time + group*time + age1 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_7)

vif(model_1_7) # max vif 6.2

model_1_8 <- lmer(mmr_pos_neg~tw*time + age1 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_8)

model_1_9 <- lmer(mmr_pos_neg~group*time + age1 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_9)

model_1_10 <- lmer(mmr_pos_neg~time + age1 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_10)

anova(model_1_4, model_1_7) #sig
anova(model_1_4, model_1_8) 
anova(model_1_4, model_1_9) # sig
anova(model_1_4, model_1_10) # sig

AIC(model_0, model_1_4, model_1_7, model_1_8, model_1_9, model_1_10) # Smallest AIC model_1_7

model_1_11 <- lmer(mmr_pos_neg~tw*time + group*time + age1*time + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_11)
anova(model_1_7, model_1_11)
AIC(model_0, model_1_7, model_1_11) # Smallest AIC model_1_7








model_1_12 <- lmer(mmr_pos_neg~tw*time + group*time + age1*group + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_12)

model_1_13 <- lmer(mmr_pos_neg~tw*time + age1*group + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_13)

model_1_14 <- lmer(mmr_pos_neg~group*time + age1*group + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_14)

anova(model_1_7, model_1_12)
anova(model_1_7, model_1_14) # sig


AIC(model_0, model_1_7, model_1_12, model_1_13, model_1_14) # Smallest AIC model_1_7








model_1_15 <- lmer(mmr_pos_neg~tw*time + group*time + age1 + b2_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_15)


model_1_16 <- lmer(mmr_pos_neg~tw*time + group*time + b2_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_16)

model_1_17 <- lmer(mmr_pos_neg~tw*time + age1 + b2_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_17)

model_1_18 <- lmer(mmr_pos_neg~ group*time + age1 + b2_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_18)

model_1_19 <- lmer(mmr_pos_neg~ age1 + b2_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_19)

model_1_20 <- lmer(mmr_pos_neg~ time + b2_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_20)

model_1_21 <- lmer(mmr_pos_neg~ time + age1+ b2_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_21)

anova(model_1_7, model_1_15)
anova(model_1_7, model_1_17) # trend
anova(model_1_7, model_1_18) # sig
anova(model_1_7, model_1_19) # sug
anova(model_1_7, model_1_20) # sig
anova(model_1_7, model_1_21) # sig

AIC(model_0, model_1_7, model_1_15, model_1_16, model_1_17, model_1_18, model_1_19, model_1_20, model_1_21) # Smallest AIC model_1_7







model_1_22 <- lmer(mmr_pos_neg~tw*time + group*time + age1 + b2_2*group + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_22)

model_1_23 <- lmer(mmr_pos_neg~tw*time + group*time + b2_2*group + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_23)

model_1_24 <- lmer(mmr_pos_neg~tw*time + age1 + b2_2*group + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_24)

model_1_25 <- lmer(mmr_pos_neg~ group*time + age1 + b2_2*group + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_25)


anova(model_1_7, model_1_22)

AIC(model_0, model_1_7, model_1_22, model_1_23, model_1_24, model_1_25) # Smallest AIC model_1_7









##################################################
########## Model plots ##########################
#################################################

model <- lmer(mmr_pos_neg~tw*time + group*time + age1 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model)


#library(sjPlot)
#plot_model(model, type="pred", show.data= T, terms=c(group", "time"))

library(ggplot2)
library(dplyr)

# time:tw
# Aggregate means for tw and time
plot_data_1 <- dat_reduced %>%
  group_by(tw, time) %>%
  summarise(mmr_mean = mean(mmr_pos_neg, na.rm = TRUE),
            se = sd(mmr_pos_neg, na.rm = TRUE) / sqrt(n()), .groups = "drop")

ggplot(plot_data_1, aes(x = time, y = mmr_mean, color = tw, group = tw)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = mmr_mean - se, ymax = mmr_mean + se), width = 0.1) +
  labs(
    title = "Interaction: Time × TW",
    x = "Time",
    y = "Mean MMR Positivity",
    color = "Time Window"
  ) +
  theme_minimal()







# time:group
plot_data_2 <- dat_reduced %>%
  group_by(group, time) %>%
  summarise(mmr_mean = mean(mmr_pos_neg, na.rm = TRUE),
            se = sd(mmr_pos_neg, na.rm = TRUE) / sqrt(n()), .groups = "drop")

ggplot(plot_data_2, aes(x = time, y = mmr_mean, color = group, group = group)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = mmr_mean - se, ymax = mmr_mean + se), width = 0.1) +
  labs(
    title = "Interaction: Time × Group",
    x = "Time",
    y = "Mean MMR",
    color = "Group"
  ) +
  theme_minimal()




# Age1
age_seq <- seq(min(dat_reduced$age1, na.rm = TRUE),
               max(dat_reduced$age1, na.rm = TRUE), length.out = 100)
# Create a new data frame for predictions
# Hold other variables constant (e.g., use reference levels and most common categories)
newdata <- data.frame(
  age1 = age_seq,
  tw = factor("1", levels = levels(dat_reduced$tw)),       # or choose most common level
  time = factor("1", levels = levels(dat_reduced$time)),
  group = factor("dld", levels = levels(dat_reduced$group)) # pick one for main effect
)

# 3. Predict using fixed effects only (re.form = NA)
newdata$pred <- predict(model, newdata = newdata, re.form = NA)

# 4. Plot
ggplot(newdata, aes(x = age1, y = pred)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  labs(title = "Predicted MMR by Age",
       x = "Age at T1 (months)",
       y = "Predicted MMR Pos-Neg") +
  theme_minimal()










# Visualization of individual trajectories of MMR (mean) amplitude change form T1 to T2

model <- lmer(mmr_pos_neg~tw*time + group*time + age1 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model)

mod.summary <- summary(model)
str(mod.summary)
mc <- mod.summary$coefficients[,"Estimate"]

# DLD, T1, MMR estimate mean with mean age at T1
mc["(Intercept)"] + 0.5*mc["tw2"] + 69*mc["age1"] # 0.75
# DLD, T2, MMR estimate mean with mean age at T1
mc["(Intercept)"] + 0.5*mc["tw2"] + mc["time2"]+ 69*mc["age1"] # -0.2
# TD, T1, MMR estimate mean with mean age at T1
mc["(Intercept)"] + 0.5*mc["tw2"] +mc["grouptd"] + 69*mc["age1"] # 1.34
# TD, T2, MMR estimate mean with mean age at T1
mc["(Intercept)"] + 0.5*mc["tw2"] +mc["grouptd"] + mc["time2"]+ mc["time2:grouptd"]+ 69*mc["age1"] # -0.85

df <- data.frame(
  mmr = c(0.75, -0.2, 1.34, -0.85),
  group = c("dld", "dld", "td", "td"),
  time = c(1, 2, 1, 2)
)

print(df)


dat <- read_excel("var.xlsx")

dat$mmr_easy_t1 <- as.numeric(dat$mmr_easy_t1)
mean(dat$mmr_easy_t1, na.rm=TRUE)
dat$mmr_easy_t2 <- as.numeric(dat$mmr_easy_t2)
mean(dat$mmr_easy_t2)
dat$age1 <- as.numeric(dat$age1)
mean(dat$age1) # 69
dat$group <- as.factor(dat$group)
dat$p_no <- as.factor(dat$p_no)

library(tidyr)
library(dplyr)
library(ggplot2)



# Step 1: Compute group means and standard errors

dat_long <- dat %>%
  pivot_longer(
    cols = c("mmr_easy_t1", "mmr_easy_t2"),
    names_to = "time",
    values_to = "mmr"
  ) %>%
  mutate(
    time = dplyr::recode(time,
                         "mmr_easy_t1" = "T1",
                         "mmr_easy_t2" = "T2")
  )

ggplot(dat_long, aes(x = time, y = mmr, group = p_no, color = group)) +
  geom_line(alpha = 0.3, linewidth = 0.5) +        # Individual lines
  geom_point(size = 3) +                           # Individual data points
  facet_wrap(~ group, scales = "free_y") +         # One panel per group
  theme_classic() +
  labs(
    title = "Raw MMR amplitude per participant",
    x = "Time",
    y = "MMR amplitude"
  ) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) +
  scale_color_manual(values = c("dld" = "#6699CC", "td" = "#FF9933")) +  # Set colors
  geom_line(data = df, aes(x = time, y = mmr, color = group, group = group),
            linewidth = 1.2)
  


### guides(fill="none"); legendi ära võtmiseks













# Visualization of random effect and slope




# Extract random effects for participants
ranef_df <- ranef(model)$p_no
ranef_df$p_no <- rownames(ranef_df)

# Check the column names to find the actual name of the slope term
colnames(ranef_df)


ggplot(ranef_df, aes(x = `(Intercept)`, y = time2)) +
  geom_point() +
  geom_text(aes(label = p_no), hjust = -0.2, size = 3) +
  labs(
    title = "Random Intercepts vs Slopes for Time",
    x = "Random Intercept (Deviation)",
    y = "Random Slope for Time"
  ) +
  theme_minimal()

# negative correlation between intercepts and slopes
# Participants who started out higher (positive intercepts) tend to show more negative change over time (negative slopes),
# and those who started lower tend to show more positive change.








##################################################
########## Model diagnostics ####################
#################################################


model <- lmer(mmr_pos_neg~tw*time + group*time + age1+ (time|p_no), data=dat_reduced, REML = FALSE)
summary(model) 




# Model residual variance
resid1 <- resid(model)
shapiro.test(resid1) # p=0.8989





# Colinearity
library(car)
vif(model) 
library(vcd)
assocstats(table(dat_reduced$time, dat_reduced$tw))$cramer # 0 
assocstats(table(dat_reduced$time, dat_reduced$group))$cramer# 0 
assocstats(table(dat_reduced$tw, dat_reduced$group))$cramer# 0 







# Plot residuals against fitted values to check any non-linear relationship and heteroscedasticity
# Ideally, no clear patterns should be visible in these plots, indicating a linear relationship.

# Extract residuals and fitted values
dat_reduced$resid <- residuals(model)
dat_reduced$fitted <- fitted(model)

# Load ggplot2 just in case
library(ggplot2)

# Plot
ggplot(dat_reduced, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +  # nonlinear trend
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal()

# This slight wave in the blue LOESS line pattern indicates some mild non-linearity — especially at the extremes of the fitted range.
# However, the curve doesn’t deviate dramatically from the red dashed zero line.
# There’s no strong systematic curvature, so this does not clearly violate linearity assumptions.


library(lmtest)
# NB Homoscedasticity - this test is not ideal for mixed models
# Extract fixed-effect fitted values and residuals from your mixed model
fixed_fit <- fitted(model, type = "response")
resid_vals <- resid(model)

# Create a simplified linear model using fixed effects only
lm_model <- lm(resid_vals^2 ~ fixed_fit)

# Run Breusch-Pagan test
bptest(lm_model) # p-value = 0.1461







# Residuals against fitted values - predictors separately

library(lme4)
library(ggplot2)
library(dplyr)

# Get residuals and fitted values from your model
dat_reduced$resid <- resid(model)
dat_reduced$fitted <- fitted(model)

# Define your predictors
predictors <- c("tw", "time", "group", "age1")

# Plot residuals vs each predictor
for (pred in predictors) {
  p <- ggplot(dat_reduced, aes_string(x = pred, y = "resid")) +
    geom_jitter(width = 0.1, alpha = 0.4) +
    geom_smooth(method = "loess", color = "blue", se = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = paste("Residuals vs", pred),
         x = pred,
         y = "Residuals") +
    theme_minimal()
  
  print(p)
}

# TW
# Residuals for both levels of tw are reasonably centered around the red dashed line (y = 0), 
# which is good. This suggests no obvious systematic bias in how the model fits each time window.
# The spread (variance) of residuals appears roughly similar between the two levels. There’s no strong indication of heteroscedasticity 
# (unequal variance between groups), which is another good sign.
# The residuals are fairly symmetric for both levels. A few extreme residuals exist (above 1 or below -1), but they don’t look too influential or systematically problematic.

# Time
# At both time points (1 and 2), the residuals are centered around 0 (the red dashed line), which is exactly what we want to see.
# The spread of residuals looks quite similar across the two time points, so there is no obvious sign of heteroscedasticity. 
# There's no noticeable systematic pattern in how residuals behave between the time points. That supports the idea that the
# model is appropriately capturing the structure of the time effect.
# The residuals appear fairly symmetric at both time points, with a few mild outliers but nothing extreme or worrying.

# Group
# Residuals for both groups (dld and td) appear centered around zero —that supports the assumption that the model fits both groups similarly well on average.
# The spread (variance) of residuals seems fairly comparable between the two groups.No clear indication of heteroscedasticity 
# — the dispersion isn’t wildly different between dld and td.
# Both groups show a fairly symmetric distribution of residuals. That’s another good sign — no obvious skew or bias in how errors are distributed.
# A few data points stick out a bit vertically (e.g., > 1 or < -1), but they don’t seem extreme or group-specific.

# Age1
# Residuals are fairly evenly scattered across the age range — no strong funnel shape (i.e., no major heteroscedasticity).
# The LOESS line is very close to zero and flat across the middle range of age1.
# No strong non-linear pattern or curvature, just a very gentle wave — this is not uncommon and doesn’t necessarily warrant modeling a non-linear effect (e.g., a quadratic).
# There's slightly more variation at the younger ages (left side), but there are also fewer data points, so it’s likely just sampling noise.





# Distribution of the random intercept and random slope
library(lme4)

# Assuming your model is named `model`
ranef_df <- ranef(model)$p_no  # Random effects for each participant
# Rename columns for clarity
colnames(ranef_df) <- c("intercept", "slope")
shapiro.test(ranef_df$intercept)  # Random intercepts; p-value = 0.8774
shapiro.test(ranef_df$slope)      # Random slopes for time; p-value = 0.3105


par(mfrow = c(2, 2))  # 2 plots per row

# QQ plot for intercept
qqnorm(ranef_df$intercept, main = "QQ Plot: Random Intercepts")
qqline(ranef_df$intercept, col = "red")

# Histogram for intercept
hist(ranef_df$intercept, col = "skyblue", main = "Histogram: Random Intercepts",
     xlab = "Random Intercept")

# QQ plot for slope
qqnorm(ranef_df$slope, main = "QQ Plot: Random Slopes (time)")
qqline(ranef_df$slope, col = "red")

# Histogram for slope
hist(ranef_df$slope, col = "lightgreen", main = "Histogram: Random Slopes (time)",
     xlab = "Random Slope")








# Correlation between random intercept and slope
# Extract random effects
re <- ranef(model)$p_no  # assumes random effects are by participant
colnames(re) <- c("intercept", "slope")
correlation <- cor(re$intercept, re$slope)
# Print the result
print(paste("Correlation between random intercept and slope:", round(correlation, 3))) # -0.928


# Extract random effects
re <- ranef(model)$p_no
colnames(re) <- c("intercept", "slope")
re$p_no <- rownames(re)

# Create group variable based on your original data
group_data <- dat_reduced %>%
  select(p_no, group) %>%
  distinct()

# Convert p_no in both datasets to character before joining
re$p_no <- as.character(re$p_no)
group_data$p_no <- as.character(group_data$p_no)

# Now can join 
re_grouped <- left_join(re, group_data, by = "p_no")


# Plot by group
ggplot(re_grouped, aes(x = intercept, y = slope, color = group)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Random Intercepts vs Slopes by Group",
    x = "Random Intercept",
    y = "Random Slope"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# High Intercept → More Negative Slope. Participants who started with higher baseline MMR tend to decrease more over time.
# Low Intercept → More Positive Slope. Those who started lower tend to increase or decrease less steeply.
# Both DLD and TD children show this pattern; no major group divergence

# Correlation by group
library(dplyr)

cor_by_group <- re_grouped %>%
  group_by(group) %>%
  summarise(
    correlation = cor(intercept, slope)
  )

print(cor_by_group) # dld  -0.925; td -0.931









# Influence plot
library(lme4)
library(influence.ME)
# Compute influence for each observation
infl <- influence(model, obs = TRUE)
plot(infl, which = "cook", main = "Cook’s Distance for Each Observation")
# Cook’s distance values
cd <- cooks.distance(infl)
# Sort and view top influential rows
top_influential <- sort(cd, decreasing = TRUE)
head(top_influential, 10)  # Show top 10

# Since no influential observations were found with high Cook’s Distance, it’s safe to say that: the model appears stable under influence diagnostics;
# No data needs to be removed, and you can continue working with the full dataset (dat_reduced) without filtering.






#########################################################
############### R-s and effect sizes ####################
#########################################################


model <- lmer(mmr_pos_neg~tw*time + group*time + age1 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model)


######### CI-s

library(sjPlot)

# Plot fixed effects with 95% CI
plot_model(model, type = "est", show.values = TRUE, value.offset = 0.3,
           title = "Fixed Effects Estimates with 95% CI")


######### Effect sizes

library(effectsize)
standardize_parameters(model)



######### Calculating variance explained (pseudo R²) for model
library(MuMIn)

r2_values <- r.squaredGLMM(model)

# View the results
r2_values



