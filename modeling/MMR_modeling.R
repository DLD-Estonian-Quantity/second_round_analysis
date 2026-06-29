########## LMM with all MMR valies #########

# Dependent variable:
# MMR means based on the positive cluster at measurement 1 and 2 and negative cluster at measurement 1 and 2. 
# All togehter 8 amplitude values from each participant as dependent variable.
# Means from two separate time-windows: positive cluster 240-290ms and 290-340 ms; negative cluster 390-440 and 440-490ms.
# Electrodes used for means: positive cluster AF3, F3, FC5, F7, Fz, FC1, C3, FC6, F8; negative cluster C4, Cp2,	Pz,	Cz,	F4,	Fc2,	F3,	Fc1

# neccessary packages
library(readxl)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)
library(showtext)
library(dplyr)
library(car)
library(vcd)
library(lmtest)
library(influence.ME)
library(MuMIn)

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

cov2cor(cov(dat[,c("age1", "mmr_all", "lang","b1_2","b2_2","b1_1","b2_1")]))

###################################################
################ Modeling ########################
##################################################

library(lme4)
library(lmerTest)

model_0 <- lmer(mmr_all~1 + (time|p_no), data=dat, REML = FALSE)
summary(model_0)





model_1 <- lmer(mmr_all~tw*time + (time|p_no), data=dat, REML = FALSE)
summary(model_1)

library(emmeans)

emm_time <- emmeans(model_1, ~ time | tw)
pairs(emm_time)





model_2 <- lmer(mmr_all~tw*time*group + (time|p_no), data=dat, REML = FALSE)
summary(model_2)

#### Contrasts of means
emm <- emmeans(model_2, ~ tw * time * group)
contrast(emm, "consec", simple = "each", combine = FALSE, adjust = "holm")

#### Contrasts of slopes by TW
emm_time2 <- emmeans(model_2, ~ time | tw * group)
group_diff_in_change <- contrast(emm_time2, interaction = "pairwise", by = "tw", adjust = "holm")
group_diff_in_change

#### Do the groups differ in overall longitudinal change, ignoring time window?

emm_time_group <- emmeans(model_2,  ~ time | group)
contrast(emm_time_group, method = "revpairwise", adjust = "holm")

#### Is there an overall longitudinal change, ignoring groups?

emm_time_tw <- emmeans(model_2,  ~ time | tw)
contrast(emm_time_tw, method = "revpairwise", adjust = "holm")

#### Confidence intervals
confint(model_2, parm = "beta_", method = "Wald")


anova(model_0, model_1) #sig diff
anova(model_0, model_2) #sig diff
anova(model_1, model_2) #sig diff

AIC(model_0, model_1, model_2) 


#####################################################
################## Model plots ######################
#####################################################


# Visualization of individual trajectories of MMR (mean) amplitude change form T1 to T2

library(ggplot2)
library(showtext)
font_add("Times New Roman", "C:/Windows/Fonts/times.ttf")
showtext_auto()


dat_plot <- dat %>%
  group_by(p_no, group, tw, time) %>%
  summarise(
    mmr = mean(mmr_all, na.rm = TRUE),
    .groups = "drop"
  )



dat_plot$time <- factor(dat_plot$time,
                        levels = c(1, 2),
                        labels = c("T1", "T2"))

dat$time <- factor(dat$time,
                   levels = c(1, 2),
                   labels = c("T1", "T2"))


table(dat_plot$time, useNA = "always")


dat_plot$group <- toupper(dat_plot$group)


ggplot(dat_plot, aes(x = time, y = mmr, group = interaction(p_no, tw))) +
  
  geom_line(aes(color = group),
            alpha = 0.35,
            linewidth = 0.5) +
  
  geom_point(aes(color = group),
             size = 2) +
  
  stat_summary(aes(group = 1),
               fun = mean,
               geom = "line",
               linewidth = 1.2,
               color = "black") +
  
  stat_summary(fun = mean,
               aes(color = group),
               geom = "point",
               size = 2.5) +
  
  facet_grid(group ~ tw, labeller = labeller(
    group = c(DLD = "DLD", TD = "TD"),
    tw = c("1" = "TW1", "2" = "TW2")
  )) +

  theme_classic() +
  
  scale_color_manual(
    values = c("DLD" = "#6699CC", "TD" = "#FF9933")
    ) +
  
  theme(
    text = element_text(family = "Times New Roman", size = 14),
    axis.title = element_text(size = 16),          # MMR amplitude, Time
    axis.text = element_text(size = 14),           # T1, T2
    strip.text = element_text(size = 14),          # DLD, TD, TW1, TW2
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none"   
  ) +
  
  labs(
    x = "Time",
    y = "MMR amplitudes (µV)"
  )



#####################################################
################ Diagnostics ########################
#####################################################


model <- lmer(mmr_all~tw*time*group + (time|p_no), data=dat, REML = FALSE)
summary(model)



# Model residual variance
resid1 <- resid(model)
shapiro.test(resid1) # 0.368




# Colinearity
library(car)
vif(model) 
library(vcd)
assocstats(table(dat$time, dat$tw))$cramer # 0 
assocstats(table(dat$time, dat$group))$cramer# 0 
assocstats(table(dat$tw, dat$group))$cramer# 0 





# Plot residuals against fitted values to check any non-linear relationship and heteroscedasticity
# Ideally, no clear patterns should be visible in these plots, indicating a linear relationship.

# Extract residuals and fitted values
dat$resid <- residuals(model)
dat$fitted <- fitted(model)

library(ggplot2)

# Plot
ggplot(dat, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +  # nonlinear trend
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal()

# This slight wave in the blue LOESS line pattern indicates some mild non-linearity.
# However, the curve doesn’t deviate dramatically from the red dashed zero line.
# There’s no strong systematic curvature, so this does not clearly violate linearity assumptions.
# No funnel shape pattern in the data witch would indicate heteroscedasticity


library(lmtest)
# NB Homoscedasticity - this test is not ideal for mixed models
# Extract fixed-effect fitted values and residuals from your mixed model
fixed_fit <- fitted(model, type = "response")
resid_vals <- resid(model)

# Create a simplified linear model using fixed effects only
lm_model <- lm(resid_vals^2 ~ fixed_fit)

# Run Breusch-Pagan test
bptest(lm_model) # p-value = 0.06





# Residuals against fitted values - predictors separately

library(lme4)
library(ggplot2)
library(dplyr)

# Get residuals and fitted values from your model
dat$resid <- resid(model)
dat$fitted <- fitted(model)

# Define your predictors
predictors <- c("tw", "time", "group")

# Plot residuals vs each predictor
for (pred in predictors) {
  p <- ggplot(dat, aes_string(x = pred, y = "resid")) +
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
# The residuals are fairly symmetric for both levels. A few extreme residuals exist (above 2 or below -2), but they don’t look too influential or systematically problematic.

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
# A few data points stick out a bit vertically (e.g., > 2 or < -2), but they don’t seem extreme or group-specific.




# Distribution of the random intercept and random slope
library(lme4)

ranef_df <- ranef(model)$p_no  # Random effects for each participant
# Rename columns for clarity
colnames(ranef_df) <- c("intercept", "slope")
shapiro.test(ranef_df$intercept)  # Random intercepts; p-value = 0.3238
shapiro.test(ranef_df$slope)      # Random slopes for time; p-value = 0.9023


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
re <- ranef(model)$p_no  
colnames(re) <- c("intercept", "slope")
correlation <- cor(re$intercept, re$slope)
# Print the result
print(paste("Correlation between random intercept and slope:", round(correlation, 3))) # -0.867


# Extract random effects
re <- ranef(model)$p_no
colnames(re) <- c("intercept", "slope")
re$p_no <- rownames(re)

# Create group variable based on your original data
group_data <- dat %>%
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

print(cor_by_group) # dld  -0.838; td -0.893








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


infl_p <- influence(model, group = "p_no")

plot(infl_p, which = "cook")
cd_p <- cooks.distance(infl_p)

sort(cd_p, decreasing = TRUE)



model_minus_7 <- update(model,data = subset(dat, p_no != 7))
summary(model_minus_7)
summary(model)


library(emmeans)

#### Contrasts of means
emm_minus_7 <- emmeans(model_minus_7, ~ tw * time * group)
contrast(emm_minus_7, "consec", simple = "each", combine = FALSE, adjust = "holm")

#### Contrasts of slopes by TW
emm_time2_minus_7 <- emmeans(model_minus_7, ~ time | tw * group)
group_diff_in_change <- contrast(emm_time2_minus_7, interaction = "pairwise", by = "tw", adjust = "holm")
group_diff_in_change


#### Do the groups differ in overall longitudinal change, ignoring time window?

emm_time_group_minus_7 <- emmeans(model_minus_7,  ~ time | group)
contrast(emm_time_group_minus_7, method = "pairwise", adjust = "holm")






#########################################################
############### CI-s and R-s #############################
#########################################################


model <- lmer(mmr_all~tw*time*group + (time|p_no), data=dat, REML = FALSE)
summary(model)


######### CI-s

# Change with time

library(emmeans)
emm_time_tw <- emmeans(model, ~ time | tw)
con_time_tw <- contrast(emm_time_tw, method = "pairwise", adjust = "holm")
# 95% CIs + tests
summary(con_time_tw, infer = c(TRUE, TRUE))   # CIs and p-values
# or explicitly:
confint(con_time_tw)



# Group diff in means

emm_gt <- emmeans(model, ~ group | tw * time)
con_gt <- contrast(emm_gt, "pairwise", adjust = "holm")
# Get estimates + 95% CIs
summary(con_gt, infer = c(TRUE, TRUE))
# or just the CIs
confint(con_gt)


# Change in time within each group

emm_time_group <- emmeans(model, ~ time | group)
chg_avg <- contrast(emm_time_group, method = "pairwise", adjust = "holm")
confint(chg_avg)



# Comparison of maturational trajectories

emm_time2 <- emmeans(model, ~ time | tw * group)
group_diff_in_change <- contrast(emm_time2, interaction = "pairwise", by = "tw", adjust = "holm")
group_diff_in_change
confint(group_diff_in_change)



######### Calculating variance explained (pseudo R²) for model
library(MuMIn)

r2_values <- r.squaredGLMM(model)

# View the results
r2_values #R2m =0.2546427, R2C 0.5345431

