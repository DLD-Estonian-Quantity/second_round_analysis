########## LMM with MMR dev32 FINAL #########

# Dependent variable:
# MMR means based "possible" negative cluster from measurment 2.
# Means from two separate time-windows 340-390ms and 390-440 ms
# Means taken from data at measurment 1 and 2 both
# Electrodes used for means: F3,	FC1,	C3,	CP1,	Cz,	Fz,	F4,	Fc2

# neccessary packages
library(readxl)
library(lme4)
library(lmerTest)
library(ggplot2)
library(ggeffects)
library(vcd)
library(car)
library(influence.ME)
library(sjPlot)
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


(cov(dat_reduced[,c("age1", "mmr_dev32", "mmr_pos_neg", "time_b", "lang","b1_2","b2_2")]))


###################################################
################ Modeling ########################
##################################################

library(lme4)
library(lmerTest)
### 0 model and main effects

model_0 <- lmer(mmr_dev32~1 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_0)

model_1 <- lmer(mmr_dev32~tw + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1) #trend

model_2 <- lmer(mmr_dev32~time + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_2) # trend

model_3 <- lmer(mmr_dev32~group + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_3) # not sig

model_4 <- lmer(mmr_dev32~age1 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_4) # trend

model_5 <- lmer(mmr_dev32~b1_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_5) # not sig

model_6 <- lmer(mmr_dev32~b2_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_6) # not sig

model_7 <- lmer(mmr_dev32~lang + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_7) # not sig






# Interactions with tw

model_01 <- lmer(mmr_dev32~tw*time + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_01) # sig

model_02 <- lmer(mmr_dev32~tw*group + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_02) # sig

model_03 <- lmer(mmr_dev32~tw*age1 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_03) # sig

model_04 <- lmer(mmr_dev32~tw*b1_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_04) # trend

model_05 <- lmer(mmr_dev32~tw*b2_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_05) # not sig

model_06 <- lmer(mmr_dev32~tw*lang + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_06) # not sig






# Interactions with time

model_001 <- lmer(mmr_dev32~time*group + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_001) # not sig

model_002 <- lmer(mmr_dev32~time*age1 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_002) # not sig

model_003 <- lmer(mmr_dev32~time*b1_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_003) # not sig

model_004 <- lmer(mmr_dev32~time*b2_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_004) # not sig

model_005 <- lmer(mmr_dev32~time*lang + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_005) # not sig






# Interactions with group

model_0001 <- lmer(mmr_dev32~group*age1 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_0001) # not sig

model_0002 <- lmer(mmr_dev32~group*b1_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_0002) # not sig

model_0003 <- lmer(mmr_dev32~group*b2_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_0003) # trend b2_2 p = 0.0657

model_0004 <- lmer(mmr_dev32~group*lang + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_0004) # not sig






# Interactions with age

model_00001 <- lmer(mmr_dev32~age1*b1_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_00001) # not sig

model_00002 <- lmer(mmr_dev32~age1*b2_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_00002) # not sig

model_00003 <- lmer(mmr_dev32~age1*lang + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_00003) # not sig





# Interactions with b1_2

model_0000001 <- lmer(mmr_dev32~b1_2*b2_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_0000001) # trend

library(ggplot2)

# Step 1: Create a prediction grid over the observed range
b1_range <- seq(min(dat_reduced$b1_2, na.rm = TRUE), max(dat_reduced$b1_2, na.rm = TRUE), length.out = 50)
b2_range <- seq(min(dat_reduced$b2_2, na.rm = TRUE), max(dat_reduced$b2_2, na.rm = TRUE), length.out = 5)  # fewer lines = clearer plot

pred_grid <- expand.grid(b1_2 = b1_range, b2_2 = b2_range)

# Step 2: Predict values from the fixed effects (exclude random effects)
pred_grid$mmr_pred <- predict(model_0000001, newdata = pred_grid, re.form = NA)

# Step 3: Plot
ggplot(pred_grid, aes(x = b1_2, y = mmr_pred, color = as.factor(b2_2), group = b2_2)) +
  geom_line(linewidth = 1) +
  scale_color_viridis_d(name = "b2_2 level") +
  labs(
    title = "Interaction Plot: b1_2 × b2_2 on Predicted MMR",
    x = "b1_2 (Behavioral Score 1)",
    y = "Predicted MMR",
    color = "b2_2"
  ) +
  theme_minimal()


library(car)
vif(model_0000001) # high

model_0000002 <- lmer(mmr_dev32~b1_2*lang + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_0000002) # not sig






# Interactions with b2_2

model_00000001 <- lmer(mmr_dev32~b2_2*lang + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_00000001) # sig

vif(model_00000001) # HIGH









model_1_1 <- lmer(mmr_dev32~tw+time + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_1) 

model_1_2 <- lmer(mmr_dev32~tw*time + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_2) 

# Load required libraries
library(ggeffects)
library(ggplot2)

# Generate predicted values for the interaction tw * time
pred <- ggpredict(model_1_2, terms = c("time", "tw"))  # or c("tw", "time")

# Plot
ggplot(pred, aes(x = x, y = predicted, color = group, group = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.2, color = NA) +
  labs(
    title = "Interaction between Time and TW on Predicted MMR",
    x = "Time Point",
    y = "Predicted MMR (mmr_dev32)",
    color = "Time-Window (TW)",
    fill = "Time-Window (TW)"
  ) +
  theme_minimal()


anova(model_0, model_1) # trend
anova(model_0, model_2) # trend
anova(model_0, model_1_1) # sig
anova(model_0, model_1_2) # sig
anova(model_1_1, model_1_2) # sig
AIC(model_0, model_1, model_2, model_1_1, model_1_2) # Smallest AIC model_1_2









model_1_3 <- lmer(mmr_dev32~tw*time + group + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_3) 

model_1_4 <- lmer(mmr_dev32~tw*time + group*tw + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_4) 

model_1_5 <- lmer(mmr_dev32~group*tw + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_5) 

model_1_6 <- lmer(mmr_dev32~group*tw*time + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_1_6) 

anova(model_1_2, model_1_3)
anova(model_1_2, model_1_4)
anova(model_1_2, model_1_5)
anova(model_1_2, model_1_6)


AIC(model_0, model_1_2, model_3, model_1_4, model_1_5, model_1_6)# Smallest AIC model_1_2



        
model_2_1 <- lmer(mmr_dev32~tw*time + b2_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_2_1) 

model_2_2 <- lmer(mmr_dev32~tw*time + group + b2_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_2_2) 

model_2_3 <- lmer(mmr_dev32~tw*time + group*b2_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_2_3) 

model_2_4 <- lmer(mmr_dev32~group*b2_2 + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_2_4) 

model_2_5 <- lmer(mmr_dev32~b2_2*tw*time + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model_2_5) 


anova(model_1_2, model_2_1)
anova(model_1_2, model_2_2)
anova(model_1_2, model_2_3)
anova(model_1_2, model_2_4)
anova(model_1_2, model_2_5)

AIC(model_0, model_1_2, model_2_1, model_2_2, model_2_3, model_2_4, model_2_5) # Smallest AIC model_1_2






    
##################################################
########## Model plots ##########################
#################################################

model <- lmer(mmr_dev32~tw*time + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model) 

# Load required libraries
library(ggeffects)
library(ggplot2)

# Generate predicted values for the interaction tw * time
pred <- ggpredict(model, terms = c("time", "tw"))  # or c("tw", "time")

# Plot
ggplot(pred, aes(x = x, y = predicted, color = group, group = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.2, color = NA) +
  labs(
    title = "Interaction between Time and TW on Predicted MMR",
    x = "Time Point",
    y = "Predicted MMR (mmr_dev32)",
    color = "Time-Window (TW)",
    fill = "Time-Window (TW)"
  ) +
  theme_minimal()










# Visualization of individual trajectories of MMR (mean) amplitude change form T1 to T2

model <- lmer(mmr_dev32~tw*time + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model)

mod.summary <- summary(model)
str(mod.summary)
mc <- mod.summary$coefficients[,"Estimate"]

# MMR estimate mean at T1
mc["(Intercept)"] + 0.5*mc["tw2"] # -0.26
# MMR estimate mean at T2
mc["(Intercept)"] + 0.5*mc["tw2"] + mc["time2"] + mc["tw2:time2"] # -0.80

df <- data.frame(
  mmr = c(-0.26, -0.8),
  time = c(1, 2)
)

print(df)

dat <- read_excel("var.xlsx")

dat$mmr_diff_t1 <- as.numeric(dat$mmr_diff_t1)
dat$mmr_diff_t2 <- as.numeric(dat$mmr_diff_t2)
dat$p_no <- as.factor(dat$p_no)


# Compute group means and standard errors

dat_long <- dat %>%
  pivot_longer(
    cols = c("mmr_diff_t1", "mmr_diff_t2"),
    names_to = "time",
    values_to = "mmr"
  ) %>%
  mutate(
    time = dplyr::recode(time,
                         "mmr_diff_t1" = "T1",
                         "mmr_diff_t2" = "T2")
  )

# Plot with individual trajectories
ggplot(dat_long) + 
  geom_line(aes(x = time, y = mmr, group = p_no, color = group), alpha = 0.3, linewidth = 0.5) + 
  geom_point(aes(x = time, y = mmr, group = p_no, color = group), size = 3) +  # Points for each time, only dots 
  labs(
    color = "Group"
  ) +
  theme_classic() +  # Minimal theme
  theme(
    legend.position = "none" 
  )+
  scale_color_manual(values = c("dld" = "blue", "td" = "#FF9933")) +  # Set colors
  geom_line(data = df, aes(x = time, y = mmr), color = "black", linewidth = 1.2)

#6699CC








##################################################
########## Model diagnostics ####################
#################################################



model <- lmer(mmr_dev32~time*tw + (time|p_no), data=dat_reduced, REML = FALSE)
summary(model) 






# Model residual variance
resid1 <- resid(model)
shapiro.test(resid1) # 0.0431 *






# Collinearity
library(vcd)
assocstats(table(dat_reduced$time, dat_reduced$tw))$cramer # 0 




# Plot residuals against fitted values to check any non-linear relationship and heteroscedasticity
# Ideally, no clear patterns should be visible in these plots, indicating a linear relationship.
# Extract residuals and fitted values
resid_vals <- resid(model)
fitted_vals <- fitted(model)

# Combine into a data frame
resid_data <- data.frame(
  fitted = fitted_vals,
  residuals = resid_vals
)

# Load ggplot2 for visualization
library(ggplot2)

# Create the plot
ggplot(resid_data, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal()

# Blue LOESS line shows a slight upward trend, which might suggest some mild non-linearity — especially at the lower and higher ends of the fitted values.
# No heteroscedasticity






# Residuals against fitted values - predictors separately
# Extract residuals and fitted values
dat_reduced$resid <- resid(model)
dat_reduced$fitted <- fitted(model)

# Double-check they are added
head(dat_reduced$resid)
head(dat_reduced$fitted)


# 1. Residuals vs TW
ggplot(dat_reduced, aes(x = tw, y = resid)) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  geom_boxplot(outlier.shape = NA, fill = "lightgray", alpha = 0.4) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs TW", x = "TW", y = "Residuals") +
  theme_minimal()

# Residuals centered around zero in both tw-s — the medians of the boxplots are nicely aligned with the red zero line.
# Spread (variance) looks very similar between the two groups — that suggests homoscedasticity (equal variance).
# No extreme skewness or obvious outliers that would hint at problematic influential cases (though a few distant points are expected).



# 2. Residuals vs time
ggplot(dat_reduced, aes(x = time, y = resid)) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  geom_boxplot(outlier.shape = NA, fill = "lightgray", alpha = 0.4) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Time", x = "Time", y = "Residuals") +
  theme_minimal()

# All good








# Distribution of the random intercept and random slope
re <- ranef(model)$p_no
colnames(re) <- c("Intercept", "Slope")
shapiro.test(re$Intercept) # p-value = 0.4016
shapiro.test(re$Slope) #  p-value = 0.7862

# Intercept
ggplot(re, aes(x = Intercept)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "skyblue", color = "black") +
  geom_density(color = "darkblue", size = 1.2) +
  labs(title = "Distribution of Random Intercepts", x = "Random Intercept", y = "Density") +
  theme_minimal()

# Intercept
qqnorm(re$Intercept, main = "Q-Q Plot of Random Intercepts")
qqline(re$Intercept, col = "red")


# Slope
ggplot(re, aes(x = Slope)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "salmon", color = "black") +
  geom_density(color = "darkred", size = 1.2) +
  labs(title = "Distribution of Random Slopes (Time)", x = "Random Slope", y = "Density") +
  theme_minimal()

# Slope
qqnorm(re$Slope, main = "Q-Q Plot of Random Slopes")
qqline(re$Slope, col = "red")










# Correlation between random intercept and slope

re <- ranef(model)$p_no
colnames(re) <- c("Intercept", "Slope")
cor(re$Intercept, re$Slope) # -0.772403




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



model <- lmer(mmr_dev32~time*tw + (time|p_no), data=dat_reduced, REML = FALSE)
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





