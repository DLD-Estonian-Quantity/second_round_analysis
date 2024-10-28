################################################
############## SLRM with MMR ####################
###############################################


library(readxl)

dat <- read_excel("table_SLRM.xlsx")


################################################
############# Modeling ########################
################################################

# Main effects

mudel_0 <- lm(mmr~1, data=dat)
summary(mudel_0)

mudel_1 <- lm(mmr~age, data=dat)
summary(mudel_1)

mudel_2 <- lm(mmr~sex, data=dat)
summary(mudel_2)

mudel_3 <- lm(mmr~group, data=dat)
summary(mudel_3)

mudel_4 <- lm(mmr~lang, data=dat)
summary(mudel_4)

mudel_5 <- lm(mmr~int, data=dat)
summary(mudel_5)

mudel_6 <- lm(mmr~d_prime1, data=dat)
summary(mudel_6)

mudel_7 <- lm(mmr~d_prime2, data=dat)
summary(mudel_7)


############################# int with age #########################################


mudel_0 <- lm(mmr~1, data=dat)
summary(mudel_0)



mudel_2 <- lm(mmr~age*sex, data=dat)
summary(mudel_2)

mudel_3 <- lm(mmr~age*group, data=dat)
summary(mudel_3)
anova(mudel_0, mudel_3)
AIC(mudel_0, mudel_3) # Smaller model is not better

#mudel_4 <- lm(mmr~age*lang, data=dat)
#summary(mudel_4)
#anova(mudel_0, mudel_4)
#AIC(mudel_0, mudel_3, mudel_4)
#vif(mudel_4) # multicolinearity!

mudel_5 <- lm(mmr~age*int, data=dat)
summary(mudel_5)

mudel_6 <- lm(mmr~age*d_prime1, data=dat)
summary(mudel_6)
anova(mudel_0, mudel_6)
AIC(mudel_0, mudel_6) # smaller model is better

mudel_7 <- lm(mmr~age*d_prime2, data=dat)
summary(mudel_7)


############################# int with sex #########################################


mudel_0 <- lm(mmr~age*group, data=dat)
summary(mudel_0)


mudel_3 <- lm(mmr~age*group+sex*group, data=dat)
summary(mudel_3)


mudel_4 <- lm(mmr~age*group+sex*lang, data=dat)
summary(mudel_4)


mudel_5 <- lm(mmr~age*group+sex*int, data=dat)
summary(mudel_5)

mudel_6 <- lm(mmr~age*group+sex*d_prime1, data=dat)
summary(mudel_6)



mudel_7 <- lm(mmr~age*group+sex*d_prime2, data=dat)
summary(mudel_7)

############################# int with group #########################################


mudel_0 <- lm(mmr~age*group, data=dat)
summary(mudel_0)


#mudel_4 <- lm(mmr~group*lang, data=dat)
#summary(mudel_4)


mudel_5 <- lm(mmr~age*group+group*int, data=dat)
summary(mudel_5)

mudel_6 <- lm(mmr~age*group+group*d_prime1, data=dat)
summary(mudel_6)


mudel_7 <- lm(mmr~age*group+group*d_prime2, data=dat)
summary(mudel_7)

############################# int with int #########################################


mudel_0 <- lm(mmr~age*group, data=dat)
summary(mudel_0)


mudel_6 <- lm(mmr~age*group+int*d_prime1, data=dat)
summary(mudel_6)


mudel_7 <- lm(mmr~age*group+int*d_prime2, data=dat)
summary(mudel_7)
anova(mudel_3, mudel_7)
AIC(mudel_3, mudel_7) # smaller is better


############################# int with d_1 #########################################


mudel_0 <- lm(mmr~age*group, data=dat)
summary(mudel_0)


mudel_7 <- lm(mmr~age*group+d_prime1*d_prime2, data=dat)
summary(mudel_7)


####################################################
##### Testing model assumptions AFTER MODELING ####
###################################################


# Plotting effects

# Load necessary libraries
library(ggplot2)

# Fit the linear model
mudel_1 <- lm(mmr ~ age * group, data = dat)
summary(mudel_1)

# Create the plot with points and regression lines
p <- ggplot(dat, aes(x = age, y = mmr, color = edu)) +
  geom_point() +  # Plot the data points, colored by 'group'
  labs(title = "Regression Plot with Interaction Effects",
       x = "age",
       y = "MMR",
       color = "Group") +
  theme_minimal() +
  geom_smooth(method = "lm", aes(color = group), se = FALSE)

# Print the plot
print(p)

# Distribution of the model residuals

shapiro.test(residuals(mudel_1)) 


# Linear relations, homoscedacity, influential points

par(mfrow = c(2, 2))
plot(mudel_1)


