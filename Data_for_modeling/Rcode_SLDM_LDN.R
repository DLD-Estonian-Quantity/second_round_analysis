#################################################
############## SLRM with LDN ####################
################################################


library(readxl)

dat <- read_excel("table_SLRM.xlsx")

################################################
############# Modeling ########################
################################################

# Main effects

mudel_0 <- lm(ldn~1, data=dat)
summary(mudel_0)

mudel_1 <- lm(ldn~age, data=dat)
summary(mudel_1)

mudel_2 <- lm(ldn~sex, data=dat)
summary(mudel_2)

mudel_3 <- lm(ldn~group, data=dat)
summary(mudel_3)

mudel_4 <- lm(ldn~lang, data=dat)
summary(mudel_4)

mudel_5 <- lm(ldn~int, data=dat)
summary(mudel_5)

mudel_6 <- lm(ldn~d_prime1, data=dat)
summary(mudel_6)

mudel_7 <- lm(ldn~d_prime2, data=dat)
summary(mudel_7)



############################# int with age #########################################


mudel_0 <- lm(ldn~1, data=dat)
summary(mudel_0)


mudel_2 <- lm(ldn~age*sex, data=dat)
summary(mudel_2)

mudel_3 <- lm(ldn~age*group, data=dat)
summary(mudel_3)

mudel_4 <- lm(ldn~age*lang, data=dat)
summary(mudel_4)

mudel_5 <- lm(ldn~age*int, data=dat)
summary(mudel_5)

mudel_6 <- lm(ldn~age*d_prime1, data=dat)
summary(mudel_6)

mudel_7 <- lm(ldn~age*d_prime2, data=dat)
summary(mudel_7)

mudel_8 <- lm(ldn~age*edu, data=dat)
summary(mudel_8)


############################# int with sex #########################################


mudel_0 <- lm(ldn~1, data=dat)
summary(mudel_0)


mudel_3 <- lm(ldn~sex*group, data=dat)
summary(mudel_3)


mudel_4 <- lm(ldn~sex*lang, data=dat)
summary(mudel_4)


mudel_5 <- lm(ldn~sex*int, data=dat)
summary(mudel_5)

mudel_6 <- lm(ldn~sex*d_prime1, data=dat)
summary(mudel_6)


mudel_7 <- lm(ldn~sex*d_prime2, data=dat)
summary(mudel_7)
anova(mudel_0, mudel_7)
AIC(mudel_0, mudel_7) # Smaller model is not better


############################# int with group #########################################


mudel_0 <- lm(ldn~sex*d_prime2, data=dat)
summary(mudel_0)


mudel_4 <- lm(ldn~sex*d_prime2+group*lang, data=dat)
summary(mudel_4)


mudel_5 <- lm(ldn~sex*d_prime2+group*int, data=dat)
summary(mudel_5)

mudel_6 <- lm(ldn~sex*d_prime2+group*d_prime1, data=dat)
summary(mudel_6)
anova(mudel_0, mudel_6)
AIC(mudel_0, mudel_6) # smaller is better

mudel_7 <- lm(ldn~sex*d_prime2+group*d_prime2, data=dat)
summary(mudel_7)
anova(mudel_0, mudel_7)
AIC(mudel_0, mudel_7) # smaller is better

############################# int with lang #########################################


mudel_0 <- lm(ldn~sex*d_prime2, data=dat)
summary(mudel_0)

mudel_5 <- lm(ldn~sex*d_prime2+lang*int, data=dat)
summary(mudel_5)

mudel_6 <- lm(ldn~sex*d_prime2+lang*d_prime1, data=dat)
summary(mudel_6)


mudel_7 <- lm(ldn~sex*d_prime2+lang*d_prime2, data=dat)
summary(mudel_7)


############################# int with int #########################################


mudel_0 <- lm(ldn~sex*d_prime2, data=dat)
summary(mudel_0)

mudel_6 <- lm(ldn~sex*d_prime2+int*d_prime1, data=dat)
summary(mudel_6)


mudel_7 <- lm(ldn~sex*d_prime2+int*d_prime2, data=dat)
summary(mudel_7)


############################# int with d_1 #########################################


mudel_0 <- lm(ldn~sex*d_prime2, data=dat)
summary(mudel_0)


mudel_7 <- lm(ldn~sex*d_prime2+d_prime1*d_prime2, data=dat)
summary(mudel_7)




####################################################
##### Testing model assumptions AFTER MODELING ####
###################################################


# Plotting model effects

# Load necessary libraries
library(ggplot2)

# Fit the linear model
mudel_1 <- lm(ldn ~ d_prime2 * ldn, data = dat)

summary(mudel_1)# Create the plot with points and regression lines
p <- ggplot(dat, aes(x = d_prime2, y = ldn, color = edu)) +
  geom_point() +  # Plot the data points, colored by 'sex'
  labs(title = "Regression Plot with Interaction Effects",
       x = "D-prime2",
       y = "LDN",
       color = "Education Level") +
  theme_minimal() +
  geom_smooth(method = "lm", aes(color = sex), se = FALSE)  

# Print the plot
print(p)

# Distribution of model residuals

shapiro.test(residuals(mudel_1)) # Mudeli jäägid on normaaljaotuses



#### Multicolinearity

library(car)
vif(mudel_1) #ok


# Linear relatsions, homoscedacity, influential points

par(mfrow = c(2, 2))
plot(mudel_1)


