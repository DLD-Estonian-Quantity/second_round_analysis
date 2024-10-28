#####################################################
############## SLRM devShortSada ####################
#####################################################


library(readxl)

dat <- read_excel("table_SLRM.xlsx")

################################################
############# Modeling ########################
################################################


# Main Effects

mudel_0 <- lm(dev64~1, data=dat)
summary(mudel_0)

mudel_1 <- lm(dev64~age, data=dat)
summary(mudel_1)

mudel_2 <- lm(dev64~sex, data=dat)
summary(mudel_2)

mudel_3 <- lm(dev64~group, data=dat)
summary(mudel_3)

mudel_4 <- lm(dev64~lang, data=dat)
summary(mudel_4)

mudel_5 <- lm(dev64~int, data=dat)
summary(mudel_5)
anova(mudel_0, mudel_5)
AIC(mudel_0, mudel_5) # Zero model is not better

mudel_6 <- lm(dev64~d_prime1, data=dat)
summary(mudel_6)

mudel_7 <- lm(dev64~d_prime2, data=dat)
summary(mudel_7)


############################# int with age #########################################


mudel_0 <- lm(dev64~ int, data=dat)
summary(mudel_0)

mudel_2 <- lm(dev64~ int + age*sex, data=dat)
summary(mudel_2)

mudel_3 <- lm(dev64~ int + age*group, data=dat)
summary(mudel_3)

mudel_4 <- lm(dev64~ int + age*lang, data=dat)
summary(mudel_4)

mudel_5 <- lm(dev64~ int + age*int, data=dat)
summary(mudel_5)

mudel_6 <- lm(dev64~ int + age*d_prime1, data=dat)
summary(mudel_6)

mudel_7 <- lm(dev64~ int + age*d_prime2, data=dat)
summary(mudel_7)

############################# int with sex #########################################


mudel_0 <- lm(dev64~int, data=dat)
summary(mudel_0)


mudel_3 <- lm(dev64~ int + sex*group, data=dat)
summary(mudel_3)


mudel_4 <- lm(dev64~ int + sex*lang, data=dat)
summary(mudel_4)


mudel_5 <- lm(dev64~ int + sex*int, data=dat)
summary(mudel_5)

mudel_6 <- lm(dev64~ int + sex*d_prime1, data=dat)
summary(mudel_6)


mudel_7 <- lm(dev64~ int + sex*d_prime2, data=dat)
summary(mudel_7)


############################# int with group #########################################


mudel_0 <- lm(dev64~int, data=dat)
summary(mudel_0)


mudel_4 <- lm(dev64~ int + group*lang, data=dat)
summary(mudel_4)


mudel_5 <- lm(dev64~ int + group*int, data=dat)
summary(mudel_5)

mudel_6 <- lm(dev64~ int + group*d_prime1, data=dat)
summary(mudel_6)


mudel_7 <- lm(dev64~ int + group*d_prime2, data=dat)
summary(mudel_7)


############################# int with lang #########################################


mudel_0 <- lm(dev64~1, data=dat)
summary(mudel_0)


mudel_5 <- lm(dev64~ int + lang*int, data=dat)
summary(mudel_5)

mudel_6 <- lm(dev64~ int + lang*d_prime1, data=dat)
summary(mudel_6)


mudel_7 <- lm(dev64~ int + lang*d_prime2, data=dat)
summary(mudel_7)


############################# int with int #########################################


mudel_0 <- lm(dev64~int, data=dat)
summary(mudel_0)


mudel_6 <- lm(dev64~ int + int*d_prime1, data=dat)
summary(mudel_6)


mudel_7 <- lm(dev64~ int + int*d_prime2, data=dat)
summary(mudel_7)

############################# int with d_1 #########################################


mudel_0 <- lm(dev64~int, data=dat)
summary(mudel_0)


mudel_7 <- lm(dev64~ int + d_prime1*d_prime2, data=dat)
summary(mudel_7)


####################################################
##### Testing model assumptions AFTER MODELING ####
###################################################

# Effect plotting

# Load necessary libraries
library(ggplot2)

# Fit the linear model
mudel_1 <- lm(dev64 ~ int, data = dat)

# Create the plot with points and regression lines
p <- ggplot(dat, aes(x = int, y = dev64)) +
  labs(title = "Regression Plot with Effects",
       x = "Int",
       y = "Dev") +
  theme_minimal() +
  geom_smooth(method = "lm", se = FALSE)  

# Print the plot
print(p)

# Distribution of the model residuals

shapiro.test(residuals(mudel_1)) 

#### Multicolinearity

library(car)
vif(mudel_1) #ok


# Linear realtions, homoscedacity, influential points

par(mfrow = c(2, 2))
plot(mudel_1)


