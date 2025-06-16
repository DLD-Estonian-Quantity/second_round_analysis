####################### Stat. analysis - other ##############################





############################################################################
######## Group diff and correlations: EEG results and other variables ######
############################################################################

library(readxl)

dat <- read_excel("var.xlsx")

dat$mmr_pos1 <- as.numeric(dat$mmr_pos1)
dat$mmr_pos2 <- as.numeric(dat$mmr_pos2)
dat$mmr_neg1 <- as.numeric(dat$mmr_neg1)
dat$mmr_neg2 <- as.numeric(dat$mmr_neg2)
dat$mmr_32_1 <- as.numeric(dat$mmr_32_1)
dat$mmr_32_2 <- as.numeric(dat$mmr_32_2)
dat$age <- as.numeric(dat$age)
dat$lang <- as.numeric(dat$lang)
dat$int <- as.numeric(dat$int)
dat$b1_2 <- as.numeric(dat$b1_2)
dat$b2_2 <- as.numeric(dat$b2_2)




#################################################
######## Cor between EEG T1 and T2 ##############
#################################################

dat <- read_excel("var.xlsx")

dat$mmr_easy_t1 <- as.numeric(as.character(na_if(dat$mmr_easy_t1, "NA")))
dat$mmr_diff_t1 <- as.numeric(as.character(na_if(dat$mmr_diff_t1, "NA")))


cor.test(dat$mmr_easy_t1, dat$mmr_easy_t2, method = "pearson", use = "complete.obs")

cor.test(dat$mmr_diff_t1, dat$mmr_diff_t2, method = "pearson", use = "complete.obs")






#######################################################
########## Group diff in variables T2 #################
#######################################################

dat <- read_excel("var.xlsx")

############### Ages between TD and DLD

shapiro.test(dat$age1) # p-value = 0.06017
shapiro.test(dat$age2) # p-value = 0.08715
hist(dat$age1)
hist(dat$age2)
qqnorm(dat$age1)
qqline(dat$age1, col = "red")
qqnorm(dat$age2)
qqline(dat$age2, col = "red")


wilcox.test(age1 ~ group, data = dat) # p-value = 0.7085
wilcox.test(age2 ~ group, data = dat) # p-value = 0.6381
wilcox.test(time_b ~ group, data = dat) # p-value = 0.5554


############### Lang between TD and DLD

wilcox.test(lang ~ group, data = dat) # p-value = 3.018e-08


############### Int between TD and DLD

wilcox.test(int ~ group, data = dat) # p-value = 0.001955


############### Diff in sex

table_sex <- table(dat2$group, dat2$sex)
chisq.test(table_sex) # X-squared = 0, df = 1, p-value = 1


###############  Diff in mother edu

table_edu <- table(dat2$group, dat2$mot_edu)
chisq.test(table_edu) # X-squared = 16.364, df = 3, p-value = 0.0009549

###############  Diff in hand

table_hand <- table(dat2$group, dat2$hand)
chisq.test(table_hand) # X-squared = 0, df = 1, p-value = 1





################################################################
#################### Some group diff at T1 #####################
################################################################

library(readxl)

dat2 <- read_excel("var_t1.xlsx")


dat2$sex <- as.factor(dat2$sex)
dat2$group <- as.factor(dat2$group)
dat2$mother_edu <- as.factor(dat2$mother_edu)
dat2$hand<- as.factor(dat2$hand)
dat2$age1 <- as.numeric(dat2$age1)


############### Diff in age


wilcox.test(age1 ~ group, data = dat2) # p-value = 0.7085


############### Diff in sex

table_sex <- table(dat2$group, dat2$sex)
chisq.test(table_sex) # X-squared = 0, df = 1, p-value = 1


###############  Diff in mother edu

table_edu <- table(dat2$group, dat2$mother_edu)
chisq.test(table_edu) # X-squared = 18.432, df = 3, p-value = 0.0003583

###############  Diff in hand

table_hand <- table(dat2$group, dat2$hand)
chisq.test(table_hand) # X-squared = 0, df = 1, p-value = 1




###############################################################
############ Group diff in behavioral results #################
###############################################################

############# NBNBNBNB  - võta siit dat2 ära ja pane lihtalt dat

library(readxl)
dat <- read_excel("var.xlsx")

class(dat2$group)

dat$sex <- as.factor(dat$sex)
dat$group <- as.factor(dat$group)
dat$mot_edu <- as.factor(dat$mot_edu)
dat$hand<- as.factor(dat$hand)
dat$sex<- as.factor(dat$sex)
class(dat$group)
class(dat$b1_2)

dat$b1_1 <- as.numeric(dat$b1_1)
dat$b2_1 <- as.numeric(dat$b2_1)
dat$b1_2 <- as.numeric(dat$b1_2)
dat$b2_2 <- as.numeric(dat$b2_2)

dat$b1_1_rt <- as.numeric(dat$b1_1_rt)
dat$b2_1_rt <- as.numeric(dat$b2_1_rt)
dat$b1_2_rt <- as.numeric(dat$b1_2_rt)
dat$b2_2_rt <- as.numeric(dat$b2_2_rt)


### Distribution

shapiro.test(dat$b1_2) # p-value = 0.01559
shapiro.test(dat$b2_2) # p-value = 0.035
shapiro.test(dat$b1_2_rt) # p-value = 0.0003825
shapiro.test(dat$b2_2_rt) #  p-value = 0.04665

shapiro.test(dat$b1_1) # p-value = 0.06036
shapiro.test(dat$b2_1) # p-value = 0.4614
shapiro.test(dat$b1_1_rt) # p-value = 2.116e-05
shapiro.test(dat$b2_1_rt) # p-value = 0.0003749


### Diff in b1_2

wilcox.test(b1_2 ~ group, data = dat) # W = 430, p-value = 0.0008953

# Run Wilcoxon test and capture the result
res <- wilcox.test(b1_2 ~ group, data = dat, exact = FALSE)

# Calculate effect size r
Z <- qnorm(res$p.value / 2, lower.tail = FALSE)  # approximate Z
N <- nrow(dat2)  # total sample size
r <- Z / sqrt(N)

cat("Effect size r:", round(r, 3), "\n") # Effect size r: 0.51



### Diff in b1_1

wilcox.test(b1_1 ~ group, data = dat) # W = 426.5, p-value = 0.001229

# Run Wilcoxon test and capture the result
res <- wilcox.test(b1_1 ~ group, data = dat, exact = FALSE)

# Calculate effect size r
Z <- qnorm(res$p.value / 2, lower.tail = FALSE)  # approximate Z
N <- nrow(dat)  # total sample size
r <- Z / sqrt(N)

cat("Effect size r:", round(r, 3), "\n") # Effect size r: 0.689  









### Diff in b2_2

wilcox.test(b2_2 ~ group, data = dat) # W = 430, p-value = 0.0008953

# Run Wilcoxon test and capture the result
res <- wilcox.test(b2_2 ~ group, data = dat, exact = FALSE)

# Calculate effect size r
Z <- qnorm(res$p.value / 2, lower.tail = FALSE)  # approximate Z
N <- nrow(dat2)  # total sample size
r <- Z / sqrt(N)

cat("Effect size r:", round(r, 3), "\n") # Effect size r: 0.708 


### Diff in b2_1

wilcox.test(b2_1 ~ group, data = dat) # W = 439.5, p-value = 0.0004675

# Run Wilcoxon test and capture the result
res <- wilcox.test(b2_1 ~ group, data = dat, exact = FALSE)

# Calculate effect size r
Z <- qnorm(res$p.value / 2, lower.tail = FALSE)  # approximate Z
N <- nrow(dat2)  # total sample size
r <- Z / sqrt(N)

cat("Effect size r:", round(r, 3), "\n") # Effect size r: 0.577 





### Diff in b1_2_rt

wilcox.test(b1_2_rt ~ group, data = dat) # W = 237, p-value = 0.4276

# Run Wilcoxon test and capture the result
res <- wilcox.test(b1_2_rt ~ group, data = dat, exact = FALSE)

# Calculate effect size r
Z <- qnorm(res$p.value / 2, lower.tail = FALSE)  # approximate Z
N <- nrow(dat)  # total sample size
r <- Z / sqrt(N)

cat("Effect size r:", round(r, 3), "\n") # Effect size r: 0.577 




### Diff in b1_1_rt 

wilcox.test(b1_1_rt ~ group, data = dat) # W = 186.5, p-value = 0.9328

# Run Wilcoxon test and capture the result
res <- wilcox.test(b1_1_rt ~ group, data = dat, exact = FALSE)

# Calculate effect size r
Z <- qnorm(res$p.value / 2, lower.tail = FALSE)  # approximate Z
N <- nrow(dat)  # total sample size
r <- Z / sqrt(N)

cat("Effect size r:", round(r, 3), "\n") # Effect size r: 0.577 



### Diff in b2_2_rt T2

wilcox.test(b2_2_rt ~ group, data = dat) # W = 237, p-value = 0.4276

# Run Wilcoxon test and capture the result
res <- wilcox.test(b2_2_rt ~ group, data = dat, exact = FALSE)

# Calculate effect size r
Z <- qnorm(res$p.value / 2, lower.tail = FALSE)  # approximate Z
N <- nrow(dat)  # total sample size
r <- Z / sqrt(N)

cat("Effect size r:", round(r, 3), "\n") # Effect size r: 0.311 





### Diff in b2_1_rt

wilcox.test(b2_1_rt ~ group, data = dat) # W = 200.5, p-value = 0.1146

# Run Wilcoxon test and capture the result
res <- wilcox.test(b2_1_rt ~ group, data = dat, exact = FALSE)

# Calculate effect size r
Z <- qnorm(res$p.value / 2, lower.tail = FALSE)  # approximate Z
N <- nrow(dat)  # total sample size
r <- Z / sqrt(N)

cat("Effect size r:", round(r, 3), "\n") # Effect size r: 0.336 








### Group diff in B1 discriminating between quantity degrees T2

# Q1 vs Q2
wilcox.test(b1_2_q12 ~ group, data = dat) # W = 381.5, p-value = 0.01888
# sum in DLD = 45; TD = 22


# Q1 vs Q3
wilcox.test(b1_2_q13 ~ group, data = dat) # W = 478, p-value = 2.246e-06
# sum in DLD = 33; TD = 4

# Q2 vs Q3
wilcox.test(b1_2_q23 ~ group, data = dat) # W = 358.5, p-value = 0.07013
# sum in DLD = 55; TD = 36






### Group diff in B2 discriminating between quantitiy degrees T2

# Q1 vs Q2
wilcox.test(b2_2_q12 ~ group, data = dat) # W = 390, p-value = 0.006088

# Q1 vs Q3
wilcox.test(b2_2_q13 ~ group, data = dat) # W = 318.5, p-value = 0.1796

# Q2 vs Q3
wilcox.test(b2_2_q23 ~ group, data = dat) # W = 404.5, p-value = 0.003967






### Group diff in B1 discriminating between quantitiy degrees T1

# Q1 vs Q2
wilcox.test(b1_1_q12 ~ group, data = dat) # W = 452.5, p-value = 0.0001394
# sum in DLD = 45; TD = 22


# Q1 vs Q3
wilcox.test(b1_1_q13 ~ group, data = dat) # W = 368.5, p-value = 0.03454
# sum in DLD = 33; TD = 4

# Q2 vs Q3
wilcox.test(b1_1_q23 ~ group, data = dat) # W = 387, p-value = 0.01658
# sum in DLD = 55; TD = 36






### Group diff in B2 discriminating between quantitiy degrees T1

# Q1 vs Q2
wilcox.test(b2_1_q12 ~ group, data = dat) # W = 422.5, p-value = 0.0007081

# Q1 vs Q3
wilcox.test(b2_1_q13 ~ group, data = dat) # W = 343.5, p-value = 0.03433

# Q2 vs Q3
wilcox.test(b2_1_q23 ~ group, data = dat) # W = 389.5, p-value = 0.01229



### Diff between T1 and T2 

wilcox.test(dat$b1_1, dat$b1_2, paired = TRUE) # V = 163, p-value = 2.253e-05

wilcox.test(dat$b2_1, dat$b2_2, paired = TRUE) # V = 257, p-value = 0.001181

wilcox.test(dat$b1_1_rt, dat$b1_2_rt, paired = TRUE) # V = 673, p-value = 8.069e-05

wilcox.test(dat$b2_1_rt, dat$b2_2_rt, paired = TRUE) # V = 801, p-value = 0.01139


# For DLD group
wilcox.test(dat$b1_1[dat$group == "dld"], dat$b1_2[dat$group == "dld"], paired = TRUE) # V = 50, p-value = 0.01147

wilcox.test(dat$b2_1[dat$group == "dld"], dat$b2_2[dat$group == "dld"], paired = TRUE) # V = 55, p-value = 0.01896

wilcox.test(dat$b1_1_rt[dat$group == "dld"], dat$b1_2_rt[dat$group == "dld"], paired = TRUE) # V = 144, p-value = 0.04937

wilcox.test(dat$b2_1_rt[dat$group == "dld"], dat$b2_2_rt[dat$group == "dld"], paired = TRUE) # V = 180, p-value = 0.08541




# For TD group
wilcox.test(dat$b1_1[dat$group == "td"], dat$b1_2[dat$group == "td"], paired = TRUE) # V = 34, p-value = 0.000572

wilcox.test(dat$b2_1[dat$group == "td"], dat$b2_2[dat$group == "td"], paired = TRUE) # V = 80, p-value = 0.02735

wilcox.test(dat$b1_1_rt[dat$group == "td"], dat$b1_2_rt[dat$group == "td"], paired = TRUE) # V = 206, p-value = 1.335e-05

wilcox.test(dat$b2_1_rt[dat$group == "td"], dat$b2_2_rt[dat$group == "td"], paired = TRUE) # V = 230, p-value = 0.07098





###########################################################
######### Correlations between variables in models #######
###########################################################

dat <- read_excel("var.xlsx")

cor.test(dat$b1_2, dat$lang) # 0.5008916  
cor.test(dat$b1_2, dat$lang, method = "spearman") # 0.4996091

cor.test(dat$b2_2, dat$lang) # 0.5958969  
cor.test(dat$b2_2, dat$lang, method = "spearman") # 0.5995706  

cor.test(dat$b1_2, dat$b2_2) # 0.5825206 
cor.test(dat$b1_2, dat$b2_2, method = "spearman") # 0.6298752 

cor.test(dat$age1, dat$lang) # 0.01475377 
cor.test(dat$age1, dat$lang, method = "spearman") # -0.1320552






##########################################################
############ Cor between MMR and therapy #################
##########################################################



dat <- read_excel("therapy.xlsx")

class(dat$therapy)
class(dat$MMR_easy)
class(dat$MMR_diff)
class(dat$b1_2)
class(dat$b2_2)

shapiro.test(dat$MMR_easy) # p-value = 0.5491
shapiro.test(dat$MMR_diff) # p-value = 0.0487
shapiro.test(dat$b1_2) # p-value = 0.2683
shapiro.test(dat$b2_2) # p-value = 0.5263
shapiro.test(dat$therapy) #  p-value = 0.1389

cor.test(dat$MMR_easy, dat$therapy, method = "pearson") # cor 0.43

library(ggplot2)

ggplot(dat, aes(x = therapy, y = MMR_easy)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  labs(
    title = "Correlation between Therapy and MMR (Easy Condition)",
    x = "Therapy Score",
    y = "MMR (Easy)"
  ) +
  theme_minimal()

cor_test1 <- cor.test(dat$MMR_easy, dat$therapy, method = "pearson")
r <- cor_test1$estimate
r_squared <- r^2

r
r_squared # small effect 18% of variance explained



# Basic linear regression
model <- lm(MMR_easy ~ therapy, data = dat)
summary(model) #sig


plot(model)
# influential points do not look dangerously influential

library(lmtest)
bptest(model) 












cor.test(dat$MMR_diff, dat$therapy, method = "pearson") # cor 0.56

ggplot(dat, aes(x = therapy, y = MMR_diff)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  labs(
    title = "Correlation between Therapy and MMR (Difficult Condition)",
    x = "Therapy Score",
    y = "MMR (Easy)"
  ) +
  theme_minimal()


cor_test2 <- cor.test(dat$MMR_diff, dat$therapy, method = "pearson")
r <- cor_test2$estimate
r_squared <- r^2

r
r_squared # medium effect; 32% of variance explained

model2 <- lm(MMR_diff ~ therapy, data = dat)
summary(model2) #sig


plot(model2)
# influential points do not look dangerously influential

library(lmtest)
bptest(model2) # homoscedasticity/heteroscedasticity, p-value = 0.05336






cor.test(dat$b1_2, dat$therapy, method = "pearson") # not sig

cor.test(dat$b2_2, dat$therapy, method = "pearson") # not sig

cor.test(dat$b1_2, dat$MMR_easy, method = "pearson") # not sig

cor.test(dat$b1_2, dat$MMR_diff, method = "pearson") # not sig

cor.test(dat$b2_2, dat$MMR_easy, method = "pearson") # not sig

cor.test(dat$b2_2, dat$MMR_diff, method = "pearson") # not sig














