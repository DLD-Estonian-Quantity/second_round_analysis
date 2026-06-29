####################### Stat. analysis - other ##############################


library(readxl)



############################################################################
######## Group diff and correlations: EEG results and other variables ######
############################################################################


#################################################
######## Cor between EEG T1 and T2 ##############
#################################################

dat <- read_excel("var_08excluded.xlsx")


dat$mmr_easy_t1 <- as.numeric(as.character(na_if(dat$mmr_easy_t1, "NA")))

cor.test(dat$mmr_easy_t1, dat$mmr_easy_t2, method = "pearson", use = "complete.obs")



#######################################################
########## Group diff in variables T2 #################
#######################################################

dat <- read_excel("var_08excluded.xlsx")

############### Ages between TD and DLD

shapiro.test(dat$age2) # p-value = 0.09236
hist(dat$age2)
qqnorm(dat$age2)
qqline(dat$age2, col = "red")


wilcox.test(age2 ~ group, data = dat) # W = 282.5, p-value = 0.6662
wilcox.test(time_b ~ group, data = dat) # W = 297, p-value = 0.3139


############### Diff in sex

table_sex <- table(dat$group, dat$sex)
chisq.test(table_sex) # X-squared = 0, df = 1, p-value = 1


###############  Diff in mother edu

table_edu <- table(dat$group, dat$mot_edu)
chisq.test(table_edu) # X-squared = 15.312, df = 3, p-value = 0.001568

###############  Diff in hand

table_hand <- table(dat$group, dat$hand)
chisq.test(table_hand) # X-squared = 0.11735, df = 1, p-value = 0.7319


################################################################
############### Group diff in behavioral tasks ##################
################################################################

dat$sex <- as.factor(dat$sex)
dat$group <- as.factor(dat$group)
dat$mot_edu <- as.factor(dat$mot_edu)
dat$hand<- as.factor(dat$hand)
dat$sex<- as.factor(dat$sex)
class(dat$group)


dat$b1_1 <- as.numeric(dat$b1_1)
dat$b2_1 <- as.numeric(dat$b2_1)
dat$b1_2 <- as.numeric(dat$b1_2)
dat$b2_2 <- as.numeric(dat$b2_2)

dat$b1_1_rt <- as.numeric(dat$b1_1_rt)
dat$b2_1_rt <- as.numeric(dat$b2_1_rt)
dat$b1_2_rt <- as.numeric(dat$b1_2_rt)
dat$b2_2_rt <- as.numeric(dat$b2_2_rt)

class(dat$b1_2)


### Diff between T1 and T2 

wilcox.test(dat$b1_1, dat$b1_2, paired = TRUE) # V = 159, p-value = 3.146e-05

wilcox.test(dat$b2_1, dat$b2_2, paired = TRUE) # V = 241, p-value = 0.001088

wilcox.test(dat$b1_1_rt, dat$b1_2_rt, paired = TRUE) # V = 650, p-value = 5.206e-05

wilcox.test(dat$b2_1_rt, dat$b2_2_rt, paired = TRUE) # V = 762, p-value = 0.01476



### Group diff in b1_2

wilcox.test(b1_2 ~ group, data = dat) # W = 104.5, p-value = 0.0005096

# Run Wilcoxon test and capture the result
res <- wilcox.test(b1_2 ~ group, data = dat, exact = FALSE)

# Calculate effect size r
Z <- qnorm(res$p.value / 2, lower.tail = FALSE)  # approximate Z
N <- nrow(dat)  # total sample size
r <- Z / sqrt(N)

cat("Effect size r:", round(r, 3), "\n") # Effect size r: 0.512 



### Diff in b1_1

wilcox.test(b1_1 ~ group, data = dat) # W = 114.5, p-value = 0.00109

# Run Wilcoxon test and capture the result
res <- wilcox.test(b1_1 ~ group, data = dat, exact = FALSE)

# Calculate effect size r
Z <- qnorm(res$p.value / 2, lower.tail = FALSE)  # approximate Z
N <- nrow(dat)  # total sample size
r <- Z / sqrt(N)

cat("Effect size r:", round(r, 3), "\n") # Effect size r: 0.482 









### Diff in b2_2

wilcox.test(b2_2 ~ group, data = dat) # W = 119, p-value = 0.001465

# Run Wilcoxon test and capture the result
res <- wilcox.test(b2_2 ~ group, data = dat, exact = FALSE)

# Calculate effect size r
Z <- qnorm(res$p.value / 2, lower.tail = FALSE)  # approximate Z
N <- nrow(dat)  # total sample size
r <- Z / sqrt(N)

cat("Effect size r:", round(r, 3), "\n") # Effect size r: 0.469


### Diff in b2_1

wilcox.test(b2_1 ~ group, data = dat) # W = 139.5, p-value = 0.006897

# Run Wilcoxon test and capture the result
res <- wilcox.test(b2_1 ~ group, data = dat, exact = FALSE)

# Calculate effect size r
Z <- qnorm(res$p.value / 2, lower.tail = FALSE)  # approximate Z
N <- nrow(dat)  # total sample size
r <- Z / sqrt(N)

cat("Effect size r:", round(r, 3), "\n") # Effect size r: 0.398





### Diff in b1_2_rt

wilcox.test(b1_2_rt ~ group, data = dat) # W = 319, p-value = 0.2168

# Run Wilcoxon test and capture the result
res <- wilcox.test(b1_2_rt ~ group, data = dat, exact = FALSE)

# Calculate effect size r
Z <- qnorm(res$p.value / 2, lower.tail = FALSE)  # approximate Z
N <- nrow(dat)  # total sample size
r <- Z / sqrt(N)

cat("Effect size r:", round(r, 3), "\n") # Effect size r: 0.182 




### Diff in b1_1_rt 

wilcox.test(b1_1_rt ~ group, data = dat) # W = 189.5, p-value = 0.7924

# Run Wilcoxon test and capture the result
res <- wilcox.test(b1_1_rt ~ group, data = dat, exact = FALSE)

# Calculate effect size r
Z <- qnorm(res$p.value / 2, lower.tail = FALSE)  # approximate Z
N <- nrow(dat)  # total sample size
r <- Z / sqrt(N)

cat("Effect size r:", round(r, 3), "\n") # Effect size r: 0.039



### Diff in b2_2_rt T2

wilcox.test(b2_2_rt ~ group, data = dat) # W = 293, p-value = 0.5119

# Run Wilcoxon test and capture the result
res <- wilcox.test(b2_2_rt ~ group, data = dat, exact = FALSE)

# Calculate effect size r
Z <- qnorm(res$p.value / 2, lower.tail = FALSE)  # approximate Z
N <- nrow(dat)  # total sample size
r <- Z / sqrt(N)

cat("Effect size r:", round(r, 3), "\n") # Effect size r: 0.098





### Diff in b2_1_rt

wilcox.test(b2_1_rt ~ group, data = dat) # W = 328.5, p-value = 0.1486

# Run Wilcoxon test and capture the result
res <- wilcox.test(b2_1_rt ~ group, data = dat, exact = FALSE)

# Calculate effect size r
Z <- qnorm(res$p.value / 2, lower.tail = FALSE)  # approximate Z
N <- nrow(dat)  # total sample size
r <- Z / sqrt(N)

cat("Effect size r:", round(r, 3), "\n") # Effect size r: 0.213 






### Group diff in B1 discriminating between quantity degrees T2


# Q1 vs Q2
class(dat$b1_2_q12)
wilcox.test(b1_2_q12 ~ group, data = dat) # W = 359.5, p-value = 0.02686
# sum in DLD = 43; TD = 22


# Q1 vs Q3
class(dat$b1_2_q13)
wilcox.test(b1_2_q13 ~ group, data = dat) # W = 455, p-value = 3.155e-06
# sum in DLD = 32; TD = 4

# Q2 vs Q3
class(dat$b1_2_q23)
wilcox.test(b1_2_q23 ~ group, data = dat) # W = 335.5, p-value = 0.1013
# sum in DLD = 50; TD = 36






### Group diff in B2 discriminating between quantity degrees T2

# Q1 vs Q2
class(dat$b2_2_q12)
wilcox.test(b2_2_q12 ~ group, data = dat) # W = 366, p-value = 0.01007
# sum in DLD = 20; TD = 8

# Q1 vs Q3
wilcox.test(b2_2_q13 ~ group, data = dat) # W = 295, p-value = 0.2835
# sum in DLD = 6; TD = 3

# Q2 vs Q3
wilcox.test(b2_2_q23 ~ group, data = dat) # W = 379.5, p-value = 0.006945
# sum in DLD = 41; TD = 16





### Group diff in B1 discriminating between quantitiy degrees T1

# Q1 vs Q2
class(dat$b1_1_q12)
wilcox.test(b1_1_q12 ~ group, data = dat) # W = 432.5, p-value = 0.0001608
# sum in DLD = 97; TD = 45


# Q1 vs Q3
wilcox.test(b1_1_q13 ~ group, data = dat) # W = 344.5, p-value = 0.05431
# sum in DLD = 39; TD = 17

# Q2 vs Q3
wilcox.test(b1_1_q23 ~ group, data = dat) # W = 362.5, p-value = 0.02695
# sum in DLD = 101; TD = 76






### Group diff in B2 discriminating between quantitiy degrees T1

# Q1 vs Q2
class(dat$b2_1_q12)
wilcox.test(b2_1_q12 ~ group, data = dat) # W = 414, p-value = 0.0003402
# sum in DLD = 45; TD = 22

# Q1 vs Q3
wilcox.test(b2_1_q13 ~ group, data = dat) # W = 332, p-value = 0.02764

# Q2 vs Q3
wilcox.test(b2_1_q23 ~ group, data = dat) # W = 369.5, p-value = 0.0155




### T1 and T2 diff in Q1 vs Q2

# Quant dif
wilcox.test(dat$b1_1_q12, dat$b1_2_q12, paired = TRUE) # V = 609, p-value = 9.476e-05
# For DLD group
wilcox.test(dat$b1_1_q12[dat$group == "dld"], dat$b1_2_q12[dat$group == "dld"], paired = TRUE) # V = 187, p-value = 0.002235
# For TD group
wilcox.test(dat$b1_1_q12[dat$group == "td"], dat$b1_2_q12[dat$group == "td"], paired = TRUE) # V = 131, p-value = 0.00911


# Lex des
wilcox.test(dat$b2_1_q12, dat$b2_2_q12, paired = TRUE) # V = 414, p-value = 0.0003402
# For DLD group
wilcox.test(dat$b2_1_q12[dat$group == "dld"], dat$b2_2_q12[dat$group == "dld"], paired = TRUE) # V = 115.5, p-value = 0.014
# For TD group
wilcox.test(dat$b2_1_q12[dat$group == "td"], dat$b2_2_q12[dat$group == "td"], paired = TRUE) # V = 32.5, p-value = 0.6267



### T1 and T2 diff in Q2 vs Q3

# Quant dif
wilcox.test(dat$b1_1_q23, dat$b1_2_q23, paired = TRUE) # V = 595, p-value = 3.705e-05
# For DLD group
wilcox.test(dat$b1_1_q23[dat$group == "dld"], dat$b1_2_q23[dat$group == "dld"], paired = TRUE) # V = 147.5, p-value = 0.0007848
# For TD group
wilcox.test(dat$b1_1_q23[dat$group == "td"], dat$b1_2_q23[dat$group == "td"], paired = TRUE) # V = 159, p-value = 0.01001



# Lex des
wilcox.test(dat$b2_1_q23, dat$b2_2_q23, paired = TRUE) # V = 378.5, p-value = 0.02633
# For DLD group
wilcox.test(dat$b2_1_q23[dat$group == "dld"], dat$b2_2_q23[dat$group == "dld"], paired = TRUE) # V = 115.5, p-value = 0.1864
# For TD group
wilcox.test(dat$b2_1_q23[dat$group == "td"], dat$b2_2_q23[dat$group == "td"], paired = TRUE) # V = 81, p-value = 0.0626







