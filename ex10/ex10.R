# ex 10
setwd("C:\\Users\\Dell\\Desktop\\school\\year2\\semester b\\reasearch_methods\\???????? ??????????\\??????????????\\??????????????\\data")


# power analysis
effect_size = 0.1

#install.packages("pwr")
library(pwr)
pwr.f2.test(u=1,f2=effect_size,sig.level = 0.05,power=0.8)

data_acustic = read.csv("forklift_acoustic.csv")
View(data_acustic)

#data_acustic$annoyance = as.factor(data_acustic$annoyance)
#data_acustic$fluc = as.factor(data_acustic$fluc)
str(data_acustic)
# model a
linear_a = lm(annoyance~fluc,data=data_acustic)
summary(linear_a)

# model b
linear_b = lm(annoyance~fluc+impulsive,data=data_acustic)
summary(linear_b)
summary(lm(linear_a,linear_b))

# graph for model a
library("ggpubr")
ggscatter(data_acustic, x = "fluc", y = "annoyance", 
          add = "reg.line",
          main = "The relation between fluctuations and annoyance",
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "fluctuations (in vasil)", ylab = "annoyance")

# adding test time parameter
data_acustic$test_time = ifelse(data_acustic$caseNum<26,0,1)
View(data_acustic)
linear_3 = lm(annoyance~test_time,data=data_acustic)
summary(linear_3)

