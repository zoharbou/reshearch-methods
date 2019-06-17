setwd("D:\\SCHOOL\\שנה_ב\\סמסטר_ב\\שיטות מחקר\\תרגיל 6")
#install.packages("lsmeans")
#install.packages("dplyr")
#install.packages("ggpubr")

#################  Q 1  #################
q6data <- read.csv("q1_data.csv", header = TRUE)
q6data$ן..sport=factor(q6data$ן..sport,
                       levels = 1:4,
                       labels = c("run", "bike",
                                  "football","basketball"))
cont1 = c(1,1,-1,-1)
cont2 = c(0,0,1,-1)
cont3 = c(1,-1,0,0)

aov_test = aov(lactic~ן..sport, data = q6data)
summary(aov_test)

library(lsmeans)
Weights1=list(cont1)
Weights2=list(cont2)
Weights3=list(cont3)
leastsquare = lsmeans(aov_test, "ן..sport")

contrast(leastsquare, Weights1, adjust="none")
contrast(leastsquare, Weights2, adjust="none") 
contrast(leastsquare, Weights3, adjust="none") 

#################  Q 4  #################
my_data = data.frame(ToothGrowth)
my_data$dose <- factor(my_data$dose, 
                       levels = c(0.5, 1, 2),
                       labels = c("Dose 0.5", "Dose 1", "Dose 2"))

#################  A  #################
two_way_aov <- aov(len ~ supp * dose, data = my_data)
summary(two_way_aov)

library("ggpubr")
ggline(my_data, x = "dose", y = "len", color = "supp",
       add = c("mean_se", "dotplot"),
       palette = c("#FC4E07", "#00AFBB"))

#################  C  #################
one_way_aov = aov(len ~ supp, data=my_data)
summary(one_way_aov)
