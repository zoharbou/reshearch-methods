
#question 1:
setwd("C:/Users/Dell/Desktop/school/year2/semester b/reasearch_methods")
# the data
df <- read.csv('favorite_laguage.csv', header = TRUE)
language_female <- df$language.level.female
language_male <- df$language.level.male

# manual t test
n_female = length(language_female)
n_male = length(language_male)
female_mean = mean(language_female)
male_mean = mean(language_male)
male_sd = sqrt(sum((language_male - male_mean)^2)/(n_male-1))
female_sd = sqrt(sum((language_female - female_mean)^2)/(n_female-1))
variance_pooled = ((n_female-1)*female_sd^2+(n_male-1)*male_sd^2)/(n_male+n_female-2)
sd_pooled = sqrt(variance_pooled)
t2=(female_mean-male_mean)/(sd_pooled*sqrt((1/n_female)+(1/n_male)))
pt(t2,n_female+n_male-2)
qt(0.975,df=(n_female+n_male-2))

# Confidence interval
half_conf = (sd_pooled*sqrt((1/n_female)+(1/n_male)))*qt(0.975,df=(n_female+n_male-2))
diff = female_mean - male_mean
min= diff-half_conf
max = diff+half_conf

# auto t test:
t.test(language_female,language_male,
       alternative = c("two.sided"),var.equal=TRUE)

###graph for displaying the results
means=c(female_mean,male_mean)
library("ggplot2")
N=c(n_female,n_male)
sd_each_g_unbias=c(sqrt(female_sd),sqrt(male_sd))
width_conf_g=qt(0.975,N-1)*sd_each_g_unbias / sqrt(N)
errorbars_each=c(female_mean-width_conf_g[1],male_mean-width_conf_g[2],
                 female_mean+width_conf_g[1],male_mean+width_conf_g[2])

ggplot(data=data.frame(means),aes(x=c("female","male"),y=means))+
  geom_bar(stat = "identity",fill = "#FF6666")+  #or geom_col() use by default the values
  xlab("gender")+ylab("the programing language level")+
  ggtitle("Mean of the favorite programing language compared by gender")+
  geom_errorbar(ymin = errorbars_each[1:2], ymax = errorbars_each[3:4], 
                width = 0.25)+ ylim(0,10)

female_favorite <- table(df$favorite.language.female)
barplot(female_favorite, main="females favorite programming language", 
        xlab="language type", ylab="number of votes",col="light pink")
male_favorite <- table(df$favorite.language.male)
barplot(male_favorite, main="males favorite programming language", 
        xlab="language type",ylab="number of votes",col="light blue")

