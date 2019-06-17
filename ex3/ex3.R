# question num 1 manually:
n=16
grades = c(24, 32, 95, 89, 14, 78, 91, 82, 38, 56, 70, 64, 27, 55, 43, 100)
mean_grades = mean(grades)
dif = grades - mean_grades
squared <- dif^2
sd = sqrt(sum(squared)/(n-1))
mu = 50
grades_statistics = (mean_grades- mu)/(sd/sqrt(n))
deg_freedom=n-1
upp_b = mean_grades + 2.131*(sd/sqrt(n))
low_b = mean_grades - 2.131*(sd/sqrt(n))

# question num 1 by R:
t.test(grades,mu=50,conf.level=0.95, alternative="two.sided")
# install.packages("gplots")
library(gplots)

conf_range <- function(n, alfa) {
deg_freedom=n-1
labs = 1000
counter_is_0_in = 0
counter_is_5_in = 0
means_ = numeric(labs)
half_width_conf_total = numeric(labs)
for(i in 1:labs){
  sample = rnorm(n,mean=5,sd=sqrt(10))
  mean_ = mean(sample)
  means_[i] = mean_
  sd = sd(sample)
  half_width_conf_group=qt((1-alfa/2),n-1)*sd / sqrt(n)
  half_width_conf_total[i] = half_width_conf_group
  upp_b = mean_ + half_width_conf_group
  low_b = mean_ - half_width_conf_group
  if(isTRUE(low_b <= 0) & isTRUE(0 <= upp_b)){
    counter_is_0_in = counter_is_0_in + 1
  }
  if(isTRUE(low_b <= 5) & isTRUE(5 <= upp_b)){
    counter_is_5_in = counter_is_5_in + 1
  }
}
#print the results:
pres_of_5 = (counter_is_5_in/labs)*100
print(counter_is_0_in)
print(pres_of_5)

#plot the results:
plotCI(x=means_, uiw=half_width_conf_total, col="light pink", barcol="purple",lwd=2,
       xaxt="n", xlim=c(0,labs), ylab = "conf range of the mean", xlab="lab sample",
       main="confidence range of the expected value
       for samples from normal distribution:" )
}


# question 2 first simulation:
conf_range(30,0.05)

# question 2 second simulation:
conf_range(100,0.05)

# question 2 third simulation:
conf_range(30,0.01)
