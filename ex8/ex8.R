library(readxl)
library(psych)

proc1_data=data.frame(read_excel("proc1.xlsx"))
psych::alpha(proc1_data[,-1])
View(proc1_data)

proc2_data=data.frame(read_excel("proc2.xlsx"))
psych::alpha(proc2_data[,-1])
View(proc2_data[2:11])

### Q1 B 
proc1 = proc1_data
proc1$time = 1
proc2 = proc2_data[1:11]
proc2$time = 2
proc = merge(proc1, proc2, all=TRUE, by.x=colnames(proc1), by.y=colnames(proc2))
View(proc)
stability <- testRetest(t1=proc, t2=NULL, id="ID", time="time", keys=c(2:11), lmer=FALSE)
stability

### Q1 C
proc2_A = proc2_data[1:11]
proc2_B = proc2_data[c(1,12:16)]

proc2_A$sum = rowSums(proc2_A[2:11])
proc2_B$sum = rowSums(proc2_B[2:6])

R=cor(proc2_A$sum,proc2_B$sum)

#Q5 b
library("readxl")
data_reading_table <- read_excel("warm.xlsx")
data_reading=data.frame(data_reading_table)

body_temp<-data_reading$body.temp
warmness<-data_reading$social.warmness

sd_x =sd(body_temp)
sd_y =sd(warmness)
cov_x_y = cov(body_temp,warmness)
cor_x_y = cov_x_y/(sd_x*sd_y)

R=cor(body_temp,warmness)
cor.test(x=body_temp, #default: two-sided
         y=warmness, 
         method=c("pearson"))

# Q5 d
linear=lm(warmness ~ body_temp)
print(linear)


#median split:
med = median(body_temp)
group1 <-warmness[body_temp>=med]
group2 <-warmness[body_temp<med]

t.test(group1,group2,
       alternative = c("two.sided"),var.equal=TRUE)
