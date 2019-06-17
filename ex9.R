library(readxl)
library(plyr)
setwd("C:/Users/Dell/Desktop/school/year2/semester b/reasearch_methods/ex9")
data=data.frame(read.csv("data_proc_qIAT.csv"))
# part a

count_trials = count(data$Response.ID)
most_common = sort(table(count_trials$freq),decreasing=TRUE)[1]
most_common = 248
count_trials = count_trials[count_trials$freq==most_common,]
data <- data[data$Response.ID %in% count_trials$x,]

# part b
data<-data[(data$Correct...c..Incorrect...x== "X" | data$Correct...c..Incorrect...x == "C"),]
data$Correct...c..Incorrect...x = ifelse(data$Correct...c..Incorrect...x == "X",1,0)
error_perc=aggregate(Correct...c..Incorrect...x ~ Response.ID, data, mean)
