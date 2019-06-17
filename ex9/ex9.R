library(readxl)
library(plyr)
setwd("C:/Users/Dell/Desktop/school/year2/semester b/reasearch_methods/ex9/reshearch-methods/ex9")
data=data.frame(read.csv("data_proc_qIAT.csv"))
colnames(data)[c(2,4)]<-c("Participant","Block")

# part b
count_trials = count(data$Response.ID)
most_common = sort(table(count_trials$freq),decreasing=TRUE)[1]
most_common
most_common = 248
count_trials = count_trials[count_trials$freq==most_common,]
data <- data[data$Response.ID %in% count_trials$x,]

# part c
data<-data[(data$Correct...c..Incorrect...x== "X" | data$Correct...c..Incorrect...x == "C"),]
data$Correct...c..Incorrect...x = ifelse(data$Correct...c..Incorrect...x == "X",1,0)
error_perc=aggregate(Correct...c..Incorrect...x ~ Response.ID, data, mean)
error_perc

# part d
hist(data$RT.of.stimulus, 
     main="Histogram for RT of stimulus", 
     xlab="Stimulus per participant", 
     ylab = "Reaction time",
     border="#a794dd",
     xlim = c(0,10000),
     col="light blue",
     las=1, 
     breaks=10000)

#part e
data["RT_clean"] <- data["RT.of.stimulus"]
data$RT_clean[which(data$RT_clean < 400 | data$RT_clean > 10000)] <- NA

#part f
percentage_from_RT = sum(is.na(data$RT_clean))/length(data$RT.of.stimulus)
percentage_from_RT

#part g
data$tooFast = ifelse(data$RT.of.stimulus < 300,1,0)
fast_perc=ddply(data, "Participant..", summarise,
                fast_perc=mean(tooFast, na.rm=TRUE))

#gart h
data_to_analyze =ddply(data, c("Participant", "Block"), summarise,
                       usable_N=sum(!is.na(RT_clean)), 
                       mean_rt = mean(RT_clean, na.rm=TRUE), 
                       var_rt=((usable_N - 1)*var(RT_clean, na.rm = TRUE)/ usable_N))

#part i


#part j
