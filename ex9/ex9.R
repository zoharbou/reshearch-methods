library(readxl)
library(plyr)
setwd("C:/Users/Dell/Desktop/school/year2/semester b/reasearch_methods/ex9/reshearch-methods/ex9")
data=data.frame(read.csv("data_proc_qIAT.csv"))
colnames(data)[c(2,4,5,12)]<-c("Participant","Block","Block.total.order","Correct.Incorrect")

# part b
count_trials = count(data$Response.ID)
most_common = sort(table(count_trials$freq),decreasing=TRUE)[1]
most_common
most_common = 248
count_trials = count_trials[count_trials$freq==most_common,]
data <- data[data$Response.ID %in% count_trials$x,]

# part c
data<-data[(data$Correct.Incorrect== "X" | data$Correct.Incorrect == "C"),]
data$Correct.Incorrect = ifelse(data$Correct.Incorrect == "X",1,0)
error_perc=ddply(data, "Participant", summarise,
                 error_perc=mean(Correct.Incorrect, na.rm=TRUE))


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
fast_perc=ddply(data, "Participant", summarise,
                fast_perc=mean(tooFast, na.rm=TRUE))

#gart h
data_to_analyze =ddply(data, c("Participant", "Block"), summarise,
                       usable_N=sum(!is.na(RT_clean)), 
                       mean_rt = mean(RT_clean, na.rm=TRUE), 
                       var_rt=((usable_N - 1)*var(RT_clean, na.rm = TRUE)/ usable_N))
data_to_analyze
#part i
data$Block
data_4_7 = data_to_analyze[(data_to_analyze$Block == 4 | data_to_analyze$Block == 7),]
data_5_8 = data_to_analyze[(data_to_analyze$Block == 5 | data_to_analyze$Block == 8),]
pooled_SD_4_7 = ddply(data_4_7, "Participant", summarise, pooled_sd = sqrt((sum(var_rt*usable_N)) / (sum(usable_N)-2)))
pooled_SD_5_8 = ddply(data_5_8, "Participant", summarise, pooled_sd = sqrt((sum(var_rt*usable_N)) / (sum(usable_N)-2)))

#part j
pooled_SD_5_8$IAT5_8 = (data_to_analyze$mean_rt[data_to_analyze$Block==8]-data_to_analyze$mean_rt[data_to_analyze$Block==5])/pooled_SD_5_8$pooled_sd
pooled_SD_4_7$IAT4_7 = (data_to_analyze$mean_rt[data_to_analyze$Block==7]-data_to_analyze$mean_rt[data_to_analyze$Block==4])/pooled_SD_4_7$pooled_sd

#part k
qIATscore_uncorrected = (pooled_SD_5_8$IAT5_8+pooled_SD_4_7$IAT4_7)/2
qIATscore_uncorrected

#parl l
data_agg_subjects = data.frame(error_perc,fast_perc$fast_perc,qIATscore_uncorrected)
colnames(data_agg_subjects)[3]<-"fast_perc"
data_agg_subjects

#part m
data_agg_subjects$condition = ddply(data, "Participant", summarise, cond = mean(as.integer(condition)))$cond
View(data_agg_subjects)

#part n
data_agg_subjects$qIATscore_corrected = data_agg_subjects$qIATscore_uncorrected
data_agg_subjects$qIATscore_corrected[data_agg_subjects$condition==1] = - data_agg_subjects$qIATscore_corrected[data_agg_subjects$condition==1]
View(data_agg_subjects)

