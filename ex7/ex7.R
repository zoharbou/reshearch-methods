# install.packages("readxl")
library("readxl")
data_reading_table <- read_excel("C:\\Users\\Dell\\Desktop\\school\\year2\\semester b\\reasearch_methods\\ex7\\data_face.xlsx")
data_reading=data.frame(data_reading_table)
data_reading
data_reading$Subject=factor(data_reading$Subject)

# install.packages("tidyr")
library(tidyr)
data_long <- gather(data= data_reading, 
                    key=facial_expression, 
                    value=RT, 
                    ...=nutral:happy, 
                    factor_key=TRUE) #to store as a factor

data_long2=data_long[order(data_long$Subject),]


#ANOVA repeated measures
aov_repeated=aov(RT ~ facial_expression + Error(Subject/facial_expression), data = data_long2)
summary(aov_repeated)

# informative graph for the results
# install.packages("Rmisc")
library(Rmisc)
dfw = summarySEwithin(data=data_long2, 
                      measurevar="RT", 
                      withinvars="facial_expression",
                      idvar="Subject", 
                      conf.interval=.95)

library(ggplot2)
ggplot(dfw, aes(x=facial_expression, y=RT, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=RT-ci, ymax=RT+ci)) +
  geom_point(shape=21, size=3, fill="black")+
  ggtitle("Within subject design \n reaction time with CI to diffrent types of face expressions")+
  xlab("Condition: face expression")+
  ylab("Reaction time ms")+
  ylim(c(0,800))+
  theme(plot.title = element_text(hjust = 0.5))

# anova 1 way betweet subjects
anova_test=aov(RT ~ facial_expression, data=data_long2)
summary(anova_test)
