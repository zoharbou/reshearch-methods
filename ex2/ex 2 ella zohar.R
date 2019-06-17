#install.packages("ggplot2") 
library("ggplot2")
pop = load(file = "C:/Users/Dell/Desktop/school/year2/semester b/reasearch_methods/population_data.R")

### question 1, a
E = mean(population)
SD = sd(population)

ggplot(data=data.frame(population), aes(population)) +
  geom_histogram(binwidth = 30) +
  xlab("grades")+
  ylab("Frequncy")+
  ggtitle("the grades distribution of the population")
print(E)
print(SD)

### question 1, b
# for (n in c(3,30,1000)){
#   for(i in 1:3){
#     sample_of_pop = sample(population,n)
#     E = mean(sample_of_pop)
#     SD = sd(sample_of_pop)
#     # title = cat("the grades distribution of the sample", n, "test num: ", i)
#     print(ggplot(data=data.frame(sample_of_pop), aes(sample_of_pop)) +
#       geom_histogram(binwidth = 30) +
#       xlab("grades")+
#       ylab("Frequncy")+
#       ggtitle(n))
#     print(cat("N= ",n))
#     print(cat("mean= ",E))
#     print(cat("sd= ",SD))
#   }
# }
# 
# for(n in c(3,30,1000)){
#   mean_sample = matrix(0, 5000,1)
#   for(i in 0:4999){
#     sample_ = sample(population,n)
#     mean_sample[i]= mean(sample_)
#   }
#   print(ggplot(data=data.frame(mean_sample), aes(mean_sample)) +
#         geom_histogram(binwidth = 5) +
#         xlab("mean of the samples")+
#         ylab("Frequency")+
#         ggtitle(n))
#   print(cat("N= ",n))
#   print(cat("mean= ",mean(mean_sample)))
#   print(cat("sd= ",mean(mean_sample)))
# }

IQ1=c(115,95,80,73,138,122,94,105,96,81,83,132,95,90,109)
IQ2=c(108,103,78,72,125,120,92,100,91,85,84,112,97,89,128)

# # question 2 a,b,c,d
# estimatedVar = function(vec)
# {
#   vecMean = mean(vec)
#   var = 0
#   for (i in vec)
#   {
#     var = var + (i-vecMean)^2
#   }
#   return (var/(length(vec)-1))
# }
# variance = estimatedVar(IQ1)
# print("manual calculation")
# print(variance)
# print(sqrt(variance))
# print("r check:")
# print (var(IQ1))
# print(sd(IQ1))


# # questoion 2, e
z_scores = (IQ1-mean(IQ1))/sd(IQ1)
print(mean(z_scores))
print(sd(z_scores))
ggplot(data=data.frame(z_scores), aes(z_scores)) +
                geom_histogram(binwidth = 0.5) +
                xlab("scores after normalization")+
                ylab("Frequency")+
                ggtitle("")

ggplot(data=data.frame(IQ1), aes(IQ1)) +
        geom_histogram(binwidth = 10) +
        xlab("the scores by IQ1")+
        ylab("Frequency")+
        ggtitle("")

# question 5
pnorm(0.46, mean = 0.5, sd = 0.04)