data = data.frame(InsectSprays)

A = data[data$spray == "A",]
B = data[data$spray == "B",]
C = data[data$spray == "C",]
D = data[data$spray == "D",]
E = data[data$spray == "E",]
G = data[data$spray == "F",]

## perform t-test for 3.A
ttest = t.test(A$count, B$count, var.equal = TRUE, alternative="greater");
ttest

## perform anova-test for 3.B
anova_test = aov(count ~ spray, data=data)
summary(anova_test)

## print the means of the bug count for senity chack and 3.C:
mean(A$count)
mean(B$count)
mean(C$count)
mean(D$count)
mean(E$count)
mean(G$count)
