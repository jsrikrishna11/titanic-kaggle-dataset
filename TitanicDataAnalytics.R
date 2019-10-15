setwd("C:/Users/purna/Desktop/Coding/R/Datasets/titanic")
test = read.csv("test.csv")
train = read.csv("train.csv")

test.survived = data.frame( Survived = rep("None", nrow(test)), test[,])

data.combined = rbind( train,test.survived)
data.combined$Survived = as.factor(data.combined$Survived)
data.combined$Pclass = as.factor(data.combined$Pclass)

table(data.combined$Pclass)
library(ggplot2)

# Hypothesis - Rich survived at a higher rate than poor
train$Pclass <- as.factor(train$Pclass)

#first argument is the variable
#"aes" set some aesthetic mappings
#
ggplot(train, aes(x = factor(Pclass), fill= factor(Survived)) )+
geom_bar()+
xlab("Pclass")+
ylab("Total Count")+
labs(fill = "Survived")
