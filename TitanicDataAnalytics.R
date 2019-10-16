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

#examine first few names in training data set
head(as.character(data.combined$Name))

#unique names are there across train and tests
length(unique(as.character(data.combined$Name)))

#two duplicate names, let's get them!
dup.names = as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])

library(stringr)

# Do Miss, Mrs. have any correlation with other variables

misses = data.combined[which(str_detect(data.combined$Name, "Miss.")),]
View(misses[1:5,1:5])

# For Mrs.

mrs = data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
View(mrs[1:5,])

table(mrs$Survived)

#Lets check for married and unmarried males
master = data.combined[which(str_detect(data.combined$Name, "Master")),]
View(master[1:5,])

table(master$Survived)


#Since we found some relationship with Pclass and survival rate and Title and survival rate, lets create a 
#title variable in the data.frame so that we can explore a potential 3-D relationship between theses three
#variables, i.e. Pclass, Title and Survival Rate

#Utility function called extract title
extractTitle <- function(name){
  name = as.character(name)
  if(length(grep("Miss.", name))>0){
    return("Miss.")
  }
  else if(length(grep("Mrs.", name))>0){
    return("Mrs.")
  }
  else if(length(grep("Master", name))>0){
    return("Master")
  }
  else if(length(grep("Mr.", name))>0){
    return("Mr.")
  }
  else{
    return("Other")
  }
}

titles <- NULL
for(i in 1:nrow(data.combined)){
  titles <- c(titles, extractTitle(data.combined$Name[i]))
}

#We have to change it into a factor and then add the column
data.combined$title <- as.factor(titles)
ggplot(data.combined[1:891,], aes(x=title, fill=Survived))+
  geom_bar(width = 0.5)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Title")+
  ylab("Total Count")+
  labs(fill = "Survived")