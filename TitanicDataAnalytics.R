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
  else if(length(grep("Master.", name))>0){
    return("Master.")
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

#What's the distribution of males to females in the entire dataset?
table(data.combined$Sex)

#Visualize the 3-way relationship between sex, pclass and survival rate?
ggplot(data.combined[1:891,], aes(x=Sex, fill=Survived))+
  geom_bar(width = 0.75)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Sex")+
  ylab("Total Count")+
  labs(fill = "Survived")

#Age and sex seem very important for survival. Lets look at age distribution of
#age over the entire dataset

summary(data.combined$Age)
#Output and Other related important notes
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.17   21.00   28.00   29.88   39.00   80.00     263 
#"NA" are roughly 20% of the observations!!!
#there are two ways to deal with this:
# 1. either we replace the "NA" with the median value (inferring the value)
# or
# 2. imputation: we create a predictive model to predict what the value in that 
# observation should be.
# 3. try to find a proxy for that variable

#:Lets just see for training data
summary(data.combined[1:891,]$Age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.42   20.12   28.00   29.70   38.00   80.00     177

#lets just check survival rate broken out by sex and pclass
ggplot(data.combined[1:891,], aes(x=Age, fill=Survived))+
  facet_wrap(~Sex + Pclass)+
  geom_histogram(bins = 10)+
  xlab("Age")+
  ylab("Total Count")+
  labs(fill="Survived")

#Validate that "Master." is a good proxy for male children?
boys <- data.combined[which(data.combined$title =="Master."),]
summary(boys$Age)

#Output
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.330   2.000   4.000   5.483   9.000  14.500       8 
#Master is a resonable proxy

#Lets try for "Miss."
girls <- data.combined[which(data.combined$title=="Miss."),]
summary(girls$Age)

#OUTPUT
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.17   15.00   22.00   21.77   30.00   63.00      50 
#"Miss." IS MORE COMPLICATED!!!
#most records i.e. almost 75% of the records tend to be towards
#adult end of the spectrum

#lets just get a ggplot
ggplot(girls[which(girls$Survived!="None" & !is.na(girls$Age)),], aes(x=Age, fill = Survived))+
  geom_bar(stat = "count", width = 5.0)+
  facet_wrap(~Pclass)
  xlab("Age")+
  ylab("Total Count")+
  labs(fill="Survived")
  
#It appears that alone misses are more likely to die?
#lets just confirm that?
girls.alone = girls[which(girls$SibSp==0 & girls$Parch==0),]
summary(girls.alone$Age)

ggplot(girls.alone[which(girls.alone$Age>=14.5),], aes(x=Age))+
  geom_histogram(stat="count")+
  xlab("Age")+
  ylab("Total Count")+
  ggtitle("Age distribution among alone women")

#lets try to get a summary of SibSp variable?
summary(girls$SibSp)
# #output
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  0.0000  0.6654  1.0000  8.0000 
#But this doesn't make any sense as there can't be 0.6654 siblings?
#So, should we treat it as a categorical data? I think we should!
length(unique(data.combined$SibSp))
data.combined$SibSp = as.factor(data.combined$SibSp)

summary(data.combined$SibSp[1:891])

ggplot(data.combined[1:891,], aes(x=SibSp , fill=Survived))+
  geom_bar(width = 1, stat="count")+
  facet_wrap(~Pclass + title)+
  xlab("SibSp")+
  ylab("Total Count")+
  labs(fill="Survived")

#do the same thing for parch
data.combined$Parch = as.factor(data.combined$Parch)

summary(data.combined$Parch[1:891])

ggplot(data.combined[1:891,], aes(x=Parch , fill=Survived))+
  geom_bar(width = 1, stat="count")+
  facet_wrap(~Pclass + title)+
  xlab("Parch")+
  ylab("Total Count")+
  labs(fill="Survived")

