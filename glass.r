library(class)
library(lattice)
library(gmodels)
library(mlbench)
library(pROC)
library(caret)
glass <- read.csv(file.choose())
View(glass)
summary(glass)
table(glass$Type)
barplot(table(glass$Type),col='forestgreen',xlab="Type of glass",ylab='Count')
glass$type = as.factor(glass$Type)

round(prop.table(table(glass$Type))*100,1)
str(glass)

#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
norm(c(1,2,3,4,5))
#Apply the normalization function to glass dataset
glass_n<- as.data.frame(lapply(glass[1:9], norm))
View(glass_n)
summary(glass_n[c("RI","Na","Mg")])

View(glass_n)

#create training and test datasets
set.seed(123)
ind <- sample(2, nrow(glass_n), replace = TRUE, prob = c(0.7,0.3))
glass_train <- glass_n[ind==1,]
glass_test <-  glass_n[ind==2,]

#Get labels for training and test datasets
set.seed(123)
ind1 <- sample(2, nrow(glass), replace = TRUE, prob = c(0.7,0.3))
glass_train_labels <- glass[ind1==1,10]
glass_test_labels <-  glass[ind1==2,10]

# Build a KNN model on taining dataset
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model

glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=3)
table(glass_test_pred,glass_test_labels)

mean(glass_test_pred==glass_test_labels) # 0.6842105

CrossTable(x=glass_test_labels,y=glass_test_pred,prop.chisq = FALSE)

# we have to test on test dataset
test_1<- NULL
train_1 <- NULL
for (i in seq(3,200,2))
{
  train_glass_pred <- knn(train=glass_train,test=glass_train,cl=glass_train_labels,k=i)
  train_1 <- c(train_1,mean(train_glass_pred==glass_train_labels))
  test_glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=i)
  test_1 <- c(test_1,mean(test_glass_pred==glass_test_labels))
}

# Testing Accuracy 

# Plotting 2 different graphs on same window
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(3,200,2),train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(3,200,2),test_acc,type="l",main="Test_accuracy",col="red")

acc_neigh_df <- data.frame(list(train_1=train_1,test_1=test_1,neigh=seq(3,200,2)))
# Plotting 2 different graphs on same co-ordinate axis
install.packages("ggplot2")
library(ggplot2)
ggplot(acc_neigh_df,aes(x=neigh))+
  geom_line(aes(y=train_1,colour="train_1"),lwd=1.5)+
  geom_line(aes(y=test_1,colour="test_1"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_1","test_1"),values = c("train_1"="green","test_1"="red"))


glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=21)
