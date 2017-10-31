library(ROCR)
library(ROSE)
library(ggplot2)
library(RColorBrewer)
library(rpart)
library(rattle)
library(rpart.plot)
library(gridExtra)

require(ggplot2)
require(ROCR)
require(ROSE)
require(RColorBrewer)
require(rpart)
require(rattle)
require(rpart.plot)
require(gridExtra)


hrdata <- read.csv("HR_comma_sep.csv",stringsAsFactors = FALSE)
str(hrdata)

hrdata$left <- as.factor(hrdata$left)
hrdata$salary <- factor(hrdata$salary)
hrdata$sales <- factor(hrdata$sales)

table(hrdata$left)
prop.table(table(hrdata$left))
plot(hrdata$left)

#Before going into oversampling, lets find what all more information can we find out from the given  data.


g1 <- ggplot(hrdata,aes(sales,fill=left))+geom_bar()
g2 <- ggplot(hrdata,aes(average_montly_hours,fill=left))+geom_bar()
grid.arrange(g1,g2,nrow=2,ncol=1)


g3 <- ggplot(hrdata,aes(last_evaluation,fill=left))+geom_bar()
g4 <- ggplot(hrdata,aes(satisfaction_level,fill=left))+geom_bar()
f1<- ggplot(hrdata,aes(satisfaction_level,last_evaluation,fill=left,color=left))+geom_jitter()
grid.arrange(g3,g4,f1,nrow=2,ncol=2)


g5 <- ggplot(hrdata,aes(number_project,fill=left))+geom_bar()
g6 <- ggplot(hrdata,aes(time_spend_company,fill=left))+geom_bar()
g7 <- ggplot(hrdata,aes(promotion_last_5years,fill=left))+geom_bar()
g8 <- ggplot(hrdata,aes(salary,fill=left))+geom_bar()
grid.arrange(g5,g6,g7,g8,nrow=2,ncol=2)





over1 <- ovun.sample(left~.,data = hrdata,method = "over")$data
table(over1$left)


#creating vectors for storing values
accs <- rep(0,10)               #defines the accuracy
sensitivity1<- rep(0,10)     #sensitivity ,i.e. model predicting ‘1” 
auc <- rep(0,10)              #area under the curve ,performance of the classifier
#data will be split  from over-sampled properly shuffled data
n <- nrow(over1)
shuffled <- over1[sample(n),]
#10-fold cross validation
for (i in 1:10) {
  # These indices indicate the interval over which data is broken
  indices <- (((i-1) * round((1/10)*nrow(shuffled))) + 1):((i*round((1/10) * nrow(shuffled))))
  # Exclude them from the training set -> thrdata
  thrdata<- shuffled[-indices,]
  # Include them in the validation set-> vhrdata
  vhrdata<- shuffled[indices,]
  #logistic regression model
mylogit <- glm(left~.,data = over1,family = "binomial")
#predicting values
predlog<- predict(mylogit,vhrdata,type = "response")

model_pred_left <- rep("0",nrow(vhrdata))
#the nrow() here is of validation or test data observation
model_pred_left[predlog>0.5] <- "1"                              
#having probabilities greater than 0.5
#creating the confusion matrix
tab <- table(model_pred_left,vhrdata$left)
#finding the accuracy of the model
accs[i] <- sum(diag(tab))/sum(tab)
#sensitivity is how good they predict the 1
sensitivity1[i] <- tab[2,2]/sum(tab[2,2]+tab[1,2])
#roc curve code(checking the performance of the classifiers)
pred <- prediction(predlog,vhrdata$left)
roc <-performance(pred,"tpr","fpr")
plot(roc,colorize=T)
abline(a=0,b=1)
par(new=TRUE)
#Area under the curve ,greater the area better the performance of classifiers
jk<-performance(pred,"auc")
jk<-unlist(slot(jk,"y.values"))
auc[i]<-round(jk,2)
}
#gives the significance of the predictors
summary(mylogit)

mean(accs)
#gives the sensitivity of the model in predicting the "1" employee leaving the company
mean(sensitivity1)
#gives the performance of the classifiers
mean(auc)

mylogit<-glm(left~satisfaction_level+last_evaluation+number_project+average_montly_hours+time_spend_company+Work_accident+promotion_last_5years+salary,data =thrdata,family = "binomial")
#gives the significance of the predictors
summary(mylogit)

#gives the accuracy of the model over 10 fold cross validation
mean(accs)
#gives the sensitivity of the model in predicting the "1" employee leaving the company
mean(sensitivity1)
#gives the performance of the classifiers
mean(auc)
Lets reload the data as it is over-sampled now

hrdata <- read.csv("HR_comma_sep.csv",stringsAsFactors = FALSE)

#lets change "left","salary","sales" to factor variable
hrdata$left <- as.factor(hrdata$left)
hrdata$salary <- factor(hrdata$salary)
hrdata$sales <- factor(hrdata$sales)


set.seed(1)
#shuffling the data
n <- nrow(hrdata)
shuffled <- hrdata[sample(n),]
# we split the data into 70% training data and 30% test data.
thrdata<-shuffled[1:round(0.7*n),]
vhrdata<-shuffled[(round(0.7*n)+1):n,]
tree <- rpart(left~.,data=thrdata,method="class",parms =list(split="information"))

 # Make a prediction on the vhrdata set using the model
pred<-predict(tree,vhrdata,type="class")

confusionMatrix(pred,vhrdata$left,positive = '1')


pred<- prediction(as.numeric(pred),as.numeric(vhrdata$left))
roc <-performance(pred,"tpr","fpr")
plot(roc,colorize=T)
abline(a=0,b=1)
#wow tat is good performance
#calculating area under curve
jk<-performance(pred,"auc")
jk<-unlist(slot(jk,"y.values"))
auc<-round(jk,2)
auc


Let’s find out.

#oversampling the data
over2<- ovun.sample(left~.,data = hrdata,method = "over")$data
table(over2$left)
 accs <- rep(0,10)
 sensitivity1<- rep(0,10)
auc<-rep(0,10)
n <- nrow(over2)
shuffled <- over2[sample(n),]
for (i in 1:10) {
# These indices indicate the interval of the vhrdata set
indices <- (((i-1) * round((1/10)*nrow(shuffled))) + 1):((i*round((1/10) * nrow(shuffled))))
# Exclude them from the train set
thrdata<- shuffled[-indices,]

# Include them in the vhrdata set
 vhrdata<- shuffled[indices,]

# A model is learned using each training set
tree2<- rpart(left~.,data=thrdata,method="class",parms =list(split="information"))

# Make a prediction on the vhrdata set using the model
pred2<-predict(tree2,vhrdata,type="class")

# Assign the confusion matrix to tab(confusion matrix)
  conf<-table(vhrdata$left,pred2)

# Print out the accuracy

accs[i]<-sum(diag(conf))/sum(conf)

sensitivity1[i] <- conf[2,2]/sum(conf[2,2]+conf[1,2])
#roc curve code(checking the performance of the classifiers)
pred <- prediction(as.numeric(vhrdata$left),as.numeric(pred2))
roc <-performance(pred,"tpr","fpr")
plot(roc,colorize=T)
abline(a=0,b=1)
par(new=TRUE)
#Area under the curve ,greater the area better the performance of classifiers
jk<-performance(pred,"auc")
jk<-unlist(slot(jk,"y.values"))
auc[i]<-round(jk,2)
}

#gives the significance of the predictors
summary(tree2)


#gives the accuracy of the model over 10 fold cross validation

mean(accs)
#gives the sensitivity of the model in predicting the "1" employee leaving the company
mean(sensitivity1)

#gives the performance of the classifiers
mean(auc)

rpart.plot(tree2,extra=4)