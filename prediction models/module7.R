library(xlsx)
library(party)
library(rpart)
library(rpart.plot)
library(pROC)
library(C50)
library(gmodels)
a1 <- read.xlsx("C:/Users/HuaLi/Desktop/¿Î³Ì/module7/Area1.xlsm", sheetIndex = 4, header = TRUE)
#a1 is data from experiment area 1
a2 <- read.xlsx("C:/Users/HuaLi/Desktop/¿Î³Ì/module7/Area2.xlsm", sheetIndex = 4, header = TRUE)
#a2 is data from experiment area 2

a3 <- rbind(a1,a2)  # combine two areas 
a4 <- a3[sample(1:nrow(a3),nrow(a3),replace = F),]  # randomly disrupt the rank of data in order to split it into train and test set

a4$Ash[which(a4$Ash=="A2")] <- "A1"
a4$Ash[which(a4$Ash=="A3")] <- "A1"
a4$Ash[which(a4$Ash=="A4")] <- "A1"
a4$Ash[which(a4$Ash=="A8")] <- "A7" # class A1 for disease level "A1-A4", class A7 for disease level "A7-A8"

a_train <- a4[1:300,]
a_test <- a4[301:405,]  # divide it into train set and test set 

# id3 decision tree method
tree_id3 <- rpart(Ash ~ . , data = a_train , method = "class" , parms = list(split="information"))
printcp(tree_id3) # show properties of this model 
tree_id3_prune <- prune(tree_id3,cp=0.001000) # cut tree 
rpart.plot(tree_id3_prune,branch=1,fallen.leaves=T,cex=0.6) # plot tree 
pre_id3 <- predict(tree_id3_prune,data=a_test,type="class") # predict the test set 
obs_id3 <- data.frame(prob=pre_id3[1:105],obs=a_test$Ash)
table(a_test$Ash,pre_id3[1:105],dnn=c("true","predict"))  # show the cross reference table of prediction result

roc_id3 <- multiclass.roc(a_test$Ash,as.numeric(pre_id3[1:105]))  # roc plot 
roc_id3$auc     # show the auc proportion 


train <- a4[1:300,-1]
test <- a4[301:405,-1]
train.label <- a4[1:300,1]
test.label <- a4[301:405,1]
train <- as.matrix(train)    # separate the ash tree column as label to fit the input data style of other methods 
#C50  method
m <- C5.0(train,train.label,trials = 10)
pred <- predict(m,test,type="class") 

#comfusion matrix 
CrossTable(pred,test.label,prop.r = F,prop.t = F,prop.chisq = F)

obs_c50 <- data.frame(prob=pred[1:105],obs=test.label)
table(test.label,pred[1:105],dnn=c("true","predict"))
# roc plot and auc proportion
roc_c50 <- multiclass.roc(test.label,as.numeric(pred[1:105]))
roc_c50$auc

#knn methods
library(class)
a2_knn <- a4
a2_knn[is.na(a2_knn)] <- 0
a2_z <- as.data.frame(scale(a2_knn[,-1])) #z score normalize
a2_z <- a2_z[,which(colSums(a2_z)>0)]
train_z <- a2_z[1:300,]
test_z <- a2_z[301:405,]
train_z.label <- a2_knn[1:300,1]
test_z.label <- a2_knn[301:405,1]

#kNN
pred <- knn(train_z,test_z,train_z.label,k=10)

#comfusion matrix 
CrossTable(pred,test_z.label,prop.r = F,prop.t = F,prop.chisq = F)
# roc plot and auc proportion
roc_knn <- multiclass.roc(test_z.label,as.numeric(pred[1:105]))
roc_knn$auc

#1R rules association 
library(RWeka)

train_1r <- a2_knn[1:300,]
test_1r <- test
test_1r[is.na(test_1r)] <- 0
m <- OneR(Ash ~ .,data=train_1r)
pred <- predict(m,test_1r)
#comfusion matrix 
CrossTable(pred,test.label,prop.r = F,prop.t = F,prop.chisq = F)
m

#JRip rules association 
m <- JRip(Ash ~ .,data=train_1r)
pred <- predict(m,test_1r)
#comfusion matrix 
CrossTable(pred,test.label,prop.r = F,prop.t = F,prop.chisq = F)
m

#glmnet linear regrression

library(glmnet)

a2_knn <- a2_knn[sample(1:nrow(a2_knn),nrow(a2_knn),replace = F),]  #repeat from here to get different glm model
train_l <- a2_knn[1:300,-1]
test_l <- a2_knn[301:405,-1]
train_value <- as.numeric(a2_knn[1:300,1])
test_value <- as.numeric(a2_knn[301:405,1])

m_lasso <- glmnet(as.matrix(train_l),train_value,family = "gaussian")

plot(data.frame(df=m_lasso$df,dev.ratio=m_lasso$dev.ratio),type="b",cex=0.6)
data.frame(df=m_lasso$df,dev.ratio=m_lasso$dev.ratio, m_lasso$lambda)
coef(m_lasso, s=0.001134363)  #min df

pred <- predict(m_lasso,newx=as.matrix(test_l),s=0.001134363)
summary(pred)
summary(test_value)
cor(test_value,pred)
MAE <- mean(abs(pred - test_value))
mean(abs(mean(train_value) - test_value))


# simple rpart decision tree

train_tree <- a_train
test_tree <- a_test[,-1]
test_tree_value <- a_test[,1]
m.rpart <- rpart(Ash~.,data = train_tree)
summary(m.rpart)
rpart.plot(m.rpart)
pred <- predict(m.rpart,test_tree)
cor(as.numeric(test_tree_value),pred)
test_tree_value <- as.numeric(test_tree_value)
mean(abs(pred - test_tree_value)) #rpart MAE
mean(abs(mean(as.numeric(train_tree$Ash)) -  test_tree_value)) #mean MAE


