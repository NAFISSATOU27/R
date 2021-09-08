#### rest avec y_9 et y_12

library(readxl)
library(gdata)
library(lubridate)# data 
library(tidyverse)
library(radiant)
library(dplyr)# calcul agrégation
library(openxlsx)
library(questionr)
library(Hmisc)
library(forcats)
library(FactoMineR)
library(Factoshiny)
library(funModeling)
library(questionr)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(visreg)
library(caret)
library(ROSE)
library(pROC)

##etude avec Y_9
summary(base_Y9)


logit
## validation vs apprentissage

set.seed(123)
trainingRowIndex9=sample(1:nrow(base_Y9),0.7*nrow(base_Y9))
training9=base_Y9[trainingRowIndex9,]  ## base apprentissage POUR Y_9
validation9=base_Y9[-trainingRowIndex9,] ## base validation POUR Y_9
table(base_Y9$Y_9)
table(validation9$Y_9)
table(training9$Y_9)


######################################""" nouveau logit
## modele logit
logit9=glm(Y_9~.
          , data=training9,family=binomial(link="logit"))
logit9
summary(logit9)



# les varaibles les plus significaif

best_logit9 = step(logit9)
summary(best_logit9)
formula(best_logit9)
logit9
best_logit9


## prevision
pred.logit9=predict(best_logit9, newdata=validation9, type="response")
pred.logit9

analyse_resultats_glm9 = data.frame(0,0,0,0,0,0,0,0)

names(analyse_resultats_glm9)= c("Cut","0_pred - 0_reel","0_pred - 1_reel","1_pred - 0_reel","1_pred - 1_reel",
                                "taux_success_total","taux_1_predits","taux_0_predits")
analyse_resultats_glm9


for (i in seq(0.01,0.5 , by=0.01)){
  result_logit9=ifelse(pred.logit9>i,"1","0")
  
  xtab=table(result_logit9,validation9$Y_9)
  
  confusion_matr = confusionMatrix(xtab)
  
  taux_sucess_total= (xtab[1,1]+xtab[2,2])/sum(xtab)
  taux_1_predits = xtab[2,2]/sum(xtab[,2])
  taux_0_predits = xtab[1,1]/sum(xtab[,1])
  
  
  resultats_i = c(i, xtab[1,1],xtab[1,2],xtab[2,1],xtab[2,2],taux_sucess_total,taux_1_predits,taux_0_predits)
  
  analyse_resultats_glm9 = rbind(analyse_resultats_glm9,resultats_i )
  
} 

analyse_resultats_glm9 = analyse_resultats_glm9[-1,]
analyse_resultats_glm9

result_logit9=ifelse(pred.logit9>0.15,"1","0")
pred.logit9
result_logit9

xtab=table(result_logit9,validation9$Y_9)
confusionMatrix(xtab)


# aire sous la courbe ROC
install.packages("pROC")
library(pROC)
?roc.curve
roc.curve(validation9$Y_9, predicted = pred.logit9)#####################AUC=0.84


## mod?le probit

probit9=glm(Y_9~., data=training9,family=binomial(link="probit"))

summary(probit9)
##
best_probit9 = step(probit9)
summary(best_probit9)
formula(best_probit9)
## prediction
pred.probit9=predict(best_probit9, newdata=validation9, type="response")
pred.probit9


analyse_resultats_probit9 = data.frame(0,0,0,0,0,0,0,0)

names(analyse_resultats_probit9)= c("Cut","0_pred - 0_reel","0_pred - 1_reel","1_pred - 0_reel","1_pred - 1_reel",
                                   "taux_sucess_total","taux_1_predits","taux_0_predits")
analyse_resultats_probit9


for (i in seq(0.01,0.4 , by=0.01)){
  result_probit9=ifelse(pred.probit9>i,"1","0")
  
  xtab=table(result_probit9,validation9$Y_9)
  
  confusion_matr = confusionMatrix(xtab)
  
  taux_sucess_total = (xtab[1,1]+xtab[2,2])/sum(xtab)
  taux_1_predits = xtab[2,2]/sum(xtab[,2])
  taux_0_predits = xtab[1,1]/sum(xtab[,1])
  
  
  resultats_i = c(i, xtab[1,1],xtab[1,2],xtab[2,1],xtab[2,2],taux_sucess_total,taux_1_predits,taux_0_predits)
  
  analyse_resultats_probit9 = rbind(analyse_resultats_probit9,resultats_i )
  
}

analyse_resultats_probit9 = analyse_resultats_probit9[-1,]
analyse_resultats_probit9

result_probit9=ifelse(pred.probit9>0.04,"1","0")
pred.probit9
result_probit9



# aire sous la courbe ROC
roc.curve(validation9$Y_9, predicted = pred.probit9)#####################AUC=0.84
auc(validation9$Y_9, pred.probit9, quiet=TRUE)###################
###################################################
## logit
logit9=glm(Y_9~.
          , data=training9,family=binomial(link="logit"))
logit9
summary(logit9)

# les varaibles les plus significatif
test9 = step(logit9)
summary(test9)
formula(test9)

## prevision
pred.logit9=predict(logit9, newdata=validation9, type="response")
## mettre en binaire

result_logit9=ifelse(pred.logit9>0.05,"1","0")

## matrice de confusion
xtab9=table(result_logit9,validation9$Y_9)
confusionMatrix(xtab9)

## tauxd'erreur de 8+219/825=0.28
# aire sous la courbe ROC
roc.curve(validation9$Y_9, pred.logit9)#####################AUC=0.837


#### XGBOOST
library(Matrix)
library(xgboost)
library(dplyr)
library(magrittr)

set.seed(123)
# creAte matrix


trainingM9=model.matrix(Y_9 ~ .,data=training9)

validationM9=model.matrix(Y_9 ~ .,data=validation9)
length(validationM)

train_matrix9=xgb.DMatrix(data=as.matrix(trainingM9),label=as.numeric(training9$Y_9)-1)
valid_matrix9=xgb.DMatrix(data=as.matrix(trainingM9),label=as.numeric(training9$Y_9)-1)

params=list(set.seed=123,
            eval_metric="auc",
            objective="binary:logistic")

gdbt9 <- xgb.train(params=params, data=train_matrix9,
                  nrounds=20, num_parallel_tree =1000, nthread = 1, verbose=1)
pred.gbm9 <- predict(gdbt9, newdata=validationM9)




analyse_resultats_gbm9 = data.frame(0,0,0,0,0,0,0,0)

names(analyse_resultats_gbm9)= c("Cut","0_pred - 0_reel","0_pred - 1_reel","1_pred - 0_reel","1_pred - 1_reel",
                                    "taux_sucess_total","taux_1_predits","taux_0_predits")
analyse_resultats_gbm9


for (i in seq(0.01,0.4 , by=0.01)){
  result_gbm9=ifelse(pred.gbm9>i,"1","0")
  
  xtab=table(result_gbm9,validation9$Y_9)
  
  confusion_matr = confusionMatrix(xtab)
  
  taux_sucess_total = (xtab[1,1]+xtab[2,2])/sum(xtab)
  taux_1_predits = xtab[2,2]/sum(xtab[,2])
  taux_0_predits = xtab[1,1]/sum(xtab[,1])
  
  
  resultats_i = c(i, xtab[1,1],xtab[1,2],xtab[2,1],xtab[2,2],taux_sucess_total,taux_1_predits,taux_0_predits)
  
  analyse_resultats_gbm9 = rbind(analyse_resultats_gbm9,resultats_i )
  
}

analyse_resultats_gbm9 = analyse_resultats_gbm9[-1,]
analyse_resultats_gbm9

result_gbm9=ifelse(pred.gbm9>0.15,"1","0")


## matrice de confusion
xtab9=table(result_gbm9,validation9$Y_9)## ACCURACY=0.70
confusionMatrix(xtab9)
roc.curve(validation9$Y_9, pred.gbm9)### AUC=0.805

## random
## random forest
install.packages("randomForest")
library(randomForest)
RFM9=randomForest(Y_9~.,data=training9,importance=TRUE, proximity=TRUE,
                  ntree=45, mtry=9)

##???
plot(RFM9$err.rate[, 1], type = "l", xlab = "nombre d'arbres", ylab = "erreur OOB")

varImpPlot(RFM9)

plot(RFM9)
print(RFM9)
summary(RFM9)

RFM9=randomForest(Y_9~.,data=training9)

## prediction
pred.random9=predict(RFM9,newdata=validation9)

## matrice de confusion
xtab9=table(pred.random9,validation9$Y_9)
confusionMatrix(xtab9)

roc.curve(validation9$Y_9, pred.random9)

# taux d'error 
plot(RFM9)
RFM9$err.rate #donne l'erreur Out Of Bag de la for?t
barplot(order(importance(RFM9),decreasing=TRUE)) ##donne l'ordre
##d'importance des variables explicatives


