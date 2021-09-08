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

##etude avec Y_12
summary(base_Y12)



## validation vs apprentissage

set.seed(123)
trainingRowIndex12=sample(1:nrow(base_Y12),0.7*nrow(base_Y12))
training12=base_Y12[trainingRowIndex12,]  ## base apprentissage POUR Y_12
validation12=base_Y12[-trainingRowIndex12,] ## base validation POUR Y_12
table(base_Y12$Y_12)
table(validation12$Y_12)
table(training12$Y_12)

#################################################### nouveau logit 
## modele logit
logit12=glm(Y_12~.
           , data=training12,family=binomial(link="logit"))
logit12
summary(logit12)



# les varaibles les plus significaif

best_logit12 = step(logit12)
summary(best_logit12)
formula(best_logit12)
logit12
best_logit12


## prevision
pred.logit12=predict(best_logit12, newdata=validation12, type="response")
pred.logit12

analyse_resultats_glm12 = data.frame(0,0,0,0,0,0,0,0)

names(analyse_resultats_glm12)= c("Cut","0_pred - 0_reel","0_pred - 1_reel","1_pred - 0_reel","1_pred - 1_reel",
                                 "taux_success_total","taux_1_predits","taux_0_predits")
analyse_resultats_glm12


for (i in seq(0.01,0.5 , by=0.01)){
  result_logit12=ifelse(pred.logit12>i,"1","0")
  
  xtab=table(result_logit12,validation12$Y_12)
  
  confusion_matr = confusionMatrix(xtab)
  
  taux_sucess_total= (xtab[1,1]+xtab[2,2])/sum(xtab)
  taux_1_predits = xtab[2,2]/sum(xtab[,2])
  taux_0_predits = xtab[1,1]/sum(xtab[,1])
  
  
  resultats_i = c(i, xtab[1,1],xtab[1,2],xtab[2,1],xtab[2,2],taux_sucess_total,taux_1_predits,taux_0_predits)
  
  analyse_resultats_glm12 = rbind(analyse_resultats_glm12,resultats_i )
  
}

analyse_resultats_glm12 = analyse_resultats_glm12[-1,]
analyse_resultats_glm12

result_logit12=ifelse(pred.logit12>0.15,"1","0")
pred.logit12
result_logit12

xtab=table(result_logit12,validation12$Y_12)
confusionMatrix(xtab)


# aire sous la courbe ROC
install.packages("pROC")
library(pROC)
?roc.curve
roc.curve(validation12$Y_12, predicted = pred.logit12)#####################AUC=0.84



#### XGBOOST
library(Matrix)
library(xgboost)
library(dplyr)
library(magrittr)

set.seed(123)
# creAte matrix


trainingM12=model.matrix(Y_12 ~ .,data=training12)

validationM12=model.matrix(Y_12 ~ .,data=validation12)


train_matrix12=xgb.DMatrix(data=as.matrix(trainingM12),label=as.numeric(training12$Y_12)-1)
valid_matrix12=xgb.DMatrix(data=as.matrix(trainingM12),label=as.numeric(training12$Y_12)-1)

params=list(set.seed=123,
            eval_metric="auc",
            objective="binary:logistic")

gdbt12 <- xgb.train(params=params, data=train_matrix12,
                   nrounds=20, num_parallel_tree =1000, nthread = 1, verbose=1)
pred.gbm12 <- predict(gdbt12, newdata=validationM12)


###
analyse_resultats_xgboost12 = data.frame(0,0,0,0,0,0,0,0)

names(analyse_resultats_xgboost12)= c("Cut","0_pred - 0_reel","0_pred - 1_reel","1_pred - 0_reel","1_pred - 1_reel",
                                     "taux_sucess_total","taux_1_predits","taux_0_predits")
analyse_resultats_xgboost12


for (i in seq(0.01,0.4 , by=0.01)){
  result_gbm12=ifelse(pred.gbm12>i,"1","0")
  
  xtab=table(result_gbm12,validation12$Y_12)
  
  confusion_matr = confusionMatrix(xtab)
  
  taux_sucess_total = (xtab[1,1]+xtab[2,2])/sum(xtab)
  taux_1_predits = xtab[2,2]/sum(xtab[,2])
  taux_0_predits = xtab[1,1]/sum(xtab[,1])
  
  
  resultats_i = c(i, xtab[1,1],xtab[1,2],xtab[2,1],xtab[2,2],taux_sucess_total,taux_1_predits,taux_0_predits)
  
  analyse_resultats_xgboost12 = rbind(analyse_resultats_xgboost12,resultats_i )
  
}

analyse_resultats_xgboost12 = analyse_resultats_xgboost12[-1,]
analyse_resultats_xgboost12


result_gbm12=ifelse(pred.gbm12>0.15,"1","0")

## matrice de confusion
xtab12=table(result_gbm12,validation12$Y_12)## ACCURACY=0.63
confusionMatrix(xtab12)
roc.curve(validation12$Y_12, pred.gbm12)### AUC=0.786

## random
## random forest
install.packages("randomForest")
library(randomForest)
RFM12=randomForest(Y_12~.,data=training12)#,importance=TRUE, proximity=TRUE,
                   #ntree=100, mtry=9)## 60 et 13



## prediction
pred.random12=predict(RFM12,newdata=validation12)

## matrice de confusion
xtab12=table(pred.random12,validation12$Y_12)
confusionMatrix(xtab12)

roc.curve(validation12$Y_12, pred.random12)

