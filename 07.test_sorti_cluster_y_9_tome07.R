### test sur les gens qui partent c'est ? dire les moins de 38 ans


str(base_Y9)
summary(base_Y9)

## filtre sur les les jeunes 
y_9_cluster=base_Y9%>%
  filter(AGE=="21-30 age"  | AGE=="31-38 age")
  

## validation vs apprentissage

set.seed(123)
trainingRowIndex=sample(1:nrow(y_9_cluster),0.7*nrow(y_9_cluster))
training9=y_9_cluster[trainingRowIndex,]  ## base apprentissage
validation9=y_9_cluster[-trainingRowIndex,] ## base validation
table(y_9_cluster$Y_9)
table(validation9$Y_9)
table(training9$Y_9)


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


## random forest
install.packages("randomForest")
library(randomForest)
RFM9=randomForest(Y_9~.,data=training9)##
## prediction
pred.random9=predict(RFM9,newdata=validation9)

## matrice de confusion
xtab9=table(pred.random9,validation9$Y_9)
confusionMatrix(xtab9)

roc.curve(validation9$Y_9, pred.random9)



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

###
analyse_resultats_xgboost9 = data.frame(0,0,0,0,0,0,0,0)

names(analyse_resultats_xgboost9)= c("Cut","0_pred - 0_reel","0_pred - 1_reel","1_pred - 0_reel","1_pred - 1_reel",
                                     "taux_sucess_total","taux_1_predits","taux_0_predits")
analyse_resultats_xgboost9


for (i in seq(0.01,0.4 , by=0.01)){
  result_gbm9=ifelse(pred.gbm9>i,"1","0")
  
  xtab=table(result_gbm9,validation9$Y_9)
  
  confusion_matr = confusionMatrix(xtab)
  
  taux_sucess_total = (xtab[1,1]+xtab[2,2])/sum(xtab)
  taux_1_predits = xtab[2,2]/sum(xtab[,2])
  taux_0_predits = xtab[1,1]/sum(xtab[,1])
  
  
  resultats_i = c(i, xtab[1,1],xtab[1,2],xtab[2,1],xtab[2,2],taux_sucess_total,taux_1_predits,taux_0_predits)
  
  analyse_resultats_xgboost9 = rbind(analyse_resultats_xgboost9,resultats_i )
  
}

analyse_resultats_xgboost9 = analyse_resultats_xgboost9[-1,]
analyse_resultats_xgboost9



result_gbm9=ifelse(pred.gbm9>0.15,"1","0")

## matrice de confusion
xtab9=table(result_gbm9,validation9$Y_9)## ACCURACY=0.70
confusionMatrix(xtab9)
roc.curve(validation9$Y_9, pred.gbm9)### AUC=0.805


## multiple roc curve

x=roc.curve(validation9$Y_9, pred.random9,col="blue")
y=roc.curve(validation9$Y_9, predicted = pred.logit9,col="red",add=TRUE)
Z=roc.curve(validation9$Y_9, pred.gbm9,col="green",add=TRUE)
##legend(lengend=c("RANDOM FOREST","regression logistique","XGBOOST"),col=c("blue","red","green"))



legend("topleft", c("RANDOM FOREST","regression logistique","XGBOOST"), 
       col=1:3, lty=1:2, lwd=2)


### ACM NOUVEAU CLUSTER

Y9_cluster_age=y_9_cluster[,-8]## j'ai enlevé l'age car je fais l'acm sur les moins de 38 ans, ils ont le meme age
outMCA_y_9_cluster=MCAshiny(Y9_cluster_age)
summary(Y9_cluster_age)

