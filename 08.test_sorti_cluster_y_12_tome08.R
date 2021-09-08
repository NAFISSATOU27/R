### test sur les gens qui partent c'est à dire les moins de 38 ans


str(base_Y12)
summary(base_Y12)

## filtre sur les les jeunes 
y_12_cluster=base_Y12%>%
  filter(AGE=="21-30 age"  | AGE=="31-38 age")


## validation vs apprentissage

set.seed(123)
trainingRowIndex=sample(1:nrow(y_12_cluster),0.7*nrow(y_12_cluster))
training12=y_12_cluster[trainingRowIndex,]  ## base apprentissage
validation12=y_12_cluster[-trainingRowIndex,] ## base validation
table(y_12_cluster$Y_12)
table(validation12$Y_12)
table(training12$Y_12)

## modele logit
logit12=glm(Y_12~.
            , data=training12,family=binomial(link="logit"))
logit12
summary(logit12)



# les variables les plus significaif

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

##
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

## multiple roc curve

x=roc.curve(validation12$Y_12, pred.random12,col="blue")
y=roc.curve(validation12$Y_12, predicted = pred.logit12,col="red",add=TRUE)
Z=roc.curve(validation12$Y_12, pred.gbm12,col="green",add=TRUE)
##legend(lengend=c("RANDOM FOREST","regression logistique","XGBOOST"),col=c("blue","red","green"))



legend("topleft", c("RANDOM FOREST","regression logistique","XGBOOST"), 
       col=1:3, lty=1:2, lwd=2)


