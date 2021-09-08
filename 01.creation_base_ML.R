
### programme de création de la base données pour le machine learning##########"""
##########################################################################


install.packages("gdata")## pour manipulation des outils
install.packages("tidyverse")
install.packages("lubridate")
install.packages("radiant")
install.packages("gapminder")
install.packages("FactoMineR")
install.packages("rio")
install.packages("questionr")
install.packages("forcats")
install.packages("margins")
getwd()

library(margins)
## importation des bases et transformation en date et factor
library(readxl)
library(gdata)
library(lubridate)# data 
library(tidyverse)
library(radiant)
library(dplyr)# calcul agrégation
library(openxlsx)
library(questionr)
library(Hmisc)
library(forcats)## por renommer la modalité
library(rio)

d_arrete="2018-12-31" ## par exemple
d_arrete=as.Date(d_arrete)
d_arrete
class(d_arrete)



## CR2ATION DE LA DATE D'arret?


## pour voir quelle est le format date du syst?me
##today=Sys.Date()
##today
##now("Europe/Paris")
##OlsonNames()
##format(today,"%d/%m/%Y")

#####################################base fixe
b_fixe=read_excel("base_fixe.xlsx")
b_fixe$DATE_NAIS=as.Date(b_fixe$DATE_NAIS,"%d/%m/%Y") 
b_fixe$PRESENT_AU=as.Date(b_fixe$PRESENT_AU,"%d/%m/%Y")
b_fixe$D_ENTR_GPR=as.Date(b_fixe$D_ENTR_GPR,"%d/%m/%Y")
b_fixe$SEXE=as.factor(b_fixe$SEXE)
str(b_fixe)
## FILTRE SUR PR2SENT AU 31/12/2018
summary(b_fixe)
b_fixe=b_fixe %>%
  filter(PRESENT_AU==d_arrete)
# compter le nombre de valeurs unique
length(unique(b_fixe$MATRICULE))
# variable Age à créer
b_fixe$Age=floor(time_length(interval(start=b_fixe$DATE_NAIS,end=d_arrete),unit="years"))



# variable ancienne groupe à créer
b_fixe$ANC_GPR=floor(time_length(interval(start=b_fixe$D_ENTR_GPR,end=d_arrete),unit="years"))
summary(b_fixe)


b_fixe_f=b_fixe%>%
  select(MATRICULE,Age,ANC_GPR,SEXE,)

any(is.na(b_fixe_f))

#################################################base emploi
b_emploi=read_excel("base_emploi.xlsx")
str(b_emploi)
b_emploi$D_DEB_POS_PROF=as.Date(b_emploi$D_DEB_POS_PROF,"%d/%m/%Y")
b_emploi$PRESENT_AU=as.Date(b_emploi$PRESENT_AU,"%d/%m/%Y")
b_emploi$C_CLASSIF=as.factor(b_emploi$C_CLASSIF)
b_emploi$CODE_EMPLOI=as.factor(b_emploi$CODE_EMPLOI)
b_emploi$CADRE=as.factor(b_emploi$CADRE)
b_emploi$C_FILIERE=as.factor(b_emploi$C_FILIERE)
str(b_emploi)


## FILTRE SUR LA DATE d'arrété AU 31/12/2018 et select des variables qui m'interessent
b_emploi=b_emploi %>%
  filter(D_DEB_POS_PROF<=d_arrete)%>%
    filter(PRESENT_AU==d_arrete)%>%
  rename(c_empl=CODE_EMPLOI)%>%
  select(MATRICULE,C_FILIERE,CADRE)

length(unique(b_emploi$MATRICULE))

any(is.na(b_emploi))

######"variable anciennté emploi à créer ,la base est pris dans la vu que Sandra a cr?er pour calculer les anciennet?
b_anc_empl=read_excel("base_anc_empl.xlsx")
str(b_anc_empl)
b_anc_empl$DEB=as.Date(b_anc_empl$DEB,"%d/%m/%Y")
b_anc_empl$FIN=as.Date(b_anc_empl$FIN,"%d/%m/%Y")
b_anc_empl$PRESENT_AU=as.Date(b_anc_empl$PRESENT_AU,"%d/%m/%Y")


## j'ai filtre sur la date d'arreté
b_anc_empl=b_anc_empl%>%
  filter(PRESENT_AU==d_arrete)%>%
  filter(DEB<=d_arrete)
  
 

length(unique(b_anc_empl$MATRICULE))
b_anc_empl$ANC_EMPL=time_length(interval(start=b_anc_empl$DEB,end=d_arrete),unit="years")
View(b_anc_empl)
summary(b_anc_empl)

## selection des varaibles qui m'int?resse

b_anc_empl=b_anc_empl%>%
  select(MATRICULE,ANC_EMPL)

any(is.na(b_anc_empl))

########################################################## PROMOTION
base_promotion=read_excel("base_promotion.xlsx")
str(base_promotion)
base_promotion$DEBUT=as.Date(base_promotion$DEBUT,"%d/%m/%Y")
base_promotion$PROMOTION=as.factor(base_promotion$PROMOTION)
str(base_promotion)
## FILTRE SUR LA DATE d'arrété AU 31/12/2018
base_promotion=base_promotion %>%
  filter(DEBUT<=d_arrete)
#le nbre de promo j'ai j'ai que 75 % de la poulation avait 1 promotion, que queLques qui ont plus, j'ai d?cid? de l'ai laiss? en facteur


#le nbre de promot
b_promotion=base_promotion%>%
  group_by(MATRICULE)%>%
  count(PROMOTION)
length(unique(b_promotion$MATRICULE))


b_promotion=b_promotion%>%
  rename(nbre_promot=n)

b_promotion=b_promotion[,c("MATRICULE","PROMOTION")]




any(is.na(b_promotion))### pour voir s'il des valeurs manquantes




############################################################# Nomination
base_nominat=read_excel("base_nomination.xlsx")
base_nominat$DEBUT=as.Date(base_nominat$DEBUT,"%d/%m/%Y")
base_nominat$NOMINATION=as.factor(base_nominat$NOMINATION)
str(base_nominat)
## FILTRE SUR LA DATE d'arrété AU 31/12/2018
base_nominat=base_nominat%>%
  filter(DEBUT<=d_arrete)

b_nomination=base_nominat%>%
  group_by(MATRICULE)%>%
  count(NOMINATION)
length(unique(b_nomination$MATRICULE))

b_nomination=b_nomination%>%
  rename(nbre_nomination=n)

b_nomination=b_nomination[,c("MATRICULE","nbre_nomination")]
b_nomination$NOMINATION='OUI'



b_nomination=b_nomination%>%
  select(MATRICULE,NOMINATION)

str(b_nomination)

b_nomination$NOMINATION=as.factor(b_nomination$NOMINATION)
summary(b_nomination)
any(is.na(b_nomination))## l'existence de valeurs manquantes




###################################################baisse classification
base_baisse_clast=read_excel("base_bais_clas.xlsx")
base_baisse_clast$DEBUT=as.Date(base_baisse_clast$DEBUT,"%d/%m/%Y")
base_baisse_clast$BAISSE_CLASSIFICATION=as.factor(base_baisse_clast$BAISSE_CLASSIFICATION)
str(base_baisse_clast)
## FILTRE SUR LA DATE d'arrété AU 31/12/2018
base_baisse_clast=base_baisse_clast%>%
  filter(DEBUT<=d_arrete)

b_bais_clas=base_baisse_clast%>%
  group_by(MATRICULE)%>%
  count(BAISSE_CLASSIFICATION)%>%
  rename(nbre_bais_clas=n)%>%
  select(MATRICULE,nbre_bais_clas)

length(unique(b_bais_clas$MATRICULE))## voir s'il y'a des valeurs doubles
summary(b_bais_clas)

any(is.na(b_bais_clas))


## varaible binaire a t il eu une baisse ou pas ? oui ou non 

b_bais_clas$BAIS_CLAS="OUI"

b_bais_clas=b_bais_clas%>%
  select(MATRICULE,BAIS_CLAS)


length(unique(b_bais_clas$MATRICULE))## voir s'il y'a des valeurs doubles
summary(b_bais_clas)

b_bais_clas$BAIS_CLAS=as.factor(b_bais_clas$BAIS_CLAS)
summary(b_bais_clas)

any(is.na(b_bais_clas))




############################################################base mob geo
base_mob_geo=read_excel("base_mob_geo.xlsx")
base_mob_geo$DEBUT=as.Date(base_mob_geo$DEBUT,"%d/%m/%Y")
base_mob_geo$MOB_GEO=as.factor(base_mob_geo$MOB_GEO)
str(base_mob_geo)
## FILTRE SUR LA DATE d'arrété AU 31/12/2018
base_mob_geo=base_mob_geo%>%
  filter(DEBUT<=d_arrete)

base_mob_geo=base_mob_geo%>%
  group_by(MATRICULE)%>%
  count(MOB_GEO)

base_mob_geo=base_mob_geo%>%
  rename(nbre_mob_geo=n)

base_mob_geo=base_mob_geo[,c("MATRICULE","nbre_mob_geo")]

summary(base_mob_geo)


length(unique(base_mob_geo$MATRICULE))


##varaible binaire a t il eu UNE MOBILIT? ? oui ou non 

base_mob_geo$MOB_GEO="OUI"

base_mob_geo=base_mob_geo%>%
select(MATRICULE,MOB_GEO)

base_mob_geo$MOB_GEO=as.factor(base_mob_geo$MOB_GEO)
summary(base_mob_geo)

############################################################## base absence
b_abs=read_excel("base_absence.xlsx")
b_abs$MATRICULE=as.factor(b_abs$MATRICULE)
b_abs$D_ARRT_INFO=as.Date(b_abs$D_ARR_INFO,"%d/%m/%Y")
str(b_abs)
## FILTRE SUR LA DATE d'arrété AU 31/12/2018
b_abs=b_abs%>%
  filter(D_ARR_INFO<=d_arrete)
# nombre d'absence
b1=b_abs%>%
  group_by(MATRICULE) %>%
  summarise(n=sum(SUM_MALADIE_ACC))%>%
  rename(abs_maladie_acc=n)
  

b2=b_abs%>%
  group_by(MATRICULE) %>%
  summarise(n=sum(SUM_MATER))%>%
  rename(abs_maternite=n)
b2
summary(b2)

b3=b_abs%>%
  group_by(MATRICULE) %>%
  summarise(n=sum(SUM_ABS_AUTRE))%>%
  rename(abs_autres=n)
  
b4=merge(b1,b2,by="MATRICULE")#all.x = TRUE,all.y = TRUE)
b_abs_final=merge(b4,b3 ,by="MATRICULE")
## barplot for multiples variables

str(b_abs_final)


summary(b_abs_final)


str(b_abs_final)
any(is.na(b_abs_final))


  

#######################################################base augmentation

## BASE AUGMentation ///// nombre d'augmentation  indidividuelle de salaire en variable facteur

##  
b_augm=read_excel("base_augm.xlsx")
str(b_augm)
b_augm$D_DEB_REM=as.Date(b_augm$D_DEB_REM,"%d/%m/%Y")
b_augm$L_TYP_AUGM_INDIV=as.factor(b_augm$L_TYP_AUGM_INDIV)
b_augm$C_TYP_AUGM_INDIV=as.factor(b_augm$C_TYP_AUGM_INDIV)

## filtre sur la date d'arrete
b_augm=b_augm%>%
  filter(D_DEB_REM<=d_arrete)

b_augm=b_augm%>%
  group_by(MATRICULE)%>%
  count(MATRICULE)%>%
  rename(nbre_augm_sal=n)

summary(b_augm)


q_nbre_augm=quantile(b_augm$nbre_augm_sal,names=FALSE)

summary(b_augm)

b_augm=b_augm%>%
  rename(Augm=nbre_augm_sal)

######################BASE REM QUI CONTIENT LA REMU annuelle pour voir combien on on capter

remu=read_excel("rem.xlsx")
remu
str(remu)
remu$PRESENT_AU=as.Date(remu$PRESENT_AU,"%d/%m/%Y")
## filtre sur la d_arrete
remu=remu%>%
  filter(PRESENT_AU==d_arrete)

remu=remu%>%
  rename(Remu=REMU_ANNUELLE)




################################base formation 

b_form=read_excel("base_form.xlsx")
b_form$DATE_DEBUT=as.Date(b_form$DATE_DEBUT,"%d/%m/%Y")
b_form$CODE_FORM=as.factor(b_form$CODE_FORM)
str(b_form)
## FILTRE SUR LA DATE d'arrété AU 31/12/2018
b_form=b_form%>%
  filter(DATE_DEBUT<=d_arrete)
## nombre de formation 
b_form=b_form %>%
  group_by(MATRICULE)%>%
  count(MATRICULE)%>%
  rename(nbre_form=n)




summary(b_form)
  
length(unique(b_form$MATRICULE))



any(is.na(b_form))

hist(b_form$nbre_form)
##table(b_form$)

### utiliser les quantiles 
# base sortie
b_sortie=read_excel("base_sortie.xlsx") ## FAUT IL PRENDRE LES GENS QUI SONT SORTIS UNE FOIS OU +SIEURS FOIS?
b_sortie$D_SORT_SOC=as.Date(b_sortie$D_SORT_SOC,"%d/%m/%Y")
b_sortie$L_MOTF_SORT_SOC=as.factor(b_sortie$L_MOTF_SORT_SOC)
str(b_sortie)

b_sortie=b_sortie%>%
  filter(D_SORT_SOC>"2018-12-31")%>%
  rename(motf_sort=L_MOTF_SORT_SOC)

  
str(b_sortie)
summary(b_sortie)
# nbre de mois entre data d'arrete et date sortie
b_sortie$Y_sortie=time_length(interval(start=d_arrete,end=b_sortie$D_SORT_SOC),unit="months")



## y_sorti 6 mois 12 mois ou 9 mois 

b_sortie$Y_6=ifelse(b_sortie$Y_sortie<= 6,"1","0")
b_sortie$Y_6=as.factor(b_sortie$Y_6)
b_sortie$Y_9=ifelse(b_sortie$Y_sortie<= 9,"1","0")
b_sortie$Y_9=as.factor(b_sortie$Y_9)
b_sortie$Y_12=ifelse(b_sortie$Y_sortie<= 12,"1","0")
b_sortie$Y_12=as.factor(b_sortie$Y_12)

str(b_sortie)
b_sortie=b_sortie%>%
  select(MATRICULE,Y_6,Y_9,Y_12)

str(b_sortie)


#base diplome
b_dipl=read_excel("base_diplm.xlsx")
b_dipl
b_dipl$DIPLOME=as.factor(b_dipl$DIPLOME)
str(b_dipl)
length(unique(b_dipl$MATRICULE))

b_dipl=b_dipl%>%
  select(MATRICULE,DIPLOME)
 

any(is.na(b_dipl))


##quantile(b_dipl,names=FALSE)

## ? mettre ? la fin
##b_fixe=b_fixe%>%
##filter(b_fixe$Age<=59)

## jointure des tables
base_final=list(b_fixe_f,b_emploi,b_anc_empl,b_promotion,b_nomination,b_bais_clas,
                base_mob_geo,b_abs_final,b_form,b_sortie,remu,b_augm,b_dipl) %>%
  reduce(left_join,by="MATRICULE")

summary(base_final)
View(base_final)
any(is.na(base_final))


## 
summary(base_final)
str(base_final)


## filtre sur l'age 
summary(base_final)
base_final=base_final%>%
  filter(base_final$Age<=59)


### traitement des valeurs manquantes dans l'ensemble de la base, vu que j'ai fait des left join , 
##il y'a des valeurs manquantes,


any(is.na(base_final))
# promotion
base_final$PROMOTION=factor(base_final$PROMOTION,levels=c("NON","OUI"))
base_final$PROMOTION[is.na(base_final$PROMOTION)]="NON"
# nomination
base_final$NOMINATION=factor(base_final$NOMINATION,levels=c("NON","OUI"))
base_final$NOMINATION[is.na(base_final$NOMINATION)]="NON"
## bais clas
base_final$BAIS_CLAS=factor(base_final$BAIS_CLAS,levels=c("NON","OUI"))
base_final$BAIS_CLAS[is.na(base_final$BAIS_CLAS)]="NON"
## MOBILITE GEO
base_final$MOB_GEO=factor(base_final$MOB_GEO,levels=c("NON","OUI"))
base_final$MOB_GEO[is.na(base_final$MOB_GEO)]="NON"


# abs
which(is.na(base_final$abs_autres)) ##
base_final$abs_autres[is.na(base_final$abs_autres )]=0
base_final$abs_maladie_acc[is.na(base_final$abs_maladie_acc )]=0
base_final$abs_maternite[is.na(base_final$abs_maternite )]=0

## formation
base_final$nbre_form [is.na(base_final$nbre_form )]=0

## Y_6,9,12
base_final$Y_6[is.na(base_final$Y_6)]='0'
base_final$Y_9[is.na(base_final$Y_9)]='0'
base_final$Y_12[is.na(base_final$Y_12)]='0'


## augmentaion
base_final$Augm[is.na(base_final$Augm )]=0

summary(base_final)
which(is.na(base_final$ANC_EMPL)) #

## diplome

base_final$DIPLOME[is.na(base_final$DIPLOME)]="<= Bac +1"

## ANC EMPLOI 
base_final$ANC_EMPL[is.na(base_final$ANC_EMPL)]=0

View(base_final)

any(is.na(base_final))
which(is.na(base_final))
## ########################quantile Age 

q_age = quantile(base_final$Age, names =FALSE)
q_age

base_final$tr_Age=cut(base_final$Age,
                      breaks=c(q_age[1],q_age[2],q_age[3],q_age[4], Inf),
                      labels=c("21-30 age","31-38 age","39-49 age","50-59 age"), 
                      right=FALSE)

## tranche anciennete GROUPE 
q_tr_anc=quantile(base_final$ANC_GPR,names=FALSE)
q_tr_anc


base_final$tr_ANC_GRP=cut(base_final$ANC_GPR,
                      breaks=c(q_tr_anc[1],q_tr_anc[2],q_tr_anc[3],q_tr_anc[4],Inf),
                      labels=c("0-4 ans","5-11 ans","11-23 ans","24-41 ans"),
                      right=FALSE)

## TRANCHE ANC EMPLOI

q_tr_anc_empl=quantile(base_final$ANC_EMPL,names=FALSE)
q_tr_anc_empl

base_final$tr_anc_empl=cut(base_final$ANC_EMPL,
                           breaks=c(-Inf,1,2,4,Inf),
                           labels=c("- 1ANC_E","1-2ANC_E","2-4ANC_E","4-5ANC_E"), #Ici à voir pour automatiser les labels !
                           right=FALSE)

#base_final$tr_anc_empl=cut(base_final$ANC_EMPL,
                          # breaks=c(q_tr_anc_empl[1],q_tr_anc_empl[2],q_tr_anc_empl[3],q_tr_anc_empl[4],Inf),
                           #labels=c("- 1ANC_E","1-2ANC_E","2-4ANC_E","4-5ANC_E"), #Ici à voir pour automatiser les labels !
                           #right=FALSE)

## tranche absenece 
q_abs_autres=quantile(base_final$abs_autres,names=FALSE)
q_abs_autres
base_final$tr_abs_autres=cut(base_final$abs_autres,
                              breaks=c(q_abs_autres[1],q_abs_autres[2],q_abs_autres[3],q_abs_autres[4],Inf),
                              labels=c("0-7Abs_A","8-76 Abs_A","77-92 Abs_A","+92Abs_A"),
                              right=FALSE)
##
## tranche abs maladie
q_abs_MA=quantile(base_final$abs_maladie_acc,names=FALSE)
q_abs_MA
base_final$tr_abs_MA=cut(base_final$abs_maladie_acc,
                                   breaks=c(q_abs_MA[1],q_abs_MA[2],q_abs_MA[3],q_abs_MA[4],Inf),
                                   labels=c("-1abs_MA","2-10abs_MA","11-40abs_MA","+ 41abs_MA"),
                                   right=FALSE)
### tr_abs_maternité
q_ab_mater=quantile(base_final$abs_maternite,names=FALSE)
q_ab_mater ##   0    0    0    0 1078

## 75 % des gens ont n'ont pas eu d'ab mater du coup je le mets en variable facteur

base_final$abs_maternite=ifelse(base_final$abs_maternite== 0,"NON","OUI")
base_final$abs_maternite=as.factor(base_final$abs_maternite)

## formation
q_form=quantile(base_final$nbre_form,names=FALSE)
q_form
base_final$tr_form=cut(base_final$nbre_form,
                   breaks = c(q_form[1],q_form[2],q_form[3],q_form[4],Inf),
                   labels = c("0-40form","41-59form","60-76form","77-148form"),
                   right=FALSE)
summary(base_final)

##tranche Augmentation
q_augm=quantile(base_final$Augm,names=FALSE)
q_augm ##  0 0 0 1 6 ## j'ai en variable factor , oui si tu as une augmentation, non sinon

base_final$Augm=ifelse(base_final$Augm== 0,"NON","OUI")
base_final$Augm=as.factor(base_final$Augm)

any(is.na(base_final))
summary(base_final)

## tranche remu 
q_remu=quantile(base_final$Remu,names=FALSE)
round(q_remu)
base_final$tr_REMU=cut(base_final$Remu,
                          breaks=c(q_remu[1],round(q_remu[2]),round(q_remu[3]),round(q_remu[4]),Inf),
                          labels=c("-28677K","28878-33842K","33843-39758K"," + 39759K"),
                          right=FALSE) ### utiliser la quntiles


any(is.na(base_final))  
summary(base_final)

## selectionner les variables

names(base_final)
base_final=base_final%>%
  select(MATRICULE,SEXE,C_FILIERE,CADRE,PROMOTION,NOMINATION,BAIS_CLAS,MOB_GEO,Y_6,Y_9,Y_12,tr_Age,DIPLOME,
         tr_ANC_GRP,tr_anc_empl,tr_abs_autres,tr_abs_MA,tr_form,tr_REMU,Augm,abs_maternite)



##renommer mes variables

base_final=base_final%>%
  rename(FILIERE=C_FILIERE,PROMO=PROMOTION,NOMI=NOMINATION,ABS_MA=tr_abs_MA,ABS_MATER=abs_maternite,ANC_EMPLOI=tr_anc_empl
         ,ANC_grp=tr_ANC_GRP,ABS_AUTRES=tr_abs_autres,AGE=tr_Age,FORM=tr_form)
 
View(base_final)

##
base_etude=base_final
## supprimer la colonne matricule
base_etude=base_etude[,-1]

summary(base_etude)
any(is.na(base_final))

##
indcal=unlist(lapply(base_etude,is.factor))
df1=base_etude[,indcal]
head(df1)

par(mfrow=c(4,4))
for(i in 1:ncol(df1)){
  barplot(table(df1[,i]),main=names(df1)[i])
}



## dataviz 
## treemap des nombres de départs les départs
install.packages("treemap")
install.packages("treemapify")
library(treemap)
library(treemapify)

write.table(b_sortie,"b_MOTIF_SORTIE.csv",sep=";")

motif_sortie=read_excel("motif8sortie.xlsx")
bg=motif_sortie%>%
  group_by(ANNEE)%>%
  count(ANNEE)%>%
  rename(DEPART=n)

treemap(bg,
        index="ANNEE",
        vSize ="DEPART",
        type="index")


### reemap des motif sortie

motif=sortie=motif_sortie%>%
  group_by(DET_MOTIF_SORTIE)%>%
  count(DET_MOTIF_SORTIE)%>%
  rename(motif_sortie=n)

treemap(motif,
        index="DET_MOTIF_SORTIE",
        vSize ="motif_sortie",
        type="index")




