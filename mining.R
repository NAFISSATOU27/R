install.packages("readtext")
install.packages("quanteda")
install.packages("quanteda.textmodels")
install.packages("rvest")
getwd()
install.packages("igraph")
install.packages("wordcloud")
library(igraph)
library(wordcloud)
library("readtext") # Ouverture de plusieurs documents  de tous les types

library("quanteda")  # textmining
library("quanteda.textmodels")
library("rvest")
library("readxl")

quanteda_options(language = "french")

fichier=read_excel("comm.xlsx",col_names = T)
View(fichier)

fichier=fichier[-c(1:3),]## suprimer les lignes initules dans le fichier de départ
colnames(fichier)=c("Matricule","EAA","Appreciation_emploi","commentaire_collab","commentaire_manager") # renommer les colonnes

## filtre sur l'entretien de 2019
E2019=subset(fichier,fichier$EAA=="Entretien Annuel Appréciation 2019")
E2019

########################################################
# création des corpus , on peut dire un corpus de texte est une partie du texte

mon_corpus1 <- corpus(E2019$Appreciation_emploi) # création du corpus 1 pour la colonne appréciation_emploi
mon_corpus1
ndoc(mon_corpus1) # nombre de documents dans le corpus

mon_corpus2 <- corpus(E2019$commentaire_collab) # création du corpus 2 pour la colonne commentaire collab
mon_corpus2


mon_corpus3 <- corpus(E2019$commentaire_manager)  # création du corpus 3 pour la colonne commentaire manager
mon_corpus3
########################################################
## nettoyer le corpus 

install.packages("textclean")
library("textclean")

mon_corpus11=replace_symbol(mon_corpus1) #  Élimine tous les symboles mais garde le reste intact
mon_corpus11

mon_corpus11=replace_non_ascii(mon_corpus11) # Force l'élimination de tous les caractères non ascii
mon_corpus11

# identifier les types de caractères contenus dans un corpus
mes_caracteres <- table(unlist(strsplit (mon_corpus11,""))) 
print(mes_caracteres)

uniq_car <- names(mes_caracteres) # pour afficher tous les types de caractères
print(uniq_car)
uniq_car[1] # donne le caractère apostrophe
uniq_car[2] # donne le caractere tiret du 6

library("stringr")
mon_corpus11= str_replace_all(mon_corpus11, uniq_car[1], " ") # remplacer l'apostrophe par un espace 
mon_corpus11

mon_corpus11= str_replace_all(mon_corpus11, uniq_car[2], " ") # remplace le tiret (-) par un espace 

##mon_corpus11= strip(mon_corpus11, char.keep = "~~", digit.remove = TRUE)##, apostrophe.remove = F) #Éliminer tous les caractères spéciaux, ou tous les chiffres, ou toutes les apostrophes
##mon_corpus11

mon_corpus22=replace_symbol(mon_corpus2) #  �limine tous les symboles mais garde le reste intact
mon_corpus22


mon_corpus22=replace_non_ascii(mon_corpus22) # Force l'élimination de tous les caractères non ascii
mon_corpus22

mon_corpus33=replace_symbol(mon_corpus3) #  �limine tous les symboles mais garde le reste intact
mon_corpus33

mon_corpus33=replace_non_ascii(mon_corpus33) # Force l'élimination de tous les caractères non ascii
mon_corpus33
########################################################
## g�n�rer de tokens( on d�coupe le corpus sur chaque espace)
toks_news1 <- tokens(mon_corpus11,remove_punct=T,remove_numbers=F)%>%
tokens_remove(., pattern = stopwords('fr'), valuetype = 'fixed') %>% tokens_tolower(., keep_acronyms = FALSE)
toks_news1[1]



## mettre en place la DFM
my_dfm1<- dfm(toks_news1[1],remove=stopwords('fr'),remove_punct=T ,stem=F)
my_dfm1
dtm_ajust1 <- dfm_weight(my_dfm1, scheme='prop')
dtm_ajust1
docfreq(dtm_ajust1, scheme='inverse') 
## AFFICHER UN WORDCLOUD (nuage de mots) pour voir voir quels mots apparaissent le plus
topfeatures(my_dfm1) # Quels mots sont les plus fr�quents dans le doc 1 de mon corpus
barplot(topfeatures(my_dfm1)) 

my_tsf <- textstat_frequency(my_dfm1,n=15)
plot(my_tsf$frequency)
text(x=2:15,y=my_tsf$frequency,labels=my_tsf$feature)

#NUAGE FE POINT 
textplot_wordcloud(my_dfm1,min_count = 2,col=1:7) 
