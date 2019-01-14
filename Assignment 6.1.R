library(readxl)
titanic3<-read_excel("Downloads/titanic3.xls")
View(titanic3)
str(titanic3)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(scales)
library(mice)
library(randomForest)

#1
titanic3$title<-gsub('(.*,)|(\\..*)','',titanic3$name)
table(titanic3$sex,titanic3$title)
rare_title<-c('Dona','Lady','the Countess','Capt','Col','Don','Dr','Major','Rev','Sir','Jonkheer')
titanic3$title[titanic3$title == 'Mlle']<-'Miss'
titanic3$title[titanic3$title == 'Ms']<-'Miss'
titanic3$title[titanic3$title == 'Mme']<-'Mrs'
titanic3$title[titanic3$title %in% rare_title]<-'Rare_title'
table(titanic3$sex,titanic3$title)

#2
titanic3$surname<-sapply(titanic3$name,function(x)strsplit(x,split='[,.]')[[1]][1])
titanic3$Fsize<-titanic3$sibsp+titanic3$parch+1
titanic3$family<-paste(titanic3$surname,titanic3$Fsize,sep='_')
ggplot(titanic3[1:891,],aes(x=Fsize,fill=factor(survived)))+
  geom_bar(stat = 'count',position = 'dodge')+
  scale_x_continuous(breaks = c(1:11))+
  labs(x='Family Size')+
  theme_few()

#3
sum(is.na(titanic3$age))
str(titanic3)
factors_vars<-c('pclass','sex','embarked','title','surname','Fsize','family')
titanic3[factors_vars]<-lapply(titanic3[factors_vars], function(x) as.factor(x))
set.seed(129)
mice_mod<-mice(titanic3[,!names(titanic3) %in% c('pclass','sex','embarked','title','surname','Fsize','family')],method = 'rf')
mice_output<-complete(mice_mod)
par(mfrow=c(1,2))
hist(titanic3$age,freq = F,main = "Age:Original Data",
     col='darkgreen',ylim=c(0,0.04))
hist(mice_output$age,freq = F,main = "Age:Mice Output",
     col='lightgreen',ylim=c(0,0.04))
