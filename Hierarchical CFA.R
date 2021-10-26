
library(haven)
library(tidyverse)
data <- read_sav("C:/Users/diego/Downloads/Rnd1&2Data.sav")#LOAD DATA. CHANGE PATH TO WHERE YOU HAVE THE DATA
data<-data%>%select(565:1201)#SELECT PERSONALITY ASSESSMENT


library(lavaan)#PACKAGE FOR CFA


HS.Model<-'HARMONY=~Q21.9recode+Q23.5recode+Q24.14recode+Q26.9recode+Q28.11recode+Q31.10recode+Q31.13recode+Q34.8recode+Q34.10recode+Q36.1recode+Q38.10recode
INDEPENDENCE=~Q21.10recode+Q22.7recode+Q22.12recodeR+Q26.15recodeR+Q27.7recodeR+Q28.13recode+Q29.9recode+Q30.9recodeR+Q31.4recodeR+Q34.5recodeR+Q37.11recodeR+Q39.12recodeR
CONCERN=~Q24.8recodeR+Q25.3recode+Q25.15recode+Q26.8recode+Q27.15recode+Q28.12recode+Q30.2recode+Q30.6recode+Q32.12recode+Q36.17recode+Q37.10recode+Q40.15recode
PATIENCE=~Q41.17recodeR+Q42.15recodeR+Q43.5recodeR+Q44.15recodeR+Q45.1recodeR+Q45.12recode+Q41.1recode+Q41.13recode+Q42.7recode+Q43.1recode+Q43.13recode+Q45.5recode+Q45.9recode
TOUGH=~Q41.8recode+Q41.12recode+Q41.16recode+Q42.2recode+Q42.6recode+Q43.4recode+Q43.8recode+Q43.12recode+Q43.16recode+Q44.6recode+Q44.10recode+Q44.14recode+Q44.18recode+Q45.8recode
AGREE=~HARMONY+INDEPENDENCE+CONCERN+PATIENCE+TOUGH' #THIS IS THE MODEL, FIRST SUBFACETS, THEN ADD THEM TOGETHER TO THE METATRAIT


fit<-cfa(HS.Model, data=data)#FIT MODEL TO DATA

library(semPlot)#PACKAGE TO CREATE THE GRAPH BASED ON THE MODEL
semPaths(fit, "std")

##FOR ALL THE META-TRAITS:
semPaths(fit, "std", layout="circle")#FUNCTION TO CREATE GRAPH
fitness<-fitMeasures(fit)
