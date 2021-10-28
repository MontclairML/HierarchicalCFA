
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
AGREE=~HARMONY+INDEPENDENCE+CONCERN+PATIENCE+TOUGH #THIS IS THE MODEL, FIRST SUBFACETS, THEN ADD THEM TOGETHER TO THE METATRAIT
ACH =~ Q32.16recodeR + Q22.11recode + Q23.2recode + Q25.1recode + 	Q25.10recode + 	Q25.13recode + Q27.8recode + 	Q27.13recode +	Q28.1recode + 	Q29.17recode + 	Q35.7recode + 	Q36.7recode + 	Q37.4recode + 	Q37.6recode
CMPL =~ Q23.8recodeR + Q27.14recodeR + Q38.12recodeR + Q40.6recodeR + Q26.12recode + Q27.5recode + Q28.3recode + Q28.15recode + Q34.7recode + Q39.10recode + Q40.5recode + Q40.9recode
DEP =~ Q23.13recode + Q29.14recode + Q32.4recode + Q32.10recode + Q33.5recode + Q33.13recode + Q34.13recode + Q35.11recode + Q38.9recode + Q39.4recode + Q39.6recode + Q40.14recode
DETO =~ Q23.14recode + Q24.5recode + Q26.2recode + Q27.16recode + Q31.11recode + Q32.2recode + Q32.3recode + Q32.6recode + Q32.13recode + Q34.14recode + Q40.3recode + Q29.11recodeR
ORG =~ Q29.12recodeR + Q35.13recodeR + Q21.1recode + Q21.7recode + Q22.2recode + Q24.1recode + Q24.3recode + Q31.8recode + Q32.8recode + Q34.12recode + Q37.3recode + Q39.14recode
CONSC =~ ACH + CMPL + DEP + DETO + ORG
ASSRT =~ Q30.4recodeR + Q25.12recode + Q26.4recode + Q26.7recode + Q26.17recode + Q30.7recode + Q31.1recode + Q31.16recode + Q32.11recode + Q33.1recode + Q33.3recode + Q37.14recode
COMPET =~ Q23.7recode + Q28.6recode + Q28.10recode + Q29.16recode + Q30.14recode + Q33.8recode + Q33.12recode + Q34.6recode + Q37.5recode + Q38.7recode + Q39.15recode + Q40.1recode
NRG =~ Q31.12recodeR + Q31.14recodeR + Q33.6recodeR + Q35.3recodeR + Q21.3recode + Q24.15recode + Q28.8recode + Q38.4recode + Q38.8recode + Q40.4recode + Q40.7recode + Q40.17recode
INFL =~ Q39.3recodeR + Q21.11recode + Q22.5recode + Q24.11recode + Q25.9recode + Q26.6recode + Q27.3recode + Q36.8recode + Q36.13recode + Q38.1recode + Q38.6recode + Q39.7recode
INIT = Q22.9recode + Q23.10recode + Q24.7recode + Q26.10recode + Q26.11recode + Q27.12recode + Q30.3recode + Q30.13recode + Q34.4recode + Q35.4recode + Q36.6recode + Q39.9recode
PERS =~ Q22.3recode + Q22.4recode + Q23.1recode + Q27.2recode + Q28.2recode + Q29.3recode + Q30.15recode + Q33.4recode + Q33.10recode + Q33.14recode + Q37.1recode + Q40.11recode
SOCIAB  =~ Q41.15recodeR + Q42.1recodeR + Q42.9recodeR + Q42.17recodeR + Q43.11recodeR + Q44.1recodeR + Q44.5recodeR + Q45.11recodeR + Q41.3recode + Q41.7recode + Q41.11recode + Q42.5recode + Q42.13recode + Q43.3recode + Q43.7recode + Q43.15recode + Q44.9recode + Q44.13recode + Q44.17recode + Q45.3recode + Q45.7recode
EXTRA =~ ASSRT + COMPET + NRG + INFL + INIT + PERS + SOCIAB



'
fit<-cfa(HS.Model, data=data)#FIT MODEL TO DATA

library(semPlot)#PACKAGE TO CREATE THE GRAPH BASED ON THE MODEL
semPaths(fit, "std")

##FOR ALL THE META-TRAITS:
semPaths(fit, "std", layout="circle")#FUNCTION TO CREATE GRAPH
fitness<-fitMeasures(fit)
