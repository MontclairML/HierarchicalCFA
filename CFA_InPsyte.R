
library(devtools)
library(readxl)
library(tidyverse)
library(dplyr)
library(psych)
library(semTools)

rawdata_df<-read.csv("rnd1&2Data.csv")
 

n<-ncol(rawdata_df)

rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] == 'Strongly Disagree'] <- 1
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] ==  'Disagree'] <- 2
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] ==  'Slightly Disagree'] <- 3
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] ==  '?'] <- 4
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] ==  'Slightly Agree'] <- 5
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] ==  'Agree'] <- 6
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] ==  'Strongly Agree'] <- 7

rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] == 'Never (A few times a year or less)'] <- 1
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] == 'Rarely (Once a month or less)'] <- 2
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] == 'Sometimes (A few times a month)'] <- 3
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] == 'Often (Once a week)'] <- 4
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] == 'Very Often (A few times a week)'] <- 5
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] == 'Always (Everyday)'] <- 6

rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] == 'Worse than most'] <- 1
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] == 'Worse than average'] <- 2
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] == 'Average'] <- 3
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] == 'Better than average'] <- 4
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] == 'Better than most'] <- 5

rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] == 'Agree'] <- 1
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] == 'Slightly agree'] <- 2
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] == 'Neither agree or disagree'] <- 3
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] == 'Slightly disagree'] <- 4
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] == 'Disagree'] <- 5

rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='Strongly disagree'] <- 1
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='Disagree'] <- 2
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='Neither agree or disagree'] <- 3
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='Agree'] <- 4
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='Strongly agree'] <- 5

rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='Very Unlikely'] <- 1
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='Unlikely'] <- 2
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='Somewhat Unlikely'] <- 3
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='Undecided'] <- 4
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='Somewhat Likely'] <- 5
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='Likely'] <- 6
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='Very Likely'] <- 7

rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='Less than once per month or never'] <- 1
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='Once or twice per month'] <- 2
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='Once or twice per week'] <- 3
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='Once or twice per day'] <- 4
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='Several times per day'] <- 5
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='Always (Everyday)'] <- 6

rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='Yes'] <- 0
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='No'] <- 1

rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='0-1 month'] <- 1
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='1-3 months'] <- 2
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='3-6 months'] <- 3
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='6-12 months'] <- 4
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='24 or more months'] <- 5

rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='1 day a week'] <- 1
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='2 days a week'] <- 2
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='3 days a week'] <- 3
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='4 days a week'] <- 4
rawdata_df[,c(1:n)][rawdata_df[,c(1:n)] =='More than 5 days a week'] <- 5

#rawdata_df[,2:n]<-mutate_all(rawdata_df[,1:n], as.numeric)

names<-read.csv("Item_codes.csv", header=FALSE)
colnames(rawdata_df)[c(27:554)] <- names$V1


negatives<-select(rawdata_df[,27:n], contains(c("TN", "MN", "NM")))%>%
  mutate_all(as.numeric)

negatives<-abs(negatives-8)
rawdata_df[,27:n]<-mutate_all(rawdata_df[,27:n], as.numeric)

rawdata_df[names(negatives)] <- negatives

rawdata_df<-rawdata_df%>%filter(rawdata_df$attchktotal<=7)

#################################################################################

Methodical<-select(rawdata_df[,26:n],contains(c("Org")))%>%
  rowMeans(na.rm=TRUE)


Achievement_Oriented<-select(rawdata_df[,26:n],contains(c("Ach")))%>%
  rowMeans(na.rm=TRUE)

Dependability<-select(rawdata_df[,26:n],contains(c("Dep")))%>%
  rowMeans(na.rm=TRUE)

Influencing<-select(rawdata_df[,26:n],contains(c("Inf")))%>%
  rowMeans(na.rm=TRUE)

Team_Oriented<-select(rawdata_df[,26:n],contains(c("Team")))%>%
  rowMeans(na.rm=TRUE)

Rule_Follower<-select(rawdata_df[,26:n],contains(c("Cmpl")))%>%
  rowMeans(na.rm=TRUE)

Detail_Oriented<-select(rawdata_df[,26:n],contains(c("Dtail")))%>%
  rowMeans(na.rm=TRUE)

Assertive<-select(rawdata_df[,26:n],contains(c("Asrt")))%>%
  rowMeans(na.rm=TRUE)

Competitive<-select(rawdata_df[,26:n],contains(c("Comp")))%>%
  rowMeans(na.rm=TRUE)

Initiative<-select(rawdata_df[,26:n],contains(c("Ini")))%>%
  rowMeans(na.rm=TRUE)


Energetic<-select(rawdata_df[,26:n],contains(c("Nrg")))%>%
  rowMeans(na.rm=TRUE)

Persistence<-select(rawdata_df[,26:n],contains(c("Per")))%>%
  rowMeans(na.rm=TRUE)

Analytical_Thinking<-select(rawdata_df[,26:n],starts_with(c("AT"), ignore.case=FALSE))%>%
  rowMeans(na.rm=TRUE)


Creative<-select(rawdata_df[,26:n],contains(c("Inn")))%>%
  rowMeans(na.rm=TRUE)

Learning_Orientation<-select(rawdata_df[,26:n],contains(c("Lo")))%>%
  rowMeans(na.rm=TRUE)

Adaptable<-select(rawdata_df[,26:n],contains(c("Adpt")))%>%
  rowMeans(na.rm=TRUE)

Optimism<-select(rawdata_df[,26:n],contains(c("Opt")))%>%
  rowMeans(na.rm=TRUE)

Self_control<-select(rawdata_df[,26:n],contains(c("Sctrl")))%>%
  rowMeans(na.rm=TRUE)

Stress_Tolerance<-select(rawdata_df[,26:n],contains(c("St")))%>%
  rowMeans(na.rm=TRUE)

Concern_for_Others<-select(rawdata_df[,26:n],contains(c("CFO")))%>%
  rowMeans(na.rm=TRUE)

Cooperation<-select(rawdata_df[,26:n],contains(c("Coop")))%>%
  rowMeans(na.rm=TRUE)

Multitasking<-select(rawdata_df[,26:n],contains(c("Multi")))%>%
  rowMeans(na.rm=TRUE)

Mindful<-select(rawdata_df[,26:n],contains(c("Mind")))%>%
  rowMeans(na.rm=TRUE)

Principled<-select(rawdata_df[,26:n],contains(c("Int")))%>%
  rowMeans(na.rm=TRUE)

Engageable<-select(rawdata_df[,26:n],contains(c("Eng")))%>%
  rowMeans(na.rm=TRUE)

Social_Desirability<-select(rawdata_df[,26:n],contains(c("SD")))%>%
  rowMeans(na.rm=TRUE)

Patience<-select(rawdata_df[,26:n],contains(c("Pat")))%>%
  rowMeans(na.rm=TRUE)


Patience<-select(rawdata_df[,26:n],contains(c("Pat")))%>%
  rowMeans(na.rm=TRUE)

Though_mind<-select(rawdata_df[,26:n],contains(c("ToM")))%>%
  rowMeans(na.rm=TRUE)

Sincerity<-select(rawdata_df[,26:n],contains(c("Sin")))%>%
  rowMeans(na.rm=TRUE)

Sociability<-select(rawdata_df[,26:n],contains(c("Soc")))%>%
  rowMeans(na.rm=TRUE)


Hexaco<-select(rawdata_df[,26:n],contains(c("Hex")))%>%
  rowMeans(na.rm=TRUE)


names<-rawdata_df$Q1.1
final_data<-data.frame(Achievement_Oriented, Adaptable, Analytical_Thinking, Assertive, 
                       Competitive, Creative, Dependability, Detail_Oriented, Energetic, 
                       Influencing, Initiative, Learning_Orientation, Methodical, Optimism, 
                       Persistence, Rule_Follower, Self_control, Team_Oriented, Stress_Tolerance, 
                       Cooperation, Concern_for_Others, Multitasking, Mindful, Principled, Engageable,
                       Social_Desirability, Patience, Though_mind, Sincerity, Sociability, Hexaco, check.names=FALSE )

final_data<-na.omit(final_data)

neuroticism<-cbind(Self_control, Optimism)

write.csv(final_data, "final_data.csv", row.names=FALSE)
data<-read.csv("final_data.csv")

HS.Model<- 'Extraversion=~ NA*Assertive+Competitive+Energetic+Influencing+Initiative+Persistence+Sociability+Though_mind
Extraversion~~1*Extraversion
Conscientiousness=~NA*Achievement_Oriented+Dependability+Detail_Oriented+Methodical+Rule_Follower
Conscientiousness~~1*Conscientiousness
Agreeableness=~NA*Team_Oriented+Cooperation+Concern_for_Others
Agreeableness~~1*Agreeableness
Neuroticism=~NA*Adaptable+Optimism+Self_control+Stress_Tolerance
Neuroticism~~1*Neuroticism
Openess=~NA*Analytical_Thinking+Creative+Learning_Orientation
Openess~~1*Openess
Other=~NA*Principled+Multitasking+Mindful
Other~~1*Other
Hex=~NA*Sincerity+Patience+Hexaco
Hex~~1*Hex'

library(lavaan)
fit1<-cfa(HS.Model, data=data)
summary(fit1, standardized=TRUE, rsq=TRUE, fit.measures=TRUE)
library(semPlot)#PACKAGE TO CREATE THE GRAPH BASED ON THE MODEL
semPaths(fit1, "std")
inspect(fit1, what="cor.all")
write.csv(cor, "correlations.csv")
reliability(fit1)
fitMeasures(fit1)

################################################################################

HS.Model2<-'HARMONY=~NA*Q21.9recode+Q23.5recode+Q24.14recode+Q26.9recode+Q28.11recode+Q31.10recode+Q31.13recode+Q34.8recode+Q34.10recode+Q36.1recode+Q38.10recode
HARMONY~~1*HARMONY
INDEPENDENCE=~NA*Q21.10recode+Q22.7recode+Q22.12recodeR+Q26.15recodeR+Q27.7recodeR+Q28.13recode+Q29.9recode+Q30.9recodeR+Q31.4recodeR+Q34.5recodeR+Q37.11recodeR+Q39.12recodeR
INDEPENDENCE~~1*INDEPENDENCE
CONCERN=~NA*Q24.8recodeR+Q25.3recode+Q25.15recode+Q26.8recode+Q27.15recode+Q28.12recode+Q30.2recode+Q30.6recode+Q32.12recode+Q36.17recode+Q37.10recode+Q40.15recode
CONCERN~~1*CONCERN
PATIENCE=~NA*Q41.17recodeR+Q42.15recodeR+Q43.5recodeR+Q44.15recodeR+Q45.1recodeR+Q45.12recode+Q41.1recode+Q41.13recode+Q42.7recode+Q43.1recode+Q43.13recode+Q45.5recode+Q45.9recode
PATIENCE~~1*PATIENCE
TOUGH=~NA*Q41.8recode+Q41.12recode+Q41.16recode+Q42.2recode+Q42.6recode+Q43.4recode+Q43.8recode+Q43.12recode+Q43.16recode+Q44.6recode+Q44.10recode+Q44.14recode+Q44.18recode+Q45.8recode
TOUGH~~1*TOUGH
AGREE=~NA*HARMONY+INDEPENDENCE+CONCERN+PATIENCE+TOUGH
AGREE~~1*AGREE

#COVARIANCES
HARMONY~~INDEPENDENCE
HARMONY~~CONCERN
HARMONY~~PATIENCE
HARMONY~~TOUGH
INDEPENDENCE~~CONCERN
INDEPENDENCE~~PATIENCE
INDEPENDENCE~~TOUGH
CONCERN~~PATIENCE
CONCERN~~TOUGH
PATIENCE~~TOUGH
'

fit2<-cfa(HS.Model2, check.gradient=FALSE, data=rawdata_df)
summary(fit2, standardized=TRUE, rsq=TRUE, fit.measures=TRUE)
semPaths(fit2, "std")

final_data<-data.frame(Achievement_Oriented, Adaptable, Analytical_Thinking, Assertive, 
                       Competitive, Creative, Dependability, Detail_Oriented, Energetic, 
                       Influencing, Initiative, Learning_Orientation, Methodical, Optimism, 
                       Persistence, Rule_Follower, Self_control, Team_Oriented, Stress_Tolerance, 
                       Cooperation, Multitasking, Mindful, Principled, Engageable,
                       Social_Desirability, Patience, Though_mind, Sincerity, Sociability, Hexaco, check.names=FALSE )

rawdata_df$Q45.12recode<-NULL
rawdata_df$Q41.1recode<-NULL
rawdata_df$Q41.13recode<-NULL
rawdata_df$Q42.7recode<-NULL
rawdata_df$Q42.7recode<-NULL
rawdata_df$Q43.13recode<-NULL
rawdata_df$Q45.5recode<-NULL
rawdata_df$Q31.13recode<-NULL
rawdata_df$Q28.11recode<-NULL
rawdata_df$Q22.7recode <-NULL
rawdata_df$Q26.15recodeR<-NULL
rawdata_df$Q28.13recode<-NULL
rawdata_df$Q42.15recodeR<-NULL
rawdata_df$Q45.9recode<-NULL
rawdata_df$Q41.12recode<-NULL
rawdata_df$Q42.6recode<-NULL
rawdata_df$Q43.12recode <-NULL
rawdata_df$Q44.14recode<-NULL
rawdata_df$Q45.8recode<-NULL




#####################################################################################

HS.model3<-'ACH =~ NA*Q32.16recodeR + Q22.11recode + Q23.2recode + Q25.1recode + 	Q25.10recode + 	Q25.13recode + Q27.8recode + 	Q27.13recode +	Q28.1recode + 	Q29.17recode + 	Q35.7recode + 	Q36.7recode + 	Q37.4recode + 	Q37.6recode
ACH~~1*ACH
CMPL =~ NA*Q23.8recodeR + Q27.14recodeR + Q38.12recodeR + Q40.6recodeR + Q26.12recode + Q27.5recode + Q28.3recode + Q28.15recode + Q34.7recode + Q39.10recode + Q40.5recode + Q40.9recode
CMPL~~1*CMPL
DEP =~ NA*Q23.13recode + Q29.14recode + Q32.4recode + Q32.10recode + Q33.5recode + Q33.13recode + Q34.13recode + Q35.11recode + Q38.9recode + Q39.4recode + Q39.6recode + Q40.14recode
DEP~~1*DEP
DETO =~ NA*Q23.14recode + Q24.5recode + Q26.2recode + Q27.16recode + Q31.11recode + Q32.2recode + Q32.3recode + Q32.6recode + Q32.13recode + Q34.14recode + Q40.3recode + Q29.11recodeR
DETO~~1*DETO
ORG =~ NA*Q29.12recodeR + Q35.13recodeR + Q21.1recode + Q21.7recode + Q22.2recode + Q24.1recode + Q24.3recode + Q31.8recode + Q32.8recode + Q34.12recode + Q37.3recode + Q39.14recode
ORG~~1*ORG
CONSC =~ NA*ACH + CMPL + DEP + DETO + ORG
CONSC~~1*CONSC

#Covariances
ACH~~CMPL
ACH~~DEP
ACH~~DETO
ACH~~ORG
CMPL~~DEP
CMPL~~DETO
CMPL~~ORG
DEP~~DETO
DEP~~ORG
DETO~~ORG
'


fit3<-cfa(HS.model3, check.gradient=FALSE, data=rawdata_df)
summary(fit3, standardized=TRUE, rsq=TRUE, fit.measures=TRUE)
semPaths(fit3, "std")
library(semTable)

semTable(fit3, file="semtable", type="html")

Conscientiousness<-cbind(Rule_Follower, Dependability, Detail_Oriented, Methodical)%>%
  rowMeans(na.rm=TRUE)

library(stargazer)
pars.factors <- standardizedSolution(fit3)[ standardizedSolution(fit3)[,'op']=='=~', c(1:5)]
stargazer(pars.factors, out="factors.html", summary=FALSE, type='html', rownames=FALSE, initial.zero=FALSE, digits=3, title='Factor loadings')


final_data<-data.frame(Adaptable, Analytical_Thinking, Assertive, Creative, Energetic, 
                       Influencing, Initiative, Competitive, Learning_Orientation, Conscientiousness, Optimism, 
                       Persistence, Self_control, Team_Oriented, Stress_Tolerance, 
                       Cooperation, Multitasking, Mindful, Principled, Engageable,
                       Social_Desirability, Patience, Though_mind, Sincerity, Sociability, Hexaco, check.names=FALSE )


rawdata_df$Q27.14recodeR<-NULL
rawdata_df$Q29.11recodeR<-NULL
rawdata_df$Q23.8recodeR<-NULL
rawdata_df$Q27.14recodeR <-NULL
rawdata_df$Q38.12recodeR<-NULL
rawdata_df$Q40.6recodeR<-NULL
rawdata_df$Q27.5recode<-NULL
rawdata_df$Q28.3recode<-NULL
rawdata_df$Q32.10recode<-NULL
rawdata_df$Q23.14recode<-NULL
rawdata_df$Q26.2recode<-NULL
rawdata_df$Q31.11recode<-NULL
rawdata_df$Q32.13recode<-NULL
rawdata_df$Q34.14recode<-NULL
rawdata_df$Q29.11recodeR<-NULL
rawdata_df$Q29.12recodeR<-NULL
rawdata_df$Q35.13recodeR<-NULL
rawdata_df$Q21.1recode<-NULL


#####################################################################################

HS.model4<-'ASSRT =~ NA*Q30.4recodeR + Q25.12recode + Q26.4recode + Q26.7recode + Q26.17recode + Q30.7recode + Q31.1recode + Q31.16recode + Q32.11recode + Q33.1recode + Q33.3recode + Q37.14recode
ASSRT~~1*ASSRT
COMPET =~ NA*Q23.7recode + Q28.6recode + Q28.10recode + Q29.16recode + Q30.14recode + Q33.8recode + Q33.12recode + Q34.6recode + Q37.5recode + Q38.7recode + Q39.15recode + Q40.1recode
COMPET~~1*COMPET
NRG =~ NA*Q31.12recodeR + Q31.14recodeR + Q33.6recodeR + Q35.3recodeR + Q21.3recode + Q24.15recode + Q28.8recode + Q38.4recode + Q38.8recode + Q40.4recode + Q40.7recode + Q40.17recode
NRG~~1*NRG
INFL =~ NA*Q39.3recodeR + Q21.11recode + Q22.5recode + Q24.11recode + Q25.9recode + Q26.6recode + Q27.3recode + Q36.8recode + Q36.13recode + Q38.1recode + Q38.6recode + Q39.7recode
INFL~~1*INFL
INIT =~ NA*Q22.9recode + Q23.10recode + Q24.7recode + Q26.10recode + Q26.11recode + Q27.12recode + Q30.3recode + Q30.13recode + Q34.4recode + Q35.4recode + Q36.6recode + Q39.9recode
INIT~~1*INIT
PERS =~ NA*Q22.3recode + Q22.4recode + Q23.1recode + Q27.2recode + Q28.2recode + Q29.3recode + Q30.15recode + Q33.4recode + Q33.10recode + Q33.14recode + Q37.1recode + Q40.11recode
PERS~~1*PERS
SOCIAB  =~ NA*Q41.15recodeR + Q42.1recodeR + Q42.9recodeR + Q42.17recodeR + Q43.11recodeR + Q44.1recodeR + Q44.5recodeR + Q45.11recodeR + Q41.3recode + Q41.7recode + Q41.11recode + Q42.5recode + Q42.13recode + Q43.3recode + Q43.7recode + Q43.15recode + Q44.9recode + Q44.13recode + Q44.17recode + Q45.3recode + Q45.7recode
SOCIAB~~1*SOCIAB
EXTRA =~ NA*ASSRT + COMPET + NRG + INFL + INIT + PERS + SOCIAB
EXTRA~~1*EXTRA

#Covariance
ASSRT~~COMPET
ASSRT~~NRG
ASSRT~~INFL
ASSRT~~INIT
ASSRT~~PERS
ASSRT~~SOCIAB
COMPET~~NRG
COMPET~~INFL
COMPET~~INIT
COMPET~~PERS
COMPET~~SOCIAB
NRG~~INFL
NRG~~INIT
NRG~~PERS
NRG~~SOCIAB
INFL~~INIT
INFL~~PERS
INFL~~SOCIAB
INIT~~PERS
INIT~~SOCIAB
PERS~~SOCIAB

'
fit4<-cfa(HS.model4, check.gradient=FALSE, data=rawdata_df)
summary(fit4, standardized=TRUE, rsq=TRUE, fit.measures=TRUE)
semPaths(fit4, "std")


final_data<-data.frame(Adaptable, Analytical_Thinking, Assertive, Creative, 
                       Influencing, Initiative, Learning_Orientation, Conscientiousness, Optimism, Self_control, Team_Oriented, Stress_Tolerance, 
                       Cooperation, Multitasking, Mindful, Principled, Engageable,
                       Social_Desirability, Patience, Though_mind, Sincerity, Hexaco, check.names=FALSE )



rawdata_df$Q24.7recode<-NULL
rawdata_df$Q33.1recode<-NULL
rawdata_df$Q32.11recode<-NULL
rawdata_df$Q30.4recodeR<-NULL
rawdata_df$Q31.1recode<-NULL
rawdata_df$Q39.3recodeR<-NULL
rawdata_df$Q22.5recode<-NULL
rawdata_df$Q36.8recode<-NULL
rawdata_df$Q24.7recode<-NULL
rawdata_df$Q30.15recode<-NULL
rawdata_df$Q33.4recode<-NULL
rawdata_df$Q41.15recodeR<-NULL
rawdata_df$Q42.1recodeR<-NULL
rawdata_df$Q42.1recodeR<-NULL
rawdata_df$Q42.9recodeR<-NULL
rawdata_df$Q42.17recodeR<-NULL
rawdata_df$Q43.11recodeR<-NULL
rawdata_df$Q44.1recodeR<-NULL
rawdata_df$Q44.5recodeR<-NULL
rawdata_df$Q45.11recodeR<-NULL
rawdata_df$Q43.15recode<-NULL
rawdata_df$Q44.13recode<-NULL
rawdata_df$Q44.17recode<-NULL

Influencing<-cbind(Competitive, Influencing, Sociability)%>%rowMeans()
Initiative<-cbind(Initiative, Persistence)%>%rowMeans()


###############################################################################

HS.model5<-'CREAT =~ NA*Q23.3recode + Q23.6recode + Q23.12recode + Q25.5recode + Q25.7recode + Q26.13recode + Q29.5recode + Q30.5recode + Q34.15recode + Q35.5recode + Q36.11recode + Q39.8recode
CREAT~~1*CREAT
PROBS =~ NA*Q21.15recode + Q22.1recode + Q22.8recode + Q25.11recode + Q26.1recode + Q28.5recode + Q28.7recode + Q29.1recode + Q29.8recodeR + Q34.3recode + Q39.1recodeR + Q40.16recodeR
PROBS~~1*PROBS
CURIOUS =~ NA*Q25.14recode + Q29.2recode + Q31.6recode + Q32.15recode + Q33.7recode + Q33.11recode + Q35.6recode + Q35.9recode + Q37.2recode + Q38.15recode + Q39.13recode + Q40.8recode
CURIOUS~~1*CURIOUS
OPEN =~ NA*CREAT + PROBS + CURIOUS
OPEN~~1*OPEN


#Covariances
CREAT~~PROBS
CREAT~~CURIOUS
PROBS~~CURIOUS

'

fit5<-cfa(HS.model5, check.gradient=FALSE, data=rawdata_df)
summary(fit5, standardized=TRUE, rsq=TRUE, fit.measures=TRUE)
semPaths(fit5, "std")


rawdata_df$Q21.15recode<-NULL
rawdata_df$Q29.8recodeR<-NULL
rawdata_df$Q40.16recodeR<-NULL
rawdata_df$Q35.9recode<-NULL

openess<-cbind(Creative, Learning_Orientation, Analytical_Thinking)%>%rowMeans



final_data<-data.frame(Adaptable, Assertive, 
                       Influencing, Initiative, Creative, Learning_Orientation, Dependability, Methodical, Optimism, Self_control, Team_Oriented, Optimism, 
                       Cooperation, Multitasking, Mindful, Principled, Engageable,
                       Social_Desirability, Patience, Though_mind, Sincerity, Hexaco, check.names=FALSE )



########################################################################################

Final_model<-'Extraversion=~ NA*Assertive+Influencing+Initiative+Though_mind
Extraversion~~1*Extraversion
Agreeableness=~NA*Team_Oriented+Cooperation
Agreeableness~~1*Agreeableness
Neuroticism=~NA*Self_control+Optimism
Neuroticism~~1*Neuroticism
Openess=~NA*Creative+Learning_Orientation
Openess~~1*Openess
Conscientiousness=~NA*Dependability+Methodical
Conscientiousness~~1*Conscientiousness
Openess~~Extraversion
Openess~~Agreeableness
Openess~~Conscientiousness
Openess~~Neuroticism
Conscientiousness~~Extraversion
Conscientiousness~~Agreeableness
Conscientiousness~~Neuroticism
Agreeableness~~Neuroticism
Agreeableness~~Extraversion

'

fit_final<-cfa(Final_model, check.gradient=FALSE, data=final_data)
summary(fit_final, standardized=TRUE, rsq=TRUE, fit.measures=TRUE)
semPaths(fit_final, "std")
rel<-reliability(fit1)
stargazer(rel, out="factors.html", summary=FALSE, type='html', rownames=TRUE, initial.zero=FALSE, digits=3, title='Reliability')












# performance<-rawdata_df[486:546]
# performance<-na.omit(performance)
# predictors<-data.frame(colMeans(data, na.rm=TRUE))
# preditors<-t(predictors)
# performance<-data.frame(colMeans(performance, na.rm=TRUE))
# performance<-t(performance)
# validity<-left_join(predictors, performance)
# validity<-t(validity)
# 
# reg<-lm(Social_Desirability~., -Social_Desirability, data=data)








