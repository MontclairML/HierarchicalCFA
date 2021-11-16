
library(devtools)
library(readxl)
library(tidyverse)
library(dplyr)
library(psych)

rawdata_df<-read.csv("Rnd1&2Data.csv")
 

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

names<-read.csv("Item_codes_Personality_only.csv", header=FALSE)
colnames(rawdata_df)[c(26:341)] <- names$V1


#rawdata_df[,29:725] %>% is.na() %>% sum()

negatives<-select(rawdata_df[,26:n], contains(c("TN", "MN")))%>%
  mutate_all(as.numeric)

negatives<-abs(negatives-7)
rawdata_df[,26:n]<-mutate_all(rawdata_df[,26:n], as.numeric)

rawdata_df[names(negatives)] <- negatives

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

names<-rawdata_df$Q1.1
final_data<-data.frame(Achievement_Oriented, Adaptable, Analytical_Thinking, Assertive, 
                       Competitive, Creative, Dependability, Detail_Oriented, Energetic, 
                       Influencing, Initiative, Learning_Orientation, Methodical, Optimism, 
                       Persistence, Rule_Follower, Self_control, Team_Oriented, Stress_Tolerance, 
                       Cooperation, Concern_for_Others, Multitasking, Mindful, Principled, Engageable,
                       Social_Desirability, check.names=FALSE )

final_data<-na.omit(final_data)

write.csv(final_data, "final_data.csv", row.names=FALSE)
data<-read.csv("final_data.csv")

HS.Model<- 'Extraversion=~ Assertive+Competitive+Energetic+Influencing+Initiative+Persistence
Conscientiousness=~Achievement_Oriented+Dependability+Detail_Oriented+Methodical+Rule_Follower
Agreeableness=~Team_Oriented+Cooperation+Concern_for_Others
Neuroticism=~Adaptable+Optimism+Self_control+Stress_Tolerance
Openess=~Analytical_Thinking+Creative+Learning_Orientation
Other=~Principled+Multitasking+Mindful'
library(lavaan)
fit1<-cfa(HS.Model, data=data)#FIT MODEL TO DATA FROM ROUND 1
library(semPlot)#PACKAGE TO CREATE THE GRAPH BASED ON THE MODEL
semPaths(fit1, "std")
fitness1<-fitMeasures(fit1)
performance<-rawdata_df[486:546]
performance<-na.omit(performance)
predictors<-data.frame(colMeans(data, na.rm=TRUE))
preditors<-t(predictors)
performance<-data.frame(colMeans(performance, na.rm=TRUE))
performance<-t(performance)
validity<-left_join(predictors, performance)
validity<-t(validity)

cor(validity)








