setwd("D:/R/WLA")
getwd()
library(devtools)
library(readxl)
library(tidyverse)
library(dplyr)
library(psych)
library(haven)

rawdata_df<-read_sav("Rnd1&2Data.sav")
write.csv(rawdata_df, "data.csv")
rawdata_df<-read.csv("data.csv")
 

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

names<-read.csv("Item_codes2.csv", header=FALSE)
colnames(rawdata_df)[c(27:523)] <- names$V1

test<-rawdata_df[27:523]

#rawdata_df[,29:725] %>% is.na() %>% sum()

negatives<-select(rawdata_df[,26:n], contains(c("TN", "MN")))%>%
  mutate_all(as.numeric)

negatives<-abs(negatives-7)
rawdata_df[,26:n]<-mutate_all(rawdata_df[,26:n], as.numeric)

rawdata_df[names(negatives)] <- negatives
write.csv(rawdata_df,"final_data.csv")
Methodical<-select(rawdata_df[,26:n],contains(c("Org")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>%"*"(100)%>%round()


`Achievement Oriented`<-select(rawdata_df[,26:n],contains(c("Ach")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>% "*"(100)%>% round()

Dependability<-select(rawdata_df[,26:n],contains(c("Dep")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>% "*"(100)%>% round()

Influencing<-select(rawdata_df[,26:n],contains(c("Inf")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>% "*"(100)%>% round()

`Team Oriented`<-select(rawdata_df[,26:n],contains(c("Team")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>% "*"(100)%>% round()

`Rule Follower`<-select(rawdata_df[,26:n],contains(c("Cmpl")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>% "*"(100)%>% round()

`Detail Oriented`<-select(rawdata_df[,26:n],contains(c("Dtail")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>% "*"(100)%>% round()

Assertive<-select(rawdata_df[,26:n],contains(c("Asrt")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>% "*"(100)%>% round()

Competitive<-select(rawdata_df[,26:n],contains(c("Comp")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>% "*"(100)%>% round()

Initiative<-select(rawdata_df[,26:n],contains(c("Ini")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>% "*"(100)%>% round()


Energetic<-select(rawdata_df[,26:n],contains(c("Nrg")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>% "*"(100)%>% round()

Persistence<-select(rawdata_df[,26:n],contains(c("Per")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  ecdf()%>% "*"(100)%>% round()

`Analytical Thinking`<-select(rawdata_df[,26:n],starts_with(c("AT"), ignore.case=FALSE))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>% "*"(100)%>% round()


Creative<-select(rawdata_df[,26:n],contains(c("Inn")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>% "*"(100)%>% round()

`Learning Orientation`<-select(rawdata_df[,26:n],contains(c("Lo")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>% "*"(100)%>% round()

Adaptable<-select(rawdata_df[,26:n],contains(c("Adpt")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>% "*"(100)%>% round()

Optimism<-select(rawdata_df[,26:n],contains(c("Opt")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>% "*"(100)%>% round()

`Self-control`<-select(rawdata_df[,26:n],contains(c("Sctrl")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>% "*"(100)%>% round()

`Stress Tolerance`<-select(rawdata_df[,26:n],contains(c("St")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>% "*"(100)%>% round()

`Concern for Others`<-select(rawdata_df[,26:n],contains(c("CFO")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>% "*"(100)%>% round()

Cooperation<-select(rawdata_df[,26:n],contains(c("Coop")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>% "*"(100)%>% round()

Multitasking<-select(rawdata_df[,26:n],contains(c("Multi")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>% "*"(100)%>% round()

Mindful<-select(rawdata_df[,26:n],contains(c("Mind")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>% "*"(100)%>% round()

Principled<-select(rawdata_df[,26:n],contains(c("Int")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>% "*"(100)%>% round()

Engageable<-select(rawdata_df[,26:n],contains(c("Eng")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>% "*"(100)%>% round()

`Social Desirability`<-select(rawdata_df[,26:n],contains(c("SD")))%>%
  rowMeans(na.rm=TRUE)%>%
  scale()%>%
  pnorm()%>% "*"(100)%>% round()

names<-rawdata_df$Q1.1
final_data<-data.frame(names,`Achievement Oriented`, Adaptable, `Analytical Thinking`, Assertive, 
                       Competitive, Creative, Dependability, `Detail Oriented`, Energetic, 
                       Influencing, Initiative, `Learning Orientation`, Methodical, Optimism, 
                       Persistence, `Rule Follower`, `Self-control`, `Team Oriented`, `Stress Tolerance`, 
                       Cooperation, `Concern for Others`, Multitasking, Mindful, Principled, Engageable, `Social Desirability`, check.names=FALSE )

final_data<-na.omit(final_data)

write.csv(final_data, "final_data.csv", row.names=FALSE)


#write.table(excel, "item_codes", row.names=FALSE)




