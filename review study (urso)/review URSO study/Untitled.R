library(tidyverse)
library(dplyr)
library(lubridate)
library(anytime)
library(readxl)
library(survminer)#survival analysis
library(ggplot2)
rm(list=ls())
# animal work from march 2019 (pilot study)
# 
  ID=c('879.1c','890.1c','890.1f','889.1b','889.1d','889.1f','889.1h','895.1d','895.1e','895.1f','896.1a','896.1b','896.1c','905.1b','905.1c','906.1a')
  D.O.B= c('Saturday, March 2, 2019','Thursday, March 7, 2019',
           'Thursday, March 7, 2019', 'Wednesday, March 6, 2019','Wednesday, March 6, 2019'
           ,'Wednesday, March 6, 2019','Wednesday, March 6, 2019','Friday, March 15, 2019'
           ,'Friday, March 15, 2019' ,'Friday, March 15, 2019','Thursday, March 21, 2019'
           ,'Thursday, March 21, 2019' ,'Thursday, March 21, 2019' ,'Friday, April 12, 2019' ,'Friday, April 12, 2019' ,'Friday, April 12, 2019')
ID <- c('879.1c','890.1c','890.1f','889.1b','889.1d','889.1f','889.1h','895.1d','895.1e','895.1f','896.1a','896.1b','896.1c','905.1b','905.1c','906.1a')
D.O.B <-  c('Saturday, March 2, 2019','Thursday, March 7, 2019','Thursday, March 7, 2019'
         ,'Wednesday, March 6, 2019','Wednesday, March 6, 2019','Wednesday, March 6, 2019','Wednesday, March 6, 2019'
         ,'Friday, March 15, 2019','Friday, March 15, 2019','Friday, March 15, 2019' ,'Thursday, March 21, 2019','Thursday, March 21, 2019','Thursday, March 21, 2019','Friday, April 12, 2019','Friday, April 12, 2019','Friday, April 12, 2019')
Gender <- c('F','M', 'F', 'M','F','F','F','M','F','F','M', 'M','F', 'M','F','M')
D.O.D <- c('28/5/19','22.05.19','30/5/19', '28.03.19','25/5/19','30/5/19','24/5/19','18.07.19','18.07.19','18.07.19','25.07.19','25.07.19','25.07.19','14.08.19','14.08.19', '14.08.19')
Group <- c('NPCUT','NPCUT','NPCUT','URSO','URSO','URSO','URSO','GENE','GENE','GENE','GENE','GENE','GENE','COMB','COMB','COMB')

mice.2019 <- as.data.frame(cbind(ID,D.O.B,D.O.D,Gender,Group))
mice.2019$Gender <- str_replace(mice.2019$Gende,'M','1')# MALE =1, FEMALE=2
mice.2019$Gender <- str_replace(mice.2019$Gende,'F','2')
mice.2019$D.O.B <- mdy(mice.2019$D.O.B)
mice.2019$D.O.D <- dmy(mice.2019$D.O.D)
mice.2019$status <- rep(2,length(mice.2019$D.O.D))# 2 = dead, 1=live
mice.2019$Survival <- mice.2019$D.O.D-mice.2019$D.O.B
mice.2019 <- mice.2019[which(mice.2019$Survival>=23),]
length(unique(mice.2019$ID))#16 (exclude the mouse <= 22 days)
#avg.mean
mice.2019 %>%group_by(Group) %>% 
  summarise(avg.days=round(mean(Survival),2),
            n=n())

# survival analysis with plot
require('survival')
sur.analysis <- survfit(Surv(Survival,status)~Group,data=mice.2019)
ggsurvplot(sur.analysis,data=mice.2019,pval=T)#pval = show the p-vale

# behavior 07519466078
##  weight 

mice.2019.w <- read_xlsx('comb thx data.xlsx',sheet = 4)
length(unique(mice.2019.w$ID)) #15
mice.2019.w$AGE <- as.numeric(mice.2019.w$DATE-mice.2019.w$DOB)
mice.2019.w <- mice.2019.w %>% arrange(desc(AGE))
mice.2019.w %>% 
  ggplot(aes(x=AGE,y=Weight,Group=ID,color=Group))+
  geom_line()+
  theme_bw()+
  NULL

## rear / central rear 

mice.2019.r <- read_xlsx('comb thx data.xlsx',sheet = 3)
length(unique(mice.2019.r$ID))#15
mice.2019.r$AGE <- as.numeric(mice.2019.r$DATE-mice.2019.r$DOB)
REAR <- mice.2019.r %>% select(ID,)na.omit()
mice.2019.r <- mice.2019.r %>% mutate(
  score=rowSums(mice.2019.r[,8:13])
)


mice.2019.r %>% 
  ggplot(aes(x=AGE,y=REAR,Group=ID,color=GROUP))+
  geom_point()+
  #geom_line()+
  theme_bw()+
  NULL

mice.2019.r %>% 
  ggplot(aes(x=AGE,y=`CENTRE REAR`,Group=ID,color=GROUP))+
  geom_point()+
  #geom_line()+
  theme_bw()+
  NULL

mice.2019.r %>% 
  ggplot(aes(x=AGE,y=score,Group=ID,color=GROUP))+
  geom_point()+
  geom_line()+
  theme_bw()+
  NULL

# Cholestrol test

Group <- c('GENE','GENE','GENE','WTUT','WTUT','NPCUT','NPCUT','COMB','COMB','COMB')
Cholestorl.c <- c('4.288323699','4.162715655','4.828727615', '0.857475423','0.703852107','3.280381356',
                  '3.582066421','1.934920969','1.765603597','2.005995591')
mice.2019.cho <- data.frame(cbind(Group,Cholestorl.c))
mice.2019.cho$Cholestorl.c <- as.numeric(mice.2019.cho$Cholestorl.c)

mice.2019.cho %>% ggplot(aes(x=Group,y=Cholestorl.c,fill=Group))+
  geom_bar(stat="identity")+
  theme_minimal()+
  scale_fill_brewer(palette="Dark2")+
  labs(title = 'Cholesterol concetration',
       y='cholesterol/protein (µg/mg)')

## statistical test the difference 





review.t <- matrix(NA,dimnames = list(c('NPC','URSO','GENE','COMB'),c('Group','Survial','Weight','(Centre)Rearing','Tremor','Gait')))




