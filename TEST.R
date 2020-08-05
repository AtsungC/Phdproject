 library(readxl)
 library(stringr)
 library(ggplot2)
 library(gganimate)
 library(lubridate)
 library(tidyverse)
 library(plotly)
 library(circular)
 library(tree)       ### 2D
 library(rpart.plot) ### 3D
 library(plotmo)
 library(dplyr)
 library(caret)
 library(gridExtra)
 library(TSstudio)
 library(xts)
 library(forecast)
 # clear up the environment
 rm(list = ls())
 
 Sys.setenv("plotly_username"="HTchen")
 Sys.setenv("plotly_api_key"="jM63FG1qTDrXPe95oFKb")
 Sys.setenv("plotly_domain" = "http://mydomain.com")
 
##############read all sheets in one go
# identify how many sheets in excel file and combine all the sheets to T
# names.sht <- excel_sheets('NPCTG.xlsx')
# for(i in 1:length(names.sht)) 
# { 
#   ifelse(i==1,T <- read_excel('NPCTG.xlsx',sheet = 1), T )
#   ifelse(i+1 <= length(names.sht),T2 <- as.data.frame(read_excel('NPCTG.xlsx',sheet = i+1)),next())
#   T  <- rbind.data.frame(T,T2)
# }
####### read.excel function #############

read.excel.sheet <- function(filename){
  names.sht <- excel_sheets(filename)
  for(i in 1:length(names.sht)) 
  { 
    if(i==1) T <- read_excel(filename,sheet = 1)
    if(i+1 <= length(names.sht)) T2 <- read_excel(filename,sheet = i+1)
    T  <- rbind(T,T2)
  }
  return(T)
}
 


 ####################################### 
#importing data from excel
all.mice <-read.excel.sheet('NPCTG1.xlsx')

# check NA and drop_NA
any(is.na(all.mice))
which(is.na(all.mice))
filter(all.mice,is.na(ID))
dim(all.mice) 
unique(all.mice$ID)
dim(filter(all.mice,Point=='Centre'))

check <- all.mice %>%  # check the number of data of each run in three points
  group_by(ID,Point) %>% 
  dplyr::summarise(count=n())


################## 
# separate ID, Run and Group with regular expression
all.mice <- all.mice %>% rename(c('ID.RUN'='ID'))

all.mice <- all.mice %>% 
  mutate(
    Run= str_extract(all.mice$ID.RUN,'RUN[0-9]+'),
    ID= str_extract(all.mice$ID.RUN,'[A-Z]{4,5}[0-9]{3}\\.[0-9][a-z]'),
    Group= str_extract(all.mice$ID.RUN,'^[A-Z]+')
  )
all.mice %>% select(ID,Group,Run,ID.RUN,everything())
 


# location.Run <- as.data.frame(str_locate(all.mice$ID,pattern = 'RUN'))
# location.Run$Length <- str_length(all.mice$ID)
# all.mice$Run <- substr(all.mice$ID,location.Run$start,location.Run$Length)
# all.mice$ID <- substr(all.mice$ID,1,location.Run$start-1)
# all.mice$Group <- substr(all.mice$ID,1,location.Run$start-7)

########## Createall.mice$Group for treatment arms
# 
# location.group <- str_extract(all.mice$ID,"[a-zA-Z]{1,5}")
# all.mice$Group <- location.group

############# function of calculation theta from Tail to Head and Centre to Head
angle.calculation.tail <- function(filename1){

  Head <-   filter(filename1, Point == 'Head')
  Centre <- filter(filename1, Point == "Centre")
  Tail <-   filter(filename1, Point == "Tail")
  
  calculation <- Head %>% select(ID.RUN,ID,Run,Group,Point)
  
  calculation <- calculation %>% 
    mutate(
      X.tail.head = Head$X-Tail$X,
      X.cent.head = Head$X-Centre$X,
      
      Y.tail.head = Head$Y-Tail$Y,
      Y.cent.head = Head$Y-Centre$Y,
      
      X.tail.horizontal = Head$X-Tail$X,
      X.cent.horizontal = Head$X-Centre$X,
      horizontal.Y = 0
      )
  
  calculation <- calculation %>% 
    mutate(
      Tail.angel =sign(Head$Y-Tail$Y)*180*(acos(calculation$X.tail.head * calculation$X.tail.horizontal /abs(calculation$X.tail.horizontal) / sqrt((calculation$X.tail.head)^2+(calculation$Y.tail.head)^2)))/(pi),
      Centre.angel=sign(Head$Y-Centre$Y)*180*(acos(calculation$X.cent.head * calculation$X.cent.horizontal /abs(calculation$X.cent.horizontal) / sqrt((calculation$X.cent.head)^2+(calculation$ Y.cent.head)^2)))/(pi)
    )
  
  #Pts <- c('Head','Centre','Tail')
  #T2 <- data.frame()
  #for (i in Pts) {
  #  T <- as.data.frame(eval(parse(text=i)))
  #  T <- T %>% 
  #    mutate(
  #      Tail.theta = calculation$Tail.angel,
  #      Centre.theta = calculation$Centre.angel
  #    )
  # if_else(i=='Head', T2 <- T ,T2 <- rbind(T2,T))
  #}
  
  
  # Head <- Head %>% 
  #   mutate(
  #     Tail.theta = calculation$Tail.angel,
  #     Centre.theta = calculation$Centre.angel
  #   )
  # Centre <- Centre %>% 
  #   mutate(
  #     Tail.theta = calculation$Tail.angel,
  #     Centre.theta = calculation$Centre.angel
  #   )
  # Tail <- Tail %>% 
  #   mutate(
  #     Tail.theta = calculation$Tail.angel,
  #     Centre.theta = calculation$Centre.angel
  #   )
  # calculation$X.tail.head<- Head$X-Tail$X
  # calculation$X.cent.head <- Head$X-Centre$X
  # calculation$Y.tail.head <-Head$Y-Tail$Y
  # calculation$ Y.cent.head <-Head$Y-Centre$Y
  # 
  # calculation$X.tail.horizontal <- Head$X-Tail$X
  # calculation$X.cent.horizontal <- Head$X-Centre$X
  # calculation$horizontal.Y <- 0
  # calculation$Tail.angel <-sign(Head$Y-Tail$Y)*180*(acos(calculation$X.tail.head * calculation$X.tail.horizontal /abs(calculation$X.tail.horizontal) / sqrt((calculation$X.tail.head)^2+(calculation$Y.tail.head)^2)))/(pi)
  # calculation$Centre.angel <-sign(Head$Y-Centre$Y)*180*(acos(calculation$X.cent.head * calculation$X.cent.horizontal /abs(calculation$X.cent.horizontal) / sqrt((calculation$X.cent.head)^2+(calculation$ Y.cent.head)^2)))/(pi)
  # Head$Tail.theta <- calculation$Tail.angel
  # Head$Centre.theta <- calculation$Centre.angel
  # Centre$Tail.theta <- calculation$Tail.angel
  # Centre$Centre.theta <- calculation$Centre.angel
  # Tail$Tail.theta <- calculation$Tail.angel
  # Tail$Centre.theta <- calculation$Centre.angel
 # all <- rbind(Head,Centre,Tail)
  
  
# calculation <- calculation %>% mutate(
 #     theta.T = circular(rad(calculation$Tail.theta),type='direction'),
  #    theta.C = circular(rad(calculation$Centre.theta),type='direction')
   #   ) 
 calculation <- calculation %>% select(-ID.RUN,-ID,-Run,-Group,-Point)
  return(calculation)
  
}
####################################
# all.mice data with angle
all.mice.theta <- angle.calculation.tail(all.mice)
test.h <- cbind(all.mice[which(all.mice$Point=='Head'),],all.mice.theta)
test.c <- cbind(all.mice[which(all.mice$Point=='Centre'),],all.mice.theta)
test.t <- cbind(all.mice[which(all.mice$Point=='Tail'),],all.mice.theta)
all.mice.theta <- rbind(test.h,test.c,test.t)

all.mice.theta <- all.mice.theta %>% mutate(
       theta.T = circular(rad(all.mice.theta$Tail.angel),type='direction'),
       theta.C = circular(rad(all.mice.theta$Centre.angel),type='direction'))


all.mice.theta <- all.mice.theta %>% select(-X.cent.head,-Y.tail.head,-X.tail.head,-Y.cent.head,-X.tail.horizontal,-X.cent.horizontal,-horizontal.Y)
any(is.na(all.mice.theta))
names(all.mice.theta)

all.mice.theta %>%
  #filter(Point=='Head') %>% 
  ggplot(aes(x=t,y=Tail.angel,colour=Run))+
  #geom_point()+
  # geom_path()+
  # geom_point(alpha=1/10)+
  geom_smooth()+
  facet_wrap(~Group)+
  NULL

WTUT292.2eRUN1.xts <- all.mice.theta %>% filter(ID.RUN=='WTUT292.2eRUN1',Point=='Centre') %>% select(t,X,Y,theta.C,theta.T)
options(digits.sec=4)
WTUT292.2eRUN1.xts$t <- seq.POSIXt(from=as.POSIXct('1970-01-01 00:00:00'),by = 0.01,length.out = length(WTUT292.2eRUN1.xts$t))
head(WTUT292.2eRUN1.xts)
WTUT292.2eRUN1.xts <- xts(x=WTUT292.2eRUN1.xts[,2:5],order.by=WTUT292.2eRUN1.xts$t)
attr(WTUT292.2eRUN1.xts,'frequency') <-60
frequency(WTUT292.2eRUN1.xts)


USgas
acf(USgas,lag.max = 60)

acf(WTUT292.2eRUN1.xts$Y,lag.max = 60)
pacf(WTUT292.2eRUN1.xts$Y,lag.max = 60)
ts_lags(WTUT292.2eRUN1.xts$Y,lags = c(1,24,25,26))
len
partition <- ts_split(as.ts(WTUT292.2eRUN1.xts$Y),sample.out=(length(WTUT292.2eRUN1.xts$X)*0.2))
partition.t <- ts_split(as.ts(WTUT292.2eRUN1.xts$theta.T),sample.out=(length(WTUT292.2eRUN1.xts$X)*0.2))
partition.c <- ts_split(as.ts(WTUT292.2eRUN1.xts$theta.C),sample.out=(length(WTUT292.2eRUN1.xts$X)*0.2))

train <- partition$train
class(train)

train.y <- ts_to_prophet(ts.obj=train)
train.t <- ts_to_prophet(partition.t$train)
train.c <- ts_to_prophet(partition.c$train)

test.y <- ts_to_prophet(partition$test)
test.t <- ts_to_prophet(partition.t$test)
test.c <- ts_to_prophet(partition.c$test)

length(WTUT292.2eRUN1.xts$Y)*0.8
df.train <- WTUT292.2eRUN1.xts[1:(length(WTUT292.2eRUN1.xts$Y)*0.8),]
df.test <-  WTUT292.2eRUN1.xts[(length(WTUT292.2eRUN1.xts$Y)*0.8-1):length(WTUT292.2eRUN1.xts$Y),]
h2o.init(max_mem_size = '4G')
train.h <- as.h2o(df.train)
test.h <- as.h2o(df.test)
auto.md <- h2o.automl()


md <- auto.arima(partition$train)
checkresiduals(md)
fc <- forecast(md,h=length(WTUT292.2eRUN1.xts$X)*0.2)
na <- naive(partition$train,h=length(WTUT292.2eRUN1.xts$X)*0.2)
accuracy(fc,partition$test)
accuracy(na,partition$test)
test_forecast(actual = as.ts(WTUT292.2eRUN1.xts$Y),forecast.obj = fc,test = partition$test)

md_f <- auto.arima(WTUT292.2eRUN1.xts$Y)
fc_f <- forecast(md_f,h=length(WTUT292.2eRUN1.xts$X)*0.2)
plot_forecast(fc_f)

library(h2o)
h2o.init(max_mem_size = '4G')




  for ( i in 1:length(unique(all.mice.theta$ID.RUN))) {
  df0 <- all.mice.theta %>% filter(ID.RUN==unique(all.mice.theta$ID.RUN)[i-1],Point=='Centre') %>% select(t,X,Y,theta.C,theta.T)
  options(digits.sec=4)
  df0$t <- seq.POSIXt(from=as.POSIXct('1970-01-01 00:00:00'),by = 0.01,length.out = length(df0$t))
  print(unique(all.mice.theta$ID.RUN)[i])
  df0 <- xts(x=df0[,2:5],order.by=df0$t)
  attr(df0,'frequency') <- 60
  
  df1 <- all.mice.theta %>% filter(ID.RUN==unique(all.mice.theta$ID.RUN)[i],Point=='Centre') %>% select(t,X,Y,theta.C,theta.T)
  options(digits.sec=4)
  df0$t <- seq.POSIXt(from=as.POSIXct('1970-01-01 00:00:00'),by = 0.01,length.out = length(df0$t))
  print(unique(all.mice.theta$ID.RUN)[i])
  df0 <- xts(x=df0[,2:5],order.by=df0$t)
  attr(df0,'frequency') <- 60
  print(i)
  # ifelse(i%%2==0,all.xts<- merge.xts(
    # paste0(unique(all.mice.theta$ID.RUN)[i-1])=df0,
    # paste0(unique(all.mice.theta$ID.RUN)[i])=df1),next())
}

NPCTG286.1gRUN1.xts <- all.mice.theta %>% filter(ID.RUN=='NPCTG286.1gRUN1',Point=='Centre') %>% select(t,X,Y,theta.C,theta.T)
options(digits.sec=4)
NPCTG286.1gRUN1.xts$t <- seq.POSIXt(from=as.POSIXct('1970-01-01 00:00:00'),by = 0.01,length.out = length(NPCTG286.1gRUN1.xts$t))
# head(WTUT292.2eRUN1.xts)
NPCTG286.1gRUN1.xts <- xts(x=NPCTG286.1gRUN1.xts[,2:5],order.by=NPCTG286.1gRUN1.xts$t)
attr(NPCTG286.1gRUN1.xts,'frequency') <-60
plot.zoo(NPCTG286.1gRUN1.xts)


WTUT292.2eRUN1.xts <- all.mice.theta %>% filter(ID.RUN=='WTUT292.2eRUN1',Point=='Centre') %>% select(t,X,Y,theta.C,theta.T)
options(digits.sec=4)
WTUT292.2eRUN1.xts$t <- seq.POSIXt(from=as.POSIXct('1970-01-01 00:00:00'),by = 0.01,length.out = length(WTUT292.2eRUN1.xts$t))
head(WTUT292.2eRUN1.xts)
WTUT292.2eRUN1.xts <- xts(x=WTUT292.2eRUN1.xts[,2:5],order.by=WTUT292.2eRUN1.xts$t)
attr(WTUT292.2eRUN1.xts,'frequency') <-60
frequency(WTUT292.2eRUN1.xts)

WTUT292.2eRUN1.xts <- all.mice.theta %>% filter(ID.RUN=='WTUT292.2eRUN1',Point=='Centre') %>% select(t,X,Y,theta.C,theta.T)
options(digits.sec=4)
WTUT292.2eRUN1.xts$t <- seq.POSIXt(from=as.POSIXct('1970-01-01 00:00:00'),by = 0.01,length.out = length(WTUT292.2eRUN1.xts$t))
# head(WTUT292.2eRUN1.xts)
WTUT292.2eRUN1.xts <- xts(x=WTUT292.2eRUN1.xts[,2:5],order.by=WTUT292.2eRUN1.xts$t)
attr(WTUT292.2eRUN1.xts,'frequency') <-60
# frequency(WTUT292.2eRUN1.xts)

NPCTG286.1gRUN1.xts <- all.mice.theta %>% filter(ID.RUN=='NPCTG286.1gRUN1',Point=='Centre') %>% select(t,X,Y,theta.C,theta.T)
options(digits.sec=4)
NPCTG286.1gRUN1.xts$t <- seq.POSIXt(from=as.POSIXct('1970-01-01 00:00:00'),by = 0.01,length.out = length(NPCTG286.1gRUN1.xts$t))
# head(WTUT292.2eRUN1.xts)
NPCTG286.1gRUN1.xts <- xts(x=NPCTG286.1gRUN1.xts[,2:5],order.by=NPCTG286.1gRUN1.xts$t)
attr(NPCTG286.1gRUN1.xts,'frequency') <-60

WTUT292.2eRUN2.ts <- all.mice.theta %>% filter(ID.RUN=='WTUT292.2eRUN2',Point=='Centre') %>% select(t,Y)
options(digits.sec=4)
WTUT292.2eRUN2.ts <- as.ts(x=WTUT292.2eRUN2.ts[,2],order.by=WTUT292.2eRUN2.ts$t)
class(WTUT292.2eRUN2.ts)

NPCUT293.2cRUN11.xts <- all.mice.theta %>% filter(ID.RUN=='NPCUT293.2cRUN11',Point=='Centre') %>% select(t,X,Y,theta.C,theta.T)
options(digits.sec=4)
NPCUT293.2cRUN11.xts$t <- seq.POSIXt(from=as.POSIXct('1970-01-01 00:00:00'),by = 0.01,length.out = length(NPCUT293.2cRUN11.xts$t))
# head(WTUT292.2eRUN1.xts)
NPCUT293.2cRUN11.xts <- xts(x=NPCUT293.2cRUN11.xts[,2:5],order.by=NPCUT293.2cRUN11.xts$t)
attr(NPCUT293.2cRUN11.xts,'frequency') <-60

NPCUT293.2cRUN7.ts <- all.mice.theta %>% filter(ID.RUN=='NPCUT293.2cRUN7',Point=='Centre') %>% select(t,Y)
options(digits.sec=4)
NPCUT293.2cRUN7.ts<- as.ts(x=NPCUT293.2cRUN7.ts[,2],order.by=NPCUT293.2cRUN7.ts$t)
class(NPCUT293.2cRUN7.ts)

NPCUT293.2cRUN3.ts <- all.mice.theta %>% filter(ID.RUN=='NPCUT293.2cRUN3',Point=='Centre') %>% select(t,Y)
options(digits.sec=4)
NPCUT293.2cRUN3.ts<- as.ts(x=NPCUT293.2cRUN3.ts[,2],order.by=NPCUT293.2cRUN3.ts$t)
class(NPCUT293.2cRUN3.ts)
wtut.md <- auto.arima(NPCUT293.2cRUN3.ts)


wtut.md <- auto.arima(as.ts(WTUT292.2eRUN1.xts$Y))
fc.wtut.md.wt <- forecast(wtut.md,h=length(WTUT292.2eRUN2.ts))
fc.wtut.md.npc <- forecast(wtut.md,h=length(NPCUT293.2cRUN7.ts))
fc.wtut.md.npc.2 <- forecast(wtut.md,h=length(NPCUT293.2cRUN3.ts))
fc.wtut.tg <- forecast(wtut.md,h=length(NPCTG286.1gRUN1.xts$Y))
accuracy(fc.wtut.md.wt,WTUT292.2eRUN2.ts)
accuracy(fc.wtut.md.npc,NPCUT293.2cRUN7.ts)
accuracy(fc.wtut.md.npc,NPCUT293.2cRUN3.ts)
accuracy(fc.wtut.tg,as.ts(NPCTG286.1gRUN1.xts$Y))
fc.wtut.md.12 <- forecast(wtut.md,h=12)
plot_forecast(fc.wtut.md.wt)

wtut.md.sim <- arima.sim(model = list(order=c(0,1,2),ma=c(-0.083,0.4)),n=500) 
ts_plot(as.ts(WTUT292.2eRUN1.xts$Y))
ts_plot(wtut.md.sim)

NPCUT.md <- auto.arima(as.ts(NPCUT293.2cRUN11.xts$Y))
fc.npc.md.wt <- forecast(NPCUT.md,h=length(WTUT292.2eRUN2.ts))
# fc.npc.md.npc <- forecast(NPCUT.md,h=length(NPCUT293.2cRUN7.ts))
fc.npc.md.npc.2 <- forecast(NPCUT.md,h=length(NPCUT293.2cRUN3.ts))
fc.npc.tg <- forecast(NPCUT.md,h=length(NPCTG286.1gRUN1.xts$Y))
accuracy(fc.npc.md.wt,WTUT292.2eRUN2.ts)
# accuracy(fc.npc.md.npc,NPCUT293.2cRUN7.ts)
accuracy(fc.npc.md.npc,NPCUT293.2cRUN3.ts)
accuracy(fc.npc.tg,as.ts(NPCTG286.1gRUN1.xts$Y))

NPCUT.md.sim <- arima.sim(model = list(order=c(1,1,1),ar=0.66,ma=-.04),n=500) 
plot(as.ts(NPCUT293.2cRUN11.xts$Y))
ts_plot(NPCUT.md.sim)


NPCUT293.2cRUN11.xts <- all.mice.theta %>% filter(ID.RUN=='NPCUT293.2cRUN11',Point=='Centre') %>% select(t,X,Y,theta.C,theta.T)
options(digits.sec=4)
NPCUT293.2cRUN11.xts$t <- seq.POSIXt(from=as.POSIXct('1970-01-01 00:00:00'),by = 0.01,length.out = length(NPCUT293.2cRUN11.xts$t))
# head(WTUT292.2eRUN1.xts)
NPCUT293.2cRUN11.xts <- xts(x=NPCUT293.2cRUN11.xts[,2:5],order.by=NPCUT293.2cRUN11.xts$t)
attr(NPCUT293.2cRUN1.xts,'frequency') <-60
# frequency(WTUT292.2eRUN1.xts)
library(TSstudio)
library(xts)
ts_info(NPCUT293.2cRUN11.xts)
ts_info(WTUT292.2eRUN1.xts)

test.xts <- merge.xts(NPCUT293.2cRUN1=NPCUT293.2cRUN11.xts,
                      WTUT292.2eRUN1=WTUT292.2eRUN1.xts,all=c(TRUE,TRUE))

plot(decompose(as.ts(test.xts$Y)))




de <- decompose(test.xts$Y)
plot.ts(de)
plot.zoo(test.xts)

plot(decompose(as.ts(NPCUT293.2cRUN1.xts$Y)))

NPCTG294.2cRUN2.xts <- 
  all.mice.theta %>% 
  filter(ID.RUN=='NPCTG294.2cRUN2',Point=='Centre') %>%
  select(t,X,Y,theta.C,theta.T)

max(NPCTG294.2cRUN2.xts$t)

a <- plot.zoo(WTUT292.2eRUN1.xts)
b <- plot.zoo(WTUT292.2eRUN1.xts$Y)
grid.arrange(a,b,nrow=1)
# attr(WTUT292.2eRUN1.xts,'frequenct') <- 0.01
plot(decompose(as.ts(WTUT292.2eRUN1.xts$theta.C)))

periodicity(WTUT292.2eRUN1.xts)
ts_info(WTUT292.2eRUN1.xts)
library(TSstudio)

decompose(as.ts(WTUT292.2eRUN1.xts$Y))
library(forecast)
findfrequency(WTUT292.2eRUN1.xts$Y)

plot(WTUT292.2eRUN1.xts$Y,main='WTUT292.2eRUN1')
abline(v=0.042)

p <- all.mice.theta %>% filter(ID=='WTUT292.2e',Point=='Head',Run %in% c('RUN1','RUN2','RUN3'))


library(gifski)
p <- all.mice.theta %>% filter(ID.RUN=='WTUT292.2eRUN1',Point=='Centre') %>% ggplot(aes(x=X,y=Y))+
  geom_line()+
  ggtitle('WTUT292.2eRUN1')+
   transition_reveal(t)+
  theme_bw()+
  # geom_path() +
  NULL
animate(p,renderer = gifski_renderer())

all.mice.theta %>% filter(ID.RUN=='WTUT292.2eRUN1',Point=='Centre') %>% ggplot(aes(x=t,y=Y))+
  geom_line()+
  ggtitle('WTUT292.2eRUN1')+
  theme_bw()+
  NULL
all.mice.theta %>% filter(ID.RUN=='WTUT292.2eRUN1',Point=='Centre') %>% ggplot(aes(x=t,y=X))+
  geom_line()+
  ggtitle('WTUT292.2eRUN1')+
  theme_bw()+
  NULL
all.mice.theta %>% filter(ID.RUN=='WTUT292.2eRUN1',Point=='Centre') %>% 
  ggplot(aes(x=t,y=theta.T))+
  geom_line()+
  ggtitle('WTUT292.2eRUN1')+
  theme_bw()+
  NULL
all.mice.theta %>% filter(ID.RUN=='WTUT292.2eRUN1',Point=='Centre') %>% 
  ggplot(aes(x=t,y=theta.C))+
  geom_line()+
  ggtitle('WTUT292.2eRUN1')+
  theme_bw()+
  NULL

a <- all.mice.theta %>% filter(Group=='WTUT',Point=='Centre') %>% 
  ggplot(aes(x=t,y=X,col=ID.RUN))+
  geom_line()+
  ggtitle('WTUT')+
  scale_x_continuous(breaks = seq(0, 5, 1)) +
  coord_cartesian(ylim = c(-300, 300)) +
  theme(legend.position = "none")+
  # theme_bw()+
  NULL
b <- all.mice.theta %>% filter(Group=='NPCUT',Point=='Centre')  %>% 
  ggplot(aes(x=t,y=X,col=ID.RUN))+
  geom_line()+
  scale_x_continuous(breaks = seq(0, 5, 1)) +
  coord_cartesian(ylim = c(-300, 300)) +
  ggtitle('NPCUT')+
  theme(legend.position = "none")+
  # theme_bw()+
  NULL
c<- all.mice.theta %>% filter(Group=='NPCTG',Point=='Centre')  %>% 
  ggplot(aes(x=t,y=Y,col=ID.RUN))+
  geom_line()+
  scale_x_continuous(breaks = seq(0, 5, 1)) +
  coord_cartesian(ylim = c(-30, 30)) +
  ggtitle('NPCTG')+
  theme(legend.position = "none")+
  # theme_bw()+
  NULL

grid.arrange(a,b,c,nrow=1)

?grid.arrange
all.mice.theta.xts <- for (i in unique(all.mice.theta$ID.RUN)) {
  df0 <- all.mice.theta %>% filter(ID.RUN==i,Point=='Centre') %>% select(t,X,Y,theta.C,theta.T,ID.RUN)
  options(digits.sec=4)
  df0$t <- seq.POSIXt(from = as.POSIXct('1970-01-01 00:00:00'),by=0.01,length.out = length(df0$X))
  df0.xts <- xts(x=df0[,2:6],order.by = df0$t)
  ID.RUN <- unique(all.mice.theta$ID.RUN)
  ifelse(i==ID.RUN[1],xts <- df0.xts ,xts <- rbind.xts(xts,df0.xts)) 
  return(xts)
  }

plot.zoo(all.mice.theta.xts)




all.mice.theta %>% filter(ID=='NPCUT293.2c') %>% group_by(ID.RUN) %>% 
  summarise(n=n())
# test.wt.xts %>%  mutate(ma.x= rollapply(test.wt.xts$X,width = 5,FUN= MEAN/0.05),
#                            ma.y= rollapply(test.wt.xts$Y,width = 5,FUN= MEAN/0.05),
#                            ma.theta= rollapply(test.wt.xts$Tail.angel,width = 5,FUN= MEAN/0.05))
# test.ma.x <- rollapply(test.wt.xts$X,width = 5,FUN= )
# test.ma.x$vel <- test.ma.x$X/0.05
## need to use window/lag to do the rolling velocity 
test.wt.xts$lag.x <- lag.xts(test.wt.xts$X,k=-5)
test.wt.xts$vel.x <- (test.wt.xts$lag.x-test.wt.xts$X)/0.05
test.wt.xts$lag.y <- lag.xts(test.wt.xts$Y,k=-5)
test.wt.xts$vel.y <- (test.wt.xts$lag.y-test.wt.xts$Y)/0.05
test.wt.xts$lag.theta <- lag.xts(test.wt.xts$Tail.angel,k=-5)
test.wt.xts$vel.theta <- (test.wt.xts$lag.theta-test.wt.xts$Tail.angel)/0.05

test.x.vel <- test.wt.xts$X
test.x.vel$T.angle <- test.wt.xts$Tail.angel
library(dygraph)


head(test.wt.xts)
plot.zoo(test.wt.xts,main='wt gait')
plot.xts(test.wt.xts$vel.y,grid.ticks.on = 'seconds')
plot(decompose(test.wt.xts))
test.wt.h <- test.wt %>% filter



all.mice %>%
  #filter(Point=='Head') %>% 
  ggplot(aes(x=t,y=Y,colour=Run))+
  #geom_point()+
  # geom_path()+
  # geom_point(alpha=1/10)+
  geom_smooth()+
  facet_wrap(~Group)+
  NULL
all.mice.theta %>% 
  ggplot(aes(Group,theta.C))+
  geom_boxplot(aes(fill=ID),alpha=1/2)+
  # geom_point(aes(color=ID),alpha=1/10)+
  # facet_wrap(~Group)+
  theme_bw()+
  NULL

all.mice.theta %>% 
  ggplot(aes(Group,theta.C))+
  geom_boxplot(aes(fill=ID),alpha=1/2)+
  # geom_point(aes(color=ID),alpha=1/10)+
  # facet_wrap(~Group)+
  theme_bw()+
  NULL

all.mice.theta %>% 
  ggplot(aes(Group,theta.T))+
  geom_boxplot(aes(fill=Group),alpha=1/2)+
  # geom_point(aes(color=ID),alpha=1/10)+
  # facet_wrap(~Group)+
  theme_bw()+
  NULL

all.mice.theta %>% 
  ggplot(aes(Group,theta.T))+
  geom_boxplot(aes(fill=ID),alpha=1/2)+
  # geom_point(aes(color=ID),alpha=1/10)+
  # facet_wrap(~Group)+
  theme_bw()+
  NULL

names(all.mice)


all.mice.theta %>% 
  ggplot(aes(Point,theta.T))+
  geom_boxplot(aes(fill=Group),alpha=1/2)+
  # geom_point(aes(color=ID),alpha=1/10)+
  facet_wrap(~Group)+
  theme_bw()+
  NULL  

all.mice.theta %>% 
  ggplot(aes(theta.C,theta.T))+
  # geom_boxplot(aes(fill=ID),alpha=1/2)+
  geom_point(aes(color=ID),alpha=1/10)+
  # facet_wrap(~Group)+
  theme_bw()+
  NULL 

wt.r6.c <- all.mice.theta %>% filter(ID.RUN=='WTUT292.2eRUN6',Point=='Centre')
wt.r6.c %>% ggplot(aes(t,Tail.angel))+
  geom_line()+
  theme_bw()+
  NULL



# Head <-  filter(T, Point == 'Head')
# Centre <- filter(T,Point == "Centre")
# Tail <-  filter(T, Point == "Tail")
# calculation <- as.data.frame(Head$ID)
# calculation$X.tail.head<- Head$X-Tail$X
# calculation$X.cent.head <- Head$X-Centre$X
# 
# calculation$Y.tail.head <-Head$Y-Tail$Y
# calculation$ Y.cent.head <-Head$Y-Centre$Y
# 
# calculation$X.tail.horizontal <- Head$X-Tail$X
# calculation$X.cent.horizontal <- Head$X-Centre$X
# 
# calculation$horizontal.Y <- 0
# 
# calculation$Tail.angel <-sign(Head$Y-Tail$Y)*180*(acos(calculation$X.tail.head * calculation$X.tail.horizontal /abs(calculation$X.tail.horizontal) / sqrt((calculation$X.tail.head)^2+(calculation$Y.tail.head)^2)))/(pi)
# calculation$Centre.angel <-sign(Head$Y-Centre$Y)*180*(acos(calculation$X.cent.head * calculation$X.cent.horizontal /abs(calculation$X.cent.horizontal) / sqrt((calculation$X.cent.head)^2+(calculation$ Y.cent.head)^2)))/(pi)
# Head$Tail.theta <- calculation$Tail.angel
# Head$Centre.theta <- calculation$Centre.angel
# Centre$Tail.theta <- calculation$Tail.angel
# Centre$Centre.theta <- calculation$Centre.angel
# Tail$Tail.theta <- calculation$Tail.angel
# Tail$Centre.theta <- calculation$Centre.angel

################################################################################
# ########### summary of Velocity and Angle function 
# all.mice$CircAngle <- circular(rad(all.mice$theta),type='direction')



Average.velocity <- function(filename2){
  ungroup(filename2)
  points <- c('Head','Centre','Tail')
  for (i in points) {
    T <-filename2 %>% 
      group_by(Group,ID,Run) %>%
      filter(Point==i) %>% 
          dplyr::summarise(Xstart=X[which(t==0)],
                       Ystart=Y[which(t==0)],
                       Xend = X[which(t==max(t))],
                       Yend = Y[which(t==max(t))],
                       mean.velocity= abs(Xstart-Xend)/max(t),
                       madY = median(abs(Y)),
                       t = max(t),
                       Point=i,
                       mean.angle.C= mean.circular(abs(theta.C), na.rm=TRUE),
                       mean.angle.T= mean.circular(abs(theta.T), na.rm=TRUE),
                       var.angle.C = var.circular(abs(theta.C), na.rm=TRUE),
                       var.angle.T = var.circular(abs(theta.T), na.rm=TRUE)
                       # mean.angle = mean.circular(CircAngle, na.rm=TRUE),
                       # var.angle = var.circular(CircAngle, na.rm=TRUE)
                        )
   
        ifelse(i=='Head',all <- T,all <- rbind(all,T))
   
     
  }
  # 
  # h <-filename2 %>% filter(Point=='Head') %>% 
  #   group_by(Group, ID,Run) %>% 
  #   dplyr::summarise(Xstart=X[which(t==0)],
  #             Ystart=Y[which(t==0)],
  #             Xend = X[which(t==max(t))],
  #             Yend = Y[which(t==max(t))],
  #             mean.velocity= abs(Xstart-Xend)/max(t),
  #             madY = median(abs(Y)),
  #             t = max(t),
  #             Point='Head',
  #             mean.angle.C= mean.circular(abs(theta.C), na.rm=TRUE),
  #             mean.angle.T= mean.circular(abs(theta.T), na.rm=TRUE),
  #             var.angle.C = var.circular(abs(theta.C), na.rm=TRUE),
  #             var.angle.T = var.circular(abs(theta.T), na.rm=TRUE)
  #   # mean.angle = mean.circular(CircAngle, na.rm=TRUE),
  #   # var.angle = var.circular(CircAngle, na.rm=TRUE)
  #   )
  # t <-filename2 %>% filter(Point=='Tail') %>% 
  #   group_by(Group, ID,Run) %>% 
  #  dplyr::summarise(Xstart=X[which(t==0)],
  #             Ystart=Y[which(t==0)],
  #             Xend = X[which(t==max(t))],
  #             Yend = Y[which(t==max(t))],
  #             mean.velocity= abs(Xstart-Xend)/max(t),
  #             madY = median(abs(Y)),
  #             t = max(t),
  #             Point='Tail',
  #             mean.angle.C= mean.circular(abs(theta.C), na.rm=TRUE),
  #             mean.angle.T= mean.circular(abs(theta.T), na.rm=TRUE),
  #             var.angle.C = var.circular(abs(theta.C), na.rm=TRUE),
  #             var.angle.T = var.circular(abs(theta.T), na.rm=TRUE)
  #             # mean.angle = mean.circular(CircAngle, na.rm=TRUE),
  #             # var.angle = var.circular(CircAngle, na.rm=TRUE)
  #   )
  # c <- filename2 %>% filter(Point=='Centre') %>% 
  #   group_by(Group, ID,Run) %>% 
  #   dplyr::summarise(Xstart=X[which(t==0)],
  #             Ystart=Y[which(t==0)],
  #             Xend = X[which(t==max(t))],
  #             Yend = Y[which(t==max(t))],
  #             mean.velocity= abs(Xstart-Xend)/max(t),
  #             madY = median(abs(Y)),
  #             t = max(t),
  #             Point='Centre',
  #             mean.angle.C= mean.circular(abs(theta.C), na.rm=TRUE),
  #             mean.angle.T= mean.circular(abs(theta.T), na.rm=TRUE),
  #             var.angle.C = var.circular(abs(theta.C), na.rm=TRUE),
  #             var.angle.T = var.circular(abs(theta.T), na.rm=TRUE)
  #             # mean.angle = mean.circular(CircAngle, na.rm=TRUE),
  #             # var.angle = var.circular(CircAngle, na.rm=TRUE)
  #   )
   # all <- rbind(Head,Centre,Tail)

  return(all %>% 
  dplyr::select(ID,Run,
         Group,Point,
         mean.velocity,
         madY,mean.angle.C,
         mean.angle.T,var.angle.C,
         var.angle.T)
  )
  
}

Vel.all.mice <- Average.velocity(all.mice.theta)


any(is.na(Vel.all.mice))
names(Vel.all.mice)
Vel.all.micez <- drop_na(Vel.all.mice)
any(is.na(Vel.all.mice))
Vel.all.mice[which(is.na(Vel.all.mice)),]


Vel.all.mice %>% 
  ggplot(aes(Point,mean.velocity))+
  geom_boxplot()+
  geom_point(aes(color=ID))+
  facet_wrap(~Group)+
  theme_bw()+
  NULL

Vel.all.mice %>% 
  ggplot(aes(Group,var.angle.T))+
  geom_boxplot()+
  geom_point(aes(color=ID))+
  # facet_wrap(~Group)+
  theme_bw()+
  NULL
Vel.all.mice %>% 
  ggplot(aes(Group,var.angle.C))+
  geom_boxplot()+
  geom_point(aes(color=ID))+
  # facet_wrap(~Group)+
  theme_bw()+
  NULL
Vel.all.mice %>% 
  ggplot(aes(var.angle.T,var.angle.C))+
  # geom_boxplot()+
  # geom_boxplot(aes(Group,var.angle.T))+
  geom_point(aes(color=ID))+
  facet_wrap(~Group)+
  theme_bw()+
  NULL

Vel.all.mice %>% 
  ggplot(aes(mean.angle.T,mean.angle.C))+
  # geom_boxplot()+
  # geom_boxplot(aes(Group,var.angle.T))+
  geom_point(aes(color=ID))+
  facet_wrap(~Group)+
  theme_bw()+
  NULL
Vel.all.mice %>% 
  ggplot(aes(Point,madY))+
  geom_boxplot()+
  geom_point(aes(color=ID))+
  facet_wrap(~Group)+
  theme_bw()+
  NULL





Summary.all.mice <- Average.velocity(all.mice)
########## plot 3D x= madY, y= mean.vel and z= mean.angle

P <- plot_ly(Summary.all.mice,x = ~madY,y= ~mean.velocity,z= ~mean.angle, color = ~Group) %>% 
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'madY'),
                      yaxis = list(title = 'mean.vel'),
                      zaxis = list(title = 'mean.ang')))

######### segment velocity in Centre 
all.mice %>% 
  filter(Point=='Head') %>%
  group_by(ID.RUN) %>% 
  summarise(t=length(t))
# segment by time 
ex.t <- function(filename,run){
  Pts <- c('Head','Centre','Tail')
  d3 <- data.frame()
  d4 <- data.frame()
  for (j in Pts) {
    # uni.run <- unique(all.mice$Run[which(all.mice$ID==filename)])
    d0 <- all.mice %>% filter(ID==filename,Point==j,Run==run)
    N.cell <- round(max(d0$t)/0.1)
    # N.cell <- ceiling(max(d0$X)/1)
    CUT <- cut(d0$t,N.cell,labels = 1:N.cell)
    d0 <- cbind(d0,CUT)
    CUT1 <- unique(d0$CUT)
    d2 <- data.frame()
    
    for (i in 1:length(CUT1)) {
      d1 <- d0 %>% filter(CUT==CUT1[i])
      d3 <- d1 %>% dplyr::summarise(
        vel= unique(sqrt(max(X)-min(X))^2+(max(Y[which(X==max(X))])-min(Y[which(X==min(X))]))^2)/(max(t)-min(t)),
        mad.Y = mad(Y),
        in.t = max(t)-min(t),
        Point=j,
        # mad.ang = mad(theta)
      )
      d2 <- rbind(d2,d3)
    }
    d4 <- rbind(d4,d2)
    
  }
  
  return(d4)
}



ex <- function(filename,run){
  Pts <- c('Head','Centre','Tail')
  d3 <- data.frame()
  d4 <- data.frame()
  for (j in Pts) {
  # uni.run <- unique(all.mice$Run[which(all.mice$ID==filename)])
  d0 <- all.mice %>% filter(ID==filename,Point==j,Run==run)
  N.cell <- ceiling((max(d0$X)-min(d0$X))/10)
  # N.cell <- ceiling(max(d0$X)/1)
  CUT <- cut(d0$X,N.cell,labels = 1:N.cell)
  d0 <- cbind(d0,CUT)
  CUT1 <- unique(d0$CUT)
  d2 <- data.frame()
  
  for (i in 1:length(CUT1)) {
    d1 <- d0 %>% filter(CUT==CUT1[i])
    d3 <- d1 %>% dplyr::summarise(
      vel= unique(sqrt(max(X)-min(X))^2+(max(Y[which(X==max(X))])-min(Y[which(X==min(X))]))^2)/(max(t)-min(t)),
      mad.Y = mad(Y),
      in.t = max(t)-min(t),
      Point=j,
      # mad.ang = mad(theta)
    )
    ifelse(d3$in.t==0,next(),d2 <- rbind(d2,d3)) 
  }
  d4 <- rbind(d4,d2)
 
  }
  
  return(d4)
}
test0 <- ex.t('WTUT292.2e','RUN2')

test0<- ex.t('NPCUT310.1d','RUN3')
test0<- ex('NPCUT310.1d','RUN3')
any(is.na(test0))
# test0 %>% filter(vel<=500) %>% ggplot(aes(y=vel))+
#   geom_boxplot()
# test1<- rbind(test1,test0) #WTUT279.5f
# 
# test2<- rbind(test2,test0) #NPCUT293.2c
ex2 <- function(filename){
  uni.run <- unique(all.mice$Run[which(all.mice$ID==filename)] )
  # leng.run <- length(uni.run)
  dd1 <- data.frame()
  for (i in uni.run) {
    dd0 <- ex.t(filename,i)
    dd0 <- dd0 %>% mutate(Run=i)
    dd1 <- rbind(dd1,dd0)
  }
  return(dd1)
}
test0 <- ex2('NPCUT310.1d')

ex3 <- function(filename){
  ID <- unique(filename$ID)
  dd1 <- data.frame()
  for (i in ID) {
    dd0 <- ex2(i)
    dd0 <- dd0 %>% mutate(Group= str_extract(i,'^[A-Z]+'),
                          ID= i)
    dd1 <- rbind(dd1,dd0)
  }
  return(dd1)
}

all.vel.summary <- drop_na(ex3(all.mice))

all.vel.summary  %>%  ggplot(aes(x=in.t))+
  geom_histogram()+
  NULL

less.t <- all.vel.summary %>% filter(in.t<=0.08)
unique(less.t$ID)

all.mice[ID="WTUT292.2e",RUN='Run2']
all.vel.summary %>% filter(ID=="WTUT292.2e")

colnames(all.vel.summary)

any(is.na(all.vel.summary))
all.vel.summary[which(is.na(all.vel.summary)),]
all.vel.summary %>% 
  # filter(Group=='WTUT')%>%  
  ggplot(aes(Point,vel))+
  coord_cartesian(ylim = c(0,1000))+
  geom_boxplot()+
  geom_point(aes(color=ID))+
  facet_wrap(~Group)+
NULL
all.vel.summary %>%
  filter(Point=='Centre') %>%  
  ggplot(aes(vel))+
  geom_histogram()+
  facet_wrap(~Group)+
  xlim(NA,3000)+
  theme_bw()+
  NULL



 
all.vel.summary %>% 
  # filter(Group=='WTUT')%>%  
  ggplot(aes(Point,vel))+
  coord_cartesian(ylim = c(0,1000))+
  geom_point()+
  geom_boxplot()+
  facet_wrap(~Group)+
  NULL

# 
# WTUT292.2e <- ex2("WTUT292.2e")
# WTUT281.4c <- ex2("WTUT281.4c")
# WTUT279.5f<- ex2("WTUT279.5f")
# WTUT <-rbind(WTUT279.5f,WTUT281.4c,WTUT292.2e)
# WTUT$Group <- "WTUT"
# NPCUT293.2c <- ex2("NPCUT293.2c")
# NPCUT293.2e <- ex2("NPCUT293.2e")
# NPCUT310.1d<- ex2("NPCUT310.1d")
# NPCUT <-rbind(NPCUT293.2c,NPCUT293.2e,NPCUT310.1d)
# NPCUT$Group <- "NPCUT"
# NPCTG286.1g <- ex2("NPCTG286.1g")
# NPCTG277.4c <- ex2("NPCTG277.4c")
# NPCTG294.2c<- ex2("NPCTG294.2c")
# NPCTG <-rbind(NPCTG286.1g,NPCTG277.4c,NPCTG294.2c)
# NPCTG$Group <- "NPCTG"
# all.vel.summary <- rbind(WTUT,NPCUT,NPCTG)
# 
# all.vel.summary %>% ggplot(aes(Group,vel))+
#   geom_boxplot()
# 
# [1] "WTUT292.2e"  "WTUT281.4c"  "WTUT279.5f"  "NPCUT293.2c" "NPCUT293.2e"
# [6] "NPCUT310.1d" "NPCTG286.1g" "NPCTG277.4c" "NPCTG294.2c"
# ############################ok ###############################################
# WTUT279.5fRUN1 <- all.mice %>% filter(ID=='WTUT279.5f',Point=='Centre',Run=='RUN1')
# N.cell <- ceiling((max(WTUT279.5fRUN1$X)-min(WTUT279.5fRUN1$X))/10)
# CUT <- cut(WTUT279.5fRUN1$X,N.cell,labels = 1:N.cell)
# WTUT279.5fRUN1 <- cbind(WTUT279.5fRUN1,CUT)
# 
# d2 <- data.frame()
# for (i in 1:N.cell) {
#   d1 <- WTUT279.5fRUN1 %>% filter(CUT==i)
#   d3 <- d1 %>% summarise(
#     vel= ((max(X)-min(X))^2+(Y[which(X==max(X))]-Y[which(X==min(X))])^2)^(-2)/(max(t)-min(t)),
# 
#     mad.Y = mad(Y),
#     in.t = max(t)-min(t),
#     mad.ang = mad(CircAngle)
#   )
#   ifelse(d3$in.t==0,next(),d2 <- rbind(d2,d3)) 
# }
# 
colors <- c('#4AC6B7', '#1972A4', '#965F8A')
P <- plot_ly(all.vel.summary,x = ~mad.Y,y= ~vel,z= ~mad.ang,color = ~Group,colors = colors) %>% 
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'madY'),
                      yaxis = list(title = 'mean.vel'),
                      zaxis = list(title = 'mad.theta')))


P


####### cross.validation 10-fold 
# vel.all.mice
is.grouped_df(Vel.all.mice)
Vel.all.mice <- ungroup(Vel.all.mice)
Vel.all.mice.1 <- Vel.all.mice %>%filter(Point=='Head'&Group!='NPCTG')  %>% select(-Point,-ID,-Run)
Vel.all.mice.2 <- Vel.all.mice %>%filter(Point=='Head')  %>% select(-Point,-ID,-Run)
# createDatePartition only works with vector
taining.samples.Vel.all.mice <- Vel.all.mice.1$Group %>%  
  createDataPartition(p=0.9,list = FALSE)

train.Vel.all.mice <- Vel.all.mice.1[taining.samples.Vel.all.mice,]
test.Vel.all.mice <- Vel.all.mice.1[-taining.samples.Vel.all.mice,]


# all.vel.summary
is.grouped_df(all.vel.summary)
all.vel.summary.1 <- all.vel.summary %>% select(-ID)
taining.samples.all.vel.summary <- all.vel.summary.1$Group %>% 
  createDataPartition(p=0.9,list = FALSE)
train.all.vel.summary <- all.vel.summary.1[taining.samples.all.vel.summary,]
test.all.vel.summary <- all.vel.summary.1[-taining.samples.all.vel.summary,]



##### rainforest 
require(randomForest)
# data.imputed <- rfImpute(as.factor(Group)~.,data = train.Vel.all.mice)

model.1 <- randomForest(as.factor(Group)~.,data=Vel.all.mice.1,proximity=TRUE)
model.2 <- randomForest(as.factor(Group)~.,data=Vel.all.mice.2,proximity=TRUE)
model.1
model.2

oob.error.data.1 <- data.frame(
  trees=rep(1:nrow(model.1$err.rate),times=3),
  type=rep(c('OOB','WTUT','NPCUT'),each=nrow(model.1$err.rate)),
  Error=c(model.1$err.rate[,'OOB'],
          model.1$err.rate[,'WTUT'],
          model.1$err.rate[,'NPCUT'])
)
oob.error.data.2 <- data.frame(
  trees=rep(1:nrow(model$err.rate),times=4),
  type=rep(c('OOB','NPCTG','NPCUT','WTUT'),each=nrow(model.2$err.rate)),
  Error=c(model.2$err.rate[,'OOB'],
          model.2$err.rate[,'NPCTG'],
          model.2$err.rate[,'NPCUT'],
          model.2$err.rate[,'WTUT'])
)
ggplot(data = oob.error.data.1,aes(trees,Error))+
  geom_line(aes(color=type))+
  theme_bw()+
  NULL
ggplot(data = oob.error.data.2,aes(trees,Error))+
  geom_line(aes(color=type))+
  theme_bw()+
  NULL


#####
data.imputed <- rfImpute(Group~.,data = all.vel.summary.1)
any(is.na(all.vel.summary.1))
# did not work with charactor, have to convert to factor
all.vel.summary.1$Point <- as.factor(all.vel.summary.1$Point)
all.vel.summary.2 <- all.vel.summary.1 %>% filter(Group!='NPCTG')

model.3 <- randomForest(as.factor(Group)~.,data=all.vel.summary.1,proximity=TRUE)
model.4<- randomForest(as.factor(Group)~.,data=all.vel.summary.2,proximity=TRUE)
model.3
model.4





# Summary.all.mice$ID <- NULL
###################################################################
Summary.all.mice$Group <- factor(Summary.all.mice$Group, levels = unique(Summary.all.mice$Group))
Summary.all.mice$mean.angle<-as.numeric(Summary.all.mice$mean.angle)
####################################################################
tree.data <- all.vel.summary %>% select(Group,vel,mad.Y,mad.ang)
model.knn <- knn3(Group~., data=tree.data, k=4)
plot(model.knn);text(model.knn)
########## classifier in 2D  ###################################################
tree.data <- Summary.all.mice %>% select(Group,mean.velocity,mean.angle)
tree.data$ID <- NULL
# tree.data$mean.angle <-tree.data$mean.angle^2
with(tree.data, plot(mean.velocity, mean.angle,col=Group), pch=19)

tree.data %>% ggplot(aes(mean.velocity,mean.angle,color=Group))+
  geom_point()+
  theme_bw()+
  NULL


### KNN
model.knn <- knn3(Group~., data=tree.data, k=1)
plot(model.knn);text(model.knn)
decisionplot(model.knn,tree.data,class ="Group",main='KNN(1)')

model.lda <- tree(Group~.,tree.data)
plot(model.lda);text(model.lda)
decisionplot( model.lda, tree.data,
              class='Group', main='linear discriminant classifier')
ggplot(tree.data, aes(y=mean.velocity, group=Group, fill=Group)) + geom_boxplot()

tree.data[tree.data$mean.velocity>100,]


model.tr1 <- tree(Group~., data= tree.data)
plot(model.tr1);text(model.tr1)

decisionPlot( model.tr1, tree.data,
              class='Group', main='tree classifier')
########## OK
model.tr2 <- rpart(Group ~ mean.angle+mean.velocity +madY, data=Summary.all.mice)
rpart.plot::prp(model.tr2)
plotmo(model.tr2, type='prob')
 #               class='Group', main='tree classifier')


partition.tree(model.tr2,label='Group')
with(Summary.all.mice, points(mean.angle,mean.velocity,pch=19,cex=1.5))
############################################################################
ggplot(Summary.all.mice,aes(mean.velocity,mean.angle,group=Group,color=Group))+
  geom_point(size=4)+
  gg.partition.tree(model.tr1,
                    label ='Group',color='black',size=3)+
  # geom_smooth(method = 'lm',se=FALSE,size=1.5)+
  theme_classic()+
  NULL
###########################################################################









Head %>%
  group_by(Group) %>% 
  ggplot(aes(Y, Tail.theta,colour=Group))+
  # geom_point()+
  geom_path(size=0.5,alpha=0.5)+
  # stat_smooth()+
  theme_bw()+
  # transition_reveal(t)+
  facet_wrap(~Group)
  # ggtitle(label =   'Now',subtitle = 'Frame {frame} of {nframes}')

Tail %>%
  group_by(Group) %>% 
  ggplot(aes(Y, Tail.theta,colour=Group))+
  geom_point(size=1,alpha=1/10)+
  # geom_path(size=0.5,alpha=0.5)+
  # stat_smooth()+
  # geom_tile(fill=pred)+
  theme_bw()+
# transition_reveal(t)+
facet_wrap(~Group)
# ggtitle(label =   'Now',subtitle = 'Frame {frame} of {nframes}')

Tail %>%
  group_by(Group) %>% 
  ggplot(aes(Y, Tail.theta,colour=Group))+
  # geom_point()+
  geom_path(size=0.5,alpha=0.5)+
  # stat_smooth()+
  theme_bw()+
# transition_reveal(t)+
  facet_wrap(~Group)
# ggtitle(label =   'Now',subtitle = 'Frame {frame} of {nframes}')


###########################################################################



# Boxplot
ggplot(Head, aes(x=Group, y=Tail.theta))+
theme_bw()+
geom_boxplot()

############################################################################

Head %>%
  group_by(Group) %>% 
  ggplot(aes(t, Tail.theta,colour=Run))+
  # geom_point()+
  geom_path(size=0.5,alpha=0.5)+
  # stat_smooth()+
  theme_bw()+
  # transition_reveal(t)+
  facet_wrap(~ID)
# ggtitle(label =   'Now',subtitle = 'Frame {frame} of {nframes}')
  
  Head %>%
  group_by(ID,Run) %>% 
  filter(ID=='NPCUT293.2c') %>% 
  ggplot(aes(t, Tail.theta,color=Run),size=0.5)+
  #geom_point()+
  geom_path()+
  theme_bw()



Mouse.ID=NPCUT293.2c
Countzero=0
for (Q in 1:(length(Mouse.ID$Point)/3))
  sign(Mouse.ID[Q,7])!=sign(Mouse.ID[Q+1,7]))
  
  predict()
  smooth.spline()
  circula
  wesanderson::was_palette()
  
test.smooth$t <- date.frame(NPCUT293.2c$t)
test.smooth$A <- NPCUT293.2c$Angle
  
  smooth.spline()
  
  
  
#ifelse(sign(Mouse.ID[Q,7])!=sign(Mouse.ID[Q+1,7]),Countzero=Countzero+1,next())


#print(Countzero)
  
  

Head %>% 
  filter(Group=="NPCUT") %>% 
  summary()


