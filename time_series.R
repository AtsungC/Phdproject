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
library(zoo) # for plotting ts with ggplot2
library(xts)
library(hms) 
library(datetime)
library(anytime)
library(tsibble)#time series data structure
library(tibbletime)#time series data structure
library(scatterplot3d) # x,y,z plotting, but can't be moved around

library(TSstudio)# ts_info()
library(h2o)
library(UKgrid)

# ch1
## Web API, Appication Programing Interface, capture data from specific sties
library(Quandl)
df2 <- Quandl(code='FRED/TOTALNSA', #the source = FRED, the name of the series is TOTALNSA
                type='raw',
              collapse='monthly',
              order='asc',
              end_date='2017-12-31')

## tibble = an improved version of the data.frame 
## different ways to extract data from a data.frome
str(iris)
iris$Sepal.Width[1:5]
iris[1:5,'Sepal.Width']
iris[1:5,which(colnames(iris)=='Sepal.Width')]

# ch2 : working with data and time object
## timestamp : date, time other index format depending on the series frequency
## POSIXCT, POSIXLT, Date = the ISO 8601
date <- Sys.Date()
time_ct <- Sys.time() # POSIXCT, store as a vector
time_lt <- as.POSIXlt(time_ct) # store as a list 
unclass(time_ct)
# [1] 1591304910
unclass(time_lt)
## POSIXct components : 
### wday = 0-6, 0= sunday
### isdst = daylight saving time flag
## importing data and time objects
url <- "https://raw.githubusercontent.com/RamiKrispin/Hands-On-Time-Series-Analysis-with-R/master/dates_formats.csv"
dates_df <- read.csv(url, stringsAsFactors = FALSE)
## creating a date or time index





read.excel.sheet <- function(filename){
  names.sht <- excel_sheets(filename)
  for(i in 1:length(names.sht)) 
  { 
    ifelse(i==1,T <- read_excel(filename,sheet = 1), T )
    ifelse(i+1 <= length(names.sht),T2 <- as.data.frame(read_excel(filename,sheet = i+1)),next())
    T  <- rbind.data.frame(T,T2)
  }
  return(T)
}
####################################### 
#importing data from excel
all.mice <-read.excel.sheet('NPCTG.1.xlsx')

# check NA and drop_NA
any(is.na(all.mice))
which(is.na(all.mice))
filter(all.mice,is.na(ID))
dim(all.mice) 

dim(filter(all.mice,Point=='Centre'))

check <- all.mice %>%  # check the number of data of each run in three points
  group_by(ID,Point) %>% 
  dplyr::summarise(n =n())

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


unique(all.mice$ID.RUN)

WTUT292.2eRUN1 <- all.mice %>% filter(ID.RUN=='WTUT292.2eRUN1',Point=='Centre')
WTUT292.2eRUN1 <- WTUT292.2eRUN1 %>% select('Y','t')
# scatterplot3d(WTUT292.2eRUN1$X,WTUT292.2eRUN1$Y,WTUT292.2eRUN1$t,pch=20,highlight.3d = TRUE)

diff(WTUT292.2eRUN1$t)# chechk the sampling interval, lag= 0.01 second

t.time <- as.POSIXct(WTUT292.2eRUN1$t, origin = '2020-06-02 00:00:00') # create index
# t.second <- as.second(t.time)
diff(t.time)

## create a equally spaced interval for time index
t=0
t.time1 <- vector(length=length(WTUT292.2eRUN1$t))
for (i in 1:length(WTUT292.2eRUN1$t)){
  t.time1[i] <- 0.01*i
}
t.time1 <- as.POSIXct(t.time1,origin = '2020-06-02 00:00:00')
diff(t.time1) 

# create a equally spaced interval for time indes via seq.POSIXt
t.time2 <- seq.POSIXt(from=as.POSIXct('2020-06-02'),units='seconds',by=0.01,length.out=length(WTUT292.2eRUN1$t))
diff(t.time2)
#t.time2 <- format(t.time2,format ='%Y-%m-%d %H:%M:%OS3' )
t.time2


t.time3 <- paste('2020-06-02 00:00:',WTUT292.2eRUN1$t,sep = '')
t.time3 <- as.POSIXct(t.time3,format='%Y-%m-%d %H:%M:%OS')
# t.time3 <- format(t.time3,format ='%Y-%m-%d %H:%M:%OS3' )
class(t.time3)

wtut.zoo <- zoo(WTUT292.2eRUN1$Y,order.by=t.time)
wtut.zoo1 <- zoo(WTUT292.2eRUN1$Y,order.by=t.time1)
wtut.zoo2 <- zoo(WTUT292.2eRUN1$Y,order.by=t.time2)
wtut.zoo3 <- zoo(WTUT292.2eRUN1$Y,order.by=t.time3)

ts_info(wtut.zoo2)
class(wtut.zoo2)
is.regular(wtut.zoo2)
plot(wtut.zoo2)
plot.zoo(wtut.zoo3)

wtut.ma <- ts_ma(wtut.zoo2,n_left=2,plot = TRUE)


#WTUT292.2eRUN1.ts <- ts(WTUT292.2eRUN1$Y,order.by=t.time)
WTUT292.2eRUN1.zoo <- zoo(WTUT292.2eRUN1$Y,order.by = t.time)
#WTUT292.2eRUN1.xts <- xts(WTUT292.2eRUN1$Y,order.by = t.time)

head(cycle(WTUT292.2eRUN1.zoo))
head(time(WTUT292.2eRUN1.zoo))
WTUT292.2eRUN1$t <- t.time
WTUT292.2eRUN1.tt <- WTUT292.2eRUN1 %>% tbl_time(index=t)
WTUT292.2eRUN1.tt %>%  ggplot(aes(x=t,y=Y))+
  geom_line()+
  theme_bw()+
  NULL


WTUT292.2eRUN1$t <- as.POSIXct(paste('2020-05-21 00:00:',WTUT292.2eRUN1$t,format='%Y-%m-%d %H:%M:%S'))
WTUT292.2eRUN1.ts <- ts(WTUT292.2eRUN1)


  




WTUT292.2eRUN1.ts <- ts(WTUT292.2eRUN1,start=1990,frequency=12)
WTUT292.2eRUN1.ts.dec <- decompose(WTUT292.2eRUN1.ts,type = 'mult')
plot(WTUT292.2eRUN1.ts$Y)
wt

trend <- WTUT292.2eRUN1.ts.dec$trend
ts.plot(WTUT292.2eRUN1.ts)
decompose(WTUT292.2eRUN1.ts))
head(WTUT292.2eRUN1)

wt.spe <-  WTUT292.2eRUN1$t
wt.spe <- paste('2020-05-20 00:00:',wt.spe)
wt.spe <- as_datetime(wt.spe)
secomd(wt.)
wt.spe <-  as_datetime(WTUT292.2eRUN1$t)
wt.spe.min <- second(wt.spe)
wt.spe.min
class(wt.spe.min)
plot(wt.spe)
WTUT292.2eRUN1
acf(WTUT292.2eRUN1.ts) 

?strptime
plot.ts(WTUT292.2eRUN1.ts$Y)
WTUT292.2eRUN1.ts.de <- decompose(WTUT292.2eRUN1.ts$Y)

AP <- AirPassengers
layout(1:2)
AP.ag <- aggregate(AP)
AP.cy <- cycle(AP)
# AP.ag %>%  ggplot(aes(x))+
class(AP)
plot.ts(aggregate(AP))
autoplot.zoo(aggregate(AP,FUN = mean))
boxplot(AP~cycle(AP))
