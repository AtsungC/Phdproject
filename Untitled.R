# all.mice dataset with theta

ggplot(all.mice[,which(Group=='WTUT')],aes(x=t,y=theta.C,colour=Group))+
  geom_line()+
  NULL


library(Quandl)
NGC <- Quandl(code='FRED/NATURALGAS',
              collapse = 'quarterly',
              type='ts',
              end_date='2018-12-31')
ts_info(NGC)
library(forecast)
findfrequency(NGC)

library(stats)
library(TSstudio)
plot.ts(NGC)
head(cycle(NGC))
str(NGC)
length(NGC)
head(time(NGC),32)
ts_info(NGC)

data("Coffee_Prices")
ts_info(Coffee_Prices)
