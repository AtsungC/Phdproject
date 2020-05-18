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
WTUT292.2eRUN1 <- WTUT292.2eRUN1 %>% select('Y')
WTUT292.2eRUN1.ts <- ts(WTUT292.2eRUN1)
plot(WTUT292.2eRUN1.ts)
head(WTUT292.2eRUN1.ts)
wt.spe <-  spectrum(WTUT292.2eRUN1,log='no',spans=c(2,2),plot=TRUE)

acf(WTUT292.2eRUN1.ts) 



plot.ts(WTUT292.2eRUN1.ts$Y)
WTUT292.2eRUN1.ts.de <- decompose(WTUT292.2eRUN1.ts$Y)

