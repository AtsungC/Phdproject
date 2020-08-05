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
 library(nlme)
 
 
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
    ifelse(i==1,T <- read_excel(filename,sheet = 1), T )
    ifelse(i+1 <= length(names.sht),T2 <- as.data.frame(read_excel(filename,sheet = i+1)),next())
    T  <- rbind.data.frame(T,T2)
  }
  return(T)
}
 #######################################
all.mice <-read.excel.sheet('NPCTG.xlsx')


################## separate ID, Run and Group

location.Run <- as.data.frame(str_locate(all.mice$ID,pattern = 'RUN'))
location.Run$Length <- str_length(all.mice$ID)
all.mice$Run <- substr(all.mice$ID,location.Run$start,location.Run$Length)
all.mice$ID <- substr(all.mice$ID,1,location.Run$start-1)
all.mice$Group <- substr(all.mice$ID,1,location.Run$start-7)

########## Createall.mice$Group for treatment arms
# 
# location.group <- str_extract(all.mice$ID,"[a-zA-Z]{1,5}")
# all.mice$Group <- location.group

############# function of calculation theta from Tail to Head and Centre to Head
angle.calculation.tail <- function(filename1){
  Head <-   filter(filename1, Point == 'Head')
  Centre <- filter(filename1,Point == "Centre")
  Tail <-   filter(filename1, Point == "Tail")
  
  calculation <- as.data.frame(Head$ID)
  
  calculation$tail.to.head.X<- Head$X-Tail$X
  calculation$centre.to.head.X <- Head$X-Centre$X
  calculation$tail.to.head.Y <-Head$Y-Tail$Y
  calculation$centre.to.head.Y <-Head$Y-Centre$Y
  calculation$Tail.horizontal.X <- Head$X-Tail$X
  calculation$Centre.horizontal.X <- Head$X-Centre$X
  calculation$horizontal.Y <- 0
  calculation$Tail.angle <-sign(Head$Y-Tail$Y)*180*(acos(calculation$tail.to.head.X * calculation$Tail.horizontal.X /abs(calculation$Tail.horizontal.X) / sqrt((calculation$tail.to.head.X)^2+(calculation$tail.to.head.Y)^2)))/(pi)
  calculation$Centre.angle <-sign(Head$Y-Centre$Y)*180*(acos(calculation$centre.to.head.X * calculation$Centre.horizontal.X /abs(calculation$Centre.horizontal.X) / sqrt((calculation$centre.to.head.X)^2+(calculation$centre.to.head.Y)^2)))/(pi)
  Head$Tail.theta <- calculation$Tail.angle
  Head$Centre.theta <- calculation$Centre.angle
  Centre$Tail.theta <- calculation$Tail.angle
  Centre$Centre.theta <- calculation$Centre.angle
  Tail$Tail.theta <- calculation$Tail.angle
  Tail$Centre.theta <- calculation$Centre.angle
  return(calculation$Tail.angle)
  
}
####################################
#all.mice data  x <- angle.calculation.tail(all.mice)
p<- data.frame(angle.calculation.tail(all.mice))




all.mice <- cbind(all.mice,angle.calculation.tail(all.mice))

colnam <- colnames(all.mice)
colnam[8] <- "theta"
colnames(all.mice)<- colnam
all.mice$ID_Run<- paste(as.character(all.mice$ID),as.character(all.mice$Run), sep="_")


b1<- apply(data.matrix(all.mice[, c(3:5, 8)]), 1, function(x){sum(is.na(x))})

all.mice<- all.mice[!b1, ] ### select records without NAs


################################################################################
# ########### summary of Velocity and Angle function 
all.mice$CircAngle <- circular(rad(all.mice$theta),type='direction')  


Average.velocity <- function(filename2){
  x <-filename2 %>% filter(Point=='Head') %>% 
    group_by(Group, ID, Run) %>% 
    summarise(Xstart=X[which(t==0)],
              Ystart=Y[which(t==0)],
              Xend = X[which(t==max(t))],
              Yend = Y[which(t==max(t))],
              mean.velocity= abs(Xstart-Xend)/max(t),
              median.Y = median(abs(Y)),
              sd.Y= sd(Y),
              sem.Y = sd.Y,
              # sem.velocity
              
              # var.velocit=sd(abs(Xstart-Xend)/max(t)),
              t = max(t),
    mean.angle = mean.circular(CircAngle, na.rm=TRUE),
    var.angle = var.circular(CircAngle, na.rm=TRUE)
    )
  
  return(x %>% select(ID,Group,Run,Xend,Xstart, mean.velocity,median.Y,
                      mean.angle,var.angle,sd.Y,sem.Y)
         )
  
}

Summary.all.mice <- Average.velocity(all.mice)



########## plot 3D x= madY, y= mean.vel and z= mean.angle

P <- plot_ly(Summary.all.mice,x = ~median.Y,y= ~mean.velocity,z= ~mean.angle, color = ~Group) %>% 
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'median.Y'),
                      yaxis = list(title = 'mean.vel'),
                      zaxis = list(title = 'mean.ang')))
Summary.all.mice %>% ggplot(aes(x=Group,y=sd.Y))+
  geom_boxplot(color=Group)+
  NULL
 ggplot(data=Summary.all.mice,aes(x=Group,y=sem.Y,color=Group))+
  geom_boxplot()+
  geom_point()+
  theme_classic()+
  NULL
Summary.all.mice %>% ggplot(aes(x=Group,y=mean.velocity))+
  geom_boxplot()+
  NULL
Summary.all.mice %>% ggplot(aes(x=Group,y=median.Y))+
  geom_boxplot()+
  NULL
Summary.all.mice %>% ggplot(aes(x=Group,y=mean.angle))+
  geom_boxplot()+
  NULL
### test it with  "NPCTG277.4c", "RUN11"
filename<- Summary.all.mice$ID[1]
run<- "RUN11"
length.interval<- 10


compute.splines.time<- function(data, num.knots=10)
  {
  n.pts<- dim(data)[1]
  vel<- with(data, sqrt(diff(X)^2 + diff(Y)^2) / diff(t))
  y<- data$Y[-n.pts]
  theta<- data$theta[-n.pts]
  t<- data$t[-n.pts]
  
  ### use the objects just defined, and XV for smoothing
  ss.vel<- smooth.spline(x=t, y=vel, nknots=num.knots) ## specify number of knots
  ss.y<- smooth.spline(x=t, y=y, nknots=num.knots) 
  ss.theta<- smooth.spline(x=t, y=theta, nknots=num.knots) 
  d2<-data.frame(t=c(ss.vel$x, ss.y$x, ss.theta$x), 
             auxY=c(ss.vel$y, ss.y$y, ss.theta$y),
             variable=rep(c('velocity','y','theta'), rep(n.pts-1,3))
             )
  invisible(d2)
}## close function computing smooth.splines

  
  #d2<- all.mice %>% filter(.$Point=='Center') %>% group_by(.$ID_Run) %>% 
  #     purrr::map(  ~ {compute.splines.time(data=.x)})
  
  

  num.knots<- 10
  d2<- data.frame()
  for (i in unique(all.mice$ID_Run))
  {
    aux.df<- all.mice[all.mice$ID_Run==i & all.mice$Point=='Centre', ]
    d0.aux<- compute.splines.time(aux.df, num.knots = num.knots)
    d0.aux<- cbind(d0.aux, ID=aux.df$ID[1], Run=aux.df$Run[1], ID_Run=i,
                   Group=aux.df$Group[1])
    d2<- rbind(d2,d0.aux) ### excludes cells with only one point
        ### this returns smooth.splines objects for three variables, indexed by 
        ### variable: theta, velocity and Y
  }
  
  ### d2 is a data.frame with variables t, auxY, variable, ID_Run
  ### NOTE that auxY is the value of velocity, y, theta
  
d2$auxY <- abs(d2$auxY)
g2<- d2 %>% ggplot(aes(t,auxY, group=ID_Run)) + 
     geom_line(aes(colour=Group), size=0.75) + 
     geom_hline(yintercept=0) + 
     theme_classic() + 
     NULL
  
g2<- g2 + facet_wrap(~ variable, scales='free') + 
     NULL

g2


d2 %>% group_by(Group,  variable) %>% summarise(avg=mean(auxY))

g3<- d2 %>% ggplot(aes(y=auxY)) + geom_boxplot(aes(Group, fill=Group))

g3 + facet_wrap(~ variable, scales='free') + theme_classic() + 
  theme(
    strip.background = element_rect(
      color="black", fill='lightgreen', size=1.5, linetype="solid"
    )
  ) + 
     NULL
  ################################
  

  for (i in unique(CUT1)) 
  {
    d1 <- d0 %>% filter(CUT==i)
    d3 <- d1 %>% summarise(
      vel= unique(
             sqrt(
               (max(X)-min(X))^2 + 
               (max(Y[which(X==max(X))])-min(Y[which(X==min(X))]))^2) ### close sqrt
                      /
                      (max(t[which(X==max(X))])-min(t[which(X==min(X))]))
             ), ### close unique
      median.Y = median(Y),
      in.t = max(t)-min(t),
      median.angle = circular::median.circular(CircAngle)
      ) ### close summarise

    ifelse(d3$in.t==0,next(),d2 <- rbind(d2,d3)) ### excludes cells with only one point
  } ## close iteration over cells
  
  return(d2)
}

### note that sum(d2$in.t) might be smaller than max(d0$t) because
### cells with only one point measured are not considered in the total amount
### of time

test0<- calculate.cells.summary('NPCUT310.1d','RUN3')



test1<- rbind(test1,test0) #WTUT279.5f

test2<- rbind(test2,test0) #NPCUT293.2c


ex2 <- function(filename){
  uni.run <- unique(all.mice$Run[which(all.mice$ID==filename)] )
  leng.run <- length(uni.run)
  dd1 <- data.frame()
  for (i in 1:leng.run) {
    dd0 <- calculate.cells.summary(filename,uni.run[i])
    dd1 <- rbind(dd1,dd0)
    
  }
  
  return(dd1)
}
WTUT292.2e <- ex2("WTUT292.2e")
WTUT281.4c <- ex2("WTUT281.4c")
WTUT279.5f<- ex2("WTUT279.5f")
WTUT <-rbind(WTUT279.5f,WTUT281.4c,WTUT292.2e)
WTUT$Group <- "WTUT"
NPCUT293.2c <- ex2("NPCUT293.2c")
NPCUT293.2e <- ex2("NPCUT293.2e")
NPCUT310.1d<- ex2("NPCUT310.1d")
NPCUT <-rbind(NPCUT293.2c,NPCUT293.2e,NPCUT310.1d)
NPCUT$Group <- "NPCUT"
NPCTG286.1g <- ex2("NPCTG286.1g")
NPCTG277.4c <- ex2("NPCTG277.4c")
NPCTG294.2c<- ex2("NPCTG294.2c")
NPCTG <-rbind(NPCTG286.1g,NPCTG277.4c,NPCTG294.2c)
NPCTG$Group <- "NPCTG"
all.vel.summary <- rbind(WTUT,NPCUT,NPCTG)

[1] "WTUT292.2e"  "WTUT281.4c"  "WTUT279.5f"  "NPCUT293.2c" "NPCUT293.2e"
[6] "NPCUT310.1d" "NPCTG286.1g" "NPCTG277.4c" "NPCTG294.2c"
############################ok ###############################################
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
# Summary.all.mice$ID <- NULL
###################################################################
Summary.all.mice$Group <- factor(Summary.all.mice$Group, levels = unique(Summary.all.mice$Group))
Summary.all.mice$mean.angle<-as.numeric(Summary.all.mice$mean.angle)
####################################################################
tree.data <- all.vel.summary %>% select(Group,vel,mad.Y,mad.ang)
model.knn  <- knn3(Group~., data=tree.data, k=4)
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
model.tr2 <- rpart(Group ~ mean.angle+mean.velocity +median.Y, data=Summary.all.mice)
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





# 26/03/2020 Linear mixed effect 
# data=Summary.all.mice
# groupedDate
Summary.all.mice <- nlme::groupedData( mean.velocity ~ 1 | ID, data=Summary.all.mice)
# this tests homogeneity of variances
bartlett.test( mean.velocity ~ Group, data=Summary.all.mice)

mod1.lme = lme( median.Y ~ Group, random=~1, data=Summary.all.mice)
mod1.lme = lme( mean.velocity ~ Group, random=~1, data=Summary.all.mice)
mod1.lme = lme( log(mean.velocity) ~ Group, random=~1, data=Summary.all.mice)


summary( mod1.lme)

library(rgl)#plot3d() be able to rotate in real-time


f <- function(x) x^4-2*x^2+3
plot(f,xlim = c(-1.5,1.5))
library(Deriv)
f.prime <- Deriv(f)
f.p.p <- Deriv(f.prime)
f.p.p
critical.p.1 <- uniroot(f.prime,interval = c(-1.5,-0.5))$root
critical.p.2 <- uniroot(f.prime,interval = c(-0.5,0.5))$root
critical.p.1 <- uniroot(f.prime,interval = c(0.5,1.5))$root

plot(f,xlim = c(-1.5,1.5),ylim=c(-2,5),col='red')
curve(f.prime,xlim = c(-1.5,1.5),col='blue',add = TRUE)
abline(h=0,lty=3)




plot(f.p.p,xlim = c(-1.5,1.5),col='green')


