#### MCB & HC, 15.05.19; 
#### MCB & HC, 05.07.19;
#### MCB & HC, 23.08.19

#### libraries ####

## install.packages('animation') do this only the first time

library(tidyverse)
library(lubridate)
library(plyr)
library(readxl) ### this worked
library(plotly)
library(gganimate)
library(RColorBrewer)
library(gifski)
library(av) ### both gifski and av are renderers for animations
library(animation)

library( splancs) ### computes areas of a surface

### get a plotly account! 
Sys.setenv("plotly_username"="MarioCortinaBorja")
Sys.setenv("plotly_api_key"="q7KXL3vJh8SKLyO12Nel")
## API key for plotly = q7KXL3vJh8SKLyO12Nel
plotly.API<-
  plotly::signup(username="MarioCortinaBorja", email='m.cortina@ucl.ac.uk', save = TRUE)


#options(java.parameters = "-Xmx1024m") 
## this sorts out Error: OutOfMemoryError (Java): GC overhead limit exceeded. (IT DIDN'T!!!)
#library(XLConnect)


#### functions ####
### function read_excel_allsheets from
### https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames

read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


radians.to.degrees<- function(theta) {(theta * 180) / (pi) }

#### data ####
### names of spreadsheets
names.sheets<- c('WTUT292.2e', 'WTUT281.4c', 'WTUT279.5f', 'NPCUT293.2c', 'NPCUT293.2e', 'NPCUT310.1d',
                 'NPCTG286.1g', 'NPCTG277.4c', 'NPCTG294.2c')


test<- read_excel_allsheets("NPCTG.xlsx")
for (i in 1:9) assign(names.sheets[i], test[[i]]) ### OK
rm(test)

for (i in 1:9) 
  {
  aux<- get(names.sheets[i])
  aux$Point<- as.factor(aux$Point)
  id<- as.factor(substring(aux$ID, 1, gregexpr(pattern='RUN', aux$ID)[[1]][1] - 1) )
  run<- as.factor(substring(aux$ID, gregexpr(pattern='RUN', aux$ID)[[1]][1], nchar(aux$ID)))
  aux$Run<- run
  aux$ID<- id
  assign(names.sheets[i], aux)
}
rm(aux)

for (i in 1:9) print(summary(get(names.sheets[i])))


compute.angle<- function(mouse.name)
{
### computes the angle from Tail to Head coordinates
### mouse.name is an element of names.sheets
  df<- get(mouse.name) ### full data.frame
  num.angles<- table(df$Point, df$Run)[1,]
  angles.df<- data.frame(Run=NA, time=NA, theta=NA)
  k0<-0
  for (ind.run in levels( df$Run))
  {
    k0<-k0+1
    b0<- ind.run==df$Run
    df.aux<- df[b0,]
    tail<- df.aux$Point=='Tail'
    head<- df.aux$Point=='Head'
    Xh<- df.aux$X[head]; Xt<- df.aux$X[tail]
    Yh<- df.aux$Y[head]; Yt<- df.aux$Y[tail]
    Hypotenuse<- sqrt((Xh - Xt)^2  + (Yh - Yt)^2)
    sign.delta<- sign(Yh - Yt) 
    Delta<- abs(Xh - Xt)  
    theta<- sign.delta*radians.to.degrees(acos(Delta/Hypotenuse))
    angles.df<- rbind(angles.df, data.frame(Run=as.factor(rep(ind.run, num.angles[k0])),
                                            time=1:num.angles[k0],
                                            theta=theta)
                      )
  } ## iteration on ind.run
  angles.df$Run<- as.factor(angles.df$Run)
  invisible(angles.df[-1,]) ### assign this data.frame to the function
} ## end of function

## join angles to to original data.frame
for(i in 1:9)
{
  print(i)
  aux<- compute.angle(names.sheets[i]) ### generate angles
  df<- get(names.sheets[i]) ### full data.frame
  df$Angle<- rep(NA, dim(df)[1])
  for (ind.run in levels(aux$Run))
  {
    print(ind.run)
    i0<- aux$Run==ind.run
    i1<- df$Run==ind.run
    df$Angle[i1]<- rep(aux$theta[i0],3)
  }### end iteration over Runs
  assign(names.sheets[i], df)
}### end iterations over files

##### compute summaries for all mice ####


max.t<- max.Y<- max.X<- max.Angle <- -1000
min.t<- min.Y<- min.X<- min.Angle<-  1000


for (i in names.sheets) 
{
  print( paste("mouse_",i,sep="") ,quote=FALSE)
  mouse.data<- get(i)
  
  min.t<- ifelse(min(mouse.data$t, na.rm=TRUE) <= min.t, min(mouse.data$t, na.rm=TRUE),  min.t)
  max.t<- ifelse(max(mouse.data$t, na.rm=TRUE) >= max.t, max(mouse.data$t, na.rm=TRUE),  max.t)
  
  min.X<- ifelse(min(mouse.data$X, na.rm=TRUE) <= min.X, min(mouse.data$X, na.rm=TRUE), min.X)
  max.X<- ifelse(max(mouse.data$X, na.rm=TRUE) >= max.X, max(mouse.data$X, na.rm=TRUE), max.X)
  
  min.Y<- ifelse(min(mouse.data$Y, na.rm=TRUE) <= min.Y, min(mouse.data$Y, na.rm=TRUE), min.Y)
  max.Y<- ifelse(max(mouse.data$Y, na.rm=TRUE) >= max.Y, max(mouse.data$Y, na.rm=TRUE), max.Y)
  
  min.Angle<- ifelse(min(mouse.data$Angle, na.rm=TRUE) <= min.Angle, min(mouse.data$Angle, na.rm=TRUE), min.Angle)
  max.Angle<- ifelse(max(mouse.data$Angle, na.rm=TRUE) >= max.Angle, max(mouse.data$Angle, na.rm=TRUE), max.Angle)
 
}
  
  
  
  
  


########################### plots ####
 
 
 plot.Y_vs_X<- function(mouse.name, Point)
 {
   g1<-
   get(mouse.name) %>% filter(Point==Point) %>%
   ggplot(aes(X,Y)) +  geom_line(col='red') + theme_bw() + 
     labs(x=expression(~italic(X)), y=expression(~italic(Y))) +
     geom_hline(yintercept=0, linetype=2, size=1.25, col='gray') + 
     ggtitle(mouse.name) + geom_smooth(col='black', size=1, se=FALSE) + 
     NULL
   g1A<- g1 + facet_wrap( ~Run, ncol=3)
   g1A
 }
 
 plot.Y_vs_X(names.sheets[9], 'Tail')
 
 
plot.Angle_vs_time<- function(mouse.name)
{
 g1<- ggplot(get(mouse.name), aes(t, Angle)) +  geom_line(col='red') + theme_bw() + 
   labs(x=expression(~italic(time)), y=expression(~italic(~theta))) +
   geom_hline(yintercept=0, linetype=2, size=1.25, col='gray') + 
   ggtitle(mouse.name) + geom_smooth(col='black', size=1, se=FALSE) + 
   NULL
   g1A<- g1 + facet_wrap( ~Run, ncol=3)
   g1A
}
 
plot.Angle_vs_time(names.sheets[3])

plot.X_vs_Angle<- function(mouse.name, Point)
{
  g1<-
  get(mouse.name) %>% filter(Point==Point) %>%
  ggplot(aes(X, Angle)) +  geom_line(col='red') + theme_bw() + 
    labs(x=expression(~italic(X)), y=expression(~italic(~theta))) +
    geom_hline(yintercept=0, linetype=2, size=1.25, col='gray') + 
    geom_vline(xintercept=0, linetype=2, size=1.25, col='gray') + 
    ggtitle(paste(mouse.name, Point, sep=' - ')) + 
    geom_smooth(col='black', size=1, se=FALSE) +
    NULL
  g1A<- g1 + facet_wrap( ~Run, ncol=3)
  g1A
}

plot.X_vs_Angle(names.sheets[3], 'Tail')


plot.Y_vs_Angle<- function(mouse.name, Point)
{
  g1<-
    get(mouse.name) %>% filter(Point==Point) %>%
    ggplot(aes(Y, Angle)) +  geom_line(col='red') + theme_bw() + 
    labs(x=expression(~italic(Y)), y=expression(~italic(~theta))) +
    geom_hline(yintercept=0, linetype=2, size=1.25, col='gray') + 
    geom_vline(xintercept=0, linetype=2, size=1.25, col='gray') + 
    ggtitle(paste(mouse.name, Point, sep=' - ')) + 
    #geom_smooth(col='black', size=1, se=FALSE) + 
    NULL
   g1 + facet_grid( ~ Run)+ gganimate::transition_reveal(t)
}

anim1<- plot.Y_vs_Angle(names.sheets[9], 'Centre')

magick::image_write(animate(anim1), path='first_plot.gif')

plot(anim1)

#### summary statistics ####


################## animation plots ####


for (j in c('Centre', 'Head','Tail'))
for (i in names.sheets) 
{
  print( paste("mouse_",i,"_",j,".gif",sep="") ,quote=FALSE)
  mouse.data<- get(i)
  
  p0<- mouse.data %>% filter(Point==j) %>%
  ggplot(aes(Y, Angle, color=t)) + geom_line(size=1) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line=element_line(colour='black')) +
  labs(x=expression(~italic(Y)), y=expression(~italic(~theta))) +
  scale_y_continuous(limits=c(-30,30), breaks=seq(-30, 30, by=5)) + 
  scale_x_continuous(limits=c(-35,31), breaks=c(-30,-15,0,15,30)) + 
  geom_hline(yintercept=0, linetype=2, size=1.25, col='gray') + 
  geom_vline(xintercept=0, linetype=2, size=1.25, col='gray') + 
  # theme(legend.position="none") + 
  geom_point(size=2, color='black') + 
  scale_colour_gradientn("time (s)", colours=rainbow(6))  + 
  NULL


animation::ani.options(loop=1) ### set value of loop

p1<- p0 + transition_reveal(t) + 
  labs(title=paste(i,j,sep=' '), subtitle="Time {frame} of {nframes}") +
  geom_label(aes(y=max.Angle,  x=0, 
                 label=as.character(round(t,2)), hjust=1/2, vjust=1/2), 
             colour='black')+ 
  facet_grid(~ Run) + 
  NULL

##p2<- animate(p1, rewind=FALSE, nframes=100, fps=10, end_pause=10)

p2<- animate(p1, nframes=100, fps=10, renderer=gifski_renderer(loop=FALSE))


gganimate::anim_save(filename=paste("mouse_",i,"_",j,".gif",sep=""),
                     animation=p2, path=getwd())

} ### end function


################################### 23.08.19 ####

### consolidate data from nine mice into a data.frame ####


  all.mice<- mouse.data[1,] ### initialises

  for (i in names.sheets) 
  {
    print( paste("mouse_",i,sep="") ,quote=FALSE)
    mouse.data<- get(i)
    all.mice<- rbind( all.mice, mouse.data)
  }
  all.mice<- all.mice[-1,]
    

#### summaries for all mice

mouse.freq<- table( all.mice$ID)  
mouse.type<- c('WT','WT','WT','NPCUT','NPCUT','NPCUT', 'NPCTG', 'NPCTG','NPCTG') 
all.mice$Type<- rep( mouse.type, mouse.freq)

### each run is about 45cm long

length.catwalk<- 45


velocity<- 
   all.mice %>% group_by(Type, ID, Run) %>% summarise(velocity=length.catwalk/max(t))
  
velocity %>% group_by(Type) %>% summarise(mean.velocity=mean(velocity),
                                          sd.velocity=sd(velocity),
                                          min.velocity=min(velocity),
                                          max.velocity=max(velocity))  
### WTUT281.4c  RUN5    105. 
### WTUT281.4c  RUN2     17.8  - possibly wrong??

############################# compute areas of ellipses ####

b0<-all.mice$ID=="WTUT279.5f" & all.mice$Run=='RUN1'

mouse.exm<- all.mice[b0, ]

mouse.exm.ell<- rbind(mouse.exm, mouse.exm[1,])


convexH<- ddply(mouse.exm.ell, .(Y, Angle), 
                function(mouse.exm.ell) 
                  mouse.exm.ell[chull(mouse.exm.ell$Y, mouse.exm.ell$Angle),])
  
ellipse<- 
  ggplot(mouse.exm.ell, 
         mapping=aes(Y, Angle, colour='black', fill='gray')) +
                         geom_line() + theme_bw() + 
                         geom_polygon(data=convexH, alpha=0.2) + 
                  NULL
ellipse
ellipse