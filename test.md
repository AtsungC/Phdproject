Statistical learning on Mice moverment
================

# ML progect introduction

This progect is on animal behavior analysis. The data was retrieved form
Catwalk videos which is coordinates of head, centre and tail of a mouse
while on gait analysis. An weight-independent variable, \(\theta\), was
firt introduced into gait analysis. This proof-of-concept project is
going to apply relevent variables into machine learning algorithm. The
aims are for classification of treatment groups.

## Outline of the proccess

1.  Importing data

<!-- end list -->

  - all.mice

  - 
<!-- end list -->

2.  Calculate the rotating angle, \*\* \(\theta\)\*\*

<!-- end list -->

  - Ploting the data

<!-- end list -->

3.  Models bulding

<!-- end list -->

  - Supervised models
      - Linear regression
          - Validate coeficients (t-test)
          - Validate the model (\(R^2\), correlation, F-test)
      - Multiple regression
      - k-nesrest neighor
      - Desicion tree

## Importing data

  - The data with coordinates were imported into R.
      - all.mice (function : read.excel.sheet() )
  - The calculation of the \(\theta\)
      - all.mice with theta.T, theta.C (function :
        angle.calculation.tail() )
  - The average velocity, theta and variety of theta
      - vel.all.micev: mean velocity and mad.Y in head, centre and tail.
        mean and var of theta.C and theta.T.
  - The interval velocity
      - all.vel.summary: vel, mad.Y, in.t

## all.mice

``` r
# function for import

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

all.mice <-read.excel.sheet('NPCTG.xlsx')

all.mice <- all.mice %>% rename(c('ID.RUN'='ID'))

all.mice <- all.mice %>% 
  mutate(
    Run= str_extract(all.mice$ID.RUN,'RUN[0-9]+'),
    ID= str_extract(all.mice$ID.RUN,'[A-Z]{4,5}[0-9]{3}\\.[0-9][a-z]'),
    Group= str_extract(all.mice$ID.RUN,'^[A-Z]+')
  )
all.mice %>% select(ID,Group,Run,ID.RUN,everything())
```

    ## # A tibble: 29,379 x 8
    ##    ID         Group Run   ID.RUN         Point      t      X     Y
    ##    <chr>      <chr> <chr> <chr>          <chr>  <dbl>  <dbl> <dbl>
    ##  1 WTUT292.2e WTUT  RUN1  WTUT292.2eRUN1 Head  0      -121.  -6.83
    ##  2 WTUT292.2e WTUT  RUN1  WTUT292.2eRUN1 Head  0.0100 -117.  -7.04
    ##  3 WTUT292.2e WTUT  RUN1  WTUT292.2eRUN1 Head  0.0200 -113.  -6.89
    ##  4 WTUT292.2e WTUT  RUN1  WTUT292.2eRUN1 Head  0.0300 -111.  -6.96
    ##  5 WTUT292.2e WTUT  RUN1  WTUT292.2eRUN1 Head  0.0400 -108.  -6.68
    ##  6 WTUT292.2e WTUT  RUN1  WTUT292.2eRUN1 Head  0.0500 -104.  -6.73
    ##  7 WTUT292.2e WTUT  RUN1  WTUT292.2eRUN1 Head  0.0600 -101.  -6.43
    ##  8 WTUT292.2e WTUT  RUN1  WTUT292.2eRUN1 Head  0.0700  -98.6 -6.83
    ##  9 WTUT292.2e WTUT  RUN1  WTUT292.2eRUN1 Head  0.08    -94.8 -6.86
    ## 10 WTUT292.2e WTUT  RUN1  WTUT292.2eRUN1 Head  0.09    -90.8 -6.77
    ## # … with 29,369 more rows

## all.mice with theta

    ##           ID.RUN Point    t         X         Y  Run         ID Group
    ## 1 WTUT292.2eRUN1  Head 0.00 -120.7007 -6.832117 RUN1 WTUT292.2e  WTUT
    ## 2 WTUT292.2eRUN1  Head 0.01 -116.6882 -7.039849 RUN1 WTUT292.2e  WTUT
    ## 3 WTUT292.2eRUN1  Head 0.02 -113.3913 -6.885866 RUN1 WTUT292.2e  WTUT
    ## 4 WTUT292.2eRUN1  Head 0.03 -110.7075 -6.962225 RUN1 WTUT292.2e  WTUT
    ## 5 WTUT292.2eRUN1  Head 0.04 -107.7575 -6.682212 RUN1 WTUT292.2e  WTUT
    ## 6 WTUT292.2eRUN1  Head 0.05 -104.3316 -6.733936 RUN1 WTUT292.2e  WTUT
    ##   Tail.theta Centre.theta    theta.T    theta.C
    ## 1  -9.280046    -11.45875 -0.1619674 -0.1999930
    ## 2  -9.519586    -11.03176 -0.1661481 -0.1925405
    ## 3  -9.975006    -10.40310 -0.1740967 -0.1815684
    ## 4 -10.173734    -11.37096 -0.1775652 -0.1984606
    ## 5 -10.020763    -12.04048 -0.1748953 -0.2101460
    ## 6  -9.964424    -11.78205 -0.1739120 -0.2056357

## vel.all.mice (summary of each run)

    ## # A tibble: 6 x 10
    ## # Groups:   Group, ID [2]
    ##   ID    Run   Group Point mean.velocity  madY mean.angle.C mean.angle.T
    ##   <chr> <chr> <chr> <chr>         <dbl> <dbl>        <dbl>        <dbl>
    ## 1 NPCT… RUN11 NPCTG Head           111.  6.18        0.333        0.374
    ## 2 NPCT… RUN4  NPCTG Head           134.  7.58        0.417        0.363
    ## 3 NPCT… RUN5  NPCTG Head           114. 13.0         0.254        0.223
    ## 4 NPCT… RUN1  NPCTG Head           147.  6.12        0.218        0.187
    ## 5 NPCT… RUN11 NPCTG Head           252.  6.36        0.160        0.169
    ## 6 NPCT… RUN12 NPCTG Head           189.  5.18        0.138        0.118
    ## # … with 2 more variables: var.angle.C <dbl>, var.angle.T <dbl>

## all.vel.mice (vel in certain interval)

    ##        vel      mad.Y in.t Group         ID
    ## 1 486.3588 0.53695892 0.02  WTUT WTUT292.2e
    ## 2 288.9293 0.01046174 0.02  WTUT WTUT292.2e
    ## 3 310.5345 0.22720703 0.04  WTUT WTUT292.2e
    ## 4 521.0958 1.67644451 0.05  WTUT WTUT292.2e
    ## 5 111.6930 0.06368485 0.08  WTUT WTUT292.2e
    ## 6 206.9412 0.32136273 0.04  WTUT WTUT292.2e
