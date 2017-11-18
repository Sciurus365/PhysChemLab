library(tidyverse)
library(latex2exp)

d <- read_csv("data1.csv")

x_point <- function(data,begin_1,end_1,begin_2,end_2){
  # 求交点
  d1 <- filter(data, t >= begin_1, t <= end_1)
  lm1 <- lm(d1$T~d1$t)
  d2 <- filter(data, t >= begin_2, t <= end_2)
  lm2 <- lm(d2$T~d2$t)
  c(lm1$coefficients[1],lm1$coefficients[2],
    lm2$coefficients[1],lm2$coefficients[2])
  lf <- matrix(c(lm1$coefficients[2],-1,lm2$coefficients[2],-1),
               nrow=2,byrow=TRUE)
  rf <- matrix(c(-lm1$coefficients[1],-lm2$coefficients[1]),
               nrow=2,byrow=TRUE)
  solution <- solve(lf,rf)
  
  #作图
  scaleFUNx <- function(x) sprintf("%d",x)
  scaleFUNy <- function(x) sprintf("%.3f",x)
  
  fy1<-function(x) (lm1$coefficients[2]*x+lm1$coefficients[1])
  fy2<-function(x) (lm2$coefficients[2]*x+lm2$coefficients[1])
  
  d1_1 <- tribble(
    ~t, ~`T`,
    begin_1-30, fy1(begin_1-30),
    solution[1,1]+30, fy1(solution[1,1]+30)
  )
  
  d2_1 <- tribble(
    ~t, ~`T`,
    end_2+30, fy2(end_2+30),
    solution[1,1]-30, fy2(solution[1,1]-30)
  )
  
  p <- ggplot(data=data,aes(x=t,y=`T`))+
    geom_point(x=solution[1,1],y=solution[2,1],shape=4,size=3)+
    geom_point(size=1)+
    geom_line(colour="black", linetype = 2)+
    geom_smooth(data = d1_1 , se=FALSE,colour="black",method="lm")+
    geom_point(data = d1 , size = 2)+
    geom_smooth(data = d2_1 , se=FALSE,colour="black",method="lm")+
    geom_point(data = d2 , size = 2)+
    
    labs(x=TeX("\\textit{t}/s"),y=TeX("\\textit{T}/K"))+
    scale_x_continuous(labels=scaleFUNx)+
    scale_y_continuous(labels=scaleFUNy)+
    theme_bw()+
    theme(legend.position = c(.9,.9),legend.background = element_blank())
  
  list(solution,p)
}

temp <- x_point(d,720,885,1095,1320)

temp[1]
temp[2]
