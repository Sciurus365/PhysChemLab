# 本模板可用R对实验数据进行作图并报告线性拟合结果
temp <- read.csv("data.csv")
library(ggplot2)
library(reshape2)
library(dplyr)
library(latex2exp)
# 作图
## 设置横纵轴小数位数
scaleFUNx <- function(x) sprintf("%.4f",x)
scaleFUNy <- function(x) sprintf("%.5f",x)

p <- ggplot(data=temp,aes(x=x,y=y))+
  geom_point(size=3)+
  geom_smooth(se=FALSE,colour="black",method="lm")+
  labs(x=TeX("x"),y=TeX("y"))+
  scale_x_continuous(labels=scaleFUNx)+
  scale_y_continuous(labels=scaleFUNy)+
  theme_bw()+
  theme(legend.position = c(.9,.9),legend.background = element_blank())
p

# 线性拟合
lmsmr <- function(y,x){
  m <- lm(y~x)
  m <- summary(m)
  斜率 <- c(m$coefficients[2,1],m$coefficients[2,2])
  截距 <- c(m$coefficients[1,1],m$coefficients[1,2])
  R2 <- c(m$r.squared,0)
  l <- data.frame(
    截距,
    斜率,
    R2
  )
  l
}

with(temp,lmsmr(y,x))