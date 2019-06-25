# 가설검정 

setwd('D:/workspace/R_statictics/ch02/06. 가설검정')
data <- read.csv('D:/workspace/R_statictics/ch02/06. 가설검정/data/2016.6th.csv')
str(data)

tmp <- subset(data, data$나이==7)
height.p <- tmp$X104.키

set.seed(9)
height <- height.p[sample(length(height.p),15)]
height

mean(height)
sd(height)
t.test(height, mu=1220)

## 모비율 검정

tmp <- read.table("http://www.amstat.org/publications.jse.datasets/babyboom.dat.txt", header=F)
str(data)
names(data) <- c('time','gender','weight','minutes')
tmp <- subset(data, gender==1)
weight <- tmp[[3]]

barx <- mean(weight)
s <- sd(weight)
n <- length(weight)
h0 <- 2800
( t.t <- (barx - h0) / (s / sqrt(n)))

alpha <- 0.05
 ( c.u <- qt(1-alpha, df=n-1))
(p.value <- 1-pt(t.t, df=n-1))

t.test(weight, mu=2800, alternative = 'greater')
#  몸무게가 기존보다 늘었다,라는 가설에 대한 검증을 할 수 있다.


par(mar=c(0,1,1,1))
x <- seq(-3,3, by = 0.001)
y <- dt(x, df=n-1)
plot(x,y, type='l', axes=F, ylim=c(-0.02,0.38),
     main='',xlab='t', ylab = '')
abline(h=0)

polygon(c(c.u, x[x>c.u],3), c(0,y[x>c.u],0), col=s)
text(c.u -0.02, expression(alpha==0.05), cex=0.8)
arrows(1.8, 0.18, 1.8, 0.09, length=0.05)

polygon(c(t.t, x[x>t.t], 3), c(0, y[x>t.t],0), density=20, angle=45)
text(t.t, -0.02, paste("t=", round(t.t, 3)), pos=4)
arrows(2.7, 0.08, 2.5, 0.03, length=0.05)

