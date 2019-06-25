# 연습문제를 만들기 
library(ggplot2)
########## 카이제곱 분포############
# 범위지정
set.seed(9)
t <- 10
p <- 0.1
x <- 0:10
n <- 1000
ch.1.mean <- rep(NA, n)
ch.10.mean <- rep(NA, n)
ch.20.mean <- rep(NA, n)
ch.30.mean <- rep(NA, n)

# for문 돌리기
for(i in 1:n) {
  ch.1.mean[i] <- mean(rchisq(1, t, p))
  ch.10.mean[i] <- mean(rchisq(10, t, p))
  ch.20.mean[i] <- mean(rchisq(20, t, p))
  ch.30.mean[i] <- mean(rchisq(30, t, p))
}

# 평균 구하기
options(digits=4)
c(mean(ch.1.mean), sd(ch.1.mean))
c(mean(ch.10.mean), sd(ch.10.mean))
c(mean(ch.20.mean), sd(ch.20.mean))
c(mean(ch.30.mean), sd(ch.30.mean))

# 그래프 그리기
par(mfrow=c(2,2))
# 표본 크기 : 1
hist(ch.1.mean,prob=T, xlim=c(0,20), main='표본 크기 : 1',
     col='orange', border='red')
x1 <- seq(min(ch.1.mean), max(ch.1.mean), length=1000)
y1 <- dnorm(x=x1, mean=10, sd=sqrt(20)/sqrt(1))
lines(x1,y1, lty=2, lwd=2, col='blue')

# 표본 크기 : 10
hist(ch.10.mean,prob=T, xlim=c(0,20), main='표본 크기 : 10',
     col='orange', border='red')
x2 <- seq(min(ch.10.mean), max(ch.10.mean), length=1000)
y2 <- dnorm(x=x2, mean = 10, sd=sqrt(20)/sqrt(10))
lines(x2,y2, lty=2, lwd=2, col='blue')

# 표본 크기 : 20
hist(ch.20.mean,prob=T, xlim=c(0,20), main='표본 크기 : 20',
     col='orange', border='red')
x3 <- seq(min(ch.20.mean), max(ch.20.mean), length=1000)
y3 <- dnorm(x=x3, mean=10, sd=sqrt(20)/sqrt(20))
lines(x3,y3, lty=2, lwd=2, col='blue')

# 표본 크기 : 30
hist(ch.30.mean,prob=T, xlim=c(0,30), main='표본 크기 : 30',
     col='orange', border='red')
x4 <- seq(min(ch.30.mean), max(ch.30.mean), length=1000)
y4 <- dnorm(x=x4, mean=10, sd=sqrt(20)/sqrt(30))
lines(x4,y4, lty=1, lwd=1, col='blue')


########## T분포 ############
# 범위지정
set.seed(9)
t <- 10
p <- 0.1
x <- 0:10
n <- 1000
t.1.mean <- rep(NA, n)
t.10.mean <- rep(NA, n)
t.20.mean <- rep(NA, n)
t.30.mean <- rep(NA, n)


# for문 돌리기
for(i in 1:n) {
  t.1.mean[i] <- mean(rt(1,t))
  t.10.mean[i] <- mean(rt(10,t))
  t.20.mean[i] <- mean(rt(20,t))
  t.30.mean[i] <- mean(rt(30,t))
}

# 평균 구하기
options(digits=4)
c(mean(t.1.mean), sd(t.1.mean))
c(mean(t.10.mean), sd(t.10.mean))
c(mean(t.20.mean), sd(t.20.mean))
c(mean(t.30.mean), sd(t.30.mean))


# 그래프 그리기
par(mfrow=c(2,2))
# 표본 크기 : 1
hist(t.1.mean, prob=T,  main='표본 크기 : 1',
     col='orange', border='red')
x1 <- seq(min(t.1.mean), max(t.1.mean), length=1000)
y1 <- dnorm(x=x1, mean=0, sd=sqrt(10/8)/sqrt(1))
lines(x1,y1, lty=2, lwd=2, col='blue')


# 표본 크기 : 10
hist(t.10.mean,prob=T, main='표본 크기 : 10',
     col='orange', border='red')
x2 <- seq(min(t.10.mean), max(t.10.mean), length=1000)
y2 <- dnorm(x=x2, mean=0, sd=sqrt(10/8)/sqrt(10))
lines(x2,y2, lty=2, lwd=2, col='blue')

# 표본 크기 : 20
hist(t.20.mean,prob=T, main='표본 크기 : 20',
     col='orange', border='red')
x3 <- seq(min(t.20.mean), max(t.20.mean), length=1000)
y3 <- dnorm(x=x3, mean=0, sd=sqrt(10/8)/sqrt(20))
lines(x3,y3, lty=2, lwd=2, col='blue')


# 표본 크기 : 30
hist(t.30.mean,prob=T,  main='표본 크기 : 30',
     col='orange', border='red')
x4 <- seq(min(t.30.mean), max(t.30.mean), length=1000)
y4 <- dnorm(x=x4, mean=0, sd=sqrt(10/8)/sqrt(30))
lines(x4, y4, lty=2, lwd=2, col='blue')

#################### f분포 #############
# 범위지정
 포기
par(mfrow=c(1,1))
set.seed(9)
x <- seq(0, 5, length.out = 101)
# f분포는 자유도 2개 필요
v1 <- 4
v2 <- 50
Z <- data.frame(x,v2)
ggplot(Z,aes(x, 1, 1))+geom_line()
