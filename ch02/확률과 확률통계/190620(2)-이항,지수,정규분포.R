# fortune 쿠키 성공율 : 90%

# P(X = a) 확률계산
# (경우의수,size=이상을 벗어날 수 없다.,prob=성공확률)
binom()
dbinom(0, size = 3, prob=0.9)
dbinom(1, size = 3, prob=0.9)
dbinom(2, size = 3, prob=0.9)
dbinom(3, size = 3, prob=0.9)

# 이하 : P(X <= a) ★ lower.tail = TRUE
# 초과 : P(X > a) ★ lower.tail =FALSE
# ~이상일때 묶어서 계산하는 함수 = P
#P(X <=1) 이하로 받을 확률
pbinom(1, size=3, prob=0.9, lower.tail = TRUE)

#1명이상 받을 확률 : P(X >=1) 확률 계산
pbinom(0, size=3, prob=0.9, lower.tail = F)
#이상이라는 기호?는 없으니 수를 줄인다/

#10명 중 8명 이상 P(X >=8) 확률 계산
pbinom(7, size=10, prob=0.9, lower.tail = F)

#graph
y <- dbinom(0:10, size =10, prob=0.5)
plot(0:10, y,
     type = 'h',
     lwd = 10,
     col = 'red',
     ylab = '확률',
     xlab = '성공확률 X',
     main = '이항분포')

# 누적그래프
z <- pbinom(0:10, size =10, prob=0.5)
plot(0:10, z,
     type = 'h',
     lwd = 10,
     col = 'red',
     ylab = '확률',
     zlab = '성공확률 X',
     main = '이항분포')


# 포아성분포(Poisson distribution)
# lambda(단위시간당 평균발생 건수): 1.5회 

#P(X = a) 확률 계산
dpois(x=0,lambda = 1.5)
dpois(x=1,lambda = 1.5)
dpois(x=2,lambda = 1.5)
dpois(x=3,lambda = 1.5)
dpois(x=4,lambda = 1.5)
dpois(x=5,lambda = 1.5)

#P(X <= 1)이하로 받을 확률
ppois(q=1,lambda = 1.5,lower.tail = T)
#2회 이상 받을 확률
ppois(q=1,lambda = 1.5,lower.tail = F)

#graph

p <-dpois(0:10,lambda = 1.5)
plot(0:10, p,
     type = 'h',
     lwd = 10,
     col = 'red',
     ylab = '확률',
     zlab = '성공확률 X',
     main = '이항분포')

# 지수분포
# lambda(rata), q=대기시간
# 람다 단위시간당 콜갯수 지수분포 != 포아성분포 

# q = X,q는 5분단위로 디폴트되어있다.
pexp(q=0.2, rate = 1.5, lower.tail=T)

#10분 P(X<=2) 이내로 받을 확률
pexp(q=2, rate = 1.5, lower.tail=T)


#95%확률로 받을수 있는 시간
qexp(p=0.95, rate=1.5, lower.tail = T)

#정규분포
# 난수함수 : rnorm(n, mean=0, sd=1)
x <- rnorm(100, 82, 7.5)
x <- sort(x)
plot(x,
     dnorm(x, 82, 7.5),
     type='l',
     main='정규분포, X~N(82, 7.5)')
abline(v=82, col='blue', lty=3)
# 확률밀도함수 : dnorm(x, mean=0, sd=1)
dnorm(75, mean=82, sd = 7.5)

# 누적분포함수(점수): pnorm(q,mean=0, sd=1,lower)
pnorm(75, mean=82, sd=7.5, lower.tail = T)
pnorm(75, mean=82, sd=7.5, lower.tail = F)
abline(v=75, col='red',lty=3)

# 누적분포함수(확률)
qnorm(0.17, mean=82, sd=7.5, lower.tail = F)




x <- seq(40, 120, length=300) ;x
y <- dnorm(x, mean=80, sd =10) ;y
plot(x,y, type='l',col='red')


x2 <- seq(65, 75, length=200) ;x
y2 <- dnorm(x2, mean=80, sd =10) ;y
polygon(c(65,x2,75),c(0,y2,0),col='gray')

pnorm(75,mean=80, sd =10)-pnorm(65, mean=80,sd=10)
