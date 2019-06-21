# 3. 확률과 확률 분포
install.packages('prob')
library(prob)


# 코인을 던졌을때 나오는 패키지
tosscoin(1)
tosscoin(2)
# 확률을 알려주는 확률코드
tosscoin(2, makespace = T)

# 주사위가 나올 경우의 수 
rolldie(1)

# 한 주머니 안에 가능한 경우의 수가 나오는 조합만을 알려주는 코드 
urnsamples(1:3, size = 2)

# 복원추출 빼고 넣고 를 반복해서 나올 수 잇는 경우의수
urnsamples(1:3, size = 2,replace = T) 
# 5 C 2 
urnsamples(c(rep('R',3), rep('B',2)),size=2) 

# 3-2, 확률변수의 평균과 기대값

x <- c(0, 1, 2)
px <- c(1/4, 2/4, 1/4)
EX <- sum( x * px )
EX
# x값하고의 확률을 더하는것 (기대값)
x2 <- x^2
x2
EX2 <- sum(x2 * px); EX2 # x 제곱의 기댓값
VARX <- EX2 - EX^2 ; VARX

# 확률 분포
# 3-3 R을 이용한 이항분포 계산

n <- 6
p <- 1/3
x <- 0:n

dbinom(2, size = n, prob = p)
dbinom(4, size = n, prob = p)
# 1~6 일 때의 값을 구한다.
px <- dbinom(x, size = n, prob = p) ; px

plot(x, px, type = 's',xlab = '성공회수', ylab='확률(P[X=x])',
     main='B(6,1/3)') 

# ggplot으로 만들기
df <- data.frame(x,px)
df
ggplot(df, aes(x=x, y=px)) + geom_bar(stat='identity')

# 
plot(x, px, type = 's',xlab = '성공회수', ylab='확률(P[X=x])',
     main='B(6,1/3)', lwd=10, col = 'red')

#ggplot으로 만들기
df <- data.frame(x,px)
df
ggplot(df, aes(x=x, y=px,fill=x)) + geom_line(stat='identity')

# nomal 디스크립션을 이렇게 수식으로 써볼수 있다.
pbinom(2, n, p)
pbinom(4, n, p)
pbinom(4, n, p) -pbinom(2, n, p)
dbinom(3, n, p) +dbinom(4, n, p)

#q바이넘
qbinom(0.1, n, p)
# 누적확률의 0.1이 됐을때의 확룰

qbinom(0.5, n, p)

#랜덤함수
set.seed(1234) ; rbinom(10, n, p)

#예제 3-4, R의 분포함수를 이용한 기댓값과 분산
n <- 6
p <- 1/3
x <- 0:n
px <- dbinom(x, size = n, prob = p)
(ex <-  sum(x * px)) # ( 결과값까지 나온다.)
ex2 <- sum(x^2 *px)
(varx <- ex2 - ex^2)

n * p           # 이항 분포의 기댓값 : np
n * p * (1-p)   # 

options(digits=3)
mu <- 170 
sigma <- 6
ll <- mu - 3*sigma
ul <- mu + 3*sigma

x <- seq(ll, ul, by=0.01)
nd <- dnorm(x, mean= mu, sd= sigma)
plot(x, nd, type='1', xlab='x', ylab='P(X=x)', lwd=2, col='red')



##############

