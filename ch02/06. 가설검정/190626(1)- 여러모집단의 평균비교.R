getwd()
setwd('D:/workspace/R_statictics/ch02/06. 가설검정')

# 7장, 여러 모집단의 평균 비교 검정
# 7-1. 모집단이 두개인 경우

data <- read.table('data/chapter7.txt',header=T)

boy <- subset(data, gender==1)
girl <- subset(data, gender==2)
var.test(data$weight ~data$gender)

# 정규성 테스트
shapiro.test(boy$weight)
shapiro.test(girl$weight)
# 그래프 그리기
qqnorm(girl$weight)
qqline(girl$weight)
# 정규성을 띈다 - 선근처에 모여있다

qqnorm(boy$weight)
qqline(boy$weight)
# 정규성을 띄지 않느다.- 선 근처에 있지 않다


# iris로 정규성 테스트 해보기
iriss <- subset(iris, Species=='setosa')
View(iris)
# P-value > 0.05, 정규성이 있다
shapiro.test(iriss$Sepal.Length)
qqnorm(iriss$Sepal.Length)
qqline(iriss$Sepal.Length)      

# P-value < 0.05, 정규성이 없다
shapiro.test(iriss$Petal.Width)  
qqnorm(iriss$Petal.Width)
qqline(iriss$Petal.Width) 


# 어마 무시하게 어려운 검정통계량의 **은 걍 버리고 
# var.test를 사용하자
# 영가설- 기존과 같다고 한다. 어떤 경우던지 p_value를 본다.
# 등분산성테스트
var.test(data$weight ~ data$gender)

# R을 이용한 검정 2-sample T test
t.test(data$weight~data$gender,
       mu=0, alternative='less', var.equal=T)
# 정규성가지고 있는 데이터끼리 비교를 해야 올바른 데이터가 나온다.
#  p-value = 0.06764 > 0.05이지만 차이가 없다라는 결론이 나왔다.

# 데이터 조작하기
data2 <- data.frame(gender=c(1,1), weight=c(3350, 3380))
data <- data.frame(data, data2)
data

# 예제2 식욕부진증 치료요법의 효과검정
install.packages("PairedData")
install.packages("psych")

library(PairedData)
library(psych)
library(ggplot2)
data(Anorexia)
data <- Anorexia
str(data)
summary(data)
describe(data)

n <- length(data$Prior - data$Post)
m <- mean(data$Prior - data$Post)
s <- sd(data$Prior - data$Post)
t.t <- m/(s/sqrt(n))
alpha <- 0.05
qt(alpha, df=16)
pt(t.t, df=16) # 검정통계량으로부터 구한 유의확률


t.test(data$Prior, data$Post, paired=T, alternative = "less")          
