getwd()
setwd('D:/workspace/R_statictics/ch02')
# 라니의 카페 데이터 
ranicafe <- read.csv('cafedata.csv',stringsAsFactors = F)
#stringAsFactors로 하면 Factor가 chrecter가 된다
str(ranicafe)
head(ranicafe)
summary(ranicafe)
dim(ranicafe)

ranicafe$Coffees <- as.numeric(ranicafe$Coffees) #chr을 int로 변경해주었다.
ranicafe$Coffees
sort(ranicafe$Coffees)[1] #최소값
sort(ranicafe$Coffees, decreasing = T)
sort(ranicafe$Coffees, decreasing = T)[1] #최대값
min(ranicafe$Coffees,na.rm = T)#na.rm=NA값을 제거하자
max(ranicafe$Coffees,na.rm = T)


stem(ranicafe$Coffees) #최빈값
hist(ranicafe$Coffees) 


rc <- ranicafe$Coffees
weight <- 1 / length(rc)
sum(rc  * weight,na.rm = T) #평균값
mean(rc, na.rm=T)

#이상치는 제거하고 계산한다. 그래야 평소의 정확한 값이 나올수 있다.
rc[rc==max(rc, na.rm = T)] <- 480 # 모든 수학적인 함수(숫자가 들어가서 계산을 해야하는)는 na.rm을 사용한다.
mean(rc, na.rm=T)

length(rc)
median.idx <- ( 1 + length(rc) - 1)/2
sort(rc)[median.idx]


height <- c(164,168,160,166,170,172,180)
height.m <- mean(height)
h.dev <- height - height.m
h.dev
sum(h.dev)
varince <- sum(h.dev)/length(height)
standard_devition <- sqrt(varince)

mean(height)
quantile(rc, na.rm = T)
qs <- quantile(rc, na.rm = T)
qs
qs[4] - qs[2]
IQR(rc, na.rm = T)

bp <- boxplot(rc,main='커피 판매량에 대한 상자도표', axes=F, )


# 이상치
boxplot(cars$dist)
qs <- quantile(cars$dist)
qs
iqr <- qs[4] - qs[2]
iqr
upperLimit <- qs[4] + 1.5 * iqr ;upperLimit
lowerLimit <- qs[2] - 1.5 * iqr ;lowerLimit
cars$dist[cars$dist > upperLimit] #이상치
cars$dist[cars$dist < lowerLimit] #이상치
