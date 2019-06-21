getwd()
setwd('D:/workspace/R_statictics/ch02')
# 벡터 만들기
# 벡터를 단순 나열하기 c(1:3),rep(1:2, 3) seq(1:2, 2)

seq(-3, 3 , length.out = 61)

week <- factor(c(1:7), level=c(1:7))

week

seq(week ,c('일','월','화','수','목','금','토'), ordered(T))


name <- c('철수', '영희','길동')
age <- c(21 , 20, 31)
gender <- factor(c('M', 'F', 'M'))
person <- data.frame(name, age, gender)

str(person)
person
