# 모집단이 세 개 이상일 경우의 평균 비교 검정

getwd()
setwd('D:/workspace/R_statictics/ch02/06. 가설검정/')
ad <- read.csv('D:/workspace/R_statictics/ch02/06. 가설검정/data/age.data.csv')

str(ad)
ad$score <- ifelse(ad$score==99,  NA, ad$score)
ad$scale <- factor(ad$scale)
ad$sex <- factor(ad$sex)
# 분석을 위한 통계량 계산과 오차제곱합 구하기
y1 <- ad$age[ad$scale == '1']
y2 <- ad$age[ad$scale == '2']
y3 <- ad$age[ad$scale == '3']

y1.mean <- mean(y1)
y2.mean <- mean(y2)
y3.mean <- mean(y3)

sse.1 <- sum((y1 - y1.mean)^2)
sse.2 <- sum((y1 - y2.mean)^2)
sse.3 <- sum((y1 - y3.mean)^2)

(sse <- sse.1 + sse.2 + sse.3)
(dfe <- (length(y1)-1)+(length(y2)-1)+(length(y3)-1))
# 처리 제곱합구하기
y.mean <- mean(ad$age)

sst.1 <- length(y1)*sum((y1.mean - y.mean)^2)                       
sst.2 <- length(y2)*sum((y2.mean - y.mean)^2)                       
sst.3 <- length(y3)*sum((y3.mean - y.mean)^2)                       

(sst <- sst.1+sst.2+sst.3)
(dft <- length(levels(ad$scale))-1)

# 전체 제곱합과 분해된 제곱합의 합 구하기
(tsq <- sum((ad$age-y.mean)^2))
(ss <- sst+sse)
# 검정통계량 구하기
mst <- sst/dft
mse <- sse/dfe
(f.t <- mst/mse)
# 기각역을 위한 임계값 구하기
alpha <- 0.05
(tol <- qf(1-alpha,2,147))
# 유의확률 구하기
(p.value <- 1-pf(f.t, 2, 147))


# 위에 있는 모든 식을 아래 세줄로 대체가능
ow <- lm(age~scale, data=ad)
oneway.test(age~scale, data=ad)
anova(ow)
