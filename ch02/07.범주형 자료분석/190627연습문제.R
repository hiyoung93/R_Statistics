### 연습문제
setwd('D:/workspace/R_statictics/ch02/07.범주형 자료분석')
# paired sample T
# 10명 선발 위약(placebo) 투여기간혈당수치 Xi, 신약(new medicine) 투여기간혈당수치 (Yi)
# 혈당 차이 유의수준 5% 비교

Xi<- c(51.4, 52.0, 45.5, 54.5, 52.3, 50.9, 52.7, 50.3, 53.8, 53.1)
Yi <- c(50.1, 51.5, 45.9, 53.1, 51.8, 50.3, 52.0, 49.9, 52.5, 53.0)
x_y <- c(1.3, 0.5, -0.4, 1.4, 0.5, 0.6, 0.7, 0.4, 1.3, 0.1) 
x_ymean <- 0.64
x_ystd <- 0.57

p_n <- data.frame(Xi=Xi, Yi=Yi)

t.test(p_n$Xi, p_n$Yi, paired=T)

# 영가설 


# 10명선발, A 신발, B신발의 일정 기간뒤 밑창이 닳은 정도의 차이비교

mA <- c(13.2, 8.2, 10.9, 14.3, 10.7, 6.6, 9.5, 10.8, 8.8, 13.3)
mB <- c(14.0, 8.8, 11.2, 14.2, 11.8, 6.4, 9.8, 11.3, 9.3, 13.6)
a-b <- c(-0.8, -0.6, -0.3, 0.1, -1.1, 0.2, -0.3, -0.5, -0.3)
abmean <- -0.41
abstd <- 0.39

a_b <- data.frame(mA=mA, mB=mB)

t.test(a_b$mA, a_b$mB, paired=T)

# 각 호수의 산소량(ppm) 측정자료이다. 호수 산소량은 같은가?
name <- c(rep('1',10),rep('2',10),rep('3',10))
lake1 <- c(5, 7, 6, 8, 6, 7, 8, 8, 6, 10)
lake1 <- c(6, 8, 9, 11, 13, 12, 10, 8, 9, 10)
lake3 <- c(14, 25, 26, 18, 19, 22, 21, 16, 20, 30)
ppm <- c(lake1,lake2,lake3)

lake <- data.frame(name=name, ppm=ppm)

anova(lm(ppm~name, data=lake))

# 7곳 도매시장의 3종 채소가격, 같은가?
name <- c(rep('a',7),rep('b',7),rep('c',7))
a <- c(15.5, 14.3, 16.3, 13.5, 15.7, 16.4, 14.7)
b <- c(14.7, 16.3, 15.5, 15.2, 16.3, 13.5, 15.4)
c <- c(15.5, 13.2, 16.5, 15.7, 15.3, 15.2, 14.8)
price <- c(a,b,c)

abc <- data.frame(name=name, price=price)

anova(lm(price~name, data=abc))
# 부적합품률 15% 시료 80개 추출후 검사결과 불량 16개 유의수준 5%적합도 검정
med <- c(16,64)
chisq.test(med,p=c(0.15,0.85))


# 음주량&흡연량 의 연관확인

drink <- matrix(c(23,31,13,21,48,23,63,159,119),3,3)
rownames(drink) <- c("반병 이상","반병 이하","못마심")
colnames(drink) <- c("1갑 이상","1갑 이하","안 피움")
addmargins(drink)
(result <- chisq.test(drink))
addmargins(result$expected)
