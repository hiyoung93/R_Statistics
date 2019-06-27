# 동질성검정

setwd('D:/workspace/R_statictics/ch02/07.범주형 자료분석')

sns.c <- read.csv('D:/workspace/R_statictics/ch02/07.범주형 자료분석/snsbyage.csv')

str(sns.c)

sns.c <- transform(sns.c, age.c = 
                     factor(age, levels=c(1, 2, 3), 
                            labels=c("20대", "30대", "40대")))

sns.c <- transform(sns.c, service.c = 
                     factor(service, levels=c("F", "T", "K", "C", "E"), 
                            ordered=TRUE))

c.tab <- table(sns.c$age.c, sns.c$service.c)
c.tab
a.n <- margin.table(c.tab, margin=1)
s.n <- margin.table(c.tab, margin=2)
s.p <- s.n / margin.table(c.tab)
expected <- a.n %*% t(s.p)

o.e <- c.tab-expected
t.t <- sum((o.e)^2 / expected)    # 검정 통계량

qchisq(0.95, df=8)

1-pchisq(t.t, df=8)               # p-value

chisq.test(c.tab)

# chisq.test에 숨겨진 값들
result <- chisq.test(c.tab) 
names(result)
result$expected
str(result)
result$p.value

addmargins(result$expected)

# 예제-3. 성별에 따른 대학원 입학 여부의 독립성 검정
data(UCBAdmissions)
UCBAdmissions
ucba.tab <- apply(UCBAdmissions, c(1, 2), sum)
ucba.tab
round(prop.table(ucba.tab, margin=2) * 100, 1)

# 독립성 검정
(a.n <- margin.table(ucba.tab, margin=1))
(g.n <- margin.table(ucba.tab, margin=2))

(a.p <- a.n / margin.table(ucba.tab))
(g.p <- g.n / margin.table(ucba.tab))

(expected <- margin.table(ucba.tab) * (a.p %*% t(g.p)))
addmargins( expected )

## chi-square statistic
o.e <- (ucba.tab - expected)^2 / expected
addmargins(o.e)

chisq.t <- sum(o.e)           # 검정 통계량
chisq.t
qchisq(0.95, df=1)
1-pchisq(chisq.t, df=1)       # p-value

chisq.test(ucba.tab)

## continuity correction
o.e2 <- (abs(ucba.tab - expected)-0.5)^2 / expected
sum(o.e2)

chisq.test(ucba.tab, correct=FALSE) 

