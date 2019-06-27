library(ggplot2)

# mtcars (am.오토/수동) mpg차이가 통계적으로 유의한가?(t-test활용)
str(mtcars)
View(mtcars)

auto <- subset(mtcars, am==0)
manual <- subset(mtcars, am==1)

var.test(mtcars$mpg ~data$gender)
shapiro.test(auto$mpg)
shapiro.test(manual$mpg)
# 정규성을 띈다
qqnorm(auto$mpg)
qqline(auto$mpg)
# 정규성을 띄지 않는다
qqnorm(manual$mpg)
qqline(manual$mpg)

t.test(mtcars$mpg ~ mtcars$am,
       mu=0, alternative='less', var.equal=T)

# MASSPackages중 Cars93 dataframe 
# Origin USA vs non-USA Price의 평균차이의 검증
library(MASS)
str(Cars93)

shapiro.test(Cars93$Price[Cars93$Origin=="USA"])
shapiro.test(Cars93$Price[Cars93$Origin=="non-USA"])

# 정규성을 띈다
qqnorm(Cars93$Price[Cars93$Origin=="non-USA"])
qqline(Cars93$Price[Cars93$Origin=="non-USA"])

# 정규성을 띄지 않는다
qqnorm(Cars93$Price[Cars93$Origin=="USA"])
qqline(Cars93$Price[Cars93$Origin=="USA"])



## mpg
# 3-1 subcompact자동차와 midsize자동차의 고속도로(hwy)연비 검정

s <- subset(mpg,class=='subcompact')
m <- subset(mpg,class=='midsize')

shapiro.test(s$hwy)
shapiro.test(m$hwy)
t.test(s$hwy, m$hwy, alternative = "less")

# 3-2 r(일반휘발유)과 P(고급휘발유)의 도시연비(cty) 검정(colunms - fl)
r<- subset(mpg,fl=='r')
p <- subset(mpg,fl=='p')

shapiro.test(r$cty)
shapiro.test(p$cty)

t.test(r$cty, p$cty, paried=T, alternative = "less")

# 3-3 subcompact의 f(전륜구동) or r(후륜구동) 에 따른 도시연비(cty) 검정(colunms - drv)
mpg


data <- subset(mpg,class == 'subcompact')

R <- subset(data,drv=='r')
f <- subset(data,drv=='f')

shapiro.test(R$cty) 
shapiro.test(f$cty)

t.test(class$subcompact~,paried=T, alternative = "less")

t.test(R$cty~subcompact, f$cty, paried=T, alternative = "less")
