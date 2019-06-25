getwd()
setwd('D:/workspace/R_statictics/ch02')
library(ggplot2)

33%/%3 # 나눈 값의 정수로만 빼주는 아이
33%%32 # 나눈 값의 '나머지'가 나온다



str(cars)
plot(cars$speed, cars$dist,
     main="속도와 제동거리", xlab='속도(mph)', ylab='제동거리(ft)',
     pch=1, col='red')

ggplot(cars, aes(x=speed, y=dist)) +
  labs(x='속도(mph)',y='제동거리(ft)') + geom_point(colour='red')
#labs 는 x,y의 이름을 변경해준다.



str(Nile)
head(Nile)
ggplot(Nile, aes(x=Time,y=Nile))+
  geom_point()
# time Series 인 데이터를 data.frame으로 변경하기
df_nile <- as.data.frame(Nile)
head(df_nile)
# 년도를 임의의 변수에 넣어준다,
# 데이터와 연관있는 변수와 데이터를 설정
year <- c(1871:1970)
df_nile$year <- year
# ggplot으로 만든 그래프
# line그래프
ggplot(df_nile, aes(x=year, y=x)) +
  geom_line()
# point 그래프
ggplot(df_nile, aes(x=year, y=x)) +
  geom_point()

