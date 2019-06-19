str(cars)
plot(cars$speed, cars$dist, 
     main = '속도와 제동거리', xlab = '속도(mph)',ylab='제동거리(ft)',
     pch=1, col='red')
jiterㄹ르 이용하면 좀 흔들리는 아이가 나온다.




Nile
str(Nile)
plot(Nile, 
     main = 'Nile강의 연도별 유량 변화', xlab='연도',ylab='유량')

plot(Nile, type='p',
     main = 'Nile강의 연도별 유량 변화', xlab='연도',ylab='유량')


df_nile <- as.data.frame(Nile)
head(df_nile)
year <- c(1871:1970)
df_nile&year <- year
ggplot(df_nile, aes(x=year, y=x)) +
  geom_line()
# ggplot은 data.frame만 가능

#time series를 dataframe으로 만들기
df_Nile <- data.frame(date = time(Nile), 
                      flood=as.matrix(Nile))

head(df_Nile)
ggplot(df_Nile, aes(x=year, y=flood)) + 
  geom_line()


