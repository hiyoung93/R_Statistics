# Coefficients       : 절편과 계수
# adjusted R-squared : 내가 만든 회귀식이 이 전체 모형의 15.13%를 설명한다는 뜻

par(mfrow=c(2,2))
plot(out)
# 좋은 선형모델이란...(350page)

### ================================================================================
###  3. 회귀분석(2) (335Page)    ==================================================
### ================================================================================

# 1차식 (y=ax+b) -----------------------------------------------------------------
women
plot(women$height,women$weight)
fit  <- lm(weight~height, data=women)
abline(fit,col="red",lwd=2)
summary(fit) # 99% 만족한다.

cor.test(women$weight, women$height)

par(mfrow=c(2,2))
plot(fit)
# 정규성 = 2번 그림 양쪽 끝이 많이 떨어진다.
# 선형성 = 1번 곡선을 이루면 안되나 포물선이다.
# 등분산 = 3번 ok


# 2차식 (y=ax^2+bx+c) -------------------------------------------------------------
fit2 <- lm(weight~height + I(height^2), data=women) # 독립변수 추가
plot(women$height,women$weight)
lines(women$height, fitted(fit2), col="green",lwd=2)
summary(fit2) # 신장의 2차항인 I의 pr(>|t|)가 더 작다 =*** 더 잘 맞다.

par(mfrow=c(2,2))
plot(fit2)
# 정규성 = 2번 만족
# 선형성 = 1번 만족
# 등분산 = 3번 만족


# 3차식 (y=ax^3+bx^2+cx+d) --------------------------------------------------------
fit3 <- lm(weight~height + I(height^2) + I(height^3), data=women) # 독립변수 추가
plot(women$height,women$weight)
lines(women$height, fitted(fit3), col="orange",lwd=2)
summary(fit3)

par(mfrow=c(2,2))
plot(fit3)
# 정규성 = 2번 
# 선형성 = 1번 
# 등분산 = 3번 

# 그러나 그림을 눈으로 확인하여 결론을 내리기 때문에 오류가 있을 수 있고 설득력이 ㄸ러어진다.
# 그래서 AIC라는 함수를 사용한다. 여기에서 숫자가 더 작은 것이 좋다.
AIC(fit2)
AIC(fit3)


# 여기서 추가로 (Overfit 현상)을 적용해보고 생각해보자 : 검색해보기
# ex) 100개의 데이터중 overfit현상을 생각하여 30은 test 70은 train으로 사용한다.


### ================================================================================
###  4. 다중회귀분석    ===========================================================
### ================================================================================

state.x77 # 미국 50개 주에 대한 데이터
head(state.x77)
states <- as.data.frame(state.x77[,c("Murder","Population",
                                     "Illiteracy","Income","Frost")])

# 살인사건에 대한 독립변수가 될 만한 것을 골라서 회귀분석을 해보자.
fit <- lm(Murder ~ Population+Illiteracy+Income+Frost, data=states)
summary(fit)
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))

fit1 <- lm(Murder ~ .,data=states) # 점(.) : 나머지 변수 전부.
summary(fit1)

# 필요없어보이는 것을 제외하고 다시
fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
summary(fit2)

# AIC(Akaike Information Criterion) : 값이 적을수록 좋은 모델
AIC(fit1, fit2)
# 이것을 봤을때 변수가 여러개일 때는 aic가 작은 놈을 볼때까지 필료한 변수를 고르는 것이 좋다.


# 그러나 일일히 
# 1. model을 만들고 
# 2. summary를 보고 
# 3. plot을 보고 
# 4. aic 숫자를 봐야하는가... 만약 변수가 20개이상이면?? 
# 해서 이것을 자동화해주는 방법이 있다. 
# stepwise 중에 두개가 있다. Backward와 Forward
# Backward(모든 변수부터 계산을 하고 그 중에서 p값이 가장 큰놈부터 변수를 제거하면서 aic값이 180까지 자동적으로 돌린다.)
# Forward (상수항부터 변수를 하나씩 하나씩 붙이면서 자동적으로 계산한다. 마찬가지로 aic값이 180까지 자동적으로 돌린다.)

# Backward stepwise regression
step(fit1, direction = 'backward')



fit3 <- lm(Murder ~1, data = states)
step(fit3, direction = 'forward',
     scope=~ Population+Illiteracy+Income+Frost)
step(fit3, direction = 'forward',
     scope =list(upper = fit1, lower=fit3))

install.packages('leaps')
library(leaps)
subsets <- regsubsets(Murder~., data=states,
                      method = 'seqrep', nbest=4)
