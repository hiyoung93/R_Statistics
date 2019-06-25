set.seed(1)
x <- rchisq(n = 1, df = 1)
y <- rchisq(x,n=1,df=1)
hist(x,prob=T, xlim=c(-10,10),main="df = 1",
     col='cyan', border='blue')

set.seed(2)
x <- rchisq(n = 10, df = 10)
y <- rchisq(x,n=10,df=10)
hist(x,prob=T, xlim=c(-10,10),main="df = 10",
     col='darkblue', border='cyan')

set.seed(3)
x <- rchisq(n = 1, df = 20)
y <- rchisq(x,n=20,df=20)
hist(x,y, xlim=c(-10,10),,main="df = 20",
     col='darkblue', border='cyan',breaks=20)

