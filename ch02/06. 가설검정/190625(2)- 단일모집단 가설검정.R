getwd()

tmp <- read.table("D:/workspace/R_statictics/ch02/06. 가설검정/data (1)/restitution.txt",
                  header=T)
rel <- ifelse(tmp$rst < 0.4134 | tmp$rst > 0.4374,1 , 0)

n <- length(rel)
nos <- sum(rel)
sp <- nos / n
hp <- 0.1
(z <- (sp - hp) /sqrt((hp * ( 1- hp) ) / n ) )

alpha <- 0.05
(c.u <- qnorm(1-alpha))
(p.value <- 1 - pnorm(z))
prop.test(nos, n, p=0.1, alternative = 'greater', correct=F)
