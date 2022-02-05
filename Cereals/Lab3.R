data <- read.csv("cereal.csv")
summary(data)


# Залежна змінна rating
Y <- data$rating

####(A)####
# Незалежна змінна x1 - калорій на порцію
x1 <- data$calories
mod1 <- lm(Y ~ x1)
summod1<-summary(mod1)
summary(mod1)

# діаграма розсіювання та регресійна лінія 
plot(x1, Y, xlab="x", ylab = "y")
abline(mod1, col=2)

summod1$r.squared 
sum(summod1$residuals^2)

var(x1)
var(Y)

hist(x1)
hist(Y)

plot(summod1$residuals)
mean(summod1$residuals)
var(summod1$residuals)
hist(summod1$residuals)

plot(mod1,1)
plot(mod1,2)
plot(mod1,3)
plot(mod1$residuals, type = "o")
# Незалежна змінна x2 - грамів жиру
x2 <- data$fat
mod2 <- lm(Y ~ x2)
summod2<-summary(mod2)
summary(mod2)


plot(x2, Y, xlab="x", ylab = "y")
abline(mod2, col=2)


summod2$r.squared 
sum(summod2$residuals^2)

var(x2)
var(Y)

hist(x2)
hist(Y)

plot(summod2$residuals)
mean(summod2$residuals)
var(summod2$residuals)
hist(summod2$residuals)

plot(mod2,1)
plot(mod2,2)
plot(mod2,3)
plot(mod2$residuals, type = "o")
# Незалежна змінна x3 - грам цукрів
x3 <- data$sugars
mod3 <- lm(Y ~ x3)
summod3<-summary(mod3)
summary(mod3)


plot(x3, Y, xlab="x", ylab = "y")
abline(mod3, col=2)


summod3$r.squared 
sum(summod3$residuals^2)

var(x3)
var(Y)

hist(x3)
hist(Y)

plot(summod3$residuals)
mean(summod3$residuals)
var(summod3$residuals)
hist(summod3$residuals)

plot(mod3,1)
plot(mod3,2)
plot(mod3,3)
plot(mod3$residuals, type = "o")

#### (B) ####

x4 <- data$protein
x5 <- data$weight

m1 <- lm(Y ~ x1+x2+x3+x4+x5)
sumM1 <- summary(m1)
sumM1

sum((Y - m1$coefficients[1] - m1$coefficients[2] * x1 - m1$coefficients[3] * x2- m1$coefficients[4] * x3- m1$coefficients[5] * x4- m1$coefficients[6] * x5)^2)

sum(m1$residuals^2)


sqrt(sum(m1$residuals^2)/m1$df.residual)
sumM1$sigma

#### (C) ####

m2 <- lm(Y ~ x1+x2+x3+x4)
summary(m2)

#### (D) ####
car::compareCoefs(m1, m2)
