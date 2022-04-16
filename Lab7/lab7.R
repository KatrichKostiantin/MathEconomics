data <- read.table(file = "cereal.csv", header = TRUE, sep = ",")
data$name <- NULL
data$mfr <- NULL
data$type <- NULL

#### (A) ####
mean(data$fat)
#1.012987

data$D <- as.factor(data$fat > mean(data$fat))

mod <- lm(rating ~ D + protein, data = data)
summary(mod)

#### (B) ####

modAll <- lm(rating ~ .-fat, data = data)
modBIC <- MASS::stepAIC(modAll, k = log(nrow(data)))

##### (C) #####

Y <- data$rating
X <- data$sugars

# m1

m1 <- lm(Y~X)
summary(m1)

# m2-m4
m234 <- lm(log(Y) ~ X)
summary(m234)

# m5
#додали 1 до Х бо є нулі
m5 <- lm(log(Y) ~ log(X+1))
summary(m5)

# m6
#додали 1 до Х бо є нулі
m6 <- lm(Y ~ I(1/(X+1)))
summary(m6)

# m7
m7 <- lm(Y ~ I(X^2))
summary(m7)

# m8
m8 <- lm(Y ~ I(X^3))
summary(m8)

# m9
m9 <- lm(Y ~ I(X^(1/2)))
summary(m9)

# m10
m10 <- lm(Y ~ I(exp(X)))
summary(m10)

# m11
m11 <- lm(Y ~ I(exp(-X)))
summary(m11)

# m12
#додали 1 до Х бо є нулі
x <- X+1
m12 <- lm(Y ~ I(x^3-log(abs(x))+2^x))
summary(m12)
         
#### (D)####

mod1 <- lm(Y ~ X)
plot(X,Y, xlab = "sugars", ylab = "rating")
abline(coef = mod1$coefficients, col = 2)

mod2 <- lm(Y ~ poly(X, degree = 2, raw = TRUE))
d <- seq(0, 15, length.out = 77)
lines(d, predict(mod2, new = data.frame(X = d)), col = 3)

mod3 <- lm(Y ~ poly(X, degree = 3, raw = TRUE))
lines(d, predict(mod3, new = data.frame(X = d)), col = 4)

mod4 <- lm(Y ~ poly(X, degree = 4, raw = TRUE))
lines(d, predict(mod4, new = data.frame(X = d)), col = 5)

mod5 <- lm(Y ~ poly(X, degree = 5, raw = TRUE))
lines(d, predict(mod5, new = data.frame(X = d)), col = 6)

mod10 <- lm(Y ~ poly(X, degree = 10, raw = TRUE))
lines(d, predict(mod10, new = data.frame(X = d)), col = 7)

BIC(mod1, mod2, mod3, mod4, mod5, mod10)


#### (E) ####

mod1 <- lm(Y ~ X)
plot(X,Y, xlab = "sugars", ylab = "rating")
abline(coef = mod1$coefficients, col = 2)

mod2 <- lm(Y ~ poly(X, degree = 2))
d <- seq(0, 15, length.out = 77)
lines(d, predict(mod2, new = data.frame(X = d)), col = 3)

mod3 <- lm(Y ~ poly(X, degree = 3))
lines(d, predict(mod3, new = data.frame(X = d)), col = 4)

mod4 <- lm(Y ~ poly(X, degree = 4))
lines(d, predict(mod4, new = data.frame(X = d)), col = 5)

mod5 <- lm(Y ~ poly(X, degree = 5))
lines(d, predict(mod5, new = data.frame(X = d)), col = 6)

mod10 <- lm(Y ~ poly(X, degree = 10))
lines(d, predict(mod10, new = data.frame(X = d)), col = 7)

BIC(mod1, mod2, mod3, mod4, mod5, mod10)



#### (F) ####

Y <- data$rating
X1 <- data$sugars
X2 <- data$potass
X3 <- data$calories

mod1 <- lm(Y~X1*X2)
summary(mod1)

mod2 <- lm(Y~X1*X2*X3)
summary(mod2)

modIntBIC <- MASS::stepAIC(object = lm(rating ~ .,data=data),
                           scope = rating ~ .^2, k = log(nobs(modBIC)), trace = 0)
summary(modIntBIC)

#### (F) ####

col <- as.integer(data$D) + 2
cex <- 0.5 + 0.25 * as.integer(data$D)

mod1<- lm(rating ~ protein+D, data = data)
summary(mod1)
plot(rating ~ protein, data = data, col = col, pch = 16, cex = cex, main = "1")
abline(a = mod1$coefficients[1], b = mod1$coefficients[2], col = 3, lwd = 2)
abline(a = mod1$coefficients[1] + mod1$coefficients[3],
       b = mod1$coefficients[2], col = 4, lwd = 2)


mod2 <- lm(rating ~ protein * D, data = data)
summary(mod2)
plot(rating ~ protein, data = data, col = col, pch = 16, cex = cex, main = "2")
abline(a = mod2$coefficients[1], b = mod2$coefficients[2], col = 3, lwd = 2)
abline(a = mod2$coefficients[1] + mod2$coefficients[3],
       b = mod2$coefficients[2] + mod2$coefficients[4], col = 4, lwd = 2)


mod3 <- lm(rating ~ protein + protein:D, data = data)
summary(mod3)
plot(rating ~ protein, data = data, col = col, pch = 16, cex = cex, main = "3")
abline(a = mod3$coefficients[1], b = mod3$coefficients[2], col = 3, lwd = 2)
abline(a = mod3$coefficients[1],
       b = mod3$coefficients[2] + mod3$coefficients[3], col = 4, lwd = 2)

