#Лабораторна робота №1 Катрича Костянтина

#Варіант 10
#=====(A)=====

x <- c(1.10, 1.55, 2.09, 2.52, 3.07, 3.57, 4.06, 4.56, 5.01, 5.53)
y <- c(1.84, 3.57, 3.54, 3.50, 5.30, 5.42, 7.12, 8.27, 8.77, 10.09)

#=====(B)=====

meanX <- mean(x)
meanY <- mean(y)
n <- length(x)

#--- a ---
varX <- 0
for(i in 1:n){
  varX = varX + (x[i] - meanX)^2
}
varX = varX / n
varX

#--- b ---
covXY <- 0
for(i in 1:n){
  covXY = covXY + (x[i] - meanX)*(y[i] - meanY)
}
covXY = covXY / n
covXY

#--- c ---
b <- covXY / varX

#--- d ---
a <- meanY - b*meanX


#=====(C)=====

varX_f <- var(x)*(n-1)/n # Це еквівалент в Excel VAR.P(x) або ДИСП.Г
varY_f <- var(y)*(n-1)/n
covXY_f <- cov(x, y)*(n-1)/n
b_f <- (cov(x, y)*(n-1)/n)/(var(x)*(n-1)/n)
a_f <- meanY - b1*meanX

a
b
plot(x, y, xlab = "X", ylab = "Y")
curve(a + b * x, add = TRUE, col = 2)

#=====(D)=====

mod <- lm(y ~ x)
plot(x, y, xlab = "X", ylab = "Y")
abline(mod)

# y = b0 + b1 * x^2
lm(y ~ I(x^2))
plot(x, y, xlab = "X", ylab = "Y")
curve( 2.3013 + 0.2658  * x^2, add = TRUE, col = 2)

# y = b0 + b1 * x + b3 * x^2
lm(y ~ x + I(x^2))
plot(x, y, xlab = "X", ylab = "Y")
curve( 1.2463 + 0.7466 * x + 0.1566  * x^2, add = TRUE, col = 2)

# y = b0 + b1*log(x)
lm(y ~ log(x))
plot(x, y, xlab = "X", ylab = "Y")
curve( 0.5505  + 4.7939  * log(x), add = TRUE, col = 2)

#=====(F)=====
# ВИСНОВОК:
#
# 1. b0 = -0.147645, b1 = 1.781502
#
# 2. Економічна модель досить лінійна, без будь-яких видів і також зростаюча
#
# 3. Якщо змінити х на 1, то у змніниться на 1.781502
#
# 4. Якщо х змінити на 10%, то у зміниться на 17.8%
#
# 5.
a
b
covXY
varX
# y = b0 + b1 * x^2
# 2.3013, 0.2658
#
# y = b0 + b1 * x + b3 * x^2
# 1.2463, 0.7466, 0.1566
#
# y = b0 + b1*log(x)
# 0.5505, 4.7939
