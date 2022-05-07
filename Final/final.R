data <- read.table(file = "cereal.csv", header = TRUE, sep = ",")
data$name <- NULL
data$mfr <- NULL
data$type <- NULL

summary(data)

# Залежна змінна rating
Y <- data$rating

# Незалежні змінні
x1 <- data$calories
x2 <- data$fiber
x3<- data$potass
x4 <- data$shelf
x5 <- data$weight
x6 <- data$cups
x7 <- data$sugars

mod_X1 <- lm(Y ~ x1)
summary(mod_X1)

mod_X2 <- lm(Y ~ x2)
summary(mod_X2)

mod_X3 <- lm(Y ~ x3)
summary(mod_X3)

mod_X4 <- lm(Y ~ x4)
summary(mod_X4)

mod_X5 <- lm(Y ~ x5)
summary(mod_X5)

mod_X6 <- lm(Y ~ x6)
summary(mod_X6)

mod_X7 <- lm(Y ~ x7)
summary(mod_X7)

hist(x1)
hist(x2)
hist(x3)
hist(x4)
hist(x5)
hist(x6)
hist(x7)

hist(Y)

mod1 <- lm(Y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7, data = data)
summary(mod1)


#=======КОРЕЛЯЦІЯ=========
X <- data[c("calories", "fiber", "potass","shelf", "weight", "cups", "sugars")]
m <- ncol(X)
n <- nrow(X)

library(GGally)
ggpairs(X)

# знаходимо кореляційну матрицю
MatrixCor <- cor(X)
MatrixCor

library(regclass)
VIF(mod1)

library(corrplot)
corrplot::corrplot(cor(data), addCoef.col = "grey")

mod1.1 <- lm(Y ~ x3 + x4 + x5 + x6 + x7, data = data)
mod1.2 <- lm(Y ~ x1 + x3 + x4 + x6 + x7, data = data)
mod1.3 <- lm(Y ~ x2 + x4 + x5 + x6 + x7, data = data)
mod1.4 <- lm(Y ~ x1 + x2 + x4 + x6 + x7, data = data)

summary(mod1.1)
VIF(mod1.1)

summary(mod1.2)
VIF(mod1.2)

summary(mod1.3)
VIF(mod1.3)

summary(mod1.4)
VIF(mod1.4)

#mod1.4 найкраща

mod2 <- lm(Y ~ x1 + x2 + x4 + x6 + x7, data = data)

confint(mod2, level = 0.90)

confint(mod2, level = 0.95)

confint(mod2, level = 0.99)

# Прогнозування середнього (регресійного) значення на 95% для всіх y^ 
predict(mod2, interval = "confidence", level = 0.90)

predict(mod2, interval = "confidence", level = 0.95)

predict(mod2, interval = "confidence", level = 0.99)

# прогноз для середнього
mean_x1 <- mean(x1)
mean_x2 <- mean(x2)
mean_x4 <- mean(x4)
mean_x6 <- mean(x6)
mean_x7 <- mean(x7)
dataNewMean <- data.frame(x1 = mean_x1,x2 = mean_x2 ,x4 = mean_x4, x6 = mean_x6, x7 = mean_x7)
predict(mod2, newdata = dataNewMean, interval = "confidence")

# прогноз на наступний період
next_x1 <- max(x1)*1.1
next_x2 <- max(x2)*1.1
next_x4 <- max(x4)*1.1
next_x6 <- max(x6)*1.1
next_x7 <- max(x7)*1.1
dataNewMax <- data.frame(x1 = next_x1, x2 = next_x2 ,x4 = next_x4, x6 = next_x6, x7 = next_x7)
predict(mod2, newdata = dataNewMax, interval = "prediction")

#=============================================

# Ця функція обчислює спрощену anova для лінійної моделі
simpleAnova <- function(object, ...) {
  
  # Обчислити таблицю anova
  tab <- anova(object, ...)
  
  # Отримати кількість предикторів
  p <- nrow(tab) - 1
  
  # Додайте рядок предикторів
  predictorsRow <- colSums(tab[1:p, 1:2])
  predictorsRow <- c(predictorsRow, predictorsRow[2] / predictorsRow[1])
  
  # F-значення
  Fval <- predictorsRow[3] / tab[p + 1, 3]
  pval <- pf(Fval, df1 = p, df2 = tab$Df[p + 1], lower.tail = FALSE)
  predictorsRow <- c(predictorsRow, Fval, pval)
  
  # Спрощена таблиця
  tab <- rbind(predictorsRow, tab[p + 1, ])
  row.names(tab)[1] <- "Predictors"
  return(tab)
  
}
#==============================

simpleAnova(mod2)
anova(mod2)

modZero <- lm(rating ~ 1,data = data)

MASS::stepAIC(mod2, direction = "backward", k = log(nrow(data)))
MASS::stepAIC(mod2, direction = "both", trace = 0, scope = list(lower = modZero, upper = mod2), k = log(nrow(data)))

AIC <- MASS::stepAIC(mod2, trace = 0, k = 2)
AIC

BIC <- MASS::stepAIC(mod2, trace = 0, k = log(nrow(data)))
BIC

#==============================

# m1
m1 <- lm(Y~x)
summary(m1)

# m2-m4
m234 <- lm(log(Y) ~ X)
summary(m234)

# m5
#додали 1 до Х бо є нулі
m5 <- lm(log(Y) ~ log(X + 1))
summary(m5)

# m6
#додали 1 до Х бо є нулі
m6 <- lm(Y ~ I(1/(X + 1)))
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
m12 <- lm(Y ~ I(x^3 - log(abs(x)) + 2^x))
summary(m12)

#m9 найкраща

mean(data$fat)
#1.012987
data$D <- as.factor(data$fat > mean(data$fat))
col <- as.integer(data$D) + 2
cex <- 0.5 + 0.25 * as.integer(data$D)

mod3 <- lm(rating ~ protein + D, data = data)
summary(mod3)
plot(rating ~ protein, data = data, col = col, pch = 16, cex = cex, main = "1")
abline(a = mod3$coefficients[1], b = mod3$coefficients[2], col = 3, lwd = 2)
abline(a = mod3$coefficients[1] + mod3$coefficients[3],
       b = mod3$coefficients[2], col = 4, lwd = 2)

