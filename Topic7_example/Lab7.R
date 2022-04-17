# RStudio 1

data <- read.table(file = "cereal.csv", header = TRUE, sep = ",")
plot(Sepal.Length ~ Petal.Width, data = iris)

plot(Sepal.Width ~ Petal.Width, data = iris)

# iris dataset -- якісні змінні в останній колонці
summary(iris)
##   Sepal.Length    Sepal.Width     Petal.Length    Petal.Width          Species  
##  Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100   setosa    :50  
##  1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300   versicolor:50  
##  Median :5.800   Median :3.000   Median :4.350   Median :1.300   virginica :50  
##  Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199                  
##  3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800                  
##  Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500

# Короткий зміст лінійної моделі
mod1 <- lm(Sepal.Length ~ ., data = iris)
summary(mod1)
## 
## Call:
## lm(formula = Sepal.Length ~ ., data = iris)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.79424 -0.21874  0.00899  0.20255  0.73103 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        2.17127    0.27979   7.760 1.43e-12 ***
## Sepal.Width        0.49589    0.08607   5.761 4.87e-08 ***
## Petal.Length       0.82924    0.06853  12.101  < 2e-16 ***
## Petal.Width       -0.31516    0.15120  -2.084  0.03889 *  
## Speciesversicolor -0.72356    0.24017  -3.013  0.00306 ** 
## Speciesvirginica  -1.02350    0.33373  -3.067  0.00258 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3068 on 144 degrees of freedom
## Multiple R-squared:  0.8673, Adjusted R-squared:  0.8627 
## F-statistic: 188.3 on 5 and 144 DF,  p-value: < 2.2e-16
# Speciesversicolor (D1): -0,72356. Середній приріст
# Sepal.Length коли вид має versicolor замість setosa (еталон)
# Speciesvirginica (D2): -1.02350. Середній приріст
# Sepal.Length коли вид є virginica замість setosa (еталон)
# Обидві фіктивні змінні є значущими

# Як встановити інший рівень як еталонний (versicolor)
iris$Species <- relevel(data$fat, ref = "versicolor")

# Ті самі оцінки, крім фіктивних коефіцієнтів
mod2 <- lm(Sepal.Length ~ ., data = iris)
summary(mod2)
## 
## Call:
## lm(formula = Sepal.Length ~ ., data = iris)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.79424 -0.21874  0.00899  0.20255  0.73103 
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       1.44770    0.28149   5.143 8.68e-07 ***
## Sepal.Width       0.49589    0.08607   5.761 4.87e-08 ***
## Petal.Length      0.82924    0.06853  12.101  < 2e-16 ***
## Petal.Width      -0.31516    0.15120  -2.084  0.03889 *  
## Speciessetosa     0.72356    0.24017   3.013  0.00306 ** 
## Speciesvirginica -0.29994    0.11898  -2.521  0.01280 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3068 on 144 degrees of freedom
## Multiple R-squared:  0.8673, Adjusted R-squared:  0.8627 
## F-statistic: 188.3 on 5 and 144 DF,  p-value: < 2.2e-16
# Speciessetosa (D1) коефіцієнт: 0.72356. Середній приріст
# Sepal.Length коли вид - setosa замість versicolor (еталон)
# Speciesvirginica (D2) коефіцієнт: -0.29994. Середній приріст
# Sepal.Length коли вид - virginica замість versicolor (еталон)
# Обидві фіктивні змінні є значущими

# Коефіцієнти моделі
confint(mod2)
##                       2.5 %      97.5 %
## (Intercept)       0.8913266  2.00408209
## Sepal.Width       0.3257653  0.66601260
## Petal.Length      0.6937939  0.96469395
## Petal.Width      -0.6140049 -0.01630542
## Speciessetosa     0.2488500  1.19827390
## Speciesvirginica -0.5351144 -0.06475727
# Коефіцієнти Speciessetosa і Speciesvirginica 
# суттєві, позитивні та негативні відповідно

# Показати фіктивні змінні, використані для кодування якісної змінної
contrasts(iris$Species)
##            setosa virginica
## versicolor      0         0
## setosa          1         0
## virginica       0         1
iris$Species <- relevel(iris$Species, ref = "setosa")
contrasts(iris$Species)
##            versicolor virginica
## setosa              0         0
## versicolor          1         0
## virginica           0         1

data$STR <- as.numeric(data$fat >= 2)


modIris <- lm(data$rating ~ data$fat)
plot(rating ~ calories, data = data, col = as.integer(data$STR) +2)
abline(a = modIris$coefficients[1], b = modIris$coefficients[2], lwd = 2)

modIris_Species <- lm(Sepal.Width ~ Petal.Width + Species, data = iris)
abline(a = modIris_Species$coefficients[1], b = modIris_Species$coefficients[2], col = 2, lwd = 1)
abline(a = modIris_Species$coefficients[1] + modIris_Species$coefficients[3], b = modIris_Species$coefficients[2], col = 3, lwd = 1)
abline(a = modIris_Species$coefficients[1] + modIris_Species$coefficients[4], b = modIris_Species$coefficients[2], col = 4, lwd = 1)


#==================================================================

# RStudio 2

# Завантажте набір даних Бостона
data(Boston, package = "MASS")

# Структура даних
str(Boston)
## 'data.frame':    506 obs. of  14 variables:
##  $ crim   : num  0.00632 0.02731 0.02729 0.03237 0.06905 ...
##  $ zn     : num  18 0 0 0 0 0 12.5 12.5 12.5 12.5 ...
##  $ indus  : num  2.31 7.07 7.07 2.18 2.18 2.18 7.87 7.87 7.87 7.87 ...
##  $ chas   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ nox    : num  0.538 0.469 0.469 0.458 0.458 0.458 0.524 0.524 0.524 0.524 ...
##  $ rm     : num  6.58 6.42 7.18 7 7.15 ...
##  $ age    : num  65.2 78.9 61.1 45.8 54.2 58.7 66.6 96.1 100 85.9 ...
##  $ dis    : num  4.09 4.97 4.97 6.06 6.06 ...
##  $ rad    : int  1 2 2 3 3 3 5 5 5 5 ...
##  $ tax    : num  296 242 242 222 222 222 311 311 311 311 ...
##  $ ptratio: num  15.3 17.8 17.8 18.7 18.7 18.7 15.2 15.2 15.2 15.2 ...
##  $ black  : num  397 397 393 395 397 ...
##  $ lstat  : num  4.98 9.14 4.03 2.94 5.33 ...
##  $ medv   : num  24 21.6 34.7 33.4 36.2 28.7 22.9 27.1 16.5 18.9 ...
# chas є фіктивною змінною, що вимірює, якщо передмістя знаходиться близько до річки (1)
#  якщо ні (0). У цьому випадку він кодується не як коефіцієнт, а як 0 чи 1
# (це dummyfied)
Boston$chas
Boston$rad
# Короткий зміст лінійної моделі
mod <- lm(medv ~ chas + crim, data = Boston)
summary(mod)
## 
## Call:
## lm(formula = medv ~ chas + crim, data = Boston)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -16.540  -5.421  -1.878   2.575  30.134 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 23.61403    0.41862  56.409  < 2e-16 ***
## chas         5.57772    1.46926   3.796 0.000165 ***
## crim        -0.40598    0.04339  -9.358  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.373 on 503 degrees of freedom
## Multiple R-squared:  0.1744, Adjusted R-squared:  0.1712 
## F-statistic: 53.14 on 2 and 503 DF,  p-value: < 2.2e-16
# Коефіцієнт, пов'язаний з chas є 5.57772. Це означає, що якщо передмістя 
# знаходиться недалеко від річки, середнє значення medv зростає в 5,57772 одиниць 
# для того самого будинку та сусідніх умов. (присутність річки додає цінну 
# інформацію для пояснення medv)

modHouse <- lm(medv ~ ., data = Boston)
modBIC <- MASS::stepAIC(modHouse, k = log(nrow(Boston)))

# Короткий зміст найкращої моделі з точки зору BIC
summary(modBIC)
## 
## Call:
## lm(formula = medv ~ crim + zn + chas + nox + rm + dis + rad + 
##     tax + ptratio + black + lstat, data = Boston)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -15.5984  -2.7386  -0.5046   1.7273  26.2373 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  36.341145   5.067492   7.171 2.73e-12 ***
## crim         -0.108413   0.032779  -3.307 0.001010 ** 
## zn            0.045845   0.013523   3.390 0.000754 ***
## chas          2.718716   0.854240   3.183 0.001551 ** 
## nox         -17.376023   3.535243  -4.915 1.21e-06 ***
## rm            3.801579   0.406316   9.356  < 2e-16 ***
## dis          -1.492711   0.185731  -8.037 6.84e-15 ***
## rad           0.299608   0.063402   4.726 3.00e-06 ***
## tax          -0.011778   0.003372  -3.493 0.000521 ***
## ptratio      -0.946525   0.129066  -7.334 9.24e-13 ***
## black         0.009291   0.002674   3.475 0.000557 ***
## lstat        -0.522553   0.047424 -11.019  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.736 on 494 degrees of freedom
## Multiple R-squared:  0.7406, Adjusted R-squared:  0.7348 
## F-statistic: 128.2 on 11 and 494 DF,  p-value: < 2.2e-16
# Коефіцієнт, пов'язаний з chas є 2.71871. Якщо передмістя знаходиться недалеко 
# від річки, середнє значення medv зростає на 2,71871 одиниць часу, 
#також є значним за наявності більшої кількості предикторів.

Boston$STR <- as.numeric(Boston$nox >= mean(Boston$nox))

mod_0 <- lm(medv ~ nox, data = Boston)
plot(medv ~ nox, data = Boston, col = as.integer(STR) + 1)

mod_10 <- lm(medv ~ lstat, data = Boston)
plot(medv ~ lstat, data = Boston, col = as.integer(STR) + 1)

mod_1 <- lm(medv ~ lstat + STR, data = Boston)
summary(mod_1)

mod_10 <- lm(medv ~ lstat, data = Boston)
plot(medv ~ lstat, data = Boston, col = as.integer(STR) + 1)
abline(a = mod_10$coefficients[1], b = mod_10$coefficients[2], lwd = 1)

mod_11 <- lm(medv ~ lstat + STR, data = Boston)
abline(a = mod_11$coefficients[1], b = mod_11$coefficients[2], col = 5, lwd = 1)
abline(a = mod_11$coefficients[1] + mod_11$coefficients[3], b = mod_11$coefficients[2], col = 2, lwd = 1)
as.factor()

car::scatterplotMatrix(~ crim + dis + medv + nox + rm, regLine = list(col = 2),
                       col = 1, smooth = list(col.smooth = 4, col.spread = 4),
                       data = Boston)

mod_11 <- lm(medv ~ crim, data = Boston)
plot(medv ~ crim, data = Boston )
abline(a = mod_11$coefficients[1], b = mod_11$coefficients[2], lwd = 1)
summary(mod_11)

mod_12 <- lm(medv ~ I(exp(-crim)), data = Boston)
plot(medv ~ I(exp(-crim)), data = Boston )
abline(a = mod_12$coefficients[1], b = mod_12$coefficients[2], lwd = 1)
summary(mod_12)

plot(medv ~ dis, data = Boston )

mod_13 <- lm(medv ~ I(log(dis)), data = Boston)
plot(medv ~ I(log(dis)), data = Boston )
abline(a = mod_13$coefficients[1], b = mod_13$coefficients[2], lwd = 1)
summary(mod_13)

mod_14 <- lm(medv ~ dis, data = Boston)
plot(medv ~ dis, data = Boston )
abline(a = mod_11$coefficients[1], b = mod_11$coefficients[2], lwd = 1)
summary(mod_14)
#============================================================

# RStudio 3

# Дані
x <- c(-2, -1.9, -1.7, -1.6, -1.4, -1.3, -1.1, -1, -0.9, -0.7, -0.6,
       -0.4, -0.3, -0.1, 0, 0.1, 0.3, 0.4, 0.6, 0.7, 0.9, 1, 1.1, 1.3,
       1.4, 1.6, 1.7, 1.9, 2, 2.1, 2.3, 2.4, 2.6, 2.7, 2.9, 3, 3.1,
       3.3, 3.4, 3.6, 3.7, 3.9, 4, 4.1, 4.3, 4.4, 4.6, 4.7, 4.9, 5)
y <- c(1.4, 0.4, 2.4, 1.7, 2.4, 0, 0.3, -1, 1.3, 0.2, -0.7, 1.2, -0.1,
       -1.2, -0.1, 1, -1.1, -0.9, 0.1, 0.8, 0, 1.7, 0.3, 0.8, 1.2, 1.1,
       2.5, 1.5, 2, 3.8, 2.4, 2.9, 2.7, 4.2, 5.8, 4.7, 5.3, 4.9, 5.1,
       6.3, 8.6, 8.1, 7.1, 7.9, 8.4, 9.2, 12, 10.5, 8.7, 13.5)

# База даних (матриця з назвами стовпця)
nonLinear <- data.frame(x = x, y = y)

# Ми створюємо нову колонку всередині нелінійної бази, називається X2, 
# що містить нову змінну x ^ 2
nonLinear$x2 <- nonLinear$x^2
# Якщо бажаєте видалити їх
# nonLinear$x2 <- NULL

plot(y ~ x2, data = nonLinear)

# Регресії
mod1 <- lm(y ~ x, data = nonLinear)
plot(y ~ x, data = nonLinear)
abline(mod1)

mod2 <- lm(y ~ x2, data = nonLinear)
plot(y ~ x2, data = nonLinear)
abline(mod2)

summary(mod1)
## 
## Call:
## lm(formula = y ~ x, data = nonLinear)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.5268 -1.7513 -0.4017  0.9750  5.0265 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   0.9771     0.3506   2.787   0.0076 ** 
## x             1.4993     0.1374  10.911 1.35e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.005 on 48 degrees of freedom
## Multiple R-squared:  0.7126, Adjusted R-squared:  0.7067 
## F-statistic:   119 on 1 and 48 DF,  p-value: 1.353e-14
summary(mod2)
## 
## Call:
## lm(formula = y ~ x2, data = nonLinear)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.0418 -0.5523 -0.1465  0.6286  1.8797 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.05891    0.18462   0.319    0.751    
## x2           0.48659    0.01891  25.725   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.9728 on 48 degrees of freedom
## Multiple R-squared:  0.9324, Adjusted R-squared:  0.931 
## F-statistic: 661.8 on 1 and 48 DF,  p-value: < 2.2e-16
# mod2 має більший R^2. Також зверніть увагу, що вільний коефіцієнт не є істотним

# Еквіварентна операція і одинаковий результат
summary(lm(y ~ I(x^2), data = nonLinear))
plot(y ~ x^2, data = nonLinear)

# Якщо потрібно зробити складніші перетворення, наприклад для 3)
summary(lm(y ~ I((x - 1)^3 - log(x + 5 )), data = nonLinear))
plot(y ~ I((x + 1)^3 - log(x + 5 )), data = nonLinear)

#===============================================================

# Створення матриці за допомогою фукції poly

x1 <- seq(-1, 1, l = 4)
poly(x = x1, degree = 2, raw = TRUE) # (X, X^2)
##               1         2
## [1,] -1.0000000 1.0000000
## [2,] -0.3333333 0.1111111
## [3,]  0.3333333 0.1111111
## [4,]  1.0000000 1.0000000
## attr(,"degree")
## [1] 1 2
## attr(,"class")
## [1] "poly"   "matrix"
poly(x = x1, degree = 2) # За замовчуванням він використовує ортогональні поліноми
##               1    2
## [1,] -0.6708204  0.5
## [2,] -0.2236068 -0.5
## [3,]  0.2236068 -0.5
## [4,]  0.6708204  0.5
## attr(,"coefs")
## attr(,"coefs")$alpha
## [1] -5.551115e-17 -4.649059e-17
## 
## attr(,"coefs")$norm2
## [1] 1.0000000 4.0000000 2.2222222 0.7901235
## 
## attr(,"degree")
## [1] 1 2
## attr(,"class")
## [1] "poly"   "matrix"

# Зображення чистих поліномів
x <- seq(-1, 1, l = 200)
degree <- 5
matplot(x, poly(x, degree = degree, raw = TRUE), type = "l", lty = 1,
        ylab = expression(x^k))
legend("bottomright", legend = paste("k =", 1:degree), col = 1:degree, lwd = 2)

# Зображення ортогональних поліномів
matplot(x, poly(x, degree = degree), type = "l", lty = 1,
        ylab = expression(p[k](x)))
legend("bottomright", legend = paste("k =", 1:degree), col = 1:degree, lwd = 2)

#=======================================================================

# RStudio 4
# Дані, що містять швидкість (миль/год) та зупиночний шлях (фути) автомобілів з 1920 року
data(cars)
plot(cars, xlab = "Speed (mph)", ylab = "Stopping distance (ft)")

# Відпавідна лінійна модель дистрибутивній швидкості
mod1 <- lm(dist ~ speed, data = cars)
abline(coef = mod1$coefficients, col = 2)

# Квадратичний
mod2 <- lm(dist ~ poly(speed, degree = 2), data = cars)
# Пристосованість - це не лінія, ми повинні шукати альтернативний підхід
d <- seq(0, 25, length.out = 200)
lines(d, predict(mod2, new = data.frame(speed = d)), col = 3)

# Кубічний
mod3 <- lm(dist ~ poly(speed, degree = 3), data = cars)
lines(d, predict(mod3, new = data.frame(speed = d)), col = 4)

# 10-й -- перенавчення
mod10 <- lm(dist ~ poly(speed, degree = 10), data = cars)
lines(d, predict(mod10, new = data.frame(speed = d)), col = 5)

#-------------------------------------------------------------

# BICs -- лінійна модель краща!
BIC(mod1, mod2, mod3, mod10)
##       df      BIC
## mod1   3 424.8929
## mod2   4 426.4202
## mod3   5 429.4451
## mod10 12 450.3523

# poly обчислює за замовчуванням ортогональні многочлени. Це не 
# X^1, X^2, ..., X^p але їх комбінації такі, що поліноми ортогональні.
# 'Raw' поліноми можливі з raw = TRUE. Вони дають тіж результати, але оцінки 
# коефіцієнта різні.
mod2Raw <- lm(dist ~ poly(speed, degree = 2, raw = TRUE), data = cars)
plot(cars, xlab = "Speed (mph)", ylab = "Stopping distance (ft)")
lines(d, predict(mod2, new = data.frame(speed = d)), col = 1)
lines(d, predict(mod2Raw, new = data.frame(speed = d)), col = 2)

#-----------------------------------------------------------------------

# Однак: різні оцінки коефіцієнтів, але однакові R ^ 2. Як це можливо?
summary(mod2)
## 
## Call:
## lm(formula = dist ~ poly(speed, degree = 2), data = cars)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -28.720  -9.184  -3.188   4.628  45.152 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                42.980      2.146  20.026  < 2e-16 ***
## poly(speed, degree = 2)1  145.552     15.176   9.591 1.21e-12 ***
## poly(speed, degree = 2)2   22.996     15.176   1.515    0.136    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.18 on 47 degrees of freedom
## Multiple R-squared:  0.6673, Adjusted R-squared:  0.6532 
## F-statistic: 47.14 on 2 and 47 DF,  p-value: 5.852e-12
summary(mod2Raw)
## 
## Call:
## lm(formula = dist ~ poly(speed, degree = 2, raw = TRUE), data = cars)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -28.720  -9.184  -3.188   4.628  45.152 
## 
## Coefficients:
##                                      Estimate Std. Error t value Pr(>|t|)
## (Intercept)                           2.47014   14.81716   0.167    0.868
## poly(speed, degree = 2, raw = TRUE)1  0.91329    2.03422   0.449    0.656
## poly(speed, degree = 2, raw = TRUE)2  0.09996    0.06597   1.515    0.136
## 
## Residual standard error: 15.18 on 47 degrees of freedom
## Multiple R-squared:  0.6673, Adjusted R-squared:  0.6532 
## F-statistic: 47.14 on 2 and 47 DF,  p-value: 5.852e-12

# Оскільки змінні в mod2Raw тісно пов’язані між собою, 
# а змінні в mod2 не пов’язані!
car::scatterplotMatrix(mod2$model[, -1], col = 1, regLine = list(col = 2),
                       smooth = list(col.smooth = 4, col.spread = 4))
car::scatterplotMatrix(mod2Raw$model[, -1],col = 1, regLine = list(col = 2),
                       smooth = list(col.smooth = 4, col.spread = 4))
cor(mod2$model[, -1])
##              1            2
## 1 1.000000e+00 4.686464e-17
## 2 4.686464e-17 1.000000e+00
cor(mod2Raw$model[, -1])
##           1         2
## 1 1.0000000 0.9794765
## 2 0.9794765 1.0000000

#========================================================================

# RStudio 5

data(Boston, package = "MASS")
# Взаємодія між lstat та age
summary(lm(medv ~ lstat + lstat:age, data = Boston))
## 
## Call:
## lm(formula = medv ~ lstat + lstat:age, data = Boston)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -15.815  -4.039  -1.335   2.086  27.491 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 36.041514   0.691334  52.133  < 2e-16 ***
## lstat       -1.388161   0.126911 -10.938  < 2e-16 ***
## lstat:age    0.004103   0.001133   3.621 0.000324 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.142 on 503 degrees of freedom
## Multiple R-squared:  0.5557, Adjusted R-squared:  0.554 
## F-statistic: 314.6 on 2 and 503 DF,  p-value: < 2.2e-16

# Для одиниці приросту у age ефект lstat у відповіді позитивно збільшується 
# на 0,004103 одиниці, зміщуючись від -1,388161 до -1,384058


# Таким чином, із збільшенням віку lstat впливає менш негативно на medv. 
# Зверніть увагу, що однакова інтерпретація НЕ виконується, якщо ми змінюємо ролі 
# age та lstat, оскільки age не є єдиним предиктором!

# Взаємодія першого порядку
summary(lm(medv ~ lstat * age, data = Boston))
## 
## Call:
## lm(formula = medv ~ lstat * age, data = Boston)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -15.806  -4.045  -1.333   2.085  27.552 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 36.0885359  1.4698355  24.553  < 2e-16 ***
## lstat       -1.3921168  0.1674555  -8.313 8.78e-16 ***
## age         -0.0007209  0.0198792  -0.036   0.9711    
## lstat:age    0.0041560  0.0018518   2.244   0.0252 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.149 on 502 degrees of freedom
## Multiple R-squared:  0.5557, Adjusted R-squared:  0.5531 
## F-statistic: 209.3 on 3 and 502 DF,  p-value: < 2.2e-16

# Взаємодія другого порядку
summary(lm(medv ~ lstat * age * indus, data = Boston))

## 
## Call:
## lm(formula = medv ~ lstat * age * indus, data = Boston)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -15.1549  -3.6437  -0.8427   2.1991  24.8751 
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     46.103752   2.891173  15.946  < 2e-16 ***
## lstat           -2.641475   0.372223  -7.096 4.43e-12 ***
## age             -0.042300   0.041668  -1.015  0.31051    
## indus           -1.849829   0.380252  -4.865 1.54e-06 ***
## lstat:age        0.014249   0.004437   3.211  0.00141 ** 
## lstat:indus      0.177418   0.037647   4.713 3.18e-06 ***
## age:indus        0.014332   0.004386   3.268  0.00116 ** 
## lstat:age:indus -0.001621   0.000408  -3.973 8.14e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.929 on 498 degrees of freedom
## Multiple R-squared:  0.5901, Adjusted R-squared:  0.5844 
## F-statistic: 102.4 on 7 and 498 DF,  p-value: < 2.2e-16

#---------------------------------------------------------

#RStudio 6

# Включіть взаємодії першого порядку в пошук найкращої моделі з точки зору BIC, 
# а не лише окремих предикторів
modHouse <- lm(medv ~ ., data = Boston)
modBIC <- MASS::stepAIC(modHouse, k = log(nrow(Boston)), trace = 0)

modIntBIC <- MASS::stepAIC(object = lm(medv ~ ., data = Boston),
                           scope = medv ~ .^2, k = log(nobs(modBIC)), trace = 0)
summary(modIntBIC)
## 
## Call:
## lm(formula = medv ~ crim + indus + chas + nox + rm + age + dis + 
##     rad + tax + ptratio + black + lstat + rm:lstat + rad:lstat + 
##     rm:rad + dis:rad + black:lstat + dis:ptratio + crim:chas + 
##     chas:nox + chas:rm + chas:ptratio + rm:ptratio + age:black + 
##     indus:dis + indus:lstat + crim:rm + crim:lstat, data = Boston)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.5845 -1.6797 -0.3157  1.5433 19.4311 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -9.673e+01  1.350e+01  -7.167 2.93e-12 ***
## crim         -1.454e+00  3.147e-01  -4.620 4.95e-06 ***
## indus         7.647e-01  1.237e-01   6.182 1.36e-09 ***
## chas          6.341e+01  1.115e+01   5.687 2.26e-08 ***
## nox          -1.691e+01  3.020e+00  -5.598 3.67e-08 ***
## rm            1.946e+01  1.730e+00  11.250  < 2e-16 ***
## age           2.233e-01  5.898e-02   3.786 0.000172 ***
## dis          -2.462e+00  6.776e-01  -3.634 0.000309 ***
## rad           3.461e+00  3.109e-01  11.132  < 2e-16 ***
## tax          -1.401e-02  2.536e-03  -5.522 5.52e-08 ***
## ptratio       1.207e+00  7.085e-01   1.704 0.089111 .  
## black         7.946e-02  1.262e-02   6.298 6.87e-10 ***
## lstat         2.939e+00  2.707e-01  10.857  < 2e-16 ***
## rm:lstat     -3.793e-01  3.592e-02 -10.559  < 2e-16 ***
## rad:lstat    -4.804e-02  4.465e-03 -10.760  < 2e-16 ***
## rm:rad       -3.490e-01  4.370e-02  -7.986 1.05e-14 ***
## dis:rad      -9.236e-02  2.603e-02  -3.548 0.000427 ***
## black:lstat  -8.337e-04  3.355e-04  -2.485 0.013292 *  
## dis:ptratio   1.371e-01  3.719e-02   3.686 0.000254 ***
## crim:chas     2.544e+00  3.813e-01   6.672 7.01e-11 ***
## chas:nox     -3.706e+01  6.202e+00  -5.976 4.48e-09 ***
## chas:rm      -3.774e+00  7.402e-01  -5.099 4.94e-07 ***
## chas:ptratio -1.185e+00  3.701e-01  -3.203 0.001451 ** 
## rm:ptratio   -3.792e-01  1.067e-01  -3.555 0.000415 ***
## age:black    -7.107e-04  1.552e-04  -4.578 5.99e-06 ***
## indus:dis    -1.316e-01  2.533e-02  -5.197 3.00e-07 ***
## indus:lstat  -2.580e-02  5.204e-03  -4.959 9.88e-07 ***
## crim:rm       1.605e-01  4.001e-02   4.011 7.00e-05 ***
## crim:lstat    1.511e-02  4.954e-03   3.051 0.002408 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.045 on 477 degrees of freedom
## Multiple R-squared:  0.8964, Adjusted R-squared:  0.8904 
## F-statistic: 147.5 on 28 and 477 DF,  p-value: < 2.2e-16

# Видалення ввиразів у modIntBIC не покращує

MASS::dropterm(modIntBIC, k = log(nobs(modIntBIC)), sorted = TRUE)
## Single term deletions
## 
## Model:
## medv ~ crim + indus + chas + nox + rm + age + dis + rad + tax + 
##     ptratio + black + lstat + rm:lstat + rad:lstat + rm:rad + 
##     dis:rad + black:lstat + dis:ptratio + crim:chas + chas:nox + 
##     chas:rm + chas:ptratio + rm:ptratio + age:black + indus:dis + 
##     indus:lstat + crim:rm + crim:lstat
##              Df Sum of Sq    RSS    AIC
## <none>                    4423.7 1277.7
## black:lstat   1     57.28 4481.0 1278.0
## crim:lstat    1     86.33 4510.1 1281.2
## chas:ptratio  1     95.15 4518.9 1282.2
## dis:rad       1    116.73 4540.5 1284.6
## rm:ptratio    1    117.23 4541.0 1284.7
## dis:ptratio   1    126.00 4549.7 1285.7
## crim:rm       1    149.24 4573.0 1288.2
## age:black     1    194.40 4618.1 1293.2
## indus:lstat   1    228.05 4651.8 1296.9
## chas:rm       1    241.11 4664.8 1298.3
## indus:dis     1    250.51 4674.2 1299.3
## tax           1    282.77 4706.5 1302.8
## chas:nox      1    331.19 4754.9 1308.0
## crim:chas     1    412.86 4836.6 1316.6
## rm:rad        1    591.45 5015.2 1335.0
## rm:lstat      1   1033.93 5457.7 1377.7
## rad:lstat     1   1073.80 5497.5 1381.4

# Ні включенням інших взаємодій термінів
MASS::addterm(modIntBIC, scope = lm(medv ~ .^2, data = Boston),
              k = log(nobs(modIntBIC)), sorted = TRUE)
## Single term additions
## 
## Model:
## medv ~ crim + indus + chas + nox + rm + age + dis + rad + tax + 
##     ptratio + black + lstat + rm:lstat + rad:lstat + rm:rad + 
##     dis:rad + black:lstat + dis:ptratio + crim:chas + chas:nox + 
##     chas:rm + chas:ptratio + rm:ptratio + age:black + indus:dis + 
##     indus:lstat + crim:rm + crim:lstat
##               Df Sum of Sq    RSS    AIC
## <none>                     4423.7 1277.7
## nox:age        1    52.205 4371.5 1277.9
## chas:lstat     1    50.231 4373.5 1278.1
## crim:nox       1    50.002 4373.7 1278.2
## indus:tax      1    46.182 4377.6 1278.6
## nox:rad        1    42.822 4380.9 1279.0
## tax:ptratio    1    37.105 4386.6 1279.6
## age:lstat      1    29.825 4393.9 1280.5
## rm:tax         1    27.221 4396.5 1280.8
## nox:rm         1    25.099 4398.6 1281.0
## nox:ptratio    1    17.994 4405.7 1281.8
## rm:age         1    16.956 4406.8 1282.0
## crim:black     1    15.566 4408.2 1282.1
## dis:tax        1    13.336 4410.4 1282.4
## dis:lstat      1    10.944 4412.8 1282.7
## rm:black       1     9.909 4413.8 1282.8
## rm:dis         1     9.312 4414.4 1282.8
## crim:indus     1     8.458 4415.3 1282.9
## tax:lstat      1     7.891 4415.8 1283.0
## ptratio:black  1     7.769 4416.0 1283.0
## rad:black      1     7.327 4416.4 1283.1
## age:ptratio    1     6.857 4416.9 1283.1
## age:tax        1     5.785 4417.9 1283.2
## nox:dis        1     5.727 4418.0 1283.2
## age:dis        1     5.618 4418.1 1283.3
## nox:tax        1     5.579 4418.2 1283.3
## crim:dis       1     5.376 4418.4 1283.3
## tax:black      1     4.867 4418.9 1283.3
## indus:age      1     4.554 4419.2 1283.4
## indus:rm       1     4.089 4419.6 1283.4
## indus:ptratio  1     4.082 4419.6 1283.4
## zn             1     3.919 4419.8 1283.5
## chas:tax       1     3.918 4419.8 1283.5
## rad:tax        1     3.155 4420.6 1283.5
## age:rad        1     3.085 4420.6 1283.5
## nox:black      1     2.939 4420.8 1283.6
## ptratio:lstat  1     2.469 4421.3 1283.6
## indus:chas     1     2.359 4421.4 1283.6
## chas:black     1     1.940 4421.8 1283.7
## indus:nox      1     1.440 4422.3 1283.7
## indus:black    1     1.177 4422.6 1283.8
## chas:rad       1     0.757 4423.0 1283.8
## chas:age       1     0.757 4423.0 1283.8
## crim:rad       1     0.678 4423.1 1283.8
## nox:lstat      1     0.607 4423.1 1283.8
## rad:ptratio    1     0.567 4423.2 1283.8
## crim:age       1     0.348 4423.4 1283.9
## indus:rad      1     0.219 4423.5 1283.9
## dis:black      1     0.077 4423.7 1283.9
## crim:ptratio   1     0.019 4423.7 1283.9
## crim:tax       1     0.004 4423.7 1283.9
## chas:dis       1     0.004 4423.7 1283.9

#-------------------

#RStudio 7

# Налаштування групи
col <- Boston$chas + 3
cex <- 0.5 + 0.25 * Boston$chas

# 1. Немає фіктивних змінних
(mod1 <- lm(medv ~ lstat, data = Boston))
## 
## Call:
## lm(formula = medv ~ lstat, data = Boston)
## 
## Coefficients:
## (Intercept)        lstat  
##       34.55        -0.95
plot(medv ~ lstat, data = Boston, col = col, pch = 16, cex = cex, main = "1")
abline(coef = mod1$coefficients, lwd = 2)

#-------------------------

# 2. Фіктивна змінна
(mod2 <- lm(medv ~ lstat + chas, data = Boston))
## 
## Call:
## lm(formula = medv ~ lstat + chas, data = Boston)
## 
## Coefficients:
## (Intercept)        lstat         chas  
##     34.0941      -0.9406       4.9200
plot(medv ~ lstat, data = Boston, col = col, pch = 16, cex = cex, main = "2")
abline(a = mod2$coefficients[1], b = mod2$coefficients[2], col = 3, lwd = 2)
abline(a = mod2$coefficients[1] + mod2$coefficients[3],
       b = mod2$coefficients[2], col = 4, lwd = 2)

#--------------------------

# 3. Фіктивна змінна, з взаємодією
(mod3 <- lm(medv ~ lstat * chas, data = Boston))
## 
## Call:
## lm(formula = medv ~ lstat * chas, data = Boston)
## 
## Coefficients:
## (Intercept)        lstat         chas   lstat:chas  
##     33.7672      -0.9150       9.8251      -0.4329
plot(medv ~ lstat, data = Boston, col = col, pch = 16, cex = cex, main = "3")
abline(a = mod3$coefficients[1], b = mod3$coefficients[2], col = 3, lwd = 2)
abline(a = mod3$coefficients[1] + mod3$coefficients[3],
       b = mod3$coefficients[2] + mod3$coefficients[4], col = 4, lwd = 2)

#------------------------------

# 4. Фіктивна змінна, присутня лише у взаємодії
(mod4 <- lm(medv ~ lstat + lstat:chas, data = Boston))
## 
## Call:
## lm(formula = medv ~ lstat + lstat:chas, data = Boston)
## 
## Coefficients:
## (Intercept)        lstat   lstat:chas  
##     34.4893      -0.9580       0.2128
plot(medv ~ lstat, data = Boston, col = col, pch = 16, cex = cex, main = "4")
abline(a = mod4$coefficients[1], b = mod4$coefficients[2], col = 3, lwd = 2)
abline(a = mod4$coefficients[1],
       b = mod4$coefficients[2] + mod4$coefficients[3], col = 4, lwd = 2)

#---------------------------------

# 5. Фіктивна змінна та відсутність предиктора
(mod5 <- lm(medv ~ chas, data = Boston))
## 
## Call:
## lm(formula = medv ~ chas, data = Boston)
## 
## Coefficients:
## (Intercept)         chas  
##      22.094        6.346
plot(medv ~ lstat, data = Boston, col = col, pch = 16, cex = cex, main = "5")
abline(a = mod5$coefficients[1], b = 0, col = 3, lwd = 2)
abline(a = mod5$coefficients[1] + mod5$coefficients[2], b = 0, col = 4, lwd = 2)

#------------------------------------

# 6. Фіктивна змінна. Взаємодія у вільному коефіцієнті та нахилі
(mod6 <- lm(medv ~ chas + lstat:chas, data = Boston))
## 
## Call:
## lm(formula = medv ~ chas + lstat:chas, data = Boston)
## 
## Coefficients:
## (Intercept)         chas   chas:lstat  
##      22.094       21.498       -1.348
plot(medv ~ lstat, data = Boston, col = col, pch = 16, cex = cex, main = "6")
abline(a = mod6$coefficients[1], b = 0, col = 3, lwd = 2)
abline(a = mod6$coefficients[1] + mod6$coefficients[2],
       b = mod6$coefficients[3], col = 4, lwd = 2)

#---------------------------------------

# 7. Фіктивна змінна. Взаємодія в нахилі
(mod7 <- lm(medv ~ lstat:chas, data = Boston))
## 
## Call:
## lm(formula = medv ~ lstat:chas, data = Boston)
## 
## Coefficients:
## (Intercept)   lstat:chas  
##    22.49484      0.04882
plot(medv ~ lstat, data = Boston, col = col, pch = 16, cex = cex, main = "7")
abline(a = mod7$coefficients[1], b = 0, col = 3, lwd = 2)
abline(a = mod7$coefficients[1], b = mod7$coefficients[2], col = 4, lwd = 2)

#--------------------------------------------------------------------------

#RStudio 8

# Моделюйте, використовуючи фіктивну змінну в повному наборі даних
lm(medv ~ lstat + chas + lstat:chas, data = Boston)
## 
## Call:
## lm(formula = medv ~ lstat + chas + lstat:chas, data = Boston)
## 
## Coefficients:
## (Intercept)        lstat         chas   lstat:chas  
##     33.7672      -0.9150       9.8251      -0.4329

# Індивідуальна модель для групи з chas == 0
lm(medv ~ lstat, data = Boston, subset = chas == 0)
## 
## Call:
## lm(formula = medv ~ lstat, data = Boston, subset = chas == 0)
## 
## Coefficients:
## (Intercept)        lstat  
##      33.767       -0.915
# Зверніть увагу, що вільний коефіцієнт та коефіцієнт lstat такі ж, як і раніше

# Індивідуальна модель для групи з chas == 1
lm(medv ~ lstat, data = Boston, subset = chas == 1)
## 
## Call:
## lm(formula = medv ~ lstat, data = Boston, subset = chas == 1)
## 
## Coefficients:
## (Intercept)        lstat  
##      43.592       -1.348
# Зверніть увагу, що вільний коефіцієнт та коефіцієнт lstat дорівнюють коефіцієнтам 
# із спільної моделі, плюс конкретні умови, пов'язані з chas

# ===========================================================

# RStudio 9

# Не враховує групи в даних
modIris <- lm(Sepal.Width ~ Petal.Width, data = iris)
modIris$coefficients
## (Intercept) Petal.Width 
##   3.3084256  -0.2093598

# Додавання взаємодії з групами
modIrisSpecies <- lm(Sepal.Width ~ Petal.Width * Species, data = iris)
modIrisSpecies$coefficients
##                   (Intercept)                   Petal.Width             Speciesversicolor              Speciesvirginica 
##                     3.2220507                     0.8371922                    -1.8491878                    -1.5272777 
## Petal.Width:Speciesversicolor  Petal.Width:Speciesvirginica 
##                     0.2164556                    -0.2057870

# Спільна лінія регресії показує негативну кореляцію, але кожна лінія регресії 
# групи показує позитивну кореляцію
plot(Sepal.Width ~ Petal.Width, data = iris, col = as.integer(Species) + 1,
     pch = 16)
abline(a = modIris$coefficients[1], b = modIris$coefficients[2], lwd = 2)
abline(a = modIrisSpecies$coefficients[1], b = modIrisSpecies$coefficients[2],
       col = 2, lwd = 2)
abline(a = modIrisSpecies$coefficients[1] + modIrisSpecies$coefficients[3],
       b = modIrisSpecies$coefficients[2] + modIrisSpecies$coefficients[5],
       col = 3, lwd = 2)
abline(a = modIrisSpecies$coefficients[1] + modIrisSpecies$coefficients[4],
       b = modIrisSpecies$coefficients[2] + modIrisSpecies$coefficients[6],
       col = 4, lwd = 2)

#==============================================================
