data <- read.csv("cereal.csv")

# Залежна змінна rating
Y <- data$rating

x1 <- data$calories
x2 <- data$protein
x3 <- data$cups

#### (A) ####

mod <- lm(Y ~ x1+x2+x3)
sumMod <- summary(mod)

#### (B) ####

# Знаходимо t_k для моделі
alpha <- 0.05
n <- length(data$calories)
p <- 3
t_k <- qt(p = 1 - alpha / 2, df = n - p - 1)
t_k
# [1] 1.992997

sumMod$coefficients

#### (С) ####

confint(mod, level = 0.90)

confint(mod, level = 0.95)

confint(mod, level = 0.99)

#### (E) ####

# Прогнозування середнього (регресійного) значення на 95% для всіх y^ 
predict(mod, interval = "confidence", level = 0.90)

predict(mod, interval = "confidence", level = 0.95)

predict(mod, interval = "confidence", level = 0.99)

#### (E) ####

# прогноз для середнього
mean(x1)
mean(x2)
mean(x3)
dataNewMean <- data.frame(x1 = 107,x2=2.5 ,x3=0.821)
predict(mod, newdata = dataNewMean, interval = "prediction")

# прогноз на наступний період
max(x1)*1.1
max(x2)*1.1
max(x3)*1.1
dataNewMax <- data.frame(x1 = 176,x2=6.6 ,x3=1.65)
predict(mod, newdata = dataNewMax, interval = "prediction")

#### (F) ####

# Перевіряємо F-statistic по summary
sumMod

#### (G) ####

# Видаляємо неважливий фактор(x3), він є неважливим, бо його можна занулити
mod2 <- lm(Y ~ x1 + x2)
summary(mod2)

#### (H) ####

mod2 <- lm(Y ~ x1 + x2)
summ2 <- summary(mod2)
summ2$r.squared
summ2$adj.r.squared

mod3 <- lm(Y ~ x1 + x3)
summ3 <- summary(mod3)
summ3$r.squared
summ3$adj.r.squared

mod4 <- lm(Y ~ x2 + x3)
summ4 <- summary(mod4)
summ4$r.squared
summ4$adj.r.squared


#### (I) ####
mod5 <- lm(Y ~ x1)
summ5 <- summary(mod5)
summ5$r.squared

mod6 <- lm(Y ~ x2)
summ6 <- summary(mod6)
summ6$r.squared

