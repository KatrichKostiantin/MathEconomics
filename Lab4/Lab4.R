data <- read.csv("cereal.csv")

# Залежна змінна rating
Y <- data$rating

x1 <- data$calories
x2 <- data$sodium
x3 <- data$protein
x4 <- data$cups

#### (A) ####

mod <- lm(Y ~ x1+x2+x3+x4)
sumMod <- summary(mod)
sumMod

#### (B) ####

# Перевіряємо F-statistic по summary
sumMod

#### (С) ####

# Знаходимо t_k для моделі
alpha <- 0.05
n <- length(data$calories)
p <- 4
t_k <- qt(p = 1 - alpha / 2, df = n - p - 1)
t_k
# [1] 1.993464

sumMod$coefficients

#### (D) ####

confint(mod, level = 0.90)

confint(mod, level = 0.95)

confint(mod, level = 0.99)

#### (E) ####

# Прогнозування середнього (регресійного) значення на 95% для всіх y^ 
predict(mod, interval = "confidence", level = 0.90)

predict(mod, interval = "confidence", level = 0.95)

predict(mod, interval = "confidence", level = 0.99)

#### (F) ####

# прогноз для середнього
mean(x1)
mean(x2)
mean(x3)
mean(x4)
dataNewMean <- data.frame(x1 = 106.8831,x2=159.6753 ,x3=2.545455, x4 = 0.821039)
predict(mod, newdata = dataNewMean, interval = "confidence")

# прогноз на наступний період
max(x1)*1.1
max(x2)*1.1
max(x3)*1.1
max(x4)*1.1
dataNewMax <- data.frame(x1 = 176,x2=352 ,x3=6.6, x4= 1.65)
predict(mod, newdata = dataNewMax, interval = "prediction")
