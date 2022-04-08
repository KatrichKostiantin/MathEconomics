data <- read.csv("train.csv")

#### (A) ####
# Перевіряємо чи є пропущені дані. Їх немає.
sum(is.na(data))

summary(data)

# Змінюємо тип у деяких даних на factor, оскільки вони мають тільки 2 можливих значення.

data$UNDER_CONSTRUCTION <- as.factor(data$UNDER_CONSTRUCTION)
summary(data$UNDER_CONSTRUCTION)

data$BHK_OR_RK <- as.factor(data$BHK_OR_RK)
summary(data$BHK_OR_RK)

data$READY_TO_MOVE <- as.factor(data$READY_TO_MOVE)
summary(data$READY_TO_MOVE)

data$RESALE <- as.factor(data$RESALE)
summary(data$RESALE)

# Змінюємо назви колонок для зручноті

colnames(data)[colnames(data)=="TARGET.PRICE_IN_LACS."] <-  "PRICE"

colnames(data)[colnames(data)=="BHK_NO."] <-  "BHK_NO"

# Залежна змінна PRICE

Y <- data$PRICE

# Незалежні

x1 <- data$RERA
x2 <- data$BHK_NO
x3 <- data$SQUARE_FT
x4 <- data$LONGITUDE
x5 <- data$LATITUDE

# Будуємо модель з 5-ма незалежними змінними

modAll <- lm(Y ~ x1 + x2 + x3 + x4+ x5)

summary(modAll)
#  (Intercept)  1.274e+02  
#  x1           1.018e+02  
#  x2           8.501e+01  
#  x3           1.389e-04  
#  x4          -4.802e+00  
#  x5          -1.571e+00

# Рахуємо коефіцієнти матричним методом
X <- cbind(1, x1,x2,x3,x4,x5)
beta <- solve(t(X) %*% X) %*% t(X) %*% Y

beta
#        1.274163e+02
#    x1  1.018060e+02
#    x2  8.500818e+01
#    x3  1.388777e-04
#    x4 -4.801891e+00
#    x5 -1.571043e+00
#    Співпадає


#### (B) ####

# Будуємо однофакторні лінійні моделі за 5-ма факторами

mod1 <- lm(Y ~ x1)
summary(mod1)

mod2 <- lm(Y ~ x2)
summary(mod2)

mod3 <- lm(Y ~ x3)
summary(mod3)

mod4 <- lm(Y ~ x4)
summary(mod4)

mod5 <- lm(Y ~ x5)
summary(mod5)


#### (C) ####

# Модель №3 є найкращою, бо R^2 є найбільшим

#### (D) ####

Y_hat <- predict(modAll)
Y_hat
e <- Y_hat-Y
e
