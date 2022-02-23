data <- read.table(file = "cereal.csv", header = TRUE, sep = ",")
data$name <- NULL
data$mfr <- NULL
data$type <- NULL

#### (A) ####

mod1 <- lm(rating ~ ., data = data)
mod2 <- lm(rating ~ . - cups, data = data)
mod3 <- lm(rating ~ calories + potass, data = data)

#### (B) ####

summary(mod1)
summary(mod2)
summary(mod3)

#### (C) ####

m1 <- lm(rating ~ calories, data = data)
m1

m0 <- lm(rating ~ 0 + calories, data = data)
m0

summary(m1)

summary(m0)


plot(rating ~ calories, data = data)
abline(m1, col = 2) # Очевидно, набагато краще
abline(m0, col = 3)

#### (D) ####

dataCen <- data.frame(scale(data, center = TRUE, scale = FALSE))
modCen1 <- lm(rating ~ calories, data = dataCen)
modCen0 <- lm(rating ~ 0 + calories, data = dataCen)

summary(modCen1)

summary(modCen0)


plot(rating ~ calories, data = dataCen)
abline(modCen1, col = 2) 
abline(modCen0, col = 3)

