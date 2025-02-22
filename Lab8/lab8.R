data <- read.table(file = "cereal.csv", header = TRUE, sep = ",")
data$name <- NULL
data$mfr <- NULL
data$type <- NULL

##### Завдання 1 #####

#### (A) ####

round(cor(data), 2)

#### (B) ####

corrplot::corrplot(cor(data), addCoef.col = "grey")

#### (C) ####

mod1 <- lm(rating ~ . ,data=data)
car::vif(mod1)

mod2 <- lm(rating ~ . - weight - potass,data=data)
car::vif(mod2)

#### (D) ####

summary(mod1)
summary(mod2)

car::compareCoefs(mod1, mod2)

confint(mod1)
confint(mod2)

##### Завдання 2 #####

#### (A) ####
mod <- lm(rating ~ . - weight -shelf - cups,data=data)

par(mfrow = c(3,3))
plot(data$calories, data$rating)
plot(data$protein, data$rating)
plot(data$fat, data$rating)
plot(data$sodium, data$rating)
plot(data$fiber, data$rating)
plot(data$carbo, data$rating)
plot(data$sugars, data$rating)
plot(data$vitamins, data$rating)
plot(data$potass, data$rating)


#### (B) ####

car::ncvTest(mod)

#### (C) ####

par(mfrow = c(1,1))
plot(mod, 3)

#### (D) ####

Y1 <- log(abs(data$rating))
Y2 <- sqrt(abs(data$rating))

modY1 <- lm(Y1 ~ . - weight -shelf - cups,data=data)
modY2 <- lm(Y2 ~ . - weight -shelf - cups,data=data)

car::ncvTest(modY1)
car::ncvTest(modY2)

#### (E) ####

# Зміщений і перетворенийD
delta <- 1 # Це налаштовується
m <- -min(data$rating) + delta
Y3 <- log(data$rating + m)
modY3 <- lm(Y3 ~ . - weight -shelf - cups,data=data)
plot(modY3, 3) 
car::ncvTest(modY3)

#### (F) ####

# Перетворено Йо-Джонсоном

# Оптимальна лямбда для Йо-Джонсона
YJ <- car::powerTransform(mod, family = "yjPower")
(lambdaYJ <- YJ$lambda)
##        Y1 
## 0.9992163

# Трансформація Йо-Джонсона
Y4 <- car::yjPower(U = data$rating, lambda = lambdaYJ)
modY4 <- lm(Y4 ~ . - weight -shelf - cups,data=data)
plot(modY4, 3) 


car::ncvTest(modY4)

#### Завдання 3 ####

secondData <- read.table(file = "cereal.csv", header = TRUE, sep = ",")

#### (A) ####

rownames(secondData) <- secondData$name
secondData$name <- NULL
secondData$mfr <- NULL
secondData$type <- NULL

#### (B) ####

# PCA
pcaData<- princomp(secondData, fix_sign = TRUE)
summary(pcaData)

#### (C) ####

# Діаграма дисперсій кожного компонента (screeplot)
plot(pcaData, type = "l")

#### (D) ####

# діаграма сукупного відсотка дисперсії
barplot(cumsum(pcaData$sdev^2) / sum(pcaData$sdev^2))

#### (E) ####

head(
  sweep(pcaData$scores %*% t(pcaData$loadings), 2, pcaData$center, "+")
)

#### (F) ####

pcaDataStd <- princomp(x = secondData, cor = TRUE, fix_sign =TRUE)
summary(pcaDataStd)

#### (G) ####

biplot(pcaData, cex = 0.75)

biplot(pcaDataStd, cex = 0.75)

#### (H) ####

Data_X<- subset(secondData, select = -c(rating))
pca_data_X <- princomp(x = Data_X, cor = TRUE, fix_sign = TRUE)
dataPCA <- data.frame("Rating" = secondData$rating, pca_data_X$scores)
modPCA <- lm(Rating ~ Comp.1 + Comp.2, dataPCA)
summary(modPCA)

