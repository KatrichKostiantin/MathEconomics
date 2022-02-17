data <- read.csv("cereal.csv")

#### (A) ####

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


# Залежна змінна rating
Y <- data$rating

x1 <- data$calories
x2 <- data$sodium
x3 <- data$protein
x4 <- data$potass
x5 <- data$cups

mod <- lm(Y ~ x1+x2+x3+x4+x5)

simpleAnova(mod)

#### (B) ####	
anova(mod)
#### (C) ####

dataCopy <- data
data$name <- NULL
data$mfr <- NULL
data$type <- NULL

### a. 
modAll <- lm(rating ~ .,data = data)
MASS::stepAIC(modAll, direction = "backward", k = log(nrow(data)))

### b. 
modZero <- lm(rating ~ 1,data = data)
MASS::stepAIC(modZero, direction = "forward", scope = list(lower = modZero, upper = modAll), k = log(nrow(data)))

### c. 
MASS::stepAIC(modAll, direction = "both", trace = 0, scope = list(lower = modZero, upper = modAll), k = log(nrow(data)))

### d.

AIC <- MASS::stepAIC(modAll, trace = 0, k = 2)
AIC

BIC <- MASS::stepAIC(modAll, trace = 0, k = log(nrow(data)))
BIC
