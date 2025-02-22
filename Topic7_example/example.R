data <- read.table(file = "cereal.csv", header = TRUE, sep = ",")
data$name <- NULL
data$mfr <- NULL
data$type <- NULL


mod1 <- lm(rating ~ calories+fiber+potass+shelf+weight+cups, data = data)
summary(mod1)
#Якщо подивитися на підсумок моделі, значення adjusted R-квадрат 0.6181 є гарним.
#F-значення є значущим, що означає, 
#що всі пояснювальні змінні разом добре пояснюють вартість пластівців.
#Однак, переходячи до індивідуальних коефіцієнтів регресії, видно,
#що аж 4 змінні не є статистично значущими.

X <- data[c("calories", "fiber", "potass","shelf", "weight", "cups")]
m <- ncol(X)
n <- nrow(X)


#Функція ggpairs() пакета GGally дозволяє побудувати матрицю діаграми розсіювання.
#У лівій частині малюнка намальовані діаграми розсіювання кожної пари числових змінних.
#Праворуч відображається кореляція Пірсона. Розподіл змінних по діагоналі.
#Матриця кореляції показує, що попарна кореляція між пояснювальними змінними достатньо висока, 
#за винятком кількох пар. 
#Найбільша кореляція між змінною potass (калій) і fiber може бути першопричиною мультиколінеарності.

library(GGally)
ggpairs(X)

#### Застосуємо алгоритм Фаррара–Глобера ####
#### 2-й крок ####

# знаходимо кореляційну матрицю
MatrixCor <- cor(X)
MatrixCor

#### 3-й крок ####

# знаходимо визначник матриці
detCor <- det(MatrixCor)
detCor

# обчислюємо експериментальне значення критерію хі квадрат
x2 <- -((n-1)-(2*m+5)/m)*log(abs(detCor))
x2

# перевіряємо отримані значення за допомогою фунції omcdiag з бібліотеки mctest
# Ця функція обчислює різні загальні міри діагностики
# мультиколінеарності для матриці регресорів
# Тут Хі квадрат(Farrar Chi-Square) 
# і визначник матриці кореляції(Determinant |X'X|) збігається з обрахованими вручну
library(mctest)
omcdiag(mod1)

# При лямбда = 0.05 і к = (1/2)(6(6-1))=15
# табличне значення хі квадрат = 25,
# Оскільки наше обраховане значення більше, то
# в масиві незалежних змінних існує мультиколінеарність

#### 4-й крок ####
# знаходимо матрицю похибок(інвертована матриця)
C <- solve(MatrixCor)
C

#### 5-й крок #### 

# розрахуємо F-критерії
F <- numeric(6)
for (i in 1:m) {
  F[i] <- (C[i,i]-1)*(n-m)/(m-1)  
}
F

# При лямбда = 0.05 і k2 = 71,k1 = 5
# табличне значення F = 2.74,
# Оскільки наші обраховані значення більше, то
# незалежна змінна x_i мультиколінеарна з іншими

# Розрахуємо коефіцієнти детермінації для кожної змінної:
R2 <- numeric(6)
for (i in 1:m) {
  R2[i] <- 1 - 1/C[i,i] 
}
R2

# Знаходимо дисперсійно-інфляційний VIF-фактор(VIF). VIF = 5-10 або
# VIF>10 говорить про мультиколінеарність 

VIF <- numeric(6)
for (i in 1:m) {
  VIF[i] <- 1/(1-R2[i]) 
}
VIF

# перевіряємо отримані значення за допомогою фунції imcdiag з бібліотеки mctest
# Ця функція обчислює різні загальні міри діагностики
# мультиколінеарності для кожного фактора 
# Тут F-критерії(Wi) 
# і дисперсійно-інфляційного VIF-фактор(VIF) збігається з обрахованими вручну
imcdiag(mod1)

#### 6-й крок #### 
# Знайти часткові коефіцієнти кореляції
r <- C
for (k in 1:m) {
  for (j in 1:m) {
    if(k == j) r[k,k] <- 1
    else r[k,j] <- -C[k,j]/sqrt(C[k,k]*C[j,j])
  }
}
r

#### 7-й крок ####
# Розрахувати t-критерії
t <- r
for (k in 1:m) {
  for (j in 1:m) {
    t[k,j] <- abs(r[k,j])*(sqrt(n-m))/sqrt(1-r[k,j]*r[k,j])
  }
}
t

# перевіряємо отримані значення за допомогою фунції pcor з бібліотеки ppcor
# Ця функція виводить 3 матриці: 
# часткові коефіцієнти кореляції(estimate), p-values, t-критерії(statistics) 
# мультиколінеарності для кожного фактора 
# Тут часткові коефіцієнти кореляції 
# і t-критерії збігаються з обрахованими вручну
library(ppcor)
pcor(X, method = "pearson")
# При лямбда = 0.05 і k = 71
# табличне значення приблизно t = 1.99,
# Обраховані значення більше між potass – fiber, calories – weight, то
# це означає, що вони можуть бути причиною мультиколінеарності.

# Видалимо weight і potass
mod2 <- lm(rating ~ calories+fiber+shelf+cups, data = data)

mod3 <- lm(rating ~ calories+fiber+shelf+cups+weight, data = data)

mod4 <- lm(rating ~ calories+potass+shelf+cups, data = data)
summary(mod1)
summary(mod3)
summary(mod2)
summary(mod4)
#Якщо подивитися на підсумок моделі, значення adjusted R-квадрат
# і F-значення покращились

library(regclass)
VIF(mod1)
VIF(mod2)
# Значення VIF для пояснювальних змінних зменшилися до дуже низких значень.
# Отже, модель тепер вільна від мультиколінеарності.

round(cor(data), 2)
install.packages('corrplot')
library(corrplot)
corrplot::corrplot(cor(data), addCoef.col = "grey")



