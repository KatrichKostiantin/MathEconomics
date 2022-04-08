data <- read.table(file = "cereal.csv", header = TRUE, sep = ",")
data$name <- NULL
data$mfr <- NULL
data$type <- NULL


mod1 <- lm(rating ~ calories+fiber+potass+shelf+weight+cups, data = data)
summary(mod1)
#1

X<-data[c("calories", "fiber", "potass","shelf", "weight", "cups")]
library(GGally)
ggpairs(X)
#2

library(corpcor)
cor2pcor(cov(X))
#3

library(mctest)
omcdiag(mod1)
#4

imcdiag(mod1)
#5


mod2 <- lm(rating ~ calories+fiber+shelf+cups, data = data)
summary(mod2)

library(regclass)
VIF(mod2)
#6
