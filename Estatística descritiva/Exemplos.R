library(repr)
library(Hmisc)

# Lista datasets disponíveis
library(help='datasets')

# Seleciona exemplo
aux <- USArrests

describe(aux) 
# n, ausentes, unicos, média, GMD (medida de dispersão de Gini), percentis 5,10,25,50,75,90,95o 
# 5 menores e 5 maiores escores

head(aux)
tail(aux)

# install.packages('pastecs')
library(pastecs)

stat.desc(aux) 
# nbr.val, nbr.null, nbr.na, min max, range, sum, 
# mediana, média, erro padrão da média (SE.mean), intervalo de confiança da média (CI.mean), 
# variância, desvio padrão, coeficiente de variação

# install.packages('psych')
library(psych)

describe(aux)
# variável, número da variável, dados válidos, média, desvio padrão, 
# mediana, mediana dos valores absolutos dos desvios em relação a mediana (mad), min, max, skew, kurtosis, se

hist(aux$Murder)

library(mlbench)
data(PimaIndiansDiabetes)

# distribuição para variável categórica
y <- PimaIndiansDiabetes$diabetes
cbind(freq=table(y), percentual=prop.table(table(y))*100)

library(e1071)
data(PimaIndiansDiabetes)

# Calcula o coeficiente de assimetria
skew <- apply(PimaIndiansDiabetes[,1:8], 2, skewness)
print(skew)

# Calcula a matriz de correlação
correlacoes <- cor(PimaIndiansDiabetes[,1:8])
print(correlacoes)

# install.packages('corrplot')
library(corrplot)

corrplot(correlacoes, method="circle")

# Análise de outliers
head(cars)

# Introduz outliers nos dados
cars1 <- cars[1:30, ]  # Dados originais
cars_outliers <- data.frame(speed=c(19,19,20,40,45), dist=c(190, 286, 421, 400, 438))  # Outliers
cars2 <- rbind(cars1, cars_outliers)  # Dados com outliers

# Gráfico de dispersão com outliers
par(mfrow=c(1, 2))
plot(cars2$speed, cars2$dist, xlim=c(0, 45), ylim=c(0, 500), main="Com outliers", xlab="speed", ylab="dist", pch="*", col="red", cex=2)
abline(lm(dist ~ speed, data=cars2), col="blue", lwd=3, lty=2)

# Gráfico de dispersão sem outliers
plot(cars1$speed, cars1$dist, xlim=c(0, 45), ylim=c(0, 500), main="Sem outliers", xlab="speed", ylab="dist", pch="*", col="red", cex=2)
abline(lm(dist ~ speed, data=cars1), col="blue", lwd=3, lty=2)

outlier_values <- boxplot.stats(cars2$speed)$out  # Outliers
boxplot(cars2$speed, main="Speed", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

ks.test(cars1$speed, "pnorm")
shapiro.test(cars1$speed)

ks.test(cars2$speed, "pnorm")
shapiro.test(cars2$speed)


# Pacote "Outiers"
library(outliers)

set.seed(1234)
y=rnorm(100)
outlier(y)

outlier(y,opposite=TRUE)

dim(y) <- c(20,5)  # Converte uma matriz 20x5
outlier(y)

outlier(y,opposite=TRUE)

set.seed(1234)
x = rnorm(10)

scores(x)  # z-scores => (x-mean)/sd
scores(x, type="chisq")  # chi-sq scores => (x - mean(x))^2/var(x)

scores(x, type="chisq", prob=0.95) 
scores(x, type="z", prob=0.95)  
scores(x, type="t", prob=0.95)

qqnorm(x)
qqline(x)

ks.test(x, "pnorm")
shapiro.test(x)