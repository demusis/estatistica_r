# install.packages('outliers')
# install.packages('OutliersO3')
# install.packages('psych')
# install.packages('nortest')

library(outliers)
library(OutliersO3)
library(psych)
library(nortest)


#
# Estatística descritiva
#

head(dados)

describe(dados)

# Limites do Box-plot
outlier_v <- boxplot.stats(dados$x1, do.conf=TRUE)$stats
outlier_v

# Gráfico para avaliação da normalidade
hist(dados$x1)
qqnorm(dados$x1)
qqline(dados$x1)

# Teste de Kolmogorov-Smirnov
lillie.test(dados$x1)

# Gerando escores Z e filtrando em H para p = 1%
aux_x1 <- dados[!scores(dados$x1, prob=0.99),'x1']
lillie.test(aux_x1)

# Histograma
hist(aux_x1)


#
# Intervalos de confiança
#

# Gera conjunto de dados
set.seed(12)
x <- rnorm(10, mean=10, sd=5)

# Intervalo de confiança da média (Normal)
norm.interval = function(data, variance = var(data), conf.level = 0.95) { 
  z = qnorm((1 - conf.level)/2, lower.tail = FALSE)
  xbar = mean(data)
  sdx = sqrt(variance/length(data))
  c(xbar - z * sdx, xbar, xbar + z * sdx)
} 
norm.interval(x)

# Distribuição t
t.test(x, conf.level=0.95)

# Distribuição binomial
prop.test(53, 100, 0.5)

# Distribuição de Poisson
z <- rpois(20, lambda=4)
poisson.test(sum(z),length(z),r=4) 


# Intervalos de confiança amostrais
n_repeticoes <- 100
m = mean(x) 
n = length(x)
dp = sd(x) 

amostras = matrix(rnorm(n_repeticoes * n, m, dp), n)

# Quantos intervalos de confiança contém a média populacional?
get.conf.int = function(x) t.test(x)$conf.int 
conf.int = apply(amostras, 2, get.conf.int) 
sum(conf.int[1, ] <= mu & conf.int[2, ] >= mu)

# Graficamente
plot(range(conf.int), c(0, 1 + n.draw), type = "n", xlab = "Desvio da média",
     ylab = "Amostragem")
for (i in 1:n.draw) lines(conf.int[, i], rep(i, 2), lwd = 2) 
abline(v = 9, lwd = 2, lty = 2)


#
# Bootstrap
#

# install.packages("boot", dep=TRUE)
library(boot)

# Carrega os dados
hsb2 <- read.table("https://stats.idre.ucla.edu/stat/data/hsb2.csv", 
                 sep=",", header=TRUE)
hsb2

# Correlação entre write e math
cor(hsb2$write, hsb2$math)

# Funcao onde o primeiro argumento é o DataSet, e o segundo índice das observacoes.  
fc <- function(d, i){
  d2 <- d[i,]
  return(cor(d2$write, d2$math))
}

# Mediana
mediana_fc <- function(d, i){
  d2 <- d[i,]
  return(median(d2$write))
}

# Bootstrap não-paramétrico da correlação
bootcorr <- boot(hsb2, fc, R=50000)
bootcorr
plot(bootcorr)

boot.ci(boot.out = bootcorr, type = c("perc"))
hist(bootcorr$t)

# Bootstrap não-paramétrico da mediana
mediana_boot <- boot(hsb2, mediana_fc, R=500)
mediana_boot
plot(mediana_boot)

boot.ci(boot.out = mediana_boot, type = c("norm", "basic", "perc", "bca"))
hist(mediana_boot$t)
