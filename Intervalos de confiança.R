# Dados
set.seed(1234)
x <- rnorm(100)

# Intervalo de confiança da média (Normal)
norm.interval = function(data, variance = var(data), conf.level = 0.95) { 
  z = qnorm((1 - conf.level)/2, lower.tail = FALSE)
  xbar = mean(data)
  sdx = sqrt(variance/length(data))
  c(xbar - z * sdx, xbar + z * sdx)
} 
norm.interval(x)

# Intervalo de confiança da variância (Normal)
var.interval = function(data, conf.level = 0.95) { 
  df = length(data) - 1
  chilower = qchisq((1 - conf.level)/2, df) 
  chiupper = qchisq((1 - conf.level)/2, df, lower.tail = FALSE) 
  v = var(data) 
  c(df * v/chiupper, df * v/chilower) 
} 
var.interval(x)

# Distribuição t
t.test(x,conf.level=0.95)

# Distribuição binomial
prop.test(83, 100, 0.75)

# Distribuição de Poisson
z <- rpois(20, lambda=4)
poisson.test(sum(z),length(z),r=4) 


# Intervalos de confiança amostrais
n.draw = 100 
mu = mean(x) 
n = length(x)
SD = sd(x) 
SD

draws = matrix(rnorm(n.draw * n, mu, SD), n)

# Quantos intervalos de confiança contém a média populacional
get.conf.int = function(x) t.test(x)$conf.int 
conf.int = apply(draws, 2, get.conf.int) 
sum(conf.int[1, ] <= mu & conf.int[2, ] >= mu)

plot(range(conf.int), c(0, 1 + n.draw), type = "n", xlab = "Desvio da média", 
     ylab = "Amostragem") 
for (i in 1:n.draw) lines(conf.int[, i], rep(i, 2), lwd = 2) 
abline(v = mu, lwd = 2, lty = 2)


# Bootstrap
# install.packages("boot",dep=TRUE)
library(boot)

# Função onde o primeiro argumento é o DataSet, e o segundo o índice das observações no DataSet.  
fc <- function(d, i){
  d2 <- d[i,]
  return(cor(d2$write, d2$math))
}


hsb2<-read.table("https://stats.idre.ucla.edu/stat/data/hsb2.csv", sep=",", header=T)
hsb2

bootcorr <- boot(hsb2, fc, R=500)
bootcorr

summary(bootcorr)
plot(bootcorr)

boot.ci(boot.out = bootcorr, type = c("norm", "basic", "perc", "bca"))

hist(bootcorr$t)
