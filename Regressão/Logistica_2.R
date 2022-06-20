library(corrplot)
library(RColorBrewer)
library(MuMIn)

# Gerando os vetores que vão receber os dados simulados
# O vetor de pólen contém os dados de número de grãos de pólen no estígma das 
# flores de macieira e consiste em números inteiros gerados a partir de uma 
# distribuição normal com média 6 e desvio padrão 2.5
pollen <- as.integer(sqrt(rnorm(n=200, mean=6, sd=2.5)^2))

# O vetor irrigation informa se as flores estão em plantas irrigadas ou não. 
# Esses dois níveis estão representados pelos valores 1 e 0 respectivamente 
irrigation <- sample(c(0,1), 200, replace = TRUE)

# O vetor fruit guarda os valores da nossa variável dependente, que consiste na 
# informação de se as flores se desenvolveram em frutos ou não, representando as 
# duas situações pelos valores 1 e 0 respectivamente. Aqui estamos criando o 
# vetor vazio do mesmo tamanho do vetor pollen criado previamente. No próximo 
# passo vamos preencher esse vetor de acordo com critérios que simularão o 
# efeito das duas variáveis explicativas criadas anteriormente (i.e. pollen e 
# irrigation)
fruit <- vector(length=length(pollen))

# O código a seguir atribuirá valores 0 e 1 ao vetor fruit de acordo com o 
# seguinte critério: em cada caso estudado (flores), se a flor recebeu 
# quantidade suficiente  de grãos de pólen (pollen > 5) e foi irrigado 
# (irrigation == 1), a flor formou um fruto, se não foi irrigado 
# (irrigation == 0) a flor tem 50% de chances de gerar um fruto ou não. Dessa 
# maneira estabelecemos que na ausência de outro fator limitante as flores que 
# receberam uma quantidade suficiente de pólen gerarão frutos e que em casos 
# onde as plantas estão sujeitas a contingência de outro fator limitante (água), 
# existe uma probabilidade de que os frutos não se formem. Nos casos onde as 
# flores não receberam quantidade suficiente de pólen para garantir que os 
# frutos se formarão na ausência de outro fator limitante (irrigation == 1) a
# flor tem uma probabilidade de 50% de formar um fruto ou não. Nos casos onde
# não há pólen suficiente e não há irrigação as flores não formam frutos.

for (i in 1:length(pollen)) {
  if (pollen[i] > 5) {
    if (irrigation[i] == 1) {
      fruit[i] <- 1
    } else {
      fruit[i] <- sample(c(0, 1), 1)
    }
  }
  else {
    if (irrigation[i] == 1) {
      fruit[i] <- sample(c(0, 1), 1)
    } else {
      fruit[i] <- 0
    }
  }
}

# Cada vez que o código acima for executado os valores dos dados simulados 
# serão diferentes...
# Agora agrupamos os três vetores no dataframe data e escrevemos nosso dataframe 
# em um arquivo .CSV no nosso diretório de trabalho

# Vendo o diretório de trabalho atual
## getwd()
# Caso queira trocar o diretório de trabalho para gravar o arquivo em .CSV
## setwd()

data <- data.frame("FRUIT"=fruit, "POLLEN"=pollen, "IRRIGATION"=irrigation)
write.csv(data, file = "pollination_test_data.csv")


# Para começar, vamos explorar as propriedades dos nossos dados. O primeiro 
# passo é carregar os dados simulados para rodar o restante do script
ourdata <- read.csv("pollination_test_data.csv", sep=",")

# Nosso dados simulados
head(ourdata)

# Resumo dos nossos dados simulados
table(ourdata$POLLEN)

table(ourdata$IRRIGATION)

table(ourdata$FRUIT)

# Histograma dos dados de pólen: Variável contínua
histograma<-hist(ourdata$POLLEN,  
                 col="lightblue", 
                 border="black", xlab="POLLEN", 
                 ylab="FREQUÊNCIA", 
                 main="Histograma dos Dados de POLLEN", 
                 breaks=10, 
                 xlim=c(0, 12)) # breaks: muda o número de barras do gráfico

# Linha de média
abline(v=mean(ourdata$POLLEN), col="red", lwd=2) #Adicionando uma linha vertical vermelha representando a média dos dados

# Acrescentando a curva normal ao gráfico
xfit <- seq(min(ourdata$POLLEN), max(ourdata$POLLEN))
yfit <- dnorm(xfit, mean=mean(ourdata$POLLEN), sd=sd(ourdata$POLLEN))
yfit <- yfit*diff(histograma$mids[1:2]) * length(ourdata$POLLEN)
lines(xfit, yfit, col="black", lwd=2)

# Adicionando a legenda
legend(x="topright", #Posição da legenda
       c("Média", "Curva normal"), #Nome da legenda
       col=c("red", "black"), #Cor
       lty=c(1, 1), #Estilo da linha
       lwd=c(2, 2)) #Grossura das linhas

# Frequência dos dados de irrigação: Variável binária
barplot(data.frame(table(ourdata$IRRIGATION))[,2], names.arg = c(0, 1), 
        col="lightblue", 
        border="black", 
        xlab="IRRIGATION",
        ylab="FREQUÊNCIA", 
        main ="Frequência de IRRIGATION")

# Frequência dos dados de fruit: Variável binária
barplot(data.frame(table(ourdata$FRUIT))[,2], 
        names.arg = c(0, 1),  
        col="lightpink", 
        border = "black", 
        xlab="FRUIT",
        ylab="FREQUÊNCIA", 
        main="Frequência de FRUIT")

# Correlações
M <-cor(ourdata[,2:4])
corrplot(M, title="Correlação entre as Variáveis: FRUIT, POLLEN e IRRIGATION", 
         mar=c(0, 0, 2, 0), 
         type="full", 
         order="hclust", 
         col=brewer.pal(n=8, name="RdYlBu"))

# Regressão logística binária
library(visreg)
library(MASS)
library(ggplot2)
library(GGally)

# Agora ajustamos nosso modelo logístico global
logit_fruit <- glm(factor(FRUIT) ~ POLLEN * factor(IRRIGATION), 
                   data=ourdata, 
                   family=binomial(link='logit'))

summary(logit_fruit)$coef

# Como podemos observar na tabela summary dos resultados da nossa regressão 
# logística binária múltipla, no item Coefficients estão os valores estimados 
# para os parâmetros na coluna Estimate em escala logit, seguido do Std. Error 
# que junto com os valores de Estimate são usados para calcular os z value de 
# cada parâmetro, que nada mais é do que uma estatística padronizada que pode 
# ser usada no teste da hipótese de que os Estimates são significativamente 
# diferentes de zero e na última coluna temos os valores de p para cada valor 
# de z.


# Estimação dos parâmetros do modelo

# Os parâmetros (b0, b1 e b2) são estimados a partir de um processo iterativo 
# onde a cada passo novos valores são atribuídos a esses parâmetros, gerando um 
# modelo alternativo a partir do qual são calculados os valores de probabilidade 
# de acordo com a equação 5 para os dois eventos P(Y=1|X1|X2) e P(Y=0|X1|X2). 
# Em posse das probabilidades dos dados de acordo com O modelo se calcula a soma 
# dos logaritmos dessas probabilidades, obtendo-se assim o logaritmo da 
# verossimilhança do modelo. Esse processo é repetido um certo número de vezes 
# até que se possa estimar qual conjunto de valores dos parâmetros possui a 
# máxima verossimilhança. Para otimizar esse processo a função glm() usa como 
# método de otimização padrão o algoritmo Iterative Weighted Least Squares (IWLS) 
# é uma implementação do Fisher Scoring Algorithm. 

# No summary do modelo também é informado o número de iterações necessárias para 
# esse algorítmo encontrar o modelo com máxima verossimilhança:
summary(logit_fruit)$iter

# Visualizamos os valores preditos na escala logit (log(odds))
visreg(logit_fruit, "POLLEN", "IRRIGATION", gg=T, overlay=T, xlab="POLLEN", ylab="F(POLLEN)") +
  ggtitle("Valores Preditos na Escala log(Odds) ou logit") + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(face = "bold"))

# Também podemos visualizar nossos coeficientes em escala de probabilidade. 
library(jtools)
summ(logit_fruit, exp = T)

# Visualizamos os valores preditos na escala de probabilidades
visreg(logit_fruit, "POLLEN", "IRRIGATION", 
       gg=T, 
       overlay=T, 
       xlab="POLLEN", 
       ylab="F(POLLEN)", 
       scale="response") +
  ggtitle("Valores Preditos na Escala de Probabilidades") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(face = "bold"))

# Sumário
summary(logit_fruit)

# Modelo na escala logit
anova(logit_fruit, test="Chisq")

# Critério de informação de Akaike (AIC)
options(na.action = "na.fail")
dredge(logit_fruit)

# Essa função gera a tabela acima com os modelos ordenados pelo menor valor de 
# AIC. O AIC é o logLik penalizado pelo número de parâmetros do modelo. O AICc 
# nada mais é do que o AIC corrigido para amostras pequenas. Quanto menor o 
# valor de AIC, melhor o modelo. No nosso caso, o melhor modelo é o que contem 
# as variáveis irrigação e pólen.
