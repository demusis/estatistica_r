## Distribuição binomial
combinacoes <- choose(60, 6)
combinacoes

probabilidade <- 1 / combinacoes
probabilidade

# Exemplo: Concurso para cientista de dados
# Em um concurso para preencher uma vaga de cientista de dados temos um total de 
# 10 questões de múltipla escolha com 3 alternativas possíveis em cada questão. 
# Cada questão tem o mesmo valor. Suponha que um candidato resolva se aventurar 
# sem ter estudado absolutamente nada. Ele resolve fazer a prova de olhos vendados 
# e chutar todas as resposta. Assumindo que a prova vale 10 pontos e a nota de 
# corte seja 5, obtenha a probabilidade deste candidato passar para a próxima etapa 
# do processo seletivo.

n <- 10 # Número de ensaios
n

# Probabilidade de sucesso
numero_de_alternativas_por_questao <- 3
p <- 1 / numero_de_alternativas_por_questao
p

# Total de eventos que se deseja obter sucesso (k)?
k <- 5
k

dbinom(x = 5, size = n, prob = p) + 
  dbinom(x = 6, size = n, prob = p) + 
  dbinom(x = 7, size = n, prob = p) + 
  dbinom(x = 8, size = n, prob = p) + 
  dbinom(x = 9, size = n, prob = p) + 
  dbinom(x = 10, size = n, prob = p)

sum(dbinom(x = 5:10, size = n, prob = p))

pbinom(q = 4, size = n, prob = p, lower.tail = F)

## Distribuição de Poisson
# Um restaurante recebe em média 20 pedidos por hora. Qual a chance de que, em 
# determinada hora escolhida ao acaso, o restaurante receba 15 pedidos?

media <- 20 # Número médio de ocorrências por hora
media

k <- 15 # Número de ocorrências que queremos obter no período
k

probabilidade <- ((exp(1) ** (-media)) * (media ** k)) / (factorial(k))
probabilidade

probabilidade <- dpois(x = k, lambda = media)
probabilidade

## Distribuição Normal
# Em um estudo sobre as alturas dos moradores de uma cidade verificou-se que o 
# conjunto de dados segue uma distribuição aproximadamente normal, com média 1,70
# e desvio padrão de 0,1. Com estas informações qual a probabilidade de uma pessoa, 
# selecionada ao acaso, ter menos de 1,80 metros.
media <- 1.7
desvio_padrao <- 0.1

Z <- (1.8 - media) / desvio_padrao
Z

pnorm(Z)

# Importando dataset
dados <- read.csv('dados.csv')

# Amostra aleatória simples
library(dplyr)

nrow(dados)

mean(dados$Renda)

set.seed(2811)
amostra <- sample_n(dados, 1000)

nrow(amostra)
mean(amostra$Renda)

prop.table(table(dados$Sexo))
prop.table(table(amostra$Sexo))


## Estimação - Teorema do Limite Central
n <- 2000
total_de_amostras <- 1500

for (i in 1:total_de_amostras){
  if(i==1){
    amostras <- data.frame('Amostra_1' = sample(dados$Idade, n))
  } else {
    amostras[paste('Amostra_', i)] <- sample(dados$Idade, n)
  }
}
amostras
colMeans(amostras)

# O Teorema do Limite Central afirma que, com o aumento do tamanho da amostra, a 
# distribuição das médias amostrais se aproxima de uma distribuição normal com 
# média igual à média da população e desvio padrão igual ao desvio padrão da 
# variável original dividido pela raiz quadrada do tamanho da amostra. Este fato 
# é assegurado para n maior ou igual a 30.

hist(
     x = colMeans(amostras),
     main = 'Histograma das Idades Médias',
     xlab = 'Idades',
     ylab = 'Frequências'
    )

mean(dados$Idade)
mean(colMeans(amostras))

sd(dados$Idade)
sd(colMeans(amostras))
sd(dados$Idade) / sqrt(n)


## Níveis de confiança e significância
# Suponha que os pesos dos sacos de arroz de uma indústria alimentícia se distribuem
# aproximadamente como uma normal de desvio padrão populacional igual a 150 g. 
# Selecionada uma amostra aleatório de 20 sacos de um lote específico, obteve-se
# um peso médio de 5.050 g. Construa um intervalo de confiança para a média 
# populacional assumindo um nível de significância de 5%.
desvio_padrao <- 150
desvio_padrao

n <- 20
n

raiz_de_n <- sqrt(20)
raiz_de_n

sigma <- desvio_padrao / raiz_de_n
sigma

library(DescTools)
MeanCI(x = 5050, sd = sigma, type = 'norm', conf.level = 0.95, sides = 'two.sided')
MeanCI(x = mean(dados$Idade), sd = sigma, type = 'norm', conf.level = 0.95, sides = 'two.sided')

# Tamanho da amostra
# Estamos estudando o rendimento mensal dos chefes de domicílios no Brasil. 
# Nosso supervisor determinou que o erro máximo em relação a média seja de 
# R $  100,00. Sabemos que o desvio padrão populacional deste grupo de trabalhadores 
# é de R $  3.323,39. Para um nível de confiança de 95%, qual deve ser o tamanho 
# da amostra de nosso estudo?
z <- qnorm(0.975)
z

sigma <- 3323.39
sigma

e <- 100
e

n <- (z * (sigma / e)) ** 2
round(n)

# Em um lote de 10.000 latas de refrigerante foi realizada uma amostra aleatória
# simples de 100 latas e foi obtido o desvio padrão amostral do conteúdo das latas
# igual a 12 ml. O fabricante estipula um erro máximo sobre a média populacional 
# de apenas 5 ml. Para garantir um nível de confiança de 95% qual o tamanho de 
# amostra deve ser selecionado para este estudo?
N <- 10000
N

s <- 12
s

e <- 5
e

n <- ((z ** 2) * (s ** 2) * (N)) / (((z ** 2) * (s ** 2)) + ((e ** 2) * (N - 1)))
round(n)

# Estamos estudando o rendimento mensal dos chefes de domicílios com renda até 
# R $  5.000,00 no Brasil. Nosso supervisor determinou que o erro máximo em 
# relação a média seja de R $  10,00. Sabemos que o desvio padrão populacional 
# deste grupo de trabalhadores é de R $  1.082,79 e que a média populacional é de 
# R $  1.426,54. Para um nível de confiança de 95%, qual deve ser o tamanho da 
# amostra de nosso estudo? Qual o intervalo de confiança para a média considerando 
# o tamanho de amostra obtido?
renda_5000 <- dados[dados$Renda <= 5000, ]

sigma <- sd(renda_5000$Renda)
sigma

media <- mean(renda_5000$Renda)
media

# Calculando o tamanho da amostra
z <- qnorm(.975)
e <- 10
n <- (z * (sigma / e)) ** 2
round(n)

# Calculando o intervalo de confiança para a média
limite_inferior <- mean(renda_5000$Renda) - (z * (sd(renda_5000$Renda) / sqrt(n)))
limite_superior <- mean(renda_5000$Renda) + (z * (sd(renda_5000$Renda) / sqrt(n)))

limite_inferior
media
limite_superior

# Gráfico
tamanho_simulacao = 1000
medias <- c()

for(i in 1:tamanho_simulacao){
  medias <- c(medias, mean(sample_n(renda_5000, n)$Renda))
}
medias <- data.frame(medias)

ggplot(data = medias, aes(x = c(1:tamanho_simulacao), y = medias)) + 
  geom_point(size = 1.5, stroke = 0) + 
  geom_hline(yintercept = media, color = 'green') + 
  geom_hline(yintercept = limite_inferior, color = 'red') + 
  geom_hline(yintercept = limite_superior, color = 'red')

