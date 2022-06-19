# Importando dataset
dados <- read.csv('dados.csv')

# Dataframes de exemplo
materias <- c('Matemática', 'Português', 'Inglês', 'Geografia', 'História', 'Física', 'Química')
Fulano <- c(8, 10, 4, 8, 6, 10, 8)
Beltrano <- c(10, 2, 0.5, 1, 3, 9.5, 10)
Sicrano <- c(7.5, 8, 7, 8, 8, 8.5, 7)

df <- data.frame(Fulano, Beltrano, Sicrano, row.names = materias)
df

# Estatísticas descritivas
mean(dados$Renda)

aggregate(list(Renda = dados$Renda), # Variável em estudo
          list(Sexo = dados$Sexo), # Particionamento
          mean)

median(dados$Renda)
aggregate(list(Renda = dados$Renda), # Variável em estudo
          list(Sexo = dados$Sexo), # Particionamento
          median)

exemplo_moda <- c(1, 2, 2, 3, 4, 4, 5, 6, 7, 7)
exemplo_moda

freq <- table(exemplo_moda)
freq

freq[freq == max(freq)]

names(freq)[freq == max(freq)]

Moda <- function(x) {
  freq <- table(x)
  return(names(freq)[freq == max(freq)])
}

Moda(exemplo_moda)
Moda(df$Fulano)
Moda(dados$Altura)

quantile(dados$Renda, c(0.25, 0.5, 0.75))

decis <- c()
for(i in 1:9){
  decis <- c(decis, i / 10)
}
decis
quantile(dados$Renda, decis)

centis <- c()
for(i in 1:99){
  centis <- c(centis, i / 100)
}
quantile(dados$Renda, centis)

library(ggplot2)
ggplot(data = dados, aes(x = Idade)) + 
  geom_histogram(
    aes(y = cumsum(..count..)/sum(..count..)), 
    binwidth = 10
  ) + 
  geom_freqpoly(
    aes(y = cumsum(..count..)/sum(..count..)), 
    binwidth = 10,
    color = 'green'
  )

# Classificação percentual
length(dados$Idade[dados$Idade <= 50]) / length(dados$Idade) * 100

# Box-Plot
sexo = c(
         'Masculino', 
         'Feminino'
        )
cor = c(
        'Indígena', 
        'Branca', 
        'Preta', 
        'Amarela', 
        'Parda'
       )
anos_de_estudo = c(
                   'Sem instrução e menos de 1 ano', 
                   '1 ano', 
                   '2 anos', 
                   '3 anos', 
                   '4 anos', 
                   '5 anos', 
                   '6 anos', 
                   '7 anos', 
                   '8 anos', 
                   '9 anos', 
                   '10 anos', 
                   '11 anos', 
                   '12 anos', 
                   '13 anos', 
                   '14 anos', 
                   '15 anos ou mais', 
                   'Não determinados'
                  )

formatos <- theme(
  plot.title = element_text(size = 14, hjust = 0.5),
  axis.title.y = element_text(size = 12, vjust = +0.2),
  axis.title.x = element_text(size = 12, vjust = -0.2),
  axis.text.y = element_text(size = 10),
  axis.text.x = element_text(size = 10)
)

ggplot(data = dados, aes(x = "", y = Altura)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = '#3274A1') + 
  coord_flip() +
  ylab("Metros") + 
  xlab("") + 
  ggtitle('Box-plot Alturas') +
  formatos

ggplot(data = dados, aes(x = Sexo, y = Altura, group = Sexo)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = c('#3274A1', "orange")) + 
  coord_flip() +
  ylab("Metros") + 
  xlab("Sexo") + 
  ggtitle('Box-plot Alturas X Sexo') +
  formatos

dados$Cat.Sexo <- factor(dados$Sexo)
levels(dados$Cat.Sexo) <- sexo
head(dados)

ggplot(data = dados, aes(x = Cat.Sexo, y = Altura)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = c('#3274A1', "orange")) + 
  coord_flip() +
  ylab("Metros") + 
  xlab("Sexo") + 
  ggtitle('Box-plot Alturas X Sexo') +
  formatos

ggplot(data = dados[dados$Renda < 10000, ], aes(x = "", y = Renda)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = '#3274A1') + 
  coord_flip() +
  ylab("R$") + 
  xlab("") + 
  ggtitle('Box-plot Renda') +
  formatos

ggplot(data = dados[dados$Renda < 10000, ], aes(x = Cat.Sexo, y = Renda)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = c('#3274A1', "orange")) + 
  coord_flip() +
  ylab("R$") + 
  xlab("Sexo") + 
  ggtitle('Box-plot Renda X Sexo') +
  formatos


dados$Cat.Anos.de.Estudo <- factor(dados$Anos.de.Estudo, order = TRUE)

levels(dados$Cat.Anos.de.Estudo) <- anos_de_estudo
head(dados)

ggplot(data = dados, aes(x = "", y = Anos.de.Estudo)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = '#3274A1') + 
  coord_flip() +
  ylab("Anos") + 
  xlab("") + 
  ggtitle('Box-plot Anos de Estudo') +
  formatos

ggplot(data = dados, aes(x = Cat.Sexo, y = Anos.de.Estudo)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = c('#3274A1', "orange")) + 
  coord_flip() +
  ylab("Anos") + 
  xlab("Sexo") + 
  ggtitle('Box-plot Anos de Estudo X Sexo') +
  formatos


# Medidas de dispersão
df

notas_fulano <- data.frame(Fulano = df$Fulano, row.names = row.names(df))
notas_fulano

nota_media_fulano <- mean(notas_fulano$Fulano)
nota_media_fulano

notas_fulano$Desvio <- notas_fulano$Fulano - nota_media_fulano
notas_fulano

sum(notas_fulano$Desvio)

notas_fulano$Desvio.Absoluto <- abs(notas_fulano$Desvio)
notas_fulano

ggplot(data = notas_fulano, aes(x = row.names(notas_fulano), y = Fulano)) + 
  geom_point() + 
  geom_hline(yintercept = mean(notas_fulano$Fulano), color = 'red') + 
  geom_segment(aes(x = 1, y = 10, xend = 1, yend = mean(notas_fulano$Fulano))) + 
  geom_segment(aes(x = 2, y = 8, xend = 2, yend = mean(notas_fulano$Fulano))) + 
  geom_segment(aes(x = 3, y = 6, xend = 3, yend = mean(notas_fulano$Fulano))) + 
  geom_segment(aes(x = 4, y = 4, xend = 4, yend = mean(notas_fulano$Fulano))) + 
  geom_segment(aes(x = 5, y = 8, xend = 5, yend = mean(notas_fulano$Fulano))) + 
  geom_segment(aes(x = 6, y = 10, xend = 6, yend = mean(notas_fulano$Fulano))) + 
  geom_segment(aes(x = 7, y = 8, xend = 7, yend = mean(notas_fulano$Fulano)))

mean(notas_fulano$Desvio.Absoluto)

# Instalando o pacote DescTools
install.packages('DescTools')
library(DescTools)

MeanAD(df$Fulano)

# Variância e desvio padrão
variancia <- var(notas_fulano$Fulano)
variancia

sqrt(variancia)
desvio_padrao <- sd(notas_fulano$Fulano)
desvio_padrao

df

summary(df)
