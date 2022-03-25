# Informações do sistema
sessionInfo()

# Importando bibliotecas
# https://www.rdocumentation.org/packages/dplyr
library(dplyr)

# Importando dataset
dados <- read.csv('dados.csv')
head(dados, 5)

# Valores distintos
unique(select(dados, Anos.de.Estudo))

# Ordena 'dados' por 'Anos.de.Estudo'
arrange(unique(select(dados, Anos.de.Estudo)), Anos.de.Estudo)

# Cria vetor por concatenação
c(arrange(unique(select(dados, Anos.de.Estudo)), Anos.de.Estudo)) # Para variável numérica
c(arrange(unique(select(dados, UF)), UF)) # Para variável nominal

# Saída
sprintf('De %s até %s anos', min(dados$Idade), max(dados$Idade))

library(glue)
glue('De {min(dados$Idade)} até {max(dados$Idade)} anos') # Quantitativa discreta
glue('De {min(dados$Altura)} até {max(dados$Altura)} metros') # Quantitativa contínua


# Tabelas de frequência para variáveis qualitativas
table(dados$Sexo)
prop.table(table(dados$Sexo))*100

dist_freq_qualitativas <- cbind(freq = table(dados$Sexo), 
                                perc = prop.table(table(dados$Sexo))*100
                                )
dist_freq_qualitativas

colnames(dist_freq_qualitativas) <- c('Frequência', 'Porcentagem (%)')
rolnames(dist_freq_qualitativas) <- c('Masculino', 'Feminino')

dist_freq_qualitativas

frequencia <- table(dados$Sexo, dados$Cor)
frequencia

rownames(frequencia) <- c('Masculino', 'Feminino')
colnames(frequencia) <- c('Indígena', 'Branca', 'Preta', 'Amarela', 'Parda')
frequencia

percentual <- prop.table(frequencia)*100
percentual

# Médias
medias <- tapply(dados$Renda, list(dados$Sexo, dados$Cor), mean)
rownames(medias) <- c('Masculino', 'Feminino')
colnames(medias) <- c('Indígena', 'Branca', 'Preta', 'Amarela', 'Parda')
medias

# Distribuições de frequências para variáveis quantitativas

# Classificação de renda:
  
# A - Acima de 20 SM
# B - De 10 a 20 SM
# C - De 4 a 10 SM
# D - De 2 a 4 SM
# E - Até 2 SM

# Onde SM é o valor do salário mínimo na época. Em nosso caso R$ 788,00 (2015):
# A - Acima de 15.760
# B - De 7.880 a 15.760
# C - De 3.152 a 7.880
# D - De 1.576 a 3.152
# E - Até 1.576

classes <- c(min(dados$Renda), 1576, 3152, 7880, 15760, max(dados$Renda))
etiquetas <- c('E', 'D', 'C', 'B', 'A')

frequencia <- table(
                    cut(x = dados$Renda,
                        breaks = classes,
                        labels = etiquetas,
                        include.lowest = TRUE
                    )
                   )
frequencia
percentual <- prop.table(frequencia)*100
percentual

dist_freq_quantitativas_personalizadas <- cbind('Frequência' = frequencia,
                                                'Porcentagem (%)' = percentual)
dist_freq_quantitativas_personalizadas
dist_freq_quantitativas_personalizadas[
  order(row.names(dist_freq_quantitativas_personalizadas)),
]

## Gráficos
#
hist(dados$Altura)
hist(
     x = dados$Altura,
     breaks = 'Sturges',
     col = 'lightblue',
     main = 'Histograma das Alturas',
     xlab = 'Altura',
     ylab = 'Frequências',
     prob = TRUE,
     las = 1
    )

library(ggplot2)
ggplot(dados, aes(x = Altura)) + 
       geom_histogram(binwidth = 0.02, color = "black", alpha = 0.9) + 
       ylab("Frequência") + 
       xlab("Alturas") + 
       ggtitle('Histograma das Alturas') +
       theme(
             plot.title = element_text(size = 14, hjust = 0.5),
             axis.title.y = element_text(size = 12, vjust = +0.2),
             axis.title.x = element_text(size = 12, vjust = -0.2),
             axis.text.y = element_text(size = 10),
             axis.text.x = element_text(size = 10)
            )

formatos <- theme(
                  plot.title = element_text(size = 14, hjust = 0.5),
                  axis.title.y = element_text(size = 12, vjust = +0.2),
                  axis.title.x = element_text(size = 12, vjust = -0.2),
                  axis.text.y = element_text(size = 10),
                  axis.text.x = element_text(size = 10)
                 )

ggplot(dados, aes(x = Altura, y = ..density..)) + 
  geom_histogram(binwidth = 0.02, color = "black", alpha = 0.9) + 
  geom_density(color = 'green')
  ylab("Frequência") + 
  xlab("Alturas") + 
  ggtitle('Histograma das Alturas') +
  formatos
  
bar_chart <- data.frame(dist_freq_quantitativas_personalizadas)
ggplot(bar_chart, aes(x = row.names(bar_chart), y = Frequência)) + 
  geom_bar(stat = "identity") + 
  ylab("Frequência") + 
  xlab("Classes de Renda") + 
  ggtitle('Gráfico Classes de Renda') +
  formatos
