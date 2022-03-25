# Definindo o projeto de curso
# Pergunta: O que afeta a qualidade do ar nas cidades?

# install.packges(Ecdat)
library(Ecdat)

data(Airq) # Carrega o banco de dados do pacote
names (Airq) # Exibir nomes das variáveis

# Variáveis (colunas)

## Variável resposta
# airq: índice de qualidade do ar (qto menor melhor)

## Variáveis explicativas
# vala: valor das emp[resas nas cidades (milhares de dólares)
# rain: quantidade de chuva (polegadas)
# coas: posição costeira da cidade (sim ou não)
# dens: densidade populacional (indivíduos por milha quadrada)
# medi: renda média per capita (dólares)


# Análise descritiva ou exploratória
summary(Airq) # Sumário das variáveis
              # As variáveis podem ser contínuas ou categóricas

plot(airq ~ vala, data = Airq)

# Criando um modelo estatístico
# y (resposta) ~ x (explicativa)
# Ex.: y (crescimento da planta) ~ x1 (quantidade de adubo) + x2 (quantidade de luz)

# airq ~ vala + coas + rain

# Montando um modelo
m1 <- lm(airq ~ vala, data = Airq)
m1

summary(m1)

# O p-valor indica a significância do modelo
# Se o p-valor for menor que 0.05 indica que o componente do modelo é significativo (existe um efeito)
# Se o p-valor for menor que 0.01 indica que o componente do modelo é altamente significativo

# Não torture os dados, não busque o p-valor. 

# A variável `vala` não influenciou a `aiq`


# A variável `coas` influenciou a `aiq`
m2 <- lm(airq ~ coas, data = Airq)
summary(m2)
plot(airq ~ coas, data = Airq)


# A variável `medi` não afetou a `aiq`
m3 <- lm(airq ~ medi, data = Airq)
summary(m3)
plot(airq ~ medi, data = Airq)


# A variável `rain` não afetou a `aiq`
m4 <- lm(airq ~ rain, data = Airq)
summary(m4)
plot(airq ~ rain, data = Airq)


# A variável `dens` não afetou a `aiq`
m5 <- lm(airq ~ dens, data = Airq)
summary(m5)
plot(airq ~ dens, data = Airq, xlab = 'Renda média per capita',
                               ylab = 'Qualidade do ar',
                               pch = 1, # Símbolo
                               col = 'blue',
                               cex.lab = 1.3, # Tamanho dos rótulos dos eixos
                               cex = 1.2, # Tamanho dos símbolos
                               main = 'Renda média (2010)'
     )
curve(m5$coefficients[1] + m5$coefficients[2]*x, add = TRUE, col = 'darkred',
                                                             lwd = 2, # Espessura
                                                             lty = 2 # Estilo de linha
      )

# Melhorando o blox-plot
plot(airq ~ coas, data = Airq, xlab = 'Posição costeira',
                               ylab = 'Qualidade do ar',
                               col = 'lightblue',
                               ylim = c(50, 170), # Limites de y
                               cex.lab = 1.3,
                               main = 'Análise da qualidade do ar'
     )


# Regressão múltipla

# Modelo 01
mRM1 <- lm(airq ~ vala + coas + vala*coas, data = Airq)
summary(mRM1)

mRM1b <- lm(airq ~ vala + coas, data = Airq)
summary(mRM1b)

# Gráfico
plot(airq ~ vala, data = Airq, xlab = 'Valor das empresas',
                               ylab = 'Qualidade do ar',
                               cex.lab = 1.3,
                               col = 'blue'
     )

# Reta para as cidades não costeiras
curve(mRM1$coefficients[1] + mRM1$coefficients[2]*x, add = TRUE, lwd = 1.4, # Espessura
                                                                 lty = 2, # Estilo de linha
                                                                 col = 'darkblue'
     )

# Reta para as cidades costeiras
curve(mRM1$coefficients[1] + mRM1$coefficients[2]*x + mRM1$coefficients[3], add = TRUE, 
                                                                            lwd = 1.4,
                                                                            lty = 1, 
                                                                            col = 'darkblue'      
     )

legend('bottomright', c('Não-costeiras', 'Costeiras'), pch = 1, 
                                                       lty = c(1, 2), 
                                                       col = c('darkblue', 'darkblue'),
                                                       bty = 'n', # Caixa em volta da legenda)
      )
       
# A qualidade do ar é afetada tanto por 'vala' qto por 'coas'


# Modelo 02
mRM2 <- lm(airq ~ vala + coas + dens, data = Airq)
summary(mRM2)


## Contraste de modelos
# Comparar um modelo completo com outro sem a variável em questão
modelocompleto <- lm(airq ~ vala + coas + dens, data = Airq)
modeloincompleto <- lm(airq ~ vala + coas, data = Airq)

# Os modelos são iguais?
anova(modelocompleto, modeloincompleto)