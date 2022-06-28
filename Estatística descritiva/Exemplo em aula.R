# install.packages('psych')
library(psych)

# Rotina
aux <- 
  Dados_brutos_v_1_03_Dados_brutos[
    # c(-40)
    , 
    'x5']

describe(aux$x5)

# variável, número da variável, dados válidos, média, desvio padrão, 
# mediana, mediana dos valores absolutos dos desvios em relação a mediana (mad), min, max, skew, kurtosis, se

hist(aux$x5)
boxplot(aux$x5)
