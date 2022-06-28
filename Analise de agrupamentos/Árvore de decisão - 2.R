library(readr)

# Lendo dataset
fundamentals <- read_csv("br-stocks-fundamentals.csv")
fundamentals <- fundamentals[, c("codigo", "ret_anual", "pl", "vm", "p_vpa", "divbr_pl")]

# Dicionário dos dados
# codigo: código da ação negociado em bolsa;
# ret_anual: retorno de 5 anos anualizado;
# pl: preço sobre lucro;
# vm: valor de mercado;
# p_vpa: preço sobre valor patrimonial;
# divbr_pl: dívida bruta sobre patrimônio líquido.

rtree2_fit <- rpart(ret_anual ~ ., 
                    fundamentals %>% select(-codigo)
)
rpart.plot(rtree2_fit)


# Aproveitando esta árvore como exemplo, é possível podar a árvore criada usando 
# a função prune(), como comentado anteriormente. É preciso definir pelo menos 
# o parâmetro cp, o qual representa um parâmetro de complexidade da 
# árvore, sendo cp = 0 o caso mais complexo, e cp = 1 o caso menos complexo.

# Existe uma forma de encontrar o cp ótimo para podar uma árvore de decisão pela 
# linguagem R, como segue:
  
# Utilizar o valor de CP para o menor xerror da tabela

# Ver graficamente o CP e o xerror
plotcp(rtree2_fit)

# Árvore podada
rpart.plot(prune(rtree2_fit, cp = 0.023387))  

# Se olharmos para um Preço/Lucro (pl) maior ou igual a 3,8; e ainda filtrarmos 
# as ações com P/VPA (p_vpa) maior ou igual a 0,74, então chegaremos ao grupo 
# com melhor rentabilidade anualizada dos últimos 5 anos. Este é o grupo mais 
# da direita, composto por 140 ações que geram um retorno médio anual de 12%.
res <- arrange(fundamentals, desc(ret_anual))
res <- res[res$pl >= 3.8 & res$p_vpa >= 0.74, ]
res

# Prova da média de retorno:
summarise(res, retorno = mean(ret_anual, na.rm = TRUE))
