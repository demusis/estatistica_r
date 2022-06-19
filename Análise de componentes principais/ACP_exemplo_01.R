# Instalar pacotes
install.packages("corrplot")
install.packages("factoextra")
install.packages("FactoMineR") 

library(corrplot)
library(factoextra)
library(FactoMineR)

# Carrega base de dados
data(decathlon2)
head(decathlon2)

# Seleciona faixa de valores
decathlon2.active <- decathlon2[ 1: 23, 1: 10] 
head(decathlon2.active[, 1: 6], 4)

# Análise de componentes principais
res.pca <- PCA( decathlon2.active, graph = FALSE)
print(res.pca)

# Autovalores
eig.val <- get_eigenvalue(res.pca)
eig.val

# Gráfico de variáveis
var <- get_pca_var(res.pca)
var

fviz_pca_var( res.pca, 
              col.var = "black")
fviz_pca_var( res.pca, 
              col.var = "cos2", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
              repel = TRUE)

# Um cos2 alto indica uma boa representação da variável no componente principal. 
# Nesse caso, a variável é posicionada próxima à circunferência do círculo de 
# correlação. 
# Um cos2 baixo indica que a variável não está perfeitamente representada pelos 
# CPs. Neste caso, a variável está próxima do centro do círculo.

corrplot( var $ cos2, is.corr = FALSE)
fviz_cos2(res.pca, choice = "var", axes = 1:2)


# Contribuições das variáveis aos componentes principais
head(var$contrib, 4)
corrplot(var$contrib, is.corr = FALSE)

# As contribuições das variáveis na contabilização da variabilidade em um 
# determinado componente principal são expressas em porcentagem. As variáveis
# que estão correlacionadas com CP1 (ou seja, Dim. 1) e CP2 (ou seja, Dim. 2) 
# são as mais importantes para explicar a variabilidade no conjunto de dados. 
# Variáveis que não se correlacionam com nenhum PC ou correlacionadas com as 
# últimas dimensões são variáveis de baixa contribuição e podem ser removidas 
# para simplificar a análise geral.

# Contribuições das variáveis a CP1 
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10) 

# Contribuições das variáveis a CP2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

# A linha tracejada vermelha no gráfico acima indica a contribuição média 
# esperada. Se a contribuição das variáveis fosse uniforme, o valor esperado 
# seria 1/quantidade(variáveis) = 1/10 = 10%. Para um determinado componente, 
# uma variável com contribuição maior que esse ponto de corte pode ser 
# considerada importante na contribuição para o componente.
              
fviz_pca_var(res.pca,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
fviz_pca_var(res.pca, 
             alpha.var = "contrib")


# Gráficos de indivíduos
ind <- get_pca_ind(res.pca)
ind

fviz_pca_ind(res.pca, 
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)

fviz_pca_ind(res.pca, 
             pointsize = "cos2", 
             pointshape = 21, 
             fill = "#E7B800", 
             repel = TRUE)

fviz_pca_ind(res.pca,
             col.ind = "cos2", 
             pointsize = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
              
fviz_cos2(res.pca, choice = "ind") # Qualidade da representação
fviz_contrib(res.pca, 
             choice = "ind", 
             axes = 1:2)

fviz_pca_biplot(res.pca, 
                repel = TRUE, 
                col.var = "#2E9FDF", # Variables color 
                col.ind = "#696969" # Individuals color 
               )

# Variáveis suplementares

# Quantitativas (colunas 11 e 12)
res.pca <- PCA(decathlon2, ind.sup = 24:27, quanti.sup = 11:12, quali.sup = 13, graph = FALSE)

res.pca$quanti.sup

fviz_pca_var(res.pca) # Variáveis suplementares quantitativas em tracejado

fviz_pca_var(res.pca, invisible = "var") # Somente suplementares
fviz_pca_var(res.pca, invisible = "quanti.sup") # Sem suplementares

# Indivíduos
res.pca$ind.sup

p <- fviz_pca_ind(res.pca, 
                  col.ind.sup = "blue", 
                  repel = TRUE) 
p <- fviz_add(p, 
              res.pca$quali.sup$coord, # Inclui variáveis suplementares 
                                       # qualitativas
              color = "red") 
p

# Qualitativas (coluna 13)
res.pca$quali

fviz_pca_ind(res.pca, 
             habillage = 13, 
             addEllipses = TRUE, 
             ellipse.type = "confidence", 
             palette = "jco", 
             repel = TRUE)
