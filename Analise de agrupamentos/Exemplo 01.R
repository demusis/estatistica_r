library(factoextra) 
library(FactoMineR)

# Calcula PCA com ncp = 3
res.pca <- PCA(USArrests, 
               ncp = 3, 
               graph = FALSE) 

# Calcula clusters hierarquicos em PCA 
res.hcpc <- HCPC(res.pca, graph = FALSE)

# Dendrograma
fviz_dend(res.hcpc, 
          cex = 0.7, # Tamanho da etiqueta 
          palette = "jco", # Paleta de cores, veja: ?ggpubr:: ggpar 
          rect = TRUE, 
          rect_fill = TRUE, # Adiciona retângulos com cantos arredondados 
          rect_border = "jco", # Cor do retângulo
          labels_track_height = 0.8 # Aumenta o espaço para as etiquetas
          )
           
fviz_cluster(res.hcpc, repel = TRUE, # Evitar sobreposição de etiquetas
             show.clust.cent = TRUE, # Apresenta os centroides
             palette = "jco", 
             ggtheme = theme_minimal(), 
             main = "Mapa fatorial" 
             )

# PCA + Dendrograma 
plot(res.hcpc, choice = "3D.map")


# Variáveis categóricas
data(tea) 

# Realizando MCA
res.mca <- MCA(tea, 
               ncp = 20, # NNúmero de componentes 
               quanti.sup = 19, # Variáveis suplementares quantitativas
               quali.sup = c( 20: 36), # Variáveis suplementares qualitativas 
               graph = FALSE)

res.hcpc <- HCPC(res.mca, 
                 graph = FALSE, 
                 max = 3)

# Dendrograma
fviz_dend(res.hcpc, 
          show_labels = FALSE) # Mapa fatorial dos indivíduos

fviz_cluster(res.hcpc, 
             geom = "point", 
             main = "Mapa dos fatores")

# Descrição por variáveis 
res.hcpc$desc.var$test.chi2

# Descrição por variáveis categóricas 
res.hcpc$desc.var$category

# As variáveis que mais caracterizam os clusters são as variáveis 
# “onde” e “como”. Cada cluster é caracterizado por uma categoria das 
# variáveis “onde” e “como”. Por exemplo, indivíduos que pertencem ao 
# primeiro cluster compram chá em saquinho de chá em redes de lojas.

res.hcpc$desc.axes

res.hcpc$desc.ind$para
