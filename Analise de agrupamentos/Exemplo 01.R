library(factoextra) 
library(FactoMineR)

# Compute PCA with ncp = 3
res.pca <- PCA(USArrests, 
               ncp = 3, 
               graph = FALSE) 

# Compute hierarchical clustering on principal components 
res.hcpc <- HCPC(res.pca, graph = FALSE)

# Dendrograma
fviz_dend(res.hcpc, 
          cex = 0.7, # Label size 
          palette = "jco", # Color palette see ?ggpubr:: ggpar 
          rect = TRUE, 
          rect_fill = TRUE, # Add rectangle around groups 
          rect_border = "jco", # Rectangle color 
          labels_track_height = 0.8 # Augment the room for labels 
          )
           
fviz_cluster(res.hcpc, repel = TRUE, # Avoid label overlapping 
             show.clust.cent = TRUE, # Show cluster centers 
             palette = "jco", # Color palette see ?ggpubr:: ggpar 
             ggtheme = theme_minimal(), 
             main = "Factor map" 
             )

# Principal components + tree 
plot(res.hcpc, choice = "3D.map")


# Variáveis categóricas
data(tea) 

# Performing MCA
res.mca <- MCA(tea, 
               ncp = 20, # Number of components kept 
               quanti.sup = 19, # Quantitative supplementary variables 
               quali.sup = c( 20: 36), # Qualitative supplementary variables 
               graph = FALSE)

res.hcpc <- HCPC(res.mca, 
                 graph = FALSE, 
                 max = 3)

# Dendrograma
fviz_dend(res.hcpc, 
          show_labels = FALSE) # Mapa fatoria dos indivíduos

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









           
                     