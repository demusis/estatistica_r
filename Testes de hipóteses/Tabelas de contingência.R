library(gmodels)

data(infert, package = "datasets")
head(infert)

CrossTable(infert$education, infert$induced, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE, chisq=TRUE)

N = matrix(c(5, 18, 19, 12, 3, 7, 46, 29, 40), 
           nrow = 3,
           
           dimnames = list(
             "Sinistro" = c('Explosão', 'Incêndio', 'Colapso estrutural'),
             "Categoria" = c('Nível 1', 'Nível 2', 'Nível 3')))  
N

# Percentuais globais
n <- sum(N)
P <- N/n
P

# Totais
colunas.totais <- colSums(P)
colunas.totais

linhas.totais <- rowSums(P)
linhas.totais

# Probabilidades esperadas
E <- linhas.totais %o% colunas.totais
E

# Resíduos
R <- P - E
R

# Índices
I <- R/E
I

# Padronização dos dados
Z = I * sqrt(E)
SVD = svd(Z) # Singular Value Decomposition
rownames(SVD$u) <- rownames(P) # Vetor singular a esquerda
rownames(SVD$v) <- colnames(P) # Vetor singular a direita
SVD
autovalores <- SVD$d^2
prop.table(autovalores)

coordenadas.padronizadas.colunas = sweep(SVD$v, 1, sqrt(colunas.totais), "/")
coordenadas.padronizadas.colunas

coordenadas.padronizadas.linhas = sweep(SVD$u, 1, sqrt(linhas.totais), "/")
coordenadas.padronizadas.linhas

# Coordenadas principais
coordenadas.principais.colunas = sweep(coordenadas.padronizadas.colunas, 2, SVD$d, "*")
coordenadas.principais.colunas

coordenadas.principais.linhas = sweep(coordenadas.padronizadas.linhas, 2, SVD$d, "*")
coordenadas.principais.linhas

# Medidas de qualidade
pc = rbind(coordenadas.principais.linhas, coordenadas.principais.colunas) 
prop.table(pc ^ 2, 1)



library(ca)
mytable <- N

prop.table(N, 1) # Linhas
prop.table(N, 2) # Colunas

fit <- ca(N)

print(fit) # Resultados básic results 
summary(fit) # extended results 

plot(fit) # symmetric map
plot(fit, mass = TRUE, contrib = "absolute", map =
       "rowgreen", arrows = c(FALSE, TRUE)) # asymmetric map






