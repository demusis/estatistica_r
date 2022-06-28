library(caTools)
library(dplyr)
library(rpart)
library(rpart.plot)
library(Lahman)
library(maptree)
library(rpart)

# Durante um período do ano é discutido nos EUA sobre a eleição para o Hall da 
# Fama do Beisebol. A votação sempre envolve uma serie de regras e decidi 
# utilizar a famosa base Lahman, para ver como uma árvore de decisão simples 
# funcionaria como classificador e o que poderíamos aprender com isso.

# Relembrando, uma arvore de decisão é um modelo que particiona recursivamente 
# um conjunto de dados em subconjuntos cada vez mais “puros”, em relação a 
# alguma variável de resposta binária ou categórica. Nossa variável de resposta 
# binária será: “O jogador foi eleito para o Hall da Fama por meio do BBWAA?”

# Utilizaremos a base HallOfFame para sumarizar os jogadores com indicações e 
# nomeações para o HallOfFame criando um índice de performance.


indicados<- HallOfFame %>%
  group_by(playerID) %>%
  filter(votedBy %in% c("BBWAA", "Special Election") & category == "Player") %>%
  summarise(anosEmVotacao = n(), 
            indicado = sum(inducted == "Y"), 
            melhor = max(votes/ballots)) %>%
  arrange(desc(melhor))
head(indicados,5)


# Para treinar nosso modelo, precisamos de um conjunto de variáveis 
# exploratórias com os índices de performance durante os jogos dos candidatos. 
# Neste caso, incluiremos apenas duas estatísticas para rebatedores(batting):
## Hits (H)
## Home runs (HR)
rebatedores <-
  Batting %>%
  group_by(playerID) %>%
  summarise(numTemporadas = length(unique(yearID)), 
            ultimaTemporada = max(yearID), 
            somaH = sum(H), 
            somaHR = sum(HR)) %>%
  arrange(desc(somaH))
head(rebatedores,5)


# Para arremessadores(pitching), incluiremos as seguintes estatísticas:
## Wins(W)
## Shutout (SO)
## Save (SV)
arremessadores <- Pitching %>%
  group_by(playerID) %>%
  summarise(numTemporadas = length(unique(yearID)), 
            ultimaTemporada = max(yearID), 
            somaW = sum(W), 
            somaSO = sum(SO), 
            somaSV = sum(SV)) %>%
  arrange(desc(somaW))
head(arremessadores,5)

# Não menos importante, incluiremos as principais premiações individuais da MLB 
# (MVP, Cy Young e Gold Glove)
premios <- AwardsPlayers %>%
  group_by(playerID) %>%
  summarise(mvp = sum(awardID == "Most Valuable Player"), 
            gg = sum(awardID == "Gold Glove"), 
            cyya = sum(awardID == "Cy Young Award"))
head(premios,5)

# Desta forma criamos quatro conjuntos de dados e precisaremos criar um 
# cruzamento com todas essas informações. Lembrando que nesta analise queremos 
# apenas jogadores que já foram nomeados candidatos para o Hall Da Fama portanto 
# devemos unir os dados de acordo com essa regra.
candidatos <- merge(x=rebatedores, y=arremessadores, by="playerID", all=TRUE)
candidatos <- merge(x=candidatos, y=premios, by="playerID", all.x=TRUE)
candidatos <- merge(x=candidatos, y=indicados, by="playerID")
head(candidatos,5)

# Como podemos notar, a nossa base possui uma serie de NAs, para solucionar 
# este problema substituiremos os dados por 0 para que possamos executar o 
# algoritmos sem maiores problemas.
candidatos[is.na(candidatos)] <- 0


# Criando o Modelo
model <- rpart(as.factor(indicado) ~ somaH + somaHR + mvp + somaW + somaSO + 
                                     somaSV + gg+ cyya, data=candidatos)

# Com o nosso modelo criado, podemos analisar como ficou a disposição da nossa 
# arvore e os respectivos graus de entropia.
prp(model)

# Vemos que a variável com menor entropia (a que gera a primeira divisão nos 
# dados) é a variável somaH que representa a soma todos os Hits executados pelo 
# atleta ao longo de sua carreira. Caso a resposta para a pergunta “Você 
# acumulou mais de 2578 hits em sua carreira?” seja Sim/yes, então seguimos 
# para a próxima pergunta, “Você já acumulou mais de 2993 hits em sua carreira?” 
# Se a resposta for sim/yes, então este atleta esta no Hall da Fama.

# Podemos também avaliar o outro lado da arvore, onde caso a reposta para a 
# primeira pergunta seja Não/no, então avaliamos se a soma total de Wins de cada 
# atleta durante a carreira foi maior que 249. Caso a resposta seja sim e o soma 
# total de Shutout for maior que 2490, este atleta estará no Hall da fama.

# Desta forma, é possível analisar todas as classes criadas na nossa arvore.

# Seguindo o comando abaixo, podemos analisar a quantidade de observações por nó 
# folha existem no modelo criado.
rpart.plot(model,type = 3)

# Mas podemos notar que a árvore obtida esta pouco complicada de interpretar e 
# visualizar, podemos definir que esta arvore esta overfitted (muito ajustada).

# Neste caso, como evitar o excesso de ajuste? Alterando o tamanho do parâmetro 
# minbucket, minsplit ou maxdepth.

# Um dos benefícios de trabalhar com árvores de decisão, é que podemos 
# interromper o treinamento com base em vários limites. Por exemplo, dada uma 
# árvore de decisão que divide os dados em dois nós de 40 e 4. Provavelmente, 4 
# é um número muito pequeno para ter como nó terminal.

# A opção minbucket fornece o menor número de observações permitidas em um nó 
# terminal. Em processo divisão de nós, os dados em um nó estiver com menos do 
# que o minbucket definido, então não será aceito.

# O parâmetro minsplit é o menor número de observações em que o nó pai pode ser 
# dividido. O padrão é 20. Se você tiver menos de 20 registros em um nó pai, ele 
# será rotulado como um nó terminal.

# Finalmente, o parâmetro maxdepth impede que a árvore cresça além de uma certa 
# profundidade / altura sendo o valor padrão como 30.

# Decidi por controlar o parâmetro de profundidade da arvore conforme o código 
# abaixo. Esta definição fica a critério de cada analista, de acordo com a sua 
# interpretação do problema.
model <- rpart(as.factor(indicado) ~ somaH + somaHR + mvp + somaW + somaSO + somaSV + gg+ cyya, data=candidatos, maxdepth = 5)
prp(model)

rpart.plot(model, type = 3)

# Como podemos notar, a maioria dos atletas não estão no Hall Da Fama da MLB, 
# pois os agrupamentos onde existem atletas pertencentes a este grupo chega ao 
# máximo de 2% de representatividade (por classe) para toda a base. Portanto 
# existe uma chance bem remota do atleta entrar o Hall da Fama, desde que ele 
# possua ótimos rendimentos a longo da sua carreira, o que demanda muitos anos 
# para acumular tais números relacionados a quantidade de Hits, Home Runs e 
# Shutouts. Não é surpreendente ver que os prêmios como Cy Young não seja 
# considerado como significativo no modelo por conta da relação tempo x carreira.


# Predição com Arvore de Classificação
# Agora vamos tentar prever os atletas que foram nomeados para o Hall Da Fama 
# usando todas as variáveis que temos disponíveis no nosso modelo.

# Dividimos os dados em dois conjunto de treinamento e teste.
set.seed(123)
split = sample.split(candidatos$indicado, 
                     SplitRatio = 0.7)
train = subset(candidatos, split==TRUE)
nrow(train)
test = subset(candidatos, split==FALSE)
nrow(test)

# Os dados de treinamento são um subconjunto dos dados de Candidatos, onde a 
# divisão é TRUE. E os dados de teste são o subconjunto dos dados de Candidatos, 
# onde a divisão é FALSE.
model_pred = rpart(as.factor(indicado) ~ somaH + somaHR + mvp + gg + somaW + 
                                         somaSO + somaSV + cyya, 
                   data=train,maxdepth = 5)
prp(model_pred)

rpart.plot(model_pred,type = 3)

# Utilizando a função summary, é possível entrar no detalhe do modelo previsto 
# para analisar o grau de importância de cada variável e as proporções para cada 
# classe.
summary(model_pred)

predictTrain <- predict(model_pred, 
                        type="class", 
                        newdata = test)
table(test$indicado,predictTrain)

# Portanto, a acurácia do modelo é (301 + 13) / (301 +13 + 24 + 6) = 91.3%

# Resultados
# As variáveis Hits, Home Run são realmente importantes.
## O premio de MVP é significativamente importante em caso de baixa performance 
#  nas outras variáveis.
## A variável ShutOuts aparece duas vezes, isso quer dizer que, de alguma forma 
#  esta variável é não linear. Pois se o valor for maior que um montante ou 
#  menor, atua de maneira distinta.
## Apesar da alta acurácia, a variável recall tem o valor de 50%. Necessário 
#  ajustes no modelo.
