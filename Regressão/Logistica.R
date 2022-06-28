setwd("/cloud/project/Regressão")

# Regressão Logistica
eleicao = read.csv("Dados_eleicao.csv", sep=',', header=T)
eleicao

plot(eleicao$DESPESAS, eleicao$SITUACAO)
summary(eleicao)

cor(eleicao$DESPESAS, eleicao$SITUACAO)

modelo = glm(SITUACAO ~ DESPESAS, data=eleicao, family="binomial") 
summary(modelo)

# Modelo comparado aos dados
plot(eleicao$DESPESAS, eleicao$SITUACAO, col='red', pch=20)
points(eleicao$DESPESAS, modelo$fitted, pch=4)

# Testar o modelo com os próprios candidatos
prever <- predict(modelo, 
                  newdata=eleicao, 
                  type="response"  )
prever <- prever >= 0.5
prever

# Avaliar performance
confusao = table(prever, eleicao$SITUACAO)
confusao
taxaacerto = (confusao[1] + confusao[4]) / sum(confusao)
taxaacerto

# Novos candidatos
prevereleicao = read.csv("Novos_candidatos.csv", sep=',', header=T)
prevereleicao

# Previsão
prevereleicao$RESULT = predict(modelo, 
                               newdata=prevereleicao, 
                               type="response") 
prevereleicao$RESULT
prevereleicao$RESULT >= 0.5

