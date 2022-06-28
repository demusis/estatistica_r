# Carrega bibliotecas
library(car)
library(carData)
library(corpcor)
library(ggplot2)
library(lattice)
library(lmtest)
library(repr)


# Devine dados para análise
jacare = data.frame(
  lnComp = c(3.87, 3.61, 4.33, 3.43, 3.81, 3.83, 3.46, 3.76,
             3.50, 3.58, 4.19, 3.78, 3.71, 3.73, 3.78),
  lnPeso = c(4.87, 3.93, 6.46, 3.33, 4.38, 4.70, 3.50, 4.50,
             3.58, 3.64, 5.90, 4.43, 4.38, 4.42, 4.25)
)


# Gráfico de dispersão
xyplot(lnPeso ~ lnComp, data = jacare,
       xlab = "Comprimento - ln(polegadas)",
       ylab = "Peso - ln(quilos)",
       main = "Jacarés"
)


# Regressão linear
mod1 = lm(lnPeso ~ lnComp, data = jacare)
summary(mod1)

anova(mod1)

confint(mod1)

# Gráfico de dispersão com intervalo de confiança
p <- ggplot(data = jacare, aes(x = lnComp, y = lnPeso)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")
p


# Previsão
dado_novo = data.frame(lnComp = 5) 

predict(mod1, dado_novo, interval="confidence") 


# Pressupostos
plot(mod1)

dwtest(lnPeso ~ lnComp, data = jacare) # Teste de autocorrelação dos resíduos
hmctest(lnPeso ~ lnComp, data = jacare) # Teste de heterocedasticidade
harvtest(lnPeso ~ lnComp, data = jacare) # Teste de linearidade

# Anamorfose
# http://www.montefiore.ulg.ac.be/~kvansteen/GBIO0009-1/ac20092010/Class8/Using%20R%20for%20linear%20regression.pdf
# Tipos de regressÃ£o https://www.r-bloggers.com/15-types-of-regression-you-should-know/

# https://cran.r-project.org/web/packages/gmodels/gmodels.pdf

# https://stackoverflow.com/questions/47615125/monte-carlo-simulation-bootstrap-and-regression-in-r
# https://www.statmethods.net/advstats/bootstrapping.html
# https://stackoverflow.com/questions/23563836/bootstrap-a-linear-regression

# Regressão múltipla
head(mtcars)

carros = mtcars[c(1,3,4,6,7)]
summary(carros)

# Matriz de correlação
cor(carros) 
pairs(carros)

# Matriz de correlações parciais
cor2pcor(cor(carros))

# Modelo linear de interesse
model.car <- lm(mpg ~ disp + hp + wt + qsec, carros)

summary(model.car)

# Predição baseada apenas no disp
model.car_disp<-lm(mpg ~ disp, carros)

summary(model.car_disp) # disp significativo

# Predição baseada somente no hp
model.car_hp<-lm(mpg ~ hp, carros)
summary(model.car_hp) # hp significativo

# Prediçãobaseada em disp e hp
model.car_disp_hp<-lm(mpg ~ disp + hp, carros)
summary(model.car_disp_hp) # hp borderline

# Existe um problema de colinearidade entre disp e hp
### Gráfico de dispersão com coeficientes de correlação
panel.cor<-function(x,y,digits=2,prefix="",cex.cor)
{
  usr<- par("usr"); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r=(cor(x,y))
  txt<- format(c(r,0.123456789),digits=digits)[1]
  txt<- paste(prefix,txt,sep="")
  if(missing(cex.cor)) cex<-0.4/strwidth(txt)
  text(0.5,0.5,txt,cex=cex)
}
pairs(carros, 
      upper.panel = panel.cor,
      main="Gráfico de dispersão com matriz de correlação")

# É melhor primeiro avaliar observações influentes (outliers).
# Diagnóstico de exclusão para identificar observações influentes
influence.measures(model.car)

## Plotando medidas de influência
influenceIndexPlot(model.car,
                   id.n=3)
influencePlot(model.car,
              id.n=3) 

# Regressão após a deleção da 17a observação
model.car1 <- lm(mpg ~ disp + hp + wt + qsec, data = carros[-17,])
summary(model.car1)

# Regressão após a deleção da 17a e 30a observação
model.car2 <- lm(mpg ~ disp + hp + wt + qsec, data = carros[-c(17,30),])
summary(model.car2)

# Fator de inflação da variância - para verificar colinearidade
vif(model.car2)

# Se vif>10 existe colinearidade

# Gráfico para verificar correlações
avPlots(model.car2, id.n=2, id.cex=0.7)

# Indicam a deleção da variável WT

# Modelo final
model.car_final <- lm(mpg ~ disp + hp + qsec, data = carros[-c(17,30),])
summary(model.car_final)

# Avalia os pressupostos
plot(model.car_final)

# Gráfico de Ajuste a normalidade
qqPlot(model.car,id.n = 5) # QQ plot of studentized residuals helps in identifying outlier 
