# Instala pacotes
# install.packages('vctrs')

library(ggplot2)
library(nortest)
library(car)


#
# ANOVA de 1 fator paramétrica
#

# Carrega dados
planta.df = PlantGrowth
planta.df$group = factor(planta.df$group,labels = c("Controle", "T1", "T2"))

# Ajuste estético
planta.df[planta.df$group=='Controle','weight'] <- planta.df[planta.df$group=='Controle','weight']*10

# Salva como .csv
write.csv(planta.df,"planta_df.csv")

# Box-plot
ggplot(planta.df, aes(x = group, y = weight)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Fator") +
  ylab("Peso seco das plantas")

# Calcula ANOVA
planta.mod1 <- lm(weight ~ group, data = planta.df)
summary(planta.mod1)

anova(planta.mod1)

# Intervalos de confiança das médias
confint(planta.mod1)

# Gráfico dos resíduos
planta.mod = data.frame(Fitted = fitted(planta.mod1),
                        Residuals = resid(planta.mod1), Treatment = planta.df$group)
ggplot(planta.mod, aes(Fitted, Residuals, colour = Treatment)) + geom_point()

# função alternativa
a1 <- aov(planta.df$weight ~ planta.df$group)
summary(a1)
plot(a1)

# Post-hoc
# Teste de Tukey
posthoc <- TukeyHSD(x=a1, 'planta.df$group', conf.level=0.95)
posthoc
plot(posthoc)

# Pressupostos
# Teste de Levene
leveneTest(planta.df$weight ~ planta.df$group)

# Testes de normalidade
ks.test(resid(a1), "pnorm")
lillie.test(resid(a1))
shapiro.test(resid(a1))


#
# ANOVA de 1 fator não-paramétrica
#

kruskal.test(planta.df$weight ~ planta.df$group)


# Teste Post-hoc
pairwise.wilcox.test(planta.df$weight,
                     planta.df$group,
                     p.adjust.method="none")
