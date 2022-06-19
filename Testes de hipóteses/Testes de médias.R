a = c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3)
b = c(12.0, 12.2, 11.2, 13.0, 15.0, 15.8, 12.2, 13.4, 12.9, 11.0)

y = c(a, b)
x = c(   1,    1,    1,    1,    1,    1,    1,    1,    1,   1, 
         0,    0,    0,    0,    0,    0,    0,    0,    0,   0)


# Teste t para duas amostras independentes
t.test(a,b)  

# Teste t para duas amostras independentes
t.test(y ~ x) 

# Teste t para duas amostras pareadas
t.test(a,b,paired=TRUE)  

# H1: E(b)>E(a)
t.test (a, b, paired = TRUE, alt = "greater")

# Teste t de uma amostra
t.test(a,mu=11,alternative="two.sided") # Ho: E(a)=11

# Teste U de Mann-Whitney para duas amostras independentes
wilcox.test(a,b) 

# Teste U de Mann-Whitney para duas amostras independentes
wilcox.test(y ~ x) 

# Teste de Wilcoxon para amostras pareadas 
wilcox.test(a,b,paired=TRUE)

# H1: E(b)>E(a)
wilcox.test(a,b,paired=TRUE, alt = "greater")

# Teste de Wilcoxon de uma amostra
wilcox.test(a,mu=11,alternative = "two.sided")
