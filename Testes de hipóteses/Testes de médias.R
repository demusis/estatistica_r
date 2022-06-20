library(nortest) 
library(randtests)

a = c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3)
b = c(12.0, 12.2, 11.2, 13.0, 15.0, 15.8, 12.2, 13.4, 12.9, 11.0)

y = c(a, b)
x = c(   1,    1,    1,    1,    1,    1,    1,    1,    1,   1, 
         0,    0,    0,    0,    0,    0,    0,    0,    0,   0)

D1 <- c(1, 2, 2, 3, 4, 8, 9, 23, 25, 30, 23, 78, 55, 99, 34, 98, 89, 89, 89, 78, 
        67, 45, 2, 3, 3, 1, 7, 2, 11, 14, 1, 2, 2, 3, 4, 8, 9, 23, 25, 30, 23, 
        78, 100, 99, 99, 98, 34, 89, 89, 78, 67, 45, 2, 3, 3, 1, 7, 2, 11, 1, 1,
        2, 2, 3, 4, 8, 9, 23, 25, 30, 23, 78, 100, 99, 99, 98, 34, 89, 89, 78, 
        67, 45, 2, 60, 3, 1, 87, 88, 80, 99)
D2 <- rnorm(101)

# Testes de aderência a normal
lillie.test(D1) # Teste de Lilliefors
shapiro.test(D1) # teste de Shapiro-Wilk

# Teste de randomicidade
runs.test(D1) # Teste de Wald-Wolfowitz (run test)

# Teste t para duas amostras independentes
t.test(a,b)  
t.test(y ~ x) 

# Teste U de Mann-Whitney para duas amostras independentes
wilcox.test(a,b) 
wilcox.test(y ~ x) 

# Teste t para duas amostras pareadas
t.test(a,b,paired=TRUE)  

# Teste de Wilcoxon para amostras pareadas 
wilcox.test(a,b,paired=TRUE)

# H1: E(b)>E(a)
t.test (a, b, paired = TRUE, alt = "greater")

# H1: E(b)>E(a)
wilcox.test(a,b,paired=TRUE, alt = "greater")


# Teste t de uma amostra
t.test(a,mu=11,alternative="two.sided") # Ho: E(a)=11

# Teste de Wilcoxon de uma amostra
wilcox.test(a,mu=11,alternative = "two.sided")
