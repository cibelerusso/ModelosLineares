# Modelos Lineares
# Prof. Cibele Russo
# Exemplo Modelo de posto incompleto

# Y: vetor resposta fictício (8 observações)
Y <- c(10.1, 11.3, 9.8, 10.5, 12.0, 12.5, 11.2, 11.7)

# colunas: Intercepto, composto2, método1, método2, método3, método4
X <- matrix(c(
  1, 0, 1, 0, 0, 0,  # Y11: comp1, met1
  1, 0, 0, 1, 0, 0,  # Y12: comp1, met2
  1, 0, 0, 0, 1, 0,  # Y13: comp1, met3
  1, 0, 0, 0, 0, 1,  # Y14: comp1, met4
  1, 1, 1, 0, 0, 0,  # Y21: comp2, met1
  1, 1, 0, 1, 0, 0,  # Y22: comp2, met2
  1, 1, 0, 0, 1, 0,  # Y23: comp2, met3
  1, 1, 0, 0, 0, 1   # Y24: comp2, met4
), byrow = TRUE, ncol = 6)

dim(X)
qr(X)$rank

colnames(X) <- c("Intercept", "Comp2", "Met1", "Met2", "Met3", "Met4")

modelo1 <- lm(Y ~ X)

model.matrix(modelo1)



# Parametrização casela de referência

composto <- factor(c(1,1,1,1,2,2,2,2))
metodo <- factor(c(1,2,3,4,1,2,3,4))

# Modelo com dois fatores (sem interação)
modelo2 <- lm(Y ~ composto + metodo)
summary(modelo2)

X2 = model.matrix(modelo2)

dim(X2)
qr(X2)$rank



# Parametrização dos desvios médios
contrasts(composto) <- contr.sum(2)
contrasts(metodo) <- contr.sum(4)

modelo3 <- lm(Y ~ composto + metodo)
summary(modelo3)

modelo3$terms

X3 = model.matrix(modelo3)

dim(X3)
qr(X3)$rank




# Definindo referência explicitamente (composto 1 e método 1)
composto <- relevel(composto, ref = "1")
metodo <- relevel(metodo, ref = "1")

modelo2 <- lm(Y ~ composto + metodo)
summary(modelo2)


