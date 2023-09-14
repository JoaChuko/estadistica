rm(list=ls())

#  KDE: Estimación de densidad por núcleos en R
ind <- function(x, min, max) {
  (min <= x) & (x <= max)
}

triangular <- function(x) {
  (1 - abs(x)) * ind(x, -1, 1)
}

gaussiano <- function(x) {
  dnorm(x, mean = 0, sd = 1)
}

rectangular <- function(x) {
  ind(x, -1, 1) / 2
}

epanechnikov <- function(x) {
  3 / 4 * (1 - x**2) * ind(x, -1, 1)
}

nucleos <- list(
  triangular = triangular,
  gaussiano = gaussiano,
  rectangular = rectangular,
  epanechnikov = epanechnikov
)

hacer_kde <- function(datos, h = 1, nucleo = "gaussiano") {
  kde <- function(xs) {
    n <- length(datos)
    K <- nucleos[[nucleo]]
    m <- length(xs)
    fs <- vector(length=m)
    for (i in seq.int(m)) {
      fs[i] <- sum(K((xs[i] - datos) / h)) / (n * h)
    }
    fs
  }
  return(kde)
}

# Creo una muestra
set.seed(123)  # Para hacer que los resultados sean reproducibles
muestra_exponencial <- rexp(1000, rate = 1)  # Cambia el tamaño de la muestra y el valor de rate según tus necesidades
datos <- data.frame(Valores = muestra_exponencial)

# Funcion empirica
funcion_empirica <- ecdf(muestra_exponencial)

# Creo el grafico
library(ggplot2)


ggplot(datos, aes(x = Valores)) +
  geom_function(fun=funcion_empirica, color ="blue", size = 1) +
  geom_function(fun = pexp, args = list(rate = 1), color = "red", size = 1) +
  labs(title = "Distribución Exponencial",
       x = "Valores",
       y = "Probabilidad Acumulada") +
  theme_minimal()

# Estimadores

Est= 1/(mean(muestra_exponencial))
#dens_est = density(muestra_exponencial)
densidad= hacer_kde(muestra_exponencial,h=0.2,"triangular")

valores=data.frame( equis=densidad(seq(from=0,to=6,length.out=100)))
distribucion <- ecdf(valores$equis)
library(ggplot2)
ggplot(datos, aes(x = Valores)) + 
  geom_function(fun = distribucion , color = "green", size = 1) +
  geom_function(fun = funcion_empirica, color = "blue", size = 1) +
  geom_function(fun = pexp, args = list(rate = 1), color = "red", size = 1) +
  labs(title = "Distribución Exponencial",
       x = "Valores",
       y = "Probabilidad Acumulada") +
  theme_minimal()

#como aproximar la densidad
ggplot(datos, aes(x = Valores)) + 
  geom_density(aes(x = Valores))
