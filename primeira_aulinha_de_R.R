x = 1
y <- -pi
y+x
y*x
107091 %% 4


v <- c(1,2,3,4)
u <- c(1,"olá")

x <- 2
x =1
u <- 1:4
u + v

u[2:3]

A <- matrix(1:20, nrow=5, ncol=4)
A

B <- matrix(1:4, nrow = 4, ncol = 1)
B

A%*%B


par <- function(n){
  if(n %% 2)
    return(paste(n, "é par"))
  else
    return(paste(n, "é ímpar"))
}
par(120485738192048)

fatorial_for <- function(n){
  prod = 1
  for(i in 1:n)
    prod = prod*i
  return(prod)
}

fatorial_for(5)


fatorial_while <- function(n){
  prod = 1
  while(n > 0){
    prod = prod * n
    n = n-1
  }
  return(prod)
}

fatorial_while(4)

fatorial_w <- function(n){
  return(prod(1:n))
}

fatorial_w(5)


### MA = SOMA/N = MEW
### MG = RAIZ(PRODUTO)
### N/SOMA(INVERSOS)
### AMOSTRAL = RAIZ(SOMA DA DIFERENÇA DOS QUADRADOS/(MEW -1))