
# Exercise 1
library(ggplot2)

# Read csv of the ex1
wein <- read.csv("https://web.tecnico.ulisboa.pt/paulo.soares/pe/projeto/winequality-white-q5.csv")

# Obatining the sqrt of the ph values
sqrt_ph <- sqrt(wein$pH)

# Obtaining the values of the quality
quality <- wein$quality

# Creating the plot
p <- ggplot(wein, aes(x = factor(quality), y = sqrt_ph)) +
        geom_boxplot(outlier.shape = 19, outlier.colour = 'red',
                     outlier.stroke = 0.5,
                     outlier.size = 2, staplewidth = 0.5,
                     fill = "lightblue", color = "darkblue") +
  labs(
    title = "Influence of the sqrt ph into quality",
    x = "Wine Quality (1 = poor, 5 = excellent)",
    y = "Square Root of pH"
  ) +
  theme_minimal()

# Visualize the plot
print(p)

# Exercise 2

library(readxl)
library(ggplot2)

# Read the file of the ex2

# Read the data from the file
wein <- read_excel("~/Secretária/R_Trabalho/wine_prod_EU.xlsx")

# Remove elementes that don't have a Category or the product group is "Non-Vinitified"
# or the year is different than 2015
wein_clean <- wein[!is.na(wein$Category) &
                     wein$`Product Group` != "Non-Vinified" &
                     wein$Year == 2015,]

# Substitute the countries that aren't in the vector c("France", "Italy", "Spain", "Portugal") as Others
wein_clean$`Member State` <- ifelse(wein_clean$`Member State` %in% c("France", "Italy", "Spain", "Portugal"),
                                     wein_clean$`Member State`,
                                     "Others")

availability <- wein_clean$Availability

Member <- wein_clean$`Member State`

p <- ggplot(wein_clean, aes(x = factor(Member), y = availability)) +
      geom_bar(stat = "Identity", fill = "steelblue", color = "steelblue", width = 0.5) +
    labs(
      title = "Avability of wine in 2015",
      x = "Countries",
      y = "Availabity"
    ) +
    theme_minimal()

# Visualise the plot
print(p)


# Exercise 3

library(ggplot2)
clima <- read.csv("https://web.tecnico.ulisboa.pt/paulo.soares/pe/projeto/clima.csv")

# Converter a coluna de data e hora 
clima$Data <- as.POSIXct(clima$Data, format="%Y-%m-%d %H:%M:%S")

inicio <- as.POSIXct("2014-11-01 00:00:00")
fim <- as.POSIXct("2014-11-30 23:59:59")

# Filtrar apenas os registos de novembro de 2014
clima_novembro <- subset(clima, Data >= inicio & Data <= fim)

clima_novembro$Dia <- as.Date(clima_novembro$Data)
mediana_aux <- tapply(clima_novembro$Pressão, clima_novembro$Dia, median, na.rm = TRUE)

mediana_diaria <- data.frame(
  Dia = as.POSIXct(names(mediana_aux)),
  Mediana = as.numeric(mediana_aux)
)

Data <- clima_novembro$Data
Pressao <- clima_novembro$Pressão
Dias <- clima_novembro$Dias
Mediana <- clima_novembro$Mediana

p <- ggplot()+
  geom_line(data = clima_novembro, aes(Data, Pressao), color = "green", linewidth = 0.5) +
  geom_line(data = mediana_diaria, aes(as.POSIXct(Dia), Mediana), color = "blue", linewidth = 0.5)+
  labs(
    title = "Pressão atmosférica do mẽs de Novembro de 2014",
    x = "Data",
    y = "Pressão"
  ) +
  theme_minimal()

p

# Exercicio 4


# Primeira parte

lambda <- 21
k <- 10

valor_esperado <- lambda * gamma(1 + 1/k)

# Segunda Parte

set.seed(815)
n <- 9000

amostra <- rweibull(n, shape = k, scale = lambda)

media_amostral <- mean(amostra)

#Resultado final 

diferenca <- round(abs(valor_esperado - media_amostral),4)

print(diferenca)


# Exercicio 5

set.seed(1420)

n <- 41000

resultados <- sample(1:6, n, replace = TRUE) +
  sample(1:6, n, replace = TRUE) +
  sample(1:6, n, replace = TRUE)

freq_soma9 <- sum(resultados == 9) /n
freq_soma10 <- sum(resultados == 10) /n


resultado <- round(abs(freq_soma9 - freq_soma10),4)

print(resultado)

#Exercicio 6

# Primeira Parte

n <- 11
x <- 5.3

k_vals <- 0:floor(x)
termos <- (-1)^k_vals * choose(n, k_vals) * (x - k_vals)^n


p_exato <- sum(termos) / factorial(n)

# Segunda parte 

# Parte a
# Valor esperado e variância de uma distribuição uniforme entre 0 e 1

Ex <- 1/2
varx <- 1/12

# como a Xi são independentes então é possível utilizar o teorema do limite
#central

X <- (x - n*Ex)/sqrt(n*varx)
p_tlc <- pnorm(X, mean <- 0 , sd <- 1)

# Parte b

set.seed(5930)
m <- 120

amostras <- matrix(runif(m*n), nrow = m, ncol = n)

Sn <- rowSums(amostras)

floor_Sn <- sum(Sn <= x)

p_sim <- floor_Sn/m


d_tlc <- abs(p_exato - p_tlc)
d_sim <- abs(p_exato - p_sim)

quociente <- d_tlc/d_sim

# Exercício 7

N <- 16
SUM <- 108.62
LOG_SUM <- 30.52
LOG_SUM_DIV_N <-   log(SUM/N)
DIV_OF_SUM_LOG_BY_N= LOG_SUM/N

verossimilhanca <- function(alpha){
  return(log(alpha) - digamma(alpha) - LOG_SUM_DIV_N + DIV_OF_SUM_LOG_BY_N)
}

alpha_hat <- uniroot(verossimilhanca, c(64.5,77.5))$root

lambda_hat <- N * alpha_hat / SUM

result = (alpha_hat - 1)/lambda_hat
print(result)



#Exercício 8

set.seed(1137)
m <- 1500
n <- 17
Ex <- 0.3
sigma <- 1.44
gama <- 0.99
m_confianca <- 0

aplha <- 1 - gama
a <- (-1)*qnorm(aplha/2)
amostra <- matrix(rnorm(m*n, Ex, sigma), nrow = m, ncol = n)


for(i in 1:m){
  media <- mean(amostra[i,])
  inf <- media - a*sigma/sqrt(n)
  sup <- media + a*sigma/sqrt(n)
  if(inf <= Ex && Ex <= sup)
    m_confianca = m_confianca + 1
}

prop <- m_confianca / m

result <- round(prop / gama, digits = 4)

print(result)



# Exercício 9

# Amostras aleatórias de uma distribuição exponencial

set.seed(4032)
m <- 900
n <- 12

# H0
mu0 <- 5

#H1
mu1 <- 5.8

alpha <- 0.1

# Obtenção do valor da estimativa de teste
amostra_exp <- matrix(rexp(m*n, rate = 1/mu1), nrow = m, ncol = n)
amostra_exp <- rowMeans(amostra_exp)
T_obs <- (2* n* amostra_exp)/ mu0

# valor crítico
critico <- qchisq(1 - alpha, 2*n)

# Erro tipo 2 experimental
beta_prev <- mean(T_obs <= critico)

# Erro tipo 2 esperado

beta <- critico * mu0/mu1
beta <- pchisq(beta, 2*n)

# resultado

razao <- beta_prev/beta

print(round(razao, 4))







# Exercício 10


dados_velocidade <- c(4, 6.8, 5.6, 6.9, 5.6, 7.5, 4.5, 1.6, 7.1, 4.5, 1.7,
                      8.7, 4.3, 2.8, 0.9, 3.2, 0.8, 7.7, 1.9, 3.5, 0.4, 5.9,
                      9.6, 4.5, 0.7, 2.5, 3.6, 7.2, 3.9, 10.1, 8.2, 3.9, 7.1,
                      3.3, 4.2, 2, 1.8, 7.2, 1.4, 8.4, 5.8, 4.7, 2.4, 3.7, 3.4,
                      4.2, 3.5, 6.6, 3.3, 5, 4.7, 6.2, 2.9, 0.6, 4.6, 6.2, 4.4,
                      3.1, 4, 1.6, 5.1, 8.1, 4.9, 2.7, 5.3, 4.3, 3.1, 7.6, 11.1,
                      6.8, 2.7, 7.1, 1.8, 3.3, 3.9, 2.9, 6.3, 4.1, 6.5, 1.7,
                      6.6, 5.2, 4.8, 5.6, 4.4, 1.3, 1.6, 5.3, 5.4, 4.9, 3.5,
                      4.2, 2.1, 5.3, 3.7, 3.3, 3.8, 2.2, 4.3, 5.2, 5.4, 2.7,
                      4.6, 2.3, 0.8, 3.7, 7.4, 4.2, 4.9, 4.7, 3.8, 6.6, 4.5,
                      2.4, 4.9, 4.1, 1.7, 6.6, 4, 5.7, 4.8, 5.8, 2.1, 3, 4.6,
                      3.3, 2.1, 7.5, 2.6, 3.2, 9.3, 6.2, 2.6, 5.8, 8.2, 1.5, 
                      2.6, 5.3, 8.4, 7.8, 2.7, 3.1, 5.9, 6, 5.3, 3.9, 2.9, 5.1,
                      8, 6.5, 4.4, 2, 4.2, 4.5, 2.3, 4.9, 7.1, 7.3, 2.6, 3.1,
                      2.9, 1.4, 5.1, 6.2, 4.6, 4, 3.7, 2.4, 7.9, 3.7, 2.4, 
                      0.7, 9.1, 8, 1, 9.6, 5.2, 6.8, 3.8, 3, 5.3, 1.9, 2.8,
                      6.4, 2.6, 3, 3.1, 1.6, 3.4, 6.7, 4, 6.1, 5, 7.4, 5.4,
                      3.7, 3.2, 3.7, 5.5, 2.1, 2.3, 5.8, 4.9, 1.1, 7.5, 3.9,
                      2.5, 3.3, 1.3, 7.1, 4.7, 3.1, 6.3, 4.4, 3.4, 3.7, 4.1,
                      4.1, 2.7, 3.5, 5.3, 4.5, 3.5, 1.3, 1.5, 4, 4.8, 3, 3.7,
                      1.2, 3, 3, 2.4, 2.2, 5.8, 5.9, 6.5, 2.3, 3.4, 1.1, 5.5,
                      4.6, 2.8, 6, 6.7, 3.5, 2.9, 3.9, 8.2, 5.5, 4.4, 4.5, 
                      8.2, 1.3, 12.4, 4.8, 1.3, 6.6, 5, 1.4, 8.6, 5.7, 1.6,
                      5, 1.7, 4, 9.6, 3.7, 1.5, 7.1)

set.seed(2851)
n <- 242
amostra_velocidade <- sample(dados_velocidade, n, replace = FALSE)
sigma <- 3.9
k <- 8

prob <- seq(0, 1, length.out = k + 1)

inv_dist_Rayleight <- function(p, sigma){
  return(sqrt(-2*sigma^2 *log(1-p)))
}


# Obter os limites para os intervalos
limites <- inv_dist_Rayleight(prob, sigma)

# distribuir os valroes observados nos respetivos intervalos
freq_observadas <- hist(amostra_velocidade, breaks = limites, plot = FALSE)$count

# O que seria esperado caso fossem todos equiprováveis
esperadas <- rep(length(amostra_velocidade) / k, k)

# Obtenção do valor do teste e do valor p
resultado <- chisq.test(x = freq_observadas, p = rep(1/k, k))

cat(resultado$statistic)
cat(resultado$p.value)



