
#EXERCÍCIOS DA COLETÂNEA

#1 Exercício
data = matrix(
    
  #Valores da matrix
  c(4.3, 6.8, 9.2, 7.2, 8.7, 8.6, 6.6, 5.2, 8.1, 10.9,
    7.4, 4.5, 3.8, 7.6, 6.8, 7.8, 8.4, 7.5, 10.5, 6.0,
    7.7, 8.1, 7.0, 8.2, 8.4, 8.8, 6.7, 8.2, 9.4, 7.7,
    6.3, 7.7, 9.1, 7.9, 7.9, 9.4, 8.2, 6.7, 8.2, 6.5),
  
  nrow = 4,
  
  ncol = 10,
  
  byrow = TRUE
  
)

#a)
media <- mean(data)
mediana <- median(data)
moda <- as.numeric(names(sort(data, decreasing = TRUE))[1])
variancia <- var(data)
desvio_padrao <- sd(data)
maximo <- max(data)
minimo <- min(data)
quartis <- quantile(data)

cat("Média:", media, "\n")
cat("Mediana:", mediana, "\n")
cat("Moda:", moda, "\n")
cat("Desvio Padrão:", desvio_padrao, "\n")
cat("Variância:", variancia, "\n")
cat("Mínimo:", minimo, "\n")
cat("Máximo:", maximo, "\n")
cat("Quartis:\n")
print(quartis)

#b)

Q1 <- quartis[2]
Q3 <- quartis[4]

amp <- Q3 - Q1

cat("Intervalo dos 25% menores pesos: até", Q1, "\n")
cat("Intervalo dos 75% menores pesos: até", Q3, "\n")
cat("Amplitude inter-quantil", amp, "\n")

#c)

quantil0_68 <- quantile(data, 0.68)
cat("0.68:", quantil0_68, "\n")

#d)

print("Diagrama de caule-folhas:")
stem(data)

#e)
par(mfrow=c(1,2)) # Configuração para exibir os gráficos lado a lado

# Histograma
hist(data, main="Histograma dos Pesos das Bicicletas", 
     xlab="Peso (kg)", col="lightblue", border="black")

# Boxplot
boxplot(data, main="Boxplot dos Pesos das Bicicletas", 
        col="lightgreen", horizontal=TRUE)

# Identificação de outliers
limite_inferior <- Q1 - 1.5 * amp
limite_superior <- Q3 + 1.5 * amp
outliers <- data[data < limite_inferior | data > limite_superior]

cat("Outliers identificados:", outliers, "\n")


#2 Exercício

#a) 
Nível_de_nicotina = c("0-13","14-49","50-99",
                        "100-149","150-199","200-249",
                        "250-259","300-399")
  
Fumadores = c(78, 133,142,
                206,197,220,
                151,412)
  
Nao_Fumadores = c(3300,72,23,
                    15,7,8,
                    9,11)
  

total_fumadores = sum(Fumadores)
total_nao_fumadores = sum(Nao_Fumadores)
total_geral = total_fumadores + total_nao_fumadores

freq_rel_fumadores = round(Fumadores/total_fumadores * 100, 2)
freq_rel_nao_fumadores = round(Nao_Fumadores/total_nao_fumadores*100, 2)

data = data.frame(
  Nível_de_nicotina,
  Fumadores,
  freq_rel_fumadores,
  Nao_Fumadores,
  freq_rel_nao_fumadores
)

data[nrow(data) + 1, ] <- c("Total", total_fumadores, 100, total_nao_fumadores, 100)

print(data)

#b)
library(ggplot2)

dados_hist <- data.frame(
  Intervalo = factor(rep(Nível_de_nicotina, 2), levels = Nível_de_nicotina),
  Frequencia = c(Fumadores, Nao_Fumadores),
  Grupo = rep(c("Fumadores", "Não-Fumadores"), each = length(Nível_de_nicotina))
)

# Criando o histograma comparativo
ggplot(dados_hist, aes(x = Intervalo, y = Frequencia, fill = Grupo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparação dos Níveis de Cotinina entre Fumadores e Não-Fumadores",
       x = "Nível de Cotinina (mg/ml)",
       y = "Frequência") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal()

