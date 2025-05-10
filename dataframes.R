dados <- data.frame(
  Nome = c("Francisco", "Ana", "Margarida"),
  Idade = c(19,20,5),
  PE = c(20, 19, 19)
)

dados[2,]

dados19 <- subset(dados, PE == 19 & Idade >= 14)

dados$Idade <- cut(dados$Idade, breaks = c(0,16,25))


dados <- read.delim()

lower <- min(dados$BP)
Q1 <- quantile(dados$BP, 0.25)
Q3 <- quantile(dados$BP, 0.75)
upper <- max(dados&BP)

dados$BP <- cut(dados$BP, breaks = c(lower,Q1,Q3,upper))

library(ggplot)

ggplot(dados)+
  geom_point(aes(RISK, S6, color = BP)) +
  labs(title = "Diabetes progression study")+
  theme_minimal()


dados <- read.csv("jaeof'okefk0akfaewk0fe") #nome do ficheiro

dados

dados$Date <- as.Date(dados$Date)
incio <- as.Date("2022-10-01")
fim <- as.Date("2022-11-30")
galp <- subset(dados, Name=="GALP ENERGIA-NOM" & Date >= inicio & Date <= fim)

ggplot(dados)+
  geom

