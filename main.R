setwd("~/Downloads") #definir espaço de trabalho

df <- read.csv("dados.csv") #importar datframe

# Media por estado

df1 <- df[c(6,111)] 

medias <- c()
sdY0 <- c()
sdY1 <- c()

estados <- unique(df1$SG_UF_RESIDENCIA)

for (estado in estados) {
  dados <- df1[df1$SG_UF_RESIDENCIA == estado,]
  dados <- dados[complete.cases(dados),]
  media <- mean(x = dados$NU_NOTA_REDACAO)
  
  medias <- c(medias, media)
  
  sdY0 <- c(sdY0, media - sd(dados$NU_NOTA_REDACAO))
  sdY1 <- c(sdY1, media + sd(dados$NU_NOTA_REDACAO))
}

cores <- c(rgb(0.2,0.1,0.1,0.6), rgb(0.2,0.8,0.8,0.6), rgb(1, 1, 1,0.6))
plot <- barplot(
  medias, names.arg = estados,
  main = "Notas Por Estado",
  xlab="Estados", ylab="Notas",
  width = 0.9, ylim = c(0, 1000),
  col=cores
)
arrows(plot, sdY0, plot, sdY1, angle=90, code=3, length = 0.05)

# ======
# Media por intervalo de idade
# ======

df2 <- df[c(7,111)] 

medias_2 <- c()
sdY0_2 <- c()
sdY1_2 <- c()

idades <- seq(from = min(df2$NU_IDADE), to = max(df2$NU_IDADE), by = 10)
dados <- df2[complete.cases(df2),]

for (idade in idades) {
  print(idade)
  dados_intervalo <- dados[(dados$NU_IDADE >= idade),]
  dados_intervalo <- dados_intervalo[(dados_intervalo$NU_IDADE < (idade + 10)),]
  
  media <- mean(x = dados_intervalo$NU_NOTA_REDACAO)
  medias_2 <- c(medias_2, media)
  
  sdY0_2 <- c(sdY0_2, media - sd(dados_intervalo$NU_NOTA_REDACAO))
  sdY1_2 <- c(sdY1_2, media + sd(dados_intervalo$NU_NOTA_REDACAO))
}

idades_legenda = lapply(idades, function(x) paste("[", x, "..", x + 9, "]"))

cores <- c(rgb(0.2,0.1,0.1,0.6), rgb(0.2,0.8,0.8,0.6), rgb(1, 1, 1,0.6))
plot <- barplot(
  medias_2, names.arg = idades_legenda,
  main = "Notas Por Intervalo de Idades",
  xlab="Intervalo de Idades", ylab="Notas",
  width = 0.9, ylim = c(0, 1000),
  col=cores
)

arrows(plot, sdY0_2, plot, sdY1_2, angle=90, code=3, length = 0.05)

# ======
# Media por renda familiar
# ======

df3 <- df[c(117,111)] 

legenda_renda = c(
  "Nenhuma renda",
  "Até R$998,00",    
  "De R$998,01 até R$1497,00",    
  "De R$1.497,01 até R$1.996,00",    
  "De R$1.996,01 até R$2.495,00",    
  "De R$2.495,01 até R$2.994,00",    
  "De R$2.495,01 até R$3.992,00",    
  "De R$3.992,01 até R$4.990,00",    
  "De R$4.990,01 até R$5.988,00",    
  "De R$5.988,01 até R$6.986,00",    
  "De R$6.986,01 até R$7.964,00",    
  "De R$7.964,01 até R$8.982,00",    
  "De R$8.982,01 até R$9.980,00",    
  "De R$9.980,01 até R$11.976,00",    
  "De R$11.976,01 até R$14.970,00",    
  "De R$14.970,01 até R$19.960,00",    
  "Mais de R$19.960,00"    
)

medias_3 <- c()
sdY0_3 <- c()
sdY1_3 <- c()

rendas <- unique(df3$Q006)
rendas <- as.list(levels(rendas))

dados <- df3[complete.cases(df3),]

for (renda in rendas) {
  dados_renda <- dados[(dados$Q006 == renda),]

  media <- mean(x = dados_renda$NU_NOTA_REDACAO)
  medias_3 <- c(medias_3, media)
  
  sdY0_3 <- c(sdY0_3, media - sd(dados_renda$NU_NOTA_REDACAO))
  sdY1_3 <- c(sdY1_3, media + sd(dados_renda$NU_NOTA_REDACAO))
}

cores <- c(rgb(0.2,0.1,0.1,0.6), rgb(0.2,0.8,0.8,0.6), rgb(1, 1, 1,0.6))

## Increase bottom margin to make room for rotated labels

par(mar = c(16, 6, 6, 6)) 

plot <- barplot(
  medias_3, names.arg = legenda_renda,
  main = "Média de Notas por Renda",
  ylab="Notas",
  width = 0.9, ylim = c(0, 1000),
  col=cores, las = 2
)

mtext(1, text = "Renda", line = 13)
arrows(plot, sdY0_3, plot, sdY1_3, angle=90, code=3, length = 0.05)
