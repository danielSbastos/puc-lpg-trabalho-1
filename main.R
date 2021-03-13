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

cores <- c(rgb(0.2,0.1,0.1,0.6), rgb(0.2,0.8,0.8,0.6), rgb(1, 1, 1,0.6))
plot <- barplot(
  medias_2, names.arg = idades,
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

a = c(0)
b = c(1:998)
c = c(999:1497)
d = c(1498:1996)
e = c(1997:2495)
f = c(2496:2994)      
g = c(2995:3992)   
h = c(3993:4990)   
i = c(4991:5988)   
j = c(5989:6986)   
k = c(6987:7964)   
l = c(7955:8982)   
m = c(8983:9980)   
n = c(9981:11976)   
o = c(11977:14970)   
p = c(14971:19960)   
q = c(19960)

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
plot <- barplot(
  medias_3, names.arg = rendas,
  main = "Média de Notas por Renda",
  xlab="Renda", ylab="Notas",
  width = 0.9, ylim = c(0, 1000),
  col=cores
)

arrows(plot, sdY0_3, plot, sdY1_3, angle=90, code=3, length = 0.05)