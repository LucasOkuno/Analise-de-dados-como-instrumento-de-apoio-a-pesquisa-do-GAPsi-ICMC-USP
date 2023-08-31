### TABELAS 7 e 8 - GRADUAÇÃO 2021

library(readxl)
library(qacBase)
library(epitools)

data <- read_excel("Graduação2021.xlsx")
data

n = length(data$scoreSQR)
n

table(data$scoreSQR)


data$TMC<- ifelse(data$scoreSQR < 7, "sem TMC", "com TMC")
table(data$TMC)


#raça/Etnia

TMC1 <- table(data$EtniaAgrupamento, data$TMC)
TMC1

porcentagem1 <- round(prop.table(TMC1) * 100,1)
porcentagem1

chisq.test(TMC1)



#Idade 

TMC2 <- table(data$IdadeAgrupamento, data$TMC)
TMC2

porcentagem2 <- round(prop.table(TMC2) * 100,1)
porcentagem2

chisq.test(TMC2)



#Gênero 

TMC3 <- table(data$GeneroAgrupamento, data$TMC)
TMC3

porcentagem3 <- round(prop.table(TMC3) * 100,1)
porcentagem3

chisq.test(TMC3)



#Ano ingresso

TMC4 <- table(data$AnoIngressoAgrupamento, data$TMC)
TMC4

porcentagem4 <- round(prop.table(TMC4) * 100,1)
porcentagem4

chisq.test(TMC4)



#-------------------------------------------------------------------------------
## Tabela 8

## Raça/etnia (2x2)

etnia <- table(data$EtniaAgrupamento, data$nivelSRQ)
etnia

# Teste de Independência
resultado <- chisq.test(etnia)
resultado$p.value
# Calculando a razão de chances e o intervalo de confiança
resultado <- oddsratio(etnia)
odds_ratio <- resultado$measure
# Exibindo o resultado
print(odds_ratio)
# p valor
chisq.test(etnia)


## Idade (6x2)

idade <- table(data$IdadeAgrupamento, data$nivelSRQ)
idade
# Calculando a razão de chances e o intervalo de confiança
resultado <- oddsratio(idade)
odds_ratio <- resultado$measure
# Exibindo o resultado
print(odds_ratio)
# Pvalor
p_value_ate20 <- chisq.test(idade["Até 20",,drop=FALSE])$p.value
p_value_mais30 <- chisq.test(idade["mais que 30",,drop=FALSE])$p.value


## Gênero (4x2)

genero <- table(data$GeneroAgrupamento, data$nivelSRQ)
genero
# Calculando a razão de chances e o intervalo de confiança
resultado <- oddsratio(genero)
odds_ratio <- resultado$measure
# Exibindo o resultado
print(odds_ratio)
# Pvalor
p_value_mulher <- chisq.test(genero["Mulher",,drop=FALSE])$p.value
p_value_outrasind <- chisq.test(genero["Outras identidades",,drop=FALSE])$p.value


## Ano de ingresso (3x2)

anoingresso <- table(data$AnoIngressoAgrupamento, data$nivelSRQ)
anoingresso
# Calculando a razão de chances e o intervalo de confiança
resultado <- oddsratio(anoingresso)
odds_ratio <- resultado$measure
# Exibindo o resultado
print(odds_ratio)
# Pvalor
p_value_ate2019 <- chisq.test(anoingresso["Até 2019",,drop=FALSE])$p.value
p_value_2021<- chisq.test(anoingresso["2021",,drop=FALSE])$p.value


