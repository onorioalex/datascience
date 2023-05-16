# projeto utilizando o pacote Caret com Cross validation devido ao dataset pequeno

setwd()
getwd()

library("readxl")
library("caTools")
library("dplyr")
library("randomForest")
library("caret")
library("rpart")
library("caTools")

dados <- read_excel("FEV-data-Excel.xlsx")
dados <- as.data.frame(dados)

View(dados)
dim(dados)
str(dados)


#renomeando as variaveis

colnames(dados) <- c("modelo_fabricante", "fabricante", "modelo",  "preco_minimo",
                     "potencia_motor", "torque_maximo", "config_freio", "tracao", 
                     "capacidade_bateria", "autonomia", "entre_eixos", 
                     "Comprimento", "comprimento2", "largura", "peso_min_vazio", 
                     "peso_bruto_permitido", "carga_maxima", "n_assentos", 
                     "portas", "aro", "velocidade_maxima", "volume_mala", 
                     "aceleração", "capacidade_recarga", "consumo_para_100km" ) 

#convertendo a primeira coluna em indice

rownames(dados) <- dados$modelo_fabricante
dados <- dados[,-1]
View(dados)

#cnvertendo variaveis string para para fator


dados$fabricante <- as.factor(dados$fabricante)
dados$config_freio <- as.factor(dados$config_freio)
dados$tracao <- as.factor(dados$tracao)

#transformando fatores em numeros.

dados$config_freio <- recode(dados$config_freio, "disc (front) + drum (rear)" = 1, "disc (front + rear)" = 2)

dados$tracao <- recode(dados$tracao, "4WD" = 1, "2WD (rear)" = 2, "2WD (front)" = 3)

dados$fabricante <- recode(dados$fabricante, "Audi" = 1, "BMW" = 2, "Citroën" = 3, "DS" = 4,
                                     "Honda" = 5, "Hyundai" = 6, "Jaguar" = 7, "Kia" = 8, "Mazda" = 9, 
                                     "Mercedes-Benz" = 10, "Mini" = 11, "Nissan" = 12, "Opel" = 13, 
                                     "Peugeot" = 14, "Porsche" = 15, "Renault" = 16, "Skoda" = 17, 
                                     "Smart" = 18, "Tesla" = 19, "Volkswagen" = 20)

str(dados)

#verificando se a variavel de autonomia segue uma distribuição normal


hist(dados$consumo_para_100km, xlab = "consumo médio", ylab = "qtd de modelos")
teste_de_normalidade <- shapiro.test(dados$consumo_para_100km)
teste_de_normalidade #não segue uma distribuição normal



# verificando linhas em branco e tratando linhas que contenham, valores em branco
# a qual julgo que não interfere no cosnumo do veiculo

any(is.na(dados))

linhas_que_contem_valores_em_branco <- sum(!complete.cases(dados)) #total de linhas com valores em branco
linhas_que_contem_valores_em_branco




linhas_sem_valores_em_branco <- sum(complete.cases(dados))
linhas_sem_valores_em_branco

percentual <- (linhas_que_contem_valores_em_branco/linhas_sem_valores_em_branco)* 100
percentual

linhas_em_branco <- which(rowSums(is.na(dados)) > 2)


colunas_com_branco <- sum(colSums(is.na(dados)) > 0)

branco_por_coluna <- as.data.frame(colSums(is.na(dados)), ncol = 2,)

#separando dados em treino e teste



dados <- dados[,sapply(dados, is.numeric)]
dados <- na.omit(dados)
any(is.na(dados))

View(dados)


#dividindo em treino e teste

divisão = sample.split(dados$consumo_para_100km, SplitRatio = 0.70)


base_de_treinamento <- subset(dados, divisão == TRUE)
base_de_teste <- subset(dados, divisão == FALSE)

#normalizando

normalizar <- function(x) {
  return ((x-min(x)) / (max(x) - min(x)))
}

dados_normalizados <- as.data.frame((lapply(base_de_treinamento,normalizar)))
View(dados_normalizados)


#treinado o modelo para verificar a importancia das variaveis

modelo_v1 <- train(consumo_para_100km ~., data = base_de_treinamento, method = "lm")

varImp(modelo_v1)
summary(modelo_v1)

#criando outro modelo com menos variaveis

modelo_v2 <- train(consumo_para_100km ~ comprimento2 + peso_bruto_permitido + autonomia  + volume_mala,
                   data = base_de_treinamento, method = "lm")

varImp(modelo_v2)
summary(modelo_v2)

modelo_v3 <- train(consumo_para_100km ~ comprimento2 + peso_bruto_permitido + autonomia  + volume_mala,
                   data = base_de_treinamento, method = "rf")

varImp(modelo_v3)
summary(modelo_v3)


# valores previstos

previsão_modelo_v1 <- predict(modelo_v1,base_de_teste)
previsão_modelo_v1

tabela_prevista <- cbind(base_de_teste$consumo_para_100km, previsão_modelo_v1)


#plotando os resultado

plot(base_de_teste$consumo_para_100km,previsão_modelo_v1 )

plot(base_de_teste$consumo_para_100km, previsão_modelo_v1, col = "blue", pch = 16,
     xlab = "Valores de teste", ylab = "Valores previstos", main = "Gráfico de dispersão entre valores previstos e valores de teste")
points(base_de_teste$consumo_para_100km, base_de_teste$consumo_para_100km, col = "green", pch = 16)





