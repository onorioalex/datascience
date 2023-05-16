#projeto com feedback sobre a eficiencia de extintores acusticos

setwd()
getwd()


library("readxl")
library("caTools")
library("dplyr")
library("randomForest")
library("caret")
library("rpart")
library("caTools")
library("ROSE")
library("class")
library("gmodels")
library("e1071")

#carregando dados

dados <- read_excel("Acoustic_Extinguisher_Fire_Dataset.xlsx")
dados <- as.data.frame(dados)
any(is.na(dados))
View(dados)
str(dados)
dim(dados)


# transformando caracter em fator

dados$FUEL <- as.factor(dados$FUEL)
dados$SIZE <- as.factor(dados$SIZE)

str(dados)

#substituindo valores categricos por numericos

dados$FUEL <- as.factor(recode(dados$FUEL, "gasoline" = 1, "thinner" = 2, "kerosene" = 3, "lpg" = 4))

#ajustando o label das variavel alvo

dados$STATUS <- sapply(dados$STATUS, function(x){ifelse(x == 1, "extinto", "não Extinto" )})

# transformando caracter em fator

dados$STATUS <- as.factor(dados$STATUS)

#verificando a importancia dos dados(apenas exploração)

Variaveis_importantes <- randomForest(STATUS ~., 
                                      data = dados,
                                      ntree = 100,
                                      nodesize =10,
                                      importance = TRUE)

varImpPlot(Variaveis_importantes)

#normalização

normalizar <- function(x) {
  return ((x-min(x)) / (max(x) - min(x)))
}

dados_normalizados <- as.data.frame(lapply(dados[c("DISTANCE", "DESIBEL", "AIRFLOW", "FREQUENCY")], normalizar))
View(dados_normalizados)

dados_normalizados <- cbind((dados[c("STATUS","FUEL", "SIZE")]),dados_normalizados)
str(dados_normalizados)

#dividindo dados em treino e teste


divisao <- sample.split(dados_normalizados$DISTANCE, SplitRatio = 0.7, set.seed(123))
base_de_treinamento <- subset(dados_normalizados, divisao == TRUE)
base_de_teste <- subset(dados_normalizados, divisao == FALSE)

dim(base_de_treinamento)
dim(base_de_teste )
dim(dados_normalizados)

# selecionando as colunas numericas para fazer o dataset de treino e teste

dados_norm_treino <- base_de_treinamento[, -1]
dados_norm_teste <- base_de_teste[,-1]

#criando dataset com os labels da variavel alvo


dados_treino_labels <- base_de_treinamento[,1]
dados_teste_labels <- base_de_teste[,1]

#criando o modelo

modelo_knn_v1 <- knn(train= dados_norm_treino,
                  test= dados_norm_teste,
                  cl = dados_treino_labels,
                  k=21)

#avaliando o modelo

summary(modelo_knn_v1)

#criando uma tabela cruzada dos dados previstos x os dados reais
  
CrossTable(x = dados_teste_labels, y = modelo_knn_v1, prop.chisq = FALSE)



#########################################################################

#criando segundo modelo---------------------------------------------------------------------

dados <- read_excel("Acoustic_Extinguisher_Fire_Dataset.xlsx")
dados <- as.data.frame(dados)
any(is.na(dados))
View(dados)
str(dados)
dim(dados)


# transformando caracter em fator

dados$FUEL <- as.factor(dados$FUEL)
dados$SIZE <- as.factor(dados$SIZE)

str(dados)

#substituindo valores categricos por numericos

dados$FUEL <- as.factor(recode(dados$FUEL, "gasoline" = 1, "thinner" = 2, "kerosene" = 3, "lpg" = 4))

#ajustando o label das variavel alvo

dados$STATUS <- sapply(dados$STATUS, function(x){ifelse(x == 1, "extinto", "não Extinto" )})

# transformando caracter em fator

dados$STATUS <- as.factor(dados$STATUS)


divisao <- sample.split(dados$SIZE, SplitRatio = 0.7, set.seed(123))
base_de_treinamento <- subset(dados, divisao == TRUE)
base_de_teste <- subset(dados, divisao == FALSE)

typeColnum <- grep('STATUS', names(dados))

modelo_svm_v1<- svm(STATUS ~.,
                   data = base_de_treinamento,
                   type = 'C-classification',
                   kernel = 'radial')

#previsões 

#previsões nos dados de treino.

previsão_treino <- predict(modelo_svm_v1, base_de_treinamento)
mean(previsão_treino == base_de_treinamento$STATUS)

#previsões com dados de teste

previsao_teste <- predict(modelo_svm_v1,base_de_teste)
mean(previsao_teste == base_de_teste$STATUS)


table(previsao_teste,base_de_teste$STATUS)


#########################################################################

#criando terceiro modelo---------------------------------------------------------------------



#criando um modelo com o algoritmo random forest

#criando modelo

library("readxl")
library("caTools")
library("dplyr")
library("randomForest")
library("caret")
library("rpart")
library("caTools")
library("ROSE")
library("class")
library("gmodels")
library("e1071")
library("rpart")
library("rpart.plot")

dados <- read_excel("Acoustic_Extinguisher_Fire_Dataset.xlsx")
dados <- as.data.frame(dados)
any(is.na(dados))


# transformando caracter em fator

dados$FUEL <- as.factor(dados$FUEL)
dados$SIZE <- as.factor(dados$SIZE)

str(dados)

#substituindo valores categricos por numericos

dados$FUEL <- as.factor(recode(dados$FUEL, "gasoline" = 1, "thinner" = 2, "kerosene" = 3, "lpg" = 4))

#ajustando o label das variavel alvo

dados$STATUS <- sapply(dados$STATUS, function(x){ifelse(x == 1, "extinto", "não Extinto" )})

# transformando caracter em fator

dados$STATUS <- as.factor(dados$STATUS)


divisao <- sample.split(dados$SIZE, SplitRatio = 0.7, set.seed(123))
base_de_treinamento <- subset(dados, divisao == TRUE)
base_de_teste <- subset(dados, divisao == FALSE)

ctrl<- trainControl(method = "cv", number = 10)

modelo_RF_v1 <- train(STATUS ~., data = base_de_treinamento, method = "rpart",
                trControl = ctrl, tuneLength = 30)


previsao <- predict(modelo_RF_v1,base_de_teste)
confusionMatrix(table(base_de_teste[,"STATUS"],previsao))
plot(modelo_RF_v1)

plot(modelo_RF_v1$finalModel)
rpart.plot(modelo_RF_v1$finalModel)#ficou horrivel 








