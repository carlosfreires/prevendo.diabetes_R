---
title: "Prevendo a ocorrência de diabetes"
author: "Carlos Freires"
date: "2025-03-22"
output:
  html_document: default
  pdf_document: default
---

# DEFINIÇÃO DO PROBLEMA

Identificar pacientes com alta probabilidade de serem diagnosticados com diabetes, tendo, no mínimo, 75% de acurácia.

O arquivo desses dados é um dataset público com características de pessoas que desenvolveram e de pessoas que não desenvolveram diabetes.

## OBTENÇÃO DOS DADOS

```{r}
diabetes <- read.csv(
  file = "D:\\Livros, documentos e tutoriais\\Cursos\\Análise de Dados em Linguagem R\\Materiais de estudo\\Material Complementar-20250321\\diabetes.csv"
)
```

# PREPARAÇÃO DOS DADOS

## Verificando o tipo dos dados das colunas do dataset

```{r}
?str
```

```{r}
str(diabetes)
```

### Verificando se existem valores não preenchidos

```{r}
?colSums()
```

```{r}
colSums(is.na(diabetes))
```

### Verificando a proporção dos valores de cada categoria

```{r}
?table
```

```{r}
table(diabetes$Outcome)
```

### Alterando o tipo da coluna "Outcome" que é int para factor

```{r}
diabetes$Outcome <- as.factor(diabetes$Outcome)
```

### Verificando valores min, max, média, mediana...

```{r}
summary(diabetes$Insulin)
```

### Criando o gráfico de boxplot para cada coluna do dataset

```{r}
boxplot(diabetes)
```

### Criando o boxplot apenas da coluna "Insulin"

```{r}
boxplot(diabetes$Insulin)
```

### Criando um histograma da coluna "Insulin"

```{r}
hist(diabetes$Insulin)
```

### Se necessario instale: install.packages("dplyr")

```{r}
library(dplyr)
```

### Filtrando o dataset por Insulin - Remoção de outliers

```{r}
diabetes2 <- diabetes %>%
  filter(Insulin <= 250)
```

```{r}
boxplot(diabetes2$Insulin)
```

## Análise exploratória

### Criação do boxplot para identificar outliers nas colunas do dataset

```{r}
boxplot(diabetes2)
```

### Criação de histogramas para visualizar a distribuição dos dados

```{r}
hist(diabetes2$Pregnancies)
```

```{r}
hist(diabetes2$Age)
```

```{r}
hist(diabetes2$BMI)
```

### Visualizando os valores de min, max, média, mediana...

```{r}
summary(diabetes2$Insulin)
```

# CONSTRUÇÃO DO MODELO

### Se necessario instale: install.packages("caTools")

```{r}
library(caTools)
```

## Divisão dos dados em treino e teste - 70% dos dados para treino e 30% dos dados para teste

```{r}
set.seed(123)
```

```{r}
index = sample.split(diabetes2$Pregnancies, SplitRatio = .70)
```

```{r}
index
```

```{r}
train = subset(diabetes2, index == TRUE)
```

```{r}
test  = subset(diabetes2, index == FALSE)
```

```{r}
dim(diabetes2)
```

```{r}
dim(train)
```

```{r}
dim(test)
```

### Se necessario instale: install.packages("caret")

```{r}
library(caret)
```

```{r}
?caret::train
```

### Se necessario instale: install.packages("e1071")

```{r}
library(e1071)
```

### Treinando a primeira versão do modelo - KNN

```{r}
modelo <- train(
  Outcome ~., data = train, method = "knn")
```

### Visualizando os resultados do modelo

```{r}
modelo$results
```

```{r}
modelo$bestTune
```

### Treinando a segunda versão do modelo - testando o comportamento do modelo com outros valores de k

```{r}
modelo2 <- train(
  Outcome ~., data = train, method = "knn",
  tuneGrid = expand.grid(k = c(1:20)))
```

### Visualizando os resultados do modelo

```{r}
modelo2$results
```

### Identificando o melhor valor de k

```{r}
modelo2$bestTune
```

### Visualizando a performance do modelo - gráfico de linhas

```{r}
plot(modelo2)
```

### Treinando a terceira versão do modelo - Naive bayes

### Se necessario instale: install.packages("naivebayes")

```{r}
library(naivebayes)
```

```{r}
modelo3 <- train(
  Outcome ~., data = train, method = "naive_bayes")
```

### Visualizando os resultados do modelo

```{r}
modelo3$results
```

```{r}
modelo3$bestTune
```

### Treinando a quarta versão do modelo - randomForest

### Se necessario instale: install.packages("randomForest")

library(randomForest)

```{r}
modelon4 <- train(
  Outcome ~., data = train, method = "rpart2"
)
```

```{r}
modelon4
```

### Verificando a importância das váriaveis para o aprendizado do modelo

```{r}
varImp(modelon4$finalModel)
```

As colunas "Insulin e Blood Pressure" não contribuem muito para o aprendizado do modelo

### Treinando o modelo sem as colunas "Insulin e BloodPressure" - train[,c(-3,-5)] exclui as colunas

```{r}
modelon4_1 <- train(
  Outcome ~., data = train[,c(-3,-5)], method = "rpart2"
)
```

```{r}
modelon4_1
```

## Visualizando a arvore de decisão

```{r}
plot(modelon4_1$finalModel)
```

```{r}
text(modelon4_1$finalModel)
```

### Se necessario instale: install.packages("kernlab")

```{r}
library(kernlab)
```

```{r}
set.seed(100)
```

```{r}
modelo5 <- train(
  Outcome ~., data = train, method = "svmRadialSigma"
  ,preProcess=c("center")
)
```

```{r}
modelo5$results
```

```{r}
modelo5$bestTune
```

## Avaliando o modelo

```{r}
?predict
```

### Testando o modelo com os dados de teste

```{r}
predicoes <- predict(modelo5,test)
```

## Visualizando o resultado das prediçoes do modelo

```{r}
predicoes
```

### Criando a confunsion matrix para Verificar os resultados do modelo

```{r}
?caret::confusionMatrix
```

```{r}
confusionMatrix(predicoes, test$Outcome)
```

## Realizando predições

### Criando um dataframe apenas com o registro de um unico paciente para simular a utilização do modelo

```{r}
novos.dados <- data.frame(
  Pregnancies = c(3),           
  Glucose = c(111.50),
  BloodPressure = c(70),
  SkinThickness = c(20),          
  Insulin = c(47.49),
  BMI = c(30.80),       
  DiabetesPedigreeFunction = c(0.34),
  Age = c(28)                     
)
```

```{r}
novos.dados
```

### Utilizando o modelo para gerar a previsão - passando os dados do paciente

```{r}
previsao <- predict(modelo5,novos.dados)
```

```{r}
resultado <- ifelse(previsao == 1, "Positivo","Negativo")
```

### Verificando o resultado da predição do modelo

```{r}
print(paste("Resultado:",resultado))
```

# VISUALIZAÇÃO DOS RESULTADOS

### Criando o arquivo com os resultados das predições

```{r}
write.csv(predicoes,'resultado.csv')
```

### Lendo o arquivo de previsões que foi gerado

```{r}
esultado.csv <- read.csv('resultado.csv')
```

### Alterando o nome das colunas do dataframe

```{r}
names(resultado.csv) <- c('Indice','Valor previsto')
```

### Visualizando o dataframe

```{r}
resultado.csv
```
