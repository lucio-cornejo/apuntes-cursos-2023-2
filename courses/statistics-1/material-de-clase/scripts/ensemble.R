#####################################################################
# Bagging                                                           #
#####################################################################
library(ElemStatLearn) #Actualizar librería
data(ozone)
ozone <- ozone[order(ozone$ozone),]
head(ozone)

# Bagged loess
ll <- matrix(NA, nrow = 10, ncol = 155)
set.seed(666)
loess.all <- loess(temperature ~ ozone, data = ozone, span = 0.2)
for(i in 1:10){
  ss <- sample(1:dim(ozone)[1], replace = T)
  ozone0 <- ozone[ss,]
  ozone0 <- ozone0[order(ozone0$ozone),]
  loess0 <- loess(temperature ~ ozone, data = ozone0, span = 0.2)
  ll[i,] <- predict(loess0, newdata = data.frame(ozone = 1:155))
}

plot(ozone$ozone, ozone$temperature, pch = 19, cex = 0.5)
lines(1:155, predict(loess.all, newdata = data.frame(ozone = 1:155)),col = "green", lwd = 2)
for(i in 1:10){
  lines(1:155, ll[i,], col = "grey", lwd = 2)
}
lines(1:155, apply(ll,2,mean, na.omit = TRUE), col = "red", lwd = 2)

# Bagged trees
set.seed(123)
library(ipred)
tree_model <- bagging(formula = temperature ~ ozone,
                        data = ozone,
                        coob = TRUE)
# Mostrar los resultados
print(tree_model)

# Predicción
pred.tree<- predict(object = tree_model,    
                            newdata = data.frame(ozone = 1:155)) 
print(pred.tree)
plot(ozone$ozone, ozone$temperature, pch = 19, cex = 0.5)
lines(1:155, apply(ll,2,mean, na.omit = TRUE), col = "blue", lwd = 2)
lines(1:155,pred.tree,col = "red", lwd = 2)

# Bagging con caret (Árboles con Inferencia Condicional)
library(caret)
library(party)

predictors <- data.frame(ozone = ozone$ozone)
temperature <- ozone$temperature
set.seed(323354)
treebag <- bag(predictors, temperature, B = 30,
               bagControl = bagControl(fit = ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))
# Términos de bagging
ctreeBag$fit
ctreeBag$pred
ctreeBag$aggregate
treebag$fits[[1]]

# Predicción
plot(ozone$ozone, temperature, col = "lightgrey", pch = 19)
lines(ozone$ozone, predict(treebag$fits[[2]]$fit, predictors), pch = 19, col = "red")
lines(ozone$ozone, predict(treebag, predictors), pch = 19, col = "blue")


#---------------------------------------------------------------------#
# Random Forests                                                      #
#---------------------------------------------------------------------#
library(randomForest)
library(ISLR2)
library(tree)

#Lectura de Datos
library(Fahrmeir)
data(credit)

#Entrenamiento del Clasificador
set.seed(666)
res1 <- randomForest( Y ~ ., data = credit, 
                      mtry = 4, importance = TRUE,
                      do.trace = 10)

print(res1)

importance(res1)
varImpPlot(res1)

pred<-predict(res1, type="response")
confusionMatrix(pred,credit$Y)

#---------------------------------------------------------------------#
# Evaluación de la Predicción: Regresión                              #
#---------------------------------------------------------------------#
library(MASS)
data("Boston")
help(Boston)
head(Boston)

# División de los datos (Entrenamiento y Evaluación)
set.seed(1)
train <- sample(1:nrow(Boston), size = nrow(Boston)/2)

# Arbol de Regresión
#--------------------
library(rpart)
arbol_regresion <- rpart(formula = medv ~ ., data = Boston, subset = train,
                         method = "anova")

arbol_regresion

plot(x = arbol_regresion)
text(x = arbol_regresion, splits = TRUE, pretty = 0, cex = 0.8, col = "firebrick")

# Podado
set.seed(3)
printcp(arbol_regresion)
plotcp(arbol_regresion)


arbol_pruning <- prune(tree = arbol_regresion, cp = 0.011)
plot(x = arbol_pruning)
text(x = arbol_pruning, splits = TRUE, pretty = 0,
     cex = 0.8, col = "firebrick")

# Evaluación en el conjunto de test
predicciones <- predict(arbol_pruning, newdata = Boston[-train,])
test_mse     <- mean((predicciones - Boston[-train, "medv"])^2)
paste("Error de test (mse) del árbol de regresión tras podado:", round(test_mse,2))
# RMSE
sqrt(test_mse)
# Interpretación: Las predicciones se alejan en promedio 5.035 unidades (5035 dólares) del valor real. 


# Bagging
#----------
set.seed(1)
modelo_bagging <- randomForest(medv ~ ., data = Boston, 
                               subset = train, mtry = 13, 
                               importance =TRUE)
modelo_bagging

predicciones <- predict(object = modelo_bagging, newdata = Boston[-train,])
test_mse_bag     <- mean((predicciones - Boston[-train, "medv"])^2)
paste("Error de test (mse) del modelo obtenido por bagging es:",
      round(test_mse_bag,2))
# RMSE
sqrt(test_mse_bag)

#Importancia de las variables
importance(modelo_bagging)
varImpPlot(modelo_bagging)

# Usando ggplot2
library(tidyverse)
library(ggpubr)

importancia_pred <- as.data.frame(importance(modelo_bagging, scale = TRUE))
importancia_pred <- rownames_to_column(importancia_pred, var = "variable")
p1 <- ggplot(data = importancia_pred, aes(x = reorder(variable, `%IncMSE`),
                                          y = `%IncMSE`,
                                          fill = `%IncMSE`)) +
  labs(x = "variable", title = "Reducción de MSE") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

p2 <- ggplot(data = importancia_pred, aes(x = reorder(variable, IncNodePurity),
                                          y = IncNodePurity,
                                          fill = IncNodePurity)) +
  labs(x = "variable", title = "Reducción de pureza") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
ggarrange(p1, p2)

# Identificando el número óptimo de árboles
oob_mse <- data.frame(oob_mse = modelo_bagging$mse,
                      arboles = seq_along(modelo_bagging$mse))
ggplot(data = oob_mse, aes(x = arboles, y = oob_mse )) +
  geom_line() +
  labs(title = "Evolución del out-of-bag-error vs número árboles",
       x = "nº árboles") +
  theme_bw()

set.seed(1)
modelo_bagging <- randomForest(medv ~ ., data = Boston, subset = train,
                               mtry = 13, ntree = 90)
modelo_bagging

# Cálculo del test error 
predicciones <- predict(object = modelo_bagging, newdata = Boston[-train,])
test_mse <- mean((predicciones - Boston[-train, "medv"])^2)
paste("Error de test (mse) del modelo obtenido por bagging es:",
      round(test_mse,2))
# RMSE
sqrt(test_mse)

# Random Forest
#---------------

# Calibración de hiperparámetros

# (1) Determinar el valor del # de variables a seleccionar: mtry (por defecto sqrt(p))
tuning_rf_mtry <- function(df, y, ntree = 500){
  # Esta función devuelve el out-of-bag-MSE de un modelo RandomForest en función
  # del número de predictores evaluados (mtry)
  
  # Argumentos:
  #   df = data frame con los predictores y variable respuesta
  #   y  = nombre de la variable respuesta
  #   ntree = número de árboles creados en el modelo randomForest
  
  require(dplyr)
  max_predictores <- ncol(df) - 1
  n_predictores   <- rep(NA, max_predictores)
  oob_mse         <- rep(NA, max_predictores)
  for (i in 1:max_predictores) {
    set.seed(123)
    f <- formula(paste(y,"~ ."))
    modelo_rf <- randomForest(formula = f, data = df, mtry = i, ntree = ntree)
    n_predictores[i] <- i
    oob_mse[i] <- tail(modelo_rf$mse, n = 1)
  }
  results <- data_frame(n_predictores, oob_mse)
  return(results)
}

hiperparametro_mtry <-  tuning_rf_mtry(df = Boston, y = "medv")
hiperparametro_mtry %>% arrange(oob_mse)

ggplot(data = hiperparametro_mtry, aes(x = n_predictores, y = oob_mse)) +
  scale_x_continuous(breaks = hiperparametro_mtry$n_predictores) +
  geom_line() +
  geom_point() +
  geom_point(data = hiperparametro_mtry %>% arrange(oob_mse) %>% head(1),
             color = "red") +
  labs(title = "Evolución del out-of-bag-error vs mtry",
       x = "nº predictores empleados") +
  theme_bw()

# (2) Determinar el valor del # de obs. en un nodo terminal: nodesize (por defecto 1)
tuning_rf_nodesize <- function(df, y, size = NULL, ntree = 500){
  # Esta función devuelve el out-of-bag-MSE de un modelo randomForest en función
  # del tamaño mínimo de los nodos terminales (nodesize).
  
  # Argumentos:
  #   df = data frame con los predictores y variable respuesta
  #   y  = nombre de la variable respuesta
  #   size = tamaños evaluados
  #   ntree = número de árboles creados en el modelo randomForest
  
  require(dplyr)
  if (is.null(size)){
    size <- seq(from = 1, to = nrow(df), by = 5)
  }
  oob_mse <- rep(NA, length(size))
  for (i in seq_along(size)) {
    set.seed(123)
    f <- formula(paste(y,"~ ."))
    modelo_rf <- randomForest(formula = f, data = df, mtry = 5, ntree = ntree,
                              nodesize = i)
    oob_mse[i] <- tail(modelo_rf$mse, n = 1)
  }
  results <- data_frame(size, oob_mse)
  return(results)
}

hiperparametro_nodesize <-  tuning_rf_nodesize(df = Boston, y = "medv",
                                               size = c(1:20))
hiperparametro_nodesize %>% arrange(oob_mse)

ggplot(data = hiperparametro_nodesize, aes(x = size, y = oob_mse)) +
  scale_x_continuous(breaks = hiperparametro_nodesize$size) +
  geom_line() +
  geom_point() +
  geom_point(data = hiperparametro_nodesize %>% arrange(oob_mse) %>% head(1),
             color = "red") +
  labs(title = "Evolución del out-of-bag-error vs nodesize",
       x = "nº observaciones en nodos terminales") +
  theme_bw()

# (3) Determinar el valor del # de árboles: ntree (por defecto 500)
modelo_randomforest <- randomForest(medv ~ ., data = Boston, subset = train,
                                    mtry = 5 , ntree = 500, nodesize = 5,
                                    importance = TRUE)
oob_mse <- data.frame(oob_mse = modelo_randomforest$mse,
                      arboles = seq_along(modelo_randomforest$mse))
ggplot(data = oob_mse, aes(x = arboles, y = oob_mse )) +
  geom_line() +
  labs(title = "Evolución del out-of-bag-error vs número árboles",
       x = "nº árboles") +
  theme_bw()

#Nota: La búsqueda de hiperparámetros no debe de realizarse de manera secuencial
#      para ejecutar una búsqueda más óptima usar caret

# Modelo con hiperparámetros calibrados
set.seed(123)
modelo_randomforest <- randomForest(medv ~ ., data = Boston, subset = train,
                                    mtry = 5 , ntree = 200, nodesize = 5,
                                    importance = TRUE)
modelo_randomforest

predicciones <- predict(object = modelo_randomforest, newdata = Boston[-train, ])
test_mse_rf <- mean((predicciones - Boston[-train, "medv"])^2)
paste("Error de test (mse) del modelo:", round(test_mse_rf, 2))
paste("Error de test (rmse) del modelo:", round(sqrt(test_mse_rf), 2))

# Empleando todas las observaciones en el proceso de randomforest
set.seed(1)
modelo_randomforest_c <- randomForest(medv ~ ., data = Boston, mtry = 5, ntree = 100,
                                    nodesize = 5, importance = TRUE)
modelo_randomforest_c

# Predictores más influyentes
library(tidyverse)
library(ggpubr)
importancia_pred <- as.data.frame(importance(modelo_randomforest, scale = TRUE))
importancia_pred <- rownames_to_column(importancia_pred, var = "variable")
p1 <- ggplot(data = importancia_pred, aes(x = reorder(variable, `%IncMSE`),
                                          y = `%IncMSE`,
                                          fill = `%IncMSE`)) +
  labs(x = "variable", title = "Reducción de MSE") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

p2 <- ggplot(data = importancia_pred, aes(x = reorder(variable, IncNodePurity),
                                          y = IncNodePurity,
                                          fill = IncNodePurity)) +
  labs(x = "variable", title = "Reducción de pureza") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
ggarrange(p1, p2)

# Usando Caret
# Hiperparámetros
hiperparametros <- expand.grid(mtry = seq(1:13), # número de predictores seleccionados
                               min.node.size = seq(1:10), # tamaño mínimo de cada nodo
                               splitrule = "variance")
# Ajuste del modelo
set.seed(1)
modelo_rf <- train(medv ~ ., data = Boston[train,],
                   method = "ranger",
                   tuneGrid = hiperparametros,
                   #metric = metrica,
                   #trControl = fitControl,
                   # Número de árboles ajustados
                   num.trees = 150)
modelo_rf

modelo_rf$finalModel

# REPRESENTACIÓN GRÁFICA
ggplot(modelo_rf, highlight = TRUE) +
  #scale_x_continuous(breaks = 1:120) +
  labs(title = "Evolución del accuracy del modelo Random Forest") +
  guides(color = guide_legend(title = "mtry"),
         shape = guide_legend(title = "mtry")) +
  theme_bw()

# Cálculo del test error 
predicciones <- predict(object = modelo_rf, newdata = Boston[-train, ])
test_mse_rf <- mean((predicciones - Boston[-train, "medv"])^2)
paste("Error de test (mse) del modelo:", round(test_mse_rf, 2))
paste("Error de test (rmse) del modelo:", round(sqrt(test_mse_rf), 2))

#####################################################################
# Boosting                                                         #
#####################################################################
library(adabag)

# Cargamos la librería a usar y los datos
library(rpart)
data(iris)
help(iris)

# Seleccion de datos de trabajo o entrenamiento,
# se seleccionan 25 de cada especie
train = c(sample(1:50, 25),
          sample(51:100, 25),
          sample(101:150, 25))

# Modelamos la especie a partir de todas las variables en los datos de entrenamiento
# mfinal -> N de iteraciones
# boos -> Permite submuestrear para generar los pesos
iris.adaboost <- boosting(Species~., data=iris[train,], boos=TRUE, mfinal=10)
iris.adaboost

# Graficamos las variables según su porcentaje de relevancia  
# El largo de pétalo es la más relevante.
barplot(iris.adaboost$imp[order(iris.adaboost$imp, decreasing = TRUE)],
        ylim = c(0, 100), main = "Importancia Relativa de las Variables",
        col = "lightblue")

# Ajustamos a los nuevos datos, y vemos el ajuste
iris.predboosting <- predict.boosting(iris.adaboost,
                                      newdata = iris[-train, ])
iris.predboosting

#---------------------------------------------------------------------#
# Evaluación de la Predicción: Regresión                              #
#---------------------------------------------------------------------#
library(gbm)
# Stochastic Gradient Boosting
# el weak learner se ajusta empleando únicamente una fracción 
# del conjunto de entrenamiento, extraída de forma aleatoria
#  y sin reemplazo (no con bootstrapping).

cv_error  <- vector("numeric")
n_arboles <- vector("numeric") # Número de Weak Learners
					 # Mayor valor puede provocar sobreajuste
shrinkage <- vector("numeric") #learning rate
					 #controla la influencia de cada modelo.
           # a menor valor se necesitan más modelos 
					 #para alcanzar buenos resultados
#Cuando los modelos son árboles se acostumbra calibrar también el parámetro
# n.minobsinnode 

# Calibrando learning rate
for (i in c(0.001, 0.01, 0.1)) {
  set.seed(123)
  arbol_boosting <- gbm(medv ~ ., data = Boston[train, ],
                        distribution = "gaussian", # bernoulli en clasificación
                        n.trees = 20000,
                        interaction.depth = 1,
                        shrinkage = i,
                        n.minobsinnode = 10,
                        bag.fraction = 0.5,
                        cv.folds = 5)
  cv_error  <- c(cv_error, arbol_boosting$cv.error)
  n_arboles <- c(n_arboles, seq_along(arbol_boosting$cv.error))
  shrinkage <- c(shrinkage, rep(i, length(arbol_boosting$cv.error)))
}
error <- data.frame(cv_error, n_arboles, shrinkage)

ggplot(data = error, aes(x = n_arboles, y = cv_error,
                         color = as.factor(shrinkage))) +
  geom_smooth() +
  labs(title = "Evolución del cv-error", color = "shrinkage") + 
  theme_bw() +
  theme(legend.position = "bottom")

# Calibrando la complejidad de los árboles
cv_error  <- vector("numeric")
n_arboles <- vector("numeric")
interaction.depth <- vector("numeric")

for (i in c(1, 3, 5, 10)) {
  set.seed(123)
  arbol_boosting <- gbm(medv ~ ., data = Boston[train, ],
                        distribution = "gaussian",
                        n.trees = 5000,
                        interaction.depth = i,
                        shrinkage = 0.01,
                        n.minobsinnode = 10,
                        bag.fraction = 0.5,
                        cv.folds = 5)
  cv_error  <- c(cv_error, arbol_boosting$cv.error)
  n_arboles <- c(n_arboles, seq_along(arbol_boosting$cv.error))
  interaction.depth <- c(interaction.depth,
                         rep(i, length(arbol_boosting$cv.error)))
}
error <- data.frame(cv_error, n_arboles, interaction.depth)

ggplot(data = error, aes(x = n_arboles, y = cv_error,
                         color = as.factor(interaction.depth))) +
  geom_smooth() +
  labs(title = "Evolución del cv-error", color = "interaction.depth") + 
  theme_bw() +
  theme(legend.position = "bottom")

# Calibrando el número mínimo de observaciones por nodo
cv_error  <- vector("numeric")
n_arboles <- vector("numeric")
n.minobsinnode <- vector("numeric")
for (i in c(1, 5, 10, 20)) {
  arbol_boosting <- gbm(medv ~ ., data = Boston[train, ],
                        distribution = "gaussian",
                        n.trees = 5000,
                        interaction.depth = 5,
                        shrinkage = 0.01,
                        n.minobsinnode = i,
                        bag.fraction = 0.5,
                        cv.folds = 5)
  cv_error  <- c(cv_error, arbol_boosting$cv.error)
  n_arboles <- c(n_arboles, seq_along(arbol_boosting$cv.error))
  n.minobsinnode <- c(n.minobsinnode,
                      rep(i, length(arbol_boosting$cv.error)))
}
error <- data.frame(cv_error, n_arboles, n.minobsinnode)

ggplot(data = error, aes(x = n_arboles, y = cv_error,
                         color = as.factor(n.minobsinnode))) +
  geom_smooth() +
  labs(title = "Evolución del cv-error", color = "n.minobsinnode") + 
  theme_bw() +
  theme(legend.position = "bottom")

# Determinando el número de árboles (iteraciones)
set.seed(123)
arbol_boosting <- gbm(medv ~ ., data = Boston[train, ],
                      distribution = "gaussian",
                      n.trees = 10000,
                      interaction.depth = 5,
                      shrinkage = 0.01,
                      n.minobsinnode = 1,
                      bag.fraction = 0.5,
                      cv.folds = 5)
error <- data.frame(cv_error = arbol_boosting$cv.error,
                    n_arboles = seq_along(arbol_boosting$cv.error))
ggplot(data = error, aes(x = n_arboles, y = cv_error)) +
  geom_line(color = "black") +
  geom_point(data = error[which.min(error$cv_error),], color = "red") +
  labs(title = "Evolución del cv-error") + 
  theme_bw() 

error[which.min(error$cv_error),]

# Se reajusta el modelo final con los hiperparámetro óptimos
set.seed(123)
arbol_boosting <- gbm(medv ~ ., data = Boston[train, ],
                      distribution = "gaussian",
                      n.trees = 521,
                      interaction.depth = 5,
                      shrinkage = 0.01,
                      n.minobsinnode = 1,
                      bag.fraction = 0.5)

#Usando caret

library(caret)

set.seed(123)
validacion <- trainControl(## 10-fold CV
                           method = "cv",
                           number = 10)

tuning_grid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                            n.trees = c(100, 1000, 2000, 3000), 
                            shrinkage = c(0.1, 0.01, 0.001),
                            n.minobsinnode = c(1, 10, 20))

set.seed(123)
mejor_modelo <- train(medv ~ ., data = Boston[train, ], 
                      method = "gbm", 
                      trControl = validacion, 
                      verbose = FALSE, 
                      tuneGrid = tuning_grid)

# Se muestran los hiperparámetros del mejor modelo 
mejor_modelo$bestTune

# Importancia de predictores
importancia_pred <- summary(arbol_boosting, plotit = FALSE)
ggplot(data = importancia_pred, aes(x = reorder(var, rel.inf), y = rel.inf,
                                    fill = rel.inf)) +
  labs(x = "variable", title = "Reducción de MSE") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

# Efecto de cada predictor (partial dependence plots)

par(mfrow = c(1,2))
plot(arbol_boosting, i.var = "rm", col = "blue")
plot(arbol_boosting, i.var = "lstat", col = "firebrick")
par(mfrow = c(1,1))

# Predicción
predicciones <- predict(object = arbol_boosting, newdata = Boston[-train,],
                        n.trees = 521)
test_mse <- mean((predicciones - Boston[-train, "medv"])^2)
paste("Error de test (mse) del modelo:", round(test_mse, 2))




## Entrenamiento en paralelo

fit <- train(Boston[,-14],Boston$medv, data = Boston,
             method="rf",
             #trControl = fitControl
             )


library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

# Parámetros de entrenamiento
fitControl <- trainControl(method = "boot", allowParallel = TRUE)

fit2 <- train(Boston[,-14],Boston$medv, data = Boston,
             method="rf",
             trControl = fitControl
)

fit$times
fit2$times



stopCluster(cluster)
registerDoSEQ()