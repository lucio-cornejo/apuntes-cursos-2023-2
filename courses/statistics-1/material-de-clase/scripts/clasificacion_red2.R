#-------------------------------------------------------------------------#
# EVALUCIÓN PREDICTIVA DE MODELOS DE CLASIFICACIÓN                        #
# ------------------------------------------------------------------------#
# Ejemplo: Créditos de  Banco Alemán                                      #
#-------------------------------------------------------------------------#

library(caret)
library(haven)
library(tidyverse)
library(Fahrmeir)
library(ggplot2)
library(ggpubr)
library(discretization)
library(randomForest)
library(recipes)
library(C50)
library(doParallel)
library(ranger)
library(e1071)
library(gbm)
library(party)
library(partykit)

# Base de datos resumida
#credit2<- read_dta("D:/Dropbox/Cursos/Ad-hoc/Intercorp/Modelamiento Predictivo/Compartido/Datos/credit.dta")

# Base de datos con variables principales
data(credit)
help(credit)

# Formato de datos
glimpse(credit)

# credit2$y<- if_else(credit2$y == 1, "Solvente", "No solvente")
# credit2$y <- as.factor(credit2$y)


#################################################
# División de los datos en entrenamiento y test #
#################################################
set.seed(123)

# Se crean los índices de las observaciones de entrenamiento
train <- createDataPartition(y = credit$Y, p = 0.75, list = FALSE, times = 1)
datos_train <- credit[train, ]
datos_test  <- credit[-train, ]

# Se verifica la distribución de la variable respuesta
prop.table(table(credit$Y))
prop.table(table(datos_train$Y))
prop.table(table(datos_test$Y))

#################################################
# Preprocesamiento                              #
#################################################
# Crear un objeto del tipo recipe
# Ver: http://rstudio-pubs-static.s3.amazonaws.com/349127_caf711562db44e10a65c1fe0ec74e00c.html
objeto_recipe <- recipe(
  formula = Y ~ Cuenta + Mes + Ppag + Uso + DM,
  data =  datos_train
)

objeto_recipe

# Eliminar predictores con varianza cercana a cero
# (muchos valores similares)
datos_train |> select(Cuenta, Mes, Ppag, Uso, DM, Sexo, Estc) |>
  nearZeroVar(saveMetrics = TRUE)

objeto_recipe <- objeto_recipe |> step_nzv(all_predictors())

# Transformación de variables cuantitativas: normalización
objeto_recipe <- objeto_recipe |> step_center(all_numeric())
objeto_recipe <- objeto_recipe |> step_scale(all_numeric())

# Binarización de variables cualitativas
objeto_recipe <- objeto_recipe |> step_dummy(all_nominal(), -all_outcomes())

# Se entrena el objeto recipe
trained_recipe <- prep(objeto_recipe, training = datos_train)
trained_recipe

# Se aplican las transformaciones al conjunto de entrenamiento y de test
datos_train_prep <- bake(trained_recipe, new_data = datos_train)
datos_test_prep  <- bake(trained_recipe, new_data = datos_test)

glimpse(datos_train_prep)

#################################################
# Construcción de Modelos                       #
#################################################
# Ver: http://topepo.github.io/caret/available-models.html

# PARALELIZACIÓN DE PROCESO
#===============================================================================
#cl <- makePSOCKcluster(4)
#registerDoParallel(cl)
cl <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cl)


# Métrica 
#===============================================================================
# Puede ser "Accuracy",   "logLoss", "ROC",   "Kappa"
metrica <- "Accuracy"

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
particiones  <- 10
repeticiones <- 5

#################################################
# K-Vecinos más Cercanos                        #
#################################################

# Hiperparámetros
hiperparametros <- data.frame(k = seq(from = 1,to = 71,by = 2))

set.seed(666)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros)) 
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_knn <- train(Y ~ ., data = datos_train_prep,
                    method = "knn",
                    tuneGrid = hiperparametros,
                    metric = metrica,
                    trControl = control_train)
modelo_knn

library(ggplot2)
ggplot(modelo_knn , highlight = TRUE) +
  scale_x_continuous(breaks = hiperparametros$k) +
  labs(title = "Evolución del accuracy del modelo KNN", x = "K") +
  theme_bw()

#################################################
# Naive Bayes                                   #
#################################################

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================

# Hiperparámetros
#hiperparametros <- data.frame(usekernel = FALSE, fL = 0 , adjust = 0)
hiperparametros <- data.frame(usekernel = FALSE, laplace = 0 , adjust = 0)

set.seed(666)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_nb <- train(Y ~ ., data = datos_train_prep,
                   method = "naive_bayes", 
                   tuneGrid = hiperparametros,
                   metric = metrica,
                   trControl = control_train)
modelo_nb


#################################################
# Árbol de Clasificación: C5.0                  #
# Ver Quinlan (1993) y Kuhn and Johnson (2013)  #
#################################################

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
# Hiperparámetros
hiperparametros <- data.frame(parameter = "none")

set.seed(666)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_C50Tree <- train(Y ~ ., data = datos_train_prep,
                        method = "C5.0Tree",
                        tuneGrid = hiperparametros,
                        metric = metrica,
                        trControl = control_train)
modelo_C50Tree

summary(modelo_C50Tree$finalModel)

#################################################
# Árbol de Clasificación: RPART                #
#################################################

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
# Hiperparámetros
hiperparametros <- data.frame(cp = seq(0.001,0.07,by = 0.0005))


set.seed(666)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_rpart <- train(Y ~ ., data = datos_train_prep,
                        method = "rpart",
                        tuneGrid = hiperparametros,
                        metric = metrica,
                        trControl = control_train)
modelo_rpart

library(ggplot2)
ggplot(modelo_rpart , highlight = TRUE) +
  scale_x_continuous(breaks = hiperparametros$cp) +
  labs(title = "Evolución del accuracy del modelo CART", x = "cp") +
  theme_bw()

modelo_rpart$finalModel

#################################################
# Árbol de Clasificación: Party                 #
#################################################

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
# Hiperparámetro
#hiperparametros <- data.frame(mincriterion = seq(0.001,0.7,by = 0.01))
hiperparametros <- data.frame(mincriterion = seq(0.001,0.17,by = 0.001))


set.seed(666)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_party <- train(Y ~ ., data = datos_train_prep,
                      method = "ctree",
                      tuneGrid = hiperparametros,
                      metric = metrica,
                      trControl = control_train)
modelo_party

ggplot(modelo_party , highlight = TRUE) +
  scale_x_continuous(breaks = hiperparametros$mincriterion) +
  labs(title = "Evolución del accuracy del modelo Party", x = "mincriterion") +
  theme_bw()

modelo_party$finalModel

#######################################################
# Random Forest                                       #
#######################################################

# Hiperparámetros
hiperparametros <- expand.grid(mtry = c(3, 5), # número de predictores seleccionados
                               #mtry = c(2, 3, 4, 5), # número de predictores seleccionados
                               min.node.size = c(10, 50, 90, 120), # tamaño mínimo de cada nodo
                               #min.node.size = c(10, 25, 50, 75,80,90,95,100,105,110,120), # tamaño mínimo de cada nodo
                               #min.node.size = c(2, 3, 4, 5, 7, 10, 15, 20, 30, 35, 40, 45), # tamaño mínimo de cada nodo
                               splitrule = "gini")

set.seed(666)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_rf <- train(Y ~ ., data = datos_train_prep,
                   method = "ranger",
                   tuneGrid = hiperparametros,
                   metric = metrica,
                   trControl = control_train,
                   # Número de árboles ajustados
                   num.trees = 750)
modelo_rf

modelo_rf$finalModel

# REPRESENTACIÓN GRÁFICA
# ==============================================================================
ggplot(modelo_rf, highlight = TRUE) +
  #scale_x_continuous(breaks = 1:120) +
  labs(title = "Evolución del accuracy del modelo Random Forest") +
  guides(color = guide_legend(title = "mtry"),
         shape = guide_legend(title = "mtry")) +
  theme_bw()

#######################################################
# Máquinas de Soporte Vectorial (SVM) - Kernel Radial #
#######################################################

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================

# Hiperparámetros
#hiperparametros <- expand.grid(sigma = c(0.001, 0.01, 0.1, 0.5, 1),
#                               C = c(1 , 20, 50, 100, 200, 500, 700))

hiperparametros <- expand.grid(sigma = seq(0.1,0.6,0.1),
                               C = seq(10,70,5))

set.seed(666)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_svmrad <- train(Y ~ ., data = datos_train_prep,
                       method = "svmRadial",
                       tuneGrid = hiperparametros,
                       metric = metrica,
                       trControl = control_train)
modelo_svmrad
modelo_svmrad$finalModel

# REPRESENTACIÓN GRÁFICA
# ==============================================================================
ggplot(modelo_svmrad, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo SVM Radial") +
  theme_bw()


#################################################
# Comparación de Modelos                        #
#################################################
modelos <- list(KNN = modelo_knn, NB = modelo_nb, 
                Arbol_C5.0 = modelo_C50Tree, Arbol_CART = modelo_rpart,
                Arbol_Party = modelo_party,
                rf = modelo_rf
                )

resultados_resamples <- resamples(modelos)
resultados_resamples$values %>% head(10)

# Se trasforma el dataframe devuelto por resamples() para separar el nombre del
# modelo y las métricas en columnas distintas.
metricas_resamples <- resultados_resamples$values %>%
  gather(key = "modelo", value = "valor", -Resample) %>%
  separate(col = "modelo", into = c("modelo", "metrica"),
           sep = "~", remove = TRUE)
metricas_resamples %>% head()

# Accuracy y Kappa promedio de cada modelo
metricas_resamples %>% 
  group_by(modelo, metrica) %>% 
  summarise(media = mean(valor)) %>%
  spread(key = metrica, value = media) %>%
  arrange(desc(Accuracy))

metricas_resamples %>%
  filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  summarise(media = mean(valor)) %>%
  ggplot(aes(x = reorder(modelo, media), y = media, label = round(media, 2))) +
  geom_segment(aes(x = reorder(modelo, media), y = 0,
                   xend = modelo, yend = media),
               color = "grey50") +
  geom_point(size = 7, color = "firebrick") +
  geom_text(color = "white", size = 2.5) +
  scale_y_continuous(limits = c(0, 0.8)) +
  # Accuracy basal
  geom_hline(yintercept = 0.70, linetype = "dashed") +
  annotate(geom = "text", y = 0.70, x = 5.5, label = "Ratio No Informativo") +
  labs(title = "Validación: Accuracy medio repeated-CV",
       subtitle = "Modelos ordenados por media",
       x = "modelo") +
  coord_flip() +
  theme_bw()

metricas_resamples %>% filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  mutate(media = mean(valor)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(modelo, media), y = valor, color = modelo)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  scale_y_continuous(limits = c(0.5, 0.8)) +
  # Accuracy basal
  geom_hline(yintercept = 0.70, linetype = "dashed") +
  annotate(geom = "text", y = 0.70, x = 6.5, label = "Ratio No Informativo") +
  theme_bw() +
  labs(title = "Validación: Accuracy medio repeated-CV",
       subtitle = "Modelos ordenados por media") +
  coord_flip() +
  theme(legend.position = "none")

#Test de Friedman para comparar el accuracy de los modelos
matriz_metricas <- metricas_resamples %>% filter(metrica == "Accuracy") %>%
  spread(key = modelo, value = valor) %>%
  select(-Resample, -metrica) %>% as.matrix()
friedman.test(y = matriz_metricas)

# Comparaciones múltiples con un test suma de rangos de Wilcoxon
# ==============================================================================

metricas_accuracy <- metricas_resamples %>% filter(metrica == "Accuracy")
comparaciones  <- pairwise.wilcox.test(x = metricas_accuracy$valor, 
                                       g = metricas_accuracy$modelo,
                                       paired = TRUE,
                                       p.adjust.method = "holm")

# Se almacenan los p_values en forma de dataframe
comparaciones <- comparaciones$p.value %>%
  as.data.frame() %>%
  rownames_to_column(var = "modeloA") %>%
  gather(key = "modeloB", value = "p_value", -modeloA) %>%
  na.omit() %>%
  arrange(modeloA) 

comparaciones

# Error en el Test
resultados.test <- predict(modelo_rf,newdata = datos_test_prep)
caret::confusionMatrix(resultados.test, datos_test_prep$Y)



# Detener clusters
stopCluster(cl)
remove(cl)
registerDoSEQ()
