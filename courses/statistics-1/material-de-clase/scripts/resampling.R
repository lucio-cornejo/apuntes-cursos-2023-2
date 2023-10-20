library(ISLR)
library(ggplot2)
library(ggpubr)
library(reshape2)
#library(tidyverse)


###############################################################################
# Ejemplos de la presentaci�n                                                 #
###############################################################################

#-------------------------------------
# Conjunto de validaci�n
#-------------------------------------

set.seed(123)
n = dim(Auto)[1]

# 11 veces partiremos los datos en train y test
# 10 grados de polinomios consideraremos
testMSEmat <- matrix(ncol = 11, nrow = 10)

for (newsample in 1:11) {
  trainid <- sample(n, n/2)
  for (polydeg in 1:10) {
    lm.fit <- lm(
      mpg ~ poly(horsepower, polydeg),
      data = Auto,
      subset = trainid
    )
    # Predecimos para los datos de test
    lm.pred <- predict(lm.fit, Auto)[-trainid]
    testMSEmat[polydeg, newsample] <- mean((Auto$mpg[-trainid] - lm.pred)^2)
  }
}
yrange <- c(15,28)

# Resultado para la primera selecci�n
plotdf <- data.frame("testMSE" = testMSEmat[,1], "degree" = 1:10)

g0 <- ggplot(plotdf, aes(x = degree)) +
  geom_line(y = testMSEmat[,1]) +
  scale_y_continuous(limits = yrange) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    y = "MSE de conjunto de validación", 
    x = "Grado del Polinomio"
  )

g0 + theme_minimal()

# Comparando con las otras muestras
cols=rainbow(10)
g1=g0+geom_line(aes(x=1:10,y=testMSEmat[,2]),colour=cols[1])
g1=g1+geom_line(aes(x=1:10,y=testMSEmat[,3]),colour=cols[2])
g1=g1+geom_line(aes(x=1:10,y=testMSEmat[,4]),colour=cols[3])
g1=g1+geom_line(aes(x=1:10,y=testMSEmat[,5]),colour=cols[4])
g1=g1+geom_line(aes(x=1:10,y=testMSEmat[,6]),colour=cols[5])
g1=g1+geom_line(aes(x=1:10,y=testMSEmat[,7]),colour=cols[6])
g1=g1+geom_line(aes(x=1:10,y=testMSEmat[,8]),colour=cols[7])
g1=g1+geom_line(aes(x=1:10,y=testMSEmat[,9]),colour=cols[8])
g1=g1+geom_line(aes(x=1:10,y=testMSEmat[,10]),colour=cols[9])
g1=g1+geom_line(aes(x=1:10,y=testMSEmat[,11]),colour=cols[10])
g1
g1valid=g1

# No siempre se llega a la misma conclusión sobre
# cuál es el mejor modelo. Por lo tanto, no estamos 
# en el caso de abudancia de datos.

#-------------------------------------
# LOOCV
#-------------------------------------
library(boot)

set.seed(123)
testMSEvec=NULL
start=Sys.time()
for (polydeg in 1:10)
{
  glm.fit=glm(mpg~poly(horsepower,polydeg),data=Auto)
  glm.cv1=cv.glm(Auto, glm.fit,K=n)
  testMSEvec=c(testMSEvec,glm.cv1$delta[1])
}
stopp=Sys.time()

yrange=c(15,28)
plotdf=data.frame("testMSE"=testMSEvec,"degree"=1:10)
g0=ggplot(plotdf,aes(x=degree,y=testMSE))+geom_line()+geom_point()+scale_y_continuous(limits = yrange)+scale_x_continuous(breaks=1:10)+labs(y="LOOCV")
g0+theme_minimal()

#-------------------------------------
# 5-CV y 10-CV
#-------------------------------------

set.seed(123)

testMSEvec5=NULL
testMSEvec10=NULL
start=Sys.time()
for (polydeg in 1:10)
{
  glm.fit=glm(mpg~poly(horsepower,polydeg),data=Auto)
  glm.cv5=cv.glm(Auto, glm.fit,K=5)
  glm.cv10=cv.glm(Auto, glm.fit,K=10)
  testMSEvec5=c(testMSEvec5,glm.cv5$delta[1])
  testMSEvec10=c(testMSEvec10,glm.cv10$delta[1])
}
stopp=Sys.time()
yrange=c(15,28)
plotdf=data.frame("testMSE5"=testMSEvec5,"degree"=1:10)
g0=ggplot(plotdf,aes(x=degree,y=testMSE5))+geom_line()+geom_point()+scale_y_continuous(limits = yrange)+scale_x_continuous(breaks=1:10)+labs(y="CV")+ggtitle("5 and 10 fold CV")
g0+geom_line(aes(y=testMSEvec10),colour="red")+geom_point(aes(y=testMSEvec10),colour="red")+ggtitle("5 fold (black), 10 fold (red)")+theme_minimal()

# Repetir 10-CV
set.seed(123)

start=Sys.time()
testMSEmat=matrix(ncol=11,nrow=10)

for (polydeg in 1:10)
{
  glm.fit=glm(mpg~poly(horsepower,polydeg),data=Auto)
  for (newsample in 1:11)
  {
    glm.cv10=cv.glm(Auto, glm.fit,K=10)
    testMSEmat[polydeg,newsample]=glm.cv10$delta[1]
  }
}

stopp=Sys.time()
yrange=c(15,28)

plotdf=data.frame("testMSE"=testMSEmat[,1],"degree"=1:10)
g0=ggplot(plotdf,aes(x=degree))+geom_line(y=testMSEmat[,1])+scale_y_continuous(limits = yrange)+scale_x_continuous(breaks=1:10)+labs(y="CV10")
#g0

cols=rainbow(10)
g1=g0+geom_line(aes(x=1:10,y=testMSEmat[,2]),colour=cols[1])
g1=g1+geom_line(aes(x=1:10,y=testMSEmat[,3]),colour=cols[2])
g1=g1+geom_line(aes(x=1:10,y=testMSEmat[,4]),colour=cols[3])
g1=g1+geom_line(aes(x=1:10,y=testMSEmat[,5]),colour=cols[4])
g1=g1+geom_line(aes(x=1:10,y=testMSEmat[,6]),colour=cols[5])
g1=g1+geom_line(aes(x=1:10,y=testMSEmat[,7]),colour=cols[6])
g1=g1+geom_line(aes(x=1:10,y=testMSEmat[,8]),colour=cols[7])
g1=g1+geom_line(aes(x=1:10,y=testMSEmat[,9]),colour=cols[8])
g1=g1+geom_line(aes(x=1:10,y=testMSEmat[,10]),colour=cols[9])
g1=g1+geom_line(aes(x=1:10,y=testMSEmat[,11]),colour=cols[10])
g1+theme_minimal()+ggtitle("10 repeticiones (particiones distintas) del m�todo 10-CV method - evaluar variabilidad")
#ggarrange(g0,g1)

###############################################################################
# Ejemplo: Clasificaci�n de Buenos Pagadores                                  #                                           #
###############################################################################

#Lectura de Datos
library(Fahrmeir)
data(credit)
help(credit)

head(credit)

#------------------------------------------------------------------------------#
## Validaci�n Cruzada (CV K-Fold)
#------------------------------------------------------------------------------#

library(vtreat)
set.seed(1243)
k <-5 # N�mero de "folds"
splitPlan <- kWayCrossValidation(nRows = dim(credit)[1], k)
splitPlan
splitPlan[[1]]


testLogLossCV5=NULL
testAUCCV5=NULL

for(i in 1:k) {
  split <- splitPlan[[i]]
  modelo_logistic <- glm(Y ~ ., family=binomial,data=credit[split$train,])
  yprob<-predict(modelo_logistic,newdata = credit[split$app,],type="response")
  ypred <- factor(as.numeric(yprob >= 0.5 ), labels = levels(credit$Y))
  testLogLossCV5[i]<-MLmetrics::LogLoss(yprob,as.numeric(credit[split$app,]$Y)-1)
  testAUCCV5[i]<-MLmetrics::AUC(yprob,as.numeric(credit[split$app,]$Y)-1)
}

# LogLoss
testLogLossCV5
mean(testLogLossCV5)

#AUC
testAUCCV5
mean(testAUCCV5)


# Usando la librer�a caret
library(caret)
# M�trica 
#===============================================================================
# Puede ser "Accuracy",   "logLoss", "ROC",   "Kappa"
metrica <- "ROC"

# HIPERPAR�METROS, N�MERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICI�N
#===============================================================================
particiones  <- 5
repeticiones <- 1

# HIPERPAR�METROS, N�MERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICI�N
#===============================================================================
hiperparametros <- data.frame(parameter = "none")

# DEFINICI�N DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones,
                              #seeds = seeds,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

set.seed(666)
modelo_logistic <- train(Y ~ ., data = credit,
                         method = "glm",
                         tuneGrid = hiperparametros,
                         metric = metrica,
                         trControl = control_train,
                         family = "binomial")
modelo_logistic

# Usando LogLoss
metrica <- "logLoss"
# DEFINICI�N DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones,
                              #seeds = seeds,
                              classProbs = TRUE,
                              summaryFunction = mnLogLoss,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

set.seed(666)
modelo_logistic <- train(Y ~ ., data = credit,
                         method = "glm",
                         tuneGrid = hiperparametros,
                         metric = metrica,
                         trControl = control_train,
                         family = "binomial")
modelo_logistic
modelo_logistic$results
modelo_logistic$resample
summary(modelo_logistic$finalModel)

## Gr�ficas de evaluaci�n de la precisi�n
p1 <- ggplot(data = modelo_logistic$resample, aes(x = logLoss)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_vline(xintercept = mean(modelo_logistic$resample$logLoss),
             linetype = "dashed") +
  theme_bw() 
p2 <- ggplot(data = modelo_logistic$resample, aes(x = 1, y = logLoss)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray50") +
  geom_jitter(width = 0.05) +
  labs(x = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

final_plot <- ggarrange(p1, p2, ncol = 2, nrow = 1)
final_plot <- annotate_figure(
  final_plot,
  top = text_grob("Evaluaci�n del Modelo Log�stico", size = 15))
final_plot


####################################################################
# # Ejemplo: Diabetes (Calibraci�n de hiperpar�metros en K-NN)   # #
####################################################################
diabetes=read.csv("DiabetesTrain.csv", stringsAsFactors=TRUE)
#diabetes=read.csv(file.choose())
head(diabetes)


## Usando validaci�n cruzada
library(class)
set.seed(007)
mean(knn.cv(train = diabetes[,1:3],cl = diabetes[,4],k=1)==diabetes[,4])
mean(knn.cv(train = diabetes[,1:3],cl = diabetes[,4],k=3)==diabetes[,4])
mean(knn.cv(train = diabetes[,1:3],cl = diabetes[,4],k=5)==diabetes[,4])
mean(knn.cv(train = diabetes[,1:3],cl = diabetes[,4],k=7)==diabetes[,4])
mean(knn.cv(train = diabetes[,1:3],cl = diabetes[,4],k=9)==diabetes[,4])
mean(knn.cv(train = diabetes[,1:3],cl = diabetes[,4],k=11)==diabetes[,4])
mean(knn.cv(train = diabetes[,1:3],cl = diabetes[,4],k=13)==diabetes[,4])
mean(knn.cv(train = diabetes[,1:3],cl = diabetes[,4],k=15)==diabetes[,4])

K_3 <- knn(train = diabetes[,1:3],test = diabetes[,1:3],cl = diabetes[,4], k = 3, prob = TRUE)
mean(K_3 == diabetes[,4])

confusionMatrix(K_3,diabetes[,4])


# Obtener la pro. de votos
K_3_prob <- attr(K_3, "prob")

# Valores predichos
head(K_3)

# Prop. de "votos"
head(K_3_prob)

#===============================================================================
# Usando Caret
#===============================================================================
# N�mero de repeticiones y semilla
k <- 10
repeticiones <- 1
hiperparametros <- data.frame(k = c(1,2,3,4, 5, 7, 9,  11, 13, 15, 17, 21, 31, 51))

set.seed(666)
seeds <- vector(mode = "list", length = (k * repeticiones) + 1)
for (i in 1:(k * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(k * repeticiones) + 1]] <- sample.int(1000, 1)


# AJUSTE DEL MODELO
# ==============================================================================
cvmod.knn <-train(class ~.,
                  data = diabetes,
                  method = "knn",
                  tuneGrid = hiperparametros,
                  metric = "Accuracy",
                  trControl = trainControl(method = "repeatedcv",
                                           number = k, seeds = seeds,
                                           repeats = repeticiones, returnResamp = "final") 
)
cvmod.knn
cvmod.knn$bestTune

library(ggplot2)
ggplot(cvmod.knn , highlight = TRUE) +
  scale_x_continuous(breaks = hiperparametros$k) +
  labs(title = "Evoluci�n del accuracy del modelo KNN", x = "K") +
  theme_bw()

###############################################################################
# Ejemplo: Evaluaci�n del modelo de pron�stico para la grasa corporal         #                                           #
###############################################################################

# Lectura de los datos
library(dplyr)
library(ggplot2)
d.bodyfat <- read.table("bodyfat.clean.txt",sep="",header=T)
d.bodyfat <- d.bodyfat[,c("bodyfat","age","gewicht","hoehe","bmi","neck",
                          "abdomen","hip")]
names(d.bodyfat) <- c("bodyfat","age","weight","height","bmi","neck","abdomen","hip")

# Modelo de Regresi�n Lineal M�ltiple
r.bodyfat = lm(bodyfat ~ bmi + age,data=d.bodyfat)
summary(r.bodyfat)

#-------------------------------------------------------------------------------
#  Resustituci�n: Evaluar con los mismos datos de entrenamiento
#------------------------------------------------------------------------------
d.bodyfat$predictions <- predict(r.bodyfat)

#------------------------------------------------------------------------------#
# Evaluaci�n de la Predicci�n                                                  #
#------------------------------------------------------------------------------#

residual <- d.bodyfat$bodyfat - d.bodyfat$predictions
residual
residuals(r.bodyfat)

#Estimaci�n de la Ra�z del Error Cuadr�tico Medio (RMSE)
RMSE <-  sqrt(mean(residual^2))
RMSE

library(forecast)
#d.bodyfat$predictions <- predict(r.bodyfat)
forecast::accuracy(object = d.bodyfat$predictions, x= d.bodyfat$bodyfat)

library(Metrics)
rmse(d.bodyfat$bodyfat, d.bodyfat$predictions)
sd(d.bodyfat$bodyfat)
#El modelo tiende a estimar mejor la grasa corporal en comparaci�n a
# simplemente tomar la media

#------------------------------------------------------------------------------#
# Evaluaci�n Gr�fica del Modelo                                                #
#------------------------------------------------------------------------------#

# Curva de Ganancias
library(WVPlots)
GainCurvePlot(d.bodyfat, "predictions", "bodyfat", "Modelo de Grasa Corporal")

#-------------------------------------------------------------------------------
#  Esquema de selecci�n: M�todo de Retenci�n 
#  (Entrenamiento = 75%, Evaluaci�n = 25%)
#------------------------------------------------------------------------------
set.seed(666)
train.rows <- sample(rownames(d.bodyfat), dim(d.bodyfat)[1]*0.75)

# seleccionar las columnas con ID de filas que pertenecen al conjunto de entrenamiento:
d.bodyfat_train <- d.bodyfat[train.rows, ]

# Asignar a las restantes al conjunto de prueba
valid.rows <- setdiff(rownames(d.bodyfat), train.rows) 
d.bodyfat_test<- d.bodyfat[valid.rows, ]

# Entrenamiento del modelo
fmla <- bodyfat ~ bmi + age
d.bodyfat_model <- lm(fmla, data = d.bodyfat_train)
summary(d.bodyfat_model)

d.bodyfat_train$pred <- predict(d.bodyfat_model)
rmse_train <- Metrics::rmse(d.bodyfat_train$bodyfat, d.bodyfat_train$pred)
rmse_train

# Evaluaci�n del modelo
d.bodyfat_test$pred <- predict(object = d.bodyfat_model,newdata = d.bodyfat_test)
rmse_test <- Metrics::rmse(d.bodyfat_test$bodyfat, d.bodyfat_test$pred)
rmse_test

library(ggplot2)
ggplot(d.bodyfat_test, aes(x = pred, y = bodyfat)) + 
  geom_point() + 
  geom_abline()

# Curva de Ganancias
GainCurvePlot(d.bodyfat_test, "pred", "bodyfat", "Price model")

#------------------------------------------------------------------------------#
## Validaci�n Cruzada (CV K-Fold)
#------------------------------------------------------------------------------#
library(vtreat)
set.seed(1243)
splitPlan <- kWayCrossValidation(nRows = dim(d.bodyfat)[1], 5)
splitPlan
splitPlan[[1]]

k <-5 # N�mero de "folds"
d.bodyfat$pred.cv <- 0 

for(i in 1:k) {
  split <- splitPlan[[i]]
  model <- lm(fmla, data = d.bodyfat[split$train,])
  d.bodyfat$pred.cv[split$app] <- predict(model, newdata = d.bodyfat[split$app,])
}

rmse_CV <- Metrics::rmse(d.bodyfat$bodyfat, d.bodyfat$pred.cv)
rmse_CV

# Usando la librer�a caret
library(caret)
set.seed(1243)
d.bodyfat_cv2<-train(fmla,
                data = d.bodyfat,
                method ="lm",
                #		preProc = c("center","scale"),
                trControl = trainControl(method = "repeatedcv",
                                         number = 5, # Por defecto number = 10
                                         repeats = 10) 
)
d.bodyfat_cv2
d.bodyfat_cv2$results
d.bodyfat_cv2$resample
summary(d.bodyfat_cv2$finalModel)
#https://topepo.github.io/caret/measuring-performance.html

## Gr�ficas de evaluaci�n de la precisi�n
p1 <- ggplot(data = d.bodyfat_cv2$resample, aes(x = RMSE)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_vline(xintercept = mean(d.bodyfat_cv2$resample$RMSE),
             linetype = "dashed") +
  theme_bw() 
p2 <- ggplot(data = d.bodyfat_cv2$resample, aes(x = 1, y = RMSE)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray50") +
  geom_jitter(width = 0.05) +
  labs(x = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
p3 <- ggplot(data = d.bodyfat_cv2$resample, aes(x = Rsquared)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_vline(xintercept = mean(d.bodyfat_cv2$resample$Rsquared),
             linetype = "dashed") +
  theme_bw() 
p4 <- ggplot(data = d.bodyfat_cv2$resample, aes(x = 1, y = Rsquared)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray50") +
  geom_jitter(width = 0.05) +
  labs(x = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
p5 <- ggplot(data = d.bodyfat_cv2$resample, aes(x = MAE)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_vline(xintercept = mean(d.bodyfat_cv2$resample$MAE),
             linetype = "dashed") +
  theme_bw() 
p6 <- ggplot(data = d.bodyfat_cv2$resample, aes(x = 1, y = MAE)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, fill = "gray50") +
  geom_jitter(width = 0.05) +
  labs(x = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

final_plot <- ggarrange(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3)
final_plot <- annotate_figure(
  final_plot,
  top = text_grob("Evaluaci�n del Modelo RLM", size = 15))
final_plot



library(caret)
data("GermanCredit")
head(GermanCredit)

