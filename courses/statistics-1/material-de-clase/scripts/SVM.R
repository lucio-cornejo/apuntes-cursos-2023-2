# Support Vector Machines
library(e1071)
# Generación de datos con fronteras no lineales
set.seed(1)
x=matrix(rnorm(200*2), ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))

# El ploteo de los datos muestra claramente que las fronteras no son lineales
plot(x, col=y)
# Se dividen los datos aleatoriamente en grupos de entrenamiento y prueba
train=sample(200,100)
# Se ajusta el svm usando un kernel radial y gamma = 1
svmfit=svm(y~., data=dat[train,], kernel="radial",  gamma=1, cost=1)
plot(svmfit, dat[train,])

# Obtener información adicional
summary(svmfit)

# Incremento del costo para reducir el número de errores de entrenamiento
svmfit=svm(y~., data=dat[train,], kernel="radial",gamma=1,cost=1e5)
plot(svmfit,dat[train,])

# Es posible realizar validación cruzada usando tune() para seleccionar la mejor opción de
# gamma y costo con un kernel radial
set.seed(1)
tune.out=tune(svm, y~., data=dat[train,], kernel="radial",
              ranges=list(cost=c(0.1,1,10,100,1000),
                          gamma=c(0.5,1,2,3,4)))
# La mejor elección es con costo=1 y gamma=0.5.
# Se realiza la predicción con el conjunto de entrenamiento
summary(tune.out)
table(true=dat[-train,"y"], pred=predict(tune.out$best.model,newx=dat[-train,]))


# SVM con Múltiples Clases

set.seed(1)
x=rbind(x, matrix(rnorm(50*2), ncol=2))
y=c(y, rep(0,50))
x[y==0,2]=x[y==0,2]+2
dat=data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))
svmfit=svm(y~., data=dat, kernel="radial", cost=10, gamma=1)
plot(svmfit, dat)

# Aplicación con Datos de Expresión Genética
# Los datos pertenencen a un conjunto de muestras de tejidos correspondientes a cuatro
# distintas células cancerígenas.  Para cada muestra, medidas de expresión genética se
# encuentran disponibles

library(ISLR2)
# La librería contiene los datos de entrenamiento xtrain e ytrain, así como datos de prueba
# xtest e ytest
names(Khan)
# Examinar las dimensiones de la ddata
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
table(Khan$ytrain)
table(Khan$ytest)

# Los datoscontiene muchas más variables que observaciones por ello se sugiere usar
# un kernel lineal
dat=data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out=svm(y~., data=dat, kernel="linear",cost=10)
summary(out)
table(out$fitted, dat$y)
dat.te=data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te=predict(out, newdata=dat.te)
table(pred.te, dat.te$y)
