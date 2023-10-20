library(rpart)
data(kyphosis)

df <- kyphosis 
head(df)

# Éxito: valor "absent" en Kyphosis
# La columna ya es de tipo factor
df$Kyphosis

# Dividir datos
train <- 1:65
validation <- 65:nrow(df)

##### 
# Regresión logística binaria
##### 

modelo_logistic <- glm(
  Kyphosis ~ .,
  family = binomial,
  data = df
)
summary(modelo_logistic)

yprob <- predict(modelo_logistic, type = 'response')
# Valores predichos
ypred <- as.numeric(yprob >= 0.5) |>
  factor(labels = levels(df$Kyphosis))

mean(ypred != df$Kyphosis) * 100

caret::confusionMatrix(
  data = ypred, reference = df$Kyphosis, 
  positive = "absent", mode = 'everything'
)


modelo_logistic <- glm(
  Kyphosis ~ .,
  family = binomial(link = "cauchit"),
  data = df
)
summary(modelo_logistic)

yprob <- predict(modelo_logistic, type = 'response')
# Valores predichos
ypred <- as.numeric(yprob >= 0.5) |>
  factor(labels = levels(df$Kyphosis))

mean(ypred != df$Kyphosis) * 100

caret::confusionMatrix(
  data = ypred, reference = df$Kyphosis, 
  positive = "absent", mode = 'everything'
)

library(naive_bayes())

a <- naive_bayes(kyphosis ~ ., data = df, usekernel = TRUE)
a