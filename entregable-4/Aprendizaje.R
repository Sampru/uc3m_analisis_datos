
source("init.R")

## Obtencion de datos
gdax = stockGetdata("^GDAXI")
gdax.df = stockSubset(gdax, "2011-01-01", tail(gdax$Date,1))

## Separar datos para training y testing
set.seed(123)
gdax.df$split = sample.split(gdax.df, SplitRatio=0.7)

train = subset(gdax.df, gdax.df$split==TRUE)
test = subset(gdax.df, gdax.df$split==FALSE)

gdax.df$split = NULL
train$split = NULL
test$split = NULL

## Preprocesar data
# Registrar subidas y bajadas
fwd65 = fwdReturns(train$Adj.Close, 65)
gdaxY = factor(fwd65 > 0, labels = c("DOWN","UP"))

# Extraer ejemplos y omitir los nulos
gdaxExamples = data.frame(Rent10 = backReturns(train$Adj.Close, 10),
  Rent65 = backReturns(train$Adj.Close, 65),
  daxY = gdaxY)

gdaxExamples = na.omit(gdaxExamples)

## Crear los modelos de entrenamiento
# K-NN
knn = train(daxY~., data = gdaxExamples, method = "knn", trControl = trainControl(method="none"),
  preProcess = c("center","scale"), tuneGrid = data.frame(k = 6))

# Naive Bayes
nb = train(daxY~., data = gdaxExamples, method = "nb", trControl = trainControl(method="cv", number=10))

# Red neuronal
nnet = train(daxY~., data = gdaxExamples, method = "nnet", trControl = trainControl(method = 'cv', number = 10),
  preProcess = c("center","scale"), tuneGrid = data.frame(size = c(10), decay = c(0.1)))

## Probar los datos
# Extraer 4 instancias del dataset de training
instances <- data.frame(Rent10 = backReturns(test$Adj.Close, 10)[66:69],
  Rent65 = backReturns(test$Adj.Close, 65)[66:69],
  daxY = c(NA_real_, NA_real_, NA_real_, NA_real_))

# Predecir con ellos
prediction <- predict(knn, newdata = instances, type = "prob")
print(prediction)
