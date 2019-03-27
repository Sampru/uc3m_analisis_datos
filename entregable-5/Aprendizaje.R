source("init.R")

## Factor
dnu = as.factor(c( "DOWN", "NEUTRAL", "UP"))

## Generate dataset
gdax.df = stockGetdata("^GDAXI")

## Preprocesar data
## Separar datos para training y testing
set.seed(123)
gdax.df$split = sample.split(gdax.df, SplitRatio=0.7)
gdax.df = na.omit(gdax.df)

# Registrar subidas y bajadas o casos neutros
gdax.df$fwd50 = fwdReturns(gdax.df$Adj.Close, 50)
gdax.df$gdaxY = rep(NA, nrow(gdax.df))
gdax.df$gdaxY[gdax.df$fwd50 > 0.08] = "UP"
gdax.df$gdaxY[gdax.df$fwd50 < -0.08] = "DOWN"
gdax.df$gdaxY[is.na(gdax.df$gdaxY)] = "NEUTRAL"

# Hacer la particion en dos sets
train = subset(gdax.df, gdax.df$split==TRUE)
test = subset(gdax.df, gdax.df$split==FALSE)

gdax.df$split = NULL
train$split = NULL
test$split = NULL

# Extraer ejemplos y omitir los nulos
gdaxExamples = data.frame(Rent10 = backReturns(train$Adj.Close, 10),
                          Rent50 = backReturns(train$Adj.Close, 50),
                          daxY = train$gdaxY)

gdaxExamples = na.omit(gdaxExamples)

# Nearest neighbor
knn = train(daxY~., data = gdaxExamples, method = "knn", trControl = trainControl(method="none"),
            preProcess = c("center","scale"), tuneGrid = data.frame(k = 15))

# Naive Bayes
nb = train(daxY~., data = gdaxExamples, method = "nb", trControl = trainControl(method="cv", number=5))

# Red neural
nnet = train(daxY~., data = gdaxExamples, method = "nnet", trControl = trainControl(method = 'cv', number = 10),
             preProcess = c("center","scale"), tuneGrid = data.frame(size = c(10), decay = c(0.1)))

## Testearlo y comparalo con un caso pseudorandom (todo neurtral)
# Crear las instancias
instancias = data.frame(Rent10 = backReturns(test$Adj.Close, 10),
                        Rent50 = backReturns(test$Adj.Close, 50),
                        daxY = rep(NA, nrow(test)))

# Predecir las instancias con el modelo entrenado
predKnn = predict(knn, newdata = instancias, type = "raw")
predNb = predict(knn, newdata = instancias, type = "raw")
predNnet = predict(knn, newdata = instancias, type = "raw")

# Las respuestas del caso de test y aleatorios
referencia = factor(tail(test$gdaxY, -50), levels=levels(dnu))

# Knn
print(confusionMatrix(referencia, predKnn))

# Naive Bayes
print(confusionMatrix(referencia, predNb))

# Red neural
print(confusionMatrix(referencia, predNnet))

# Predictor aleatorio
random = head(factor(rep(dnu, length(referencia)+3), levels=levels(dnu)), length(referencia))
print(confusionMatrix(referencia, random))

