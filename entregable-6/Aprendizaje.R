source("init.R")

## Definiciones
downUp = as.factor(c( "DOWN", "UP"))
confusionMatrix = list(Knn = list(), Rf = list(), Nnet = list(), Rnd = list())

## Generate dataset
gdax.df = stockGetdata("^GDAXI")

minYear = as.integer(substr(head(gdax.df$Date, 1), start = 1, stop = 4))
maxYear = as.integer(substr(tail(gdax.df$Date, 1), start = 1, stop = 4))

# No tener en cuenta el ultimo año, pocos datos
maxYear = maxYear - 1

## Preprocesar data
# Omitir nulos
gdax.df = na.omit(gdax.df)

# Registrar subidas y bajadas a 50 dias
gdax.df$fwd50 = fwdReturns(gdax.df$Adj.Close, 50)
gdax.df$gdaxY = rep(NA, nrow(gdax.df))
gdax.df$gdaxY[gdax.df$fwd50 > 0] = "UP"
gdax.df$gdaxY[gdax.df$fwd50 < 0] = "DOWN"

# Hacer la particion segun el año que toque
for (year in (minYear):(maxYear-1)) {
  print(paste("DEBUG: Entrenando para año", year, "prediciendo para", year+1))
  train = stockSubset(gdax.df, paste(minYear, "01-01", sep = "-"), paste(year, "12-31", sep = "-"))
  test = stockSubset(gdax.df, paste(year + 1, "01-01", sep = "-"), paste(year + 1, "12-31", sep = "-"))
  
  # Extraer ejemplos y omitir los nulos
  gdaxExamples = data.frame(Rent10 = backReturns(train$Adj.Close, 10),
                            Rent50 = backReturns(train$Adj.Close, 50),
                            daxY = train$gdaxY)
  
  gdaxExamples = na.omit(gdaxExamples)
  
  # Nearest neighbor
  print("DEBUG: K-nn")
  set.seed(year)
  knn = train(daxY~., data = gdaxExamples, method = "knn", trControl = trainControl(method="cv", number=10),
              preProcess = c("center","scale"), tuneGrid = data.frame(k = 15))
  
  # Random Forest
  print("DEBUG: Random forest")
  set.seed(year)
  rf = train(daxY~., data = gdaxExamples, method="rf", metric="Accuracy", trControl=trainControl(method="cv", number=10),
             tuneGrid=expand.grid(.mtry=sqrt(ncol(gdaxExamples))))
  
  # Red neural
  print("DEBUG: Neural network")
  set.seed(year)
  nnet = train(daxY~., data = gdaxExamples, method = "nnet", trControl = trainControl(method = 'cv', number = 10),
               preProcess = c("center","scale"), tuneGrid = data.frame(size = c(10), decay = c(0.1)))
  
  ## Testearlo y comparalo con un caso pseudorandom (todo neurtral)
  # Crear las instancias
  instancias = data.frame(Rent10 = backReturns(test$Adj.Close, 10),
                          Rent50 = backReturns(test$Adj.Close, 50),
                          daxY = rep(NA, nrow(test)))

  # Predecir las instancias con el modelo entrenado
  predKnn = predict(knn, newdata = instancias, type = "raw")
  predRf = predict(rf, newdata = instancias, type = "raw")
  predNnet = predict(nnet, newdata = instancias, type = "raw")
  
  # Las respuestas del caso de test y aleatorios
  referencia = factor(tail(test$gdaxY, -50), levels=levels(downUp))
  
  # Knn
  confusionMatrix$Knn[year-minYear] = confusionMatrix(referencia, predKnn)[2]
  
  # Random forest
  confusionMatrix$Rf[year-minYear] = confusionMatrix(referencia, predRf)[2]
  
  # Red neural
  confusionMatrix$Nnet[year-minYear] = confusionMatrix(referencia, predNnet)[2]
  
  # Predictor aleatorio
  set.seed(year)
  random = sample(downUp, length(referencia), replace=TRUE)
  confusionMatrix$Rnd[year-minYear] = confusionMatrix(referencia, random)[2]
}

## Impresion de resultados
for (i in 1:(maxYear - (minYear+1))) {
  print(paste("K-NN para año", (minYear+i)))
  print(confusionMatrix$Knn[i])
}
for (i in 1:(maxYear - (minYear+1))) {
  print(paste("Random forest para año", (minYear+i)))
  print(confusionMatrix$Rf[i])
}
for (i in 1:(maxYear - (minYear+1))) {
  print(paste("Red neuronal para año", (minYear+i)))
  print(confusionMatrix$Nnet[i])
}
for (i in 1:(maxYear - (minYear+1))) {
  print(paste("Random para año", (minYear+i)))
  print(confusionMatrix$Rnd[i])
}

## Creación del gráfico
plot(gdax.df$Date, gdax.df$fwd50, type="line")
