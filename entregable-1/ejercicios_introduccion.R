# Vector para ejemplos
v = sample(1:20, 10, replace = T)
v

# Ejercicio 1
max_and_following  <- function(v) {
  tail(v, 1 + length(v) - which.max(v))
}

# Ejemplo de funcionamiento
max_and_following(v)

# Ejercicio 2
suavizado <- function(v, a){
  s = vector()
  s[1] = v [1]
  for(i in 2:length(v)){
    s[i] = a * v[i] + (1 - a) * s[i - 1]
  }
  s
}

# Ejemplo de funcionamiento
suavizado(v, 0.7)

# Ejercicio 3
evaluar_columnas <- function(){
  df = data.frame(col1=sample(1:20, 10, replace = T), col2=sample(1:20, 10, replace = T))
  df[,'cmp'] = df[,'col1'] > df[,'col2']
  df
}

# Ejemplo de funcionamiento
evaluar_columnas()
