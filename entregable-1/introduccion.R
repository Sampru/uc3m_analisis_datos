# vector por concatenacion
v1 <- c(1, 2, 3)

# vector por sucesion
v2 <- 1:3

# vector por repeticion
v3 <- rep (1, 3)

# recorriendo vectores
v <- 0:15

v[1]
v[1:5]
v[c(1, 5)]
v > 7
v [v%%2==0]

# funcion que devuelve los pares en un vector
es_par <- function(v){
  v [v%%2==0]
}

# funcion que devuelve los pares en un vector
es_impar <- function(v){
  v [v%%2==1]
}

# funcion que devuelve algun elemento es primo
es_primo <- function(num){
  if (num == 2) TRUE
  else if (any(num %% 2:(num/2) == 0)) FALSE
  else TRUE
}

# adaptacion para vectores
vector_primo <- function(v){
  for (num in v) 
    vp <- c(vp, es_primo(num))
  vp
}

# dataframe de ejemplo
df = data.frame(clave=1:3, valor=c('a', 'b', 'c'), nombre=c('letra a', 'letra b', 'letra c'))

# columna 'nombre'
df$nombre

# columnas 'clave' y 'nombre' de 'clave' superior a 1
df[df$clave > 1, c('clave', 'nombre')]

# funcion de concatenado
paste("Hola", " ", "mundo!")
