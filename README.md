```{r, eval=FALSE, include=TRUE}
"Protocolo:

1. Daniel Felipe Villa Rengifo

2. Lenguaje: R

3. Tema: Modelo Lineal

4. Fuentes:
   https://bookdown.org/matiasandina/R-intro/modelos-lineales.html"

```

Yo se que este tema no lo ibamos a tratar, pero todos estos dias he estado aprendiendo sobre el tema, ya que en un futuro replit tratare un tema de correlación y me parecio un tema relevante.

un compañenro me ayudo con la explicación del tema, poco a poco sacamos este documento.

```{r}
"Utilizaremos una base de datos de la paqueteria modelr (Estos sencillos conjuntos de datos simulados son útiles para enseñar los fundamentos de la modelización), asi trabajaremos con datos que sean de compresión y facil explicación, la base de datos se llama sim1"

# importamos la base de datos:
sim1 <- read.csv(file = "sim1.csv", header = T, sep = ",", dec = ".")

# Para observar si los datos tienen un patron fuerte haremos un plot de dispersión
library(ggplot2)
png(filename = "plot1.png", height = 720, width = 720)

plot1 <- ggplot(sim1, aes(x, y))+
  geom_point()+
  labs(title = "Grafico de dispersión sobre dos conjunto de datos")

plot1
dev.off()

# Como lo anterior es cierto (patron fuerte entre los datos), a primera vista pareciera que el modelo lineal [y = a + b*x] podria  servir.
```

```{r}
# Según los estudiado, primero veamos modelos al azar, asi mira cual mejor iria en los datos dados:

# Gracias a las funcionalidades de ggplot2, tenemos la opcion geom_abline(), de esta manera podemos ingresar la pendiente y los intenceptos como parametros:

# Creamos los parametros con la ayuda de la libreia tibble (datos "tidy" o limpios):

# Cargamos la libreria:
library(tibble)

# Creamosla tabla para hacer modelos al azar:
m <- tibble(a = runif(250, -20, 40), b = runif(250, -5, 5))

print(m) # visualizamos la tabla

# Una de las ventajas de ggplot(), es que no es necesario crear nuevas graficas, solamente a ggplot se le nombre una variable y poco a poco como un sistema de capas se van añadiendo los que designas después del "+"

plot1 <- plot1 + geom_abline(aes(intercept = a1, slope = a2), data = m, alpha = 1/4)

#pero por cuestiones de estetica lo graficaremos de nuevo con los puntos de otros colores, es decir, plot1 ya se le añade lo mismo que haremos a continuación

png(filename = "ModelosAlazar.png", height = 720, width = 720)


modelazar <- ggplot(sim1, aes(x, y))+
  # el "a" y el "b" son de la funcion anterior dada (funcion lineal)
  # a = intercepto | b = pendiente
  geom_abline(aes(intercept = a, slope = b), data = m, alpha = 1/4) +
  geom_point(color = "red")+
  labs(title = "Modelos lineales Al azar")

modelazar
dev.off()

"A simple vista podemos apreciar que algunos modelos son mejores que otros. Pero necesitamos una forma de cuantificar cuales son los mejores modelos."
```
```{r}
# calcular distancias:
"Una forma de definir mejor es pensar en aquel modelo que minimiza la distancia vertical con cada punto:

Para eso, eligamos un modelo cualquiera:"

############ y = a + b*x ###############
#definimos aletrorio a "a" y "b":

a <- as.numeric(sample(5:10, size = 1, replace = T))
b <- round(runif(1, min = 1, max= 2), digits = 2)

# cargamos la libreria:
library(dplyr)

# Creamos una base datos:
#para que se vean mejor las distancias,
#corremos un poquito cada punto sobre el eje x

distancia1 <- sim1 %>%
  mutate(
    evade = rep(c(-1,0,1)/20, 10),
    x1 = x + evade,
    pred = a + x1*b
  )

# Cremaos el grafico para ver las distacias de una funcion lienal respecto  a los puntos dados y encontar el mejor modelo:

png(filename = "DistanciasAletorias.png", height = 720, width = 720)

dist1 <- ggplot(distancia1, aes(x1, y)) +
  # Esta es la funcion lineal definida por "a" y "b" pseudoaletorios
  geom_abline(intercept = a, slope = b, colour = "black") +
  # Esto grafica los puntos de distancia 1
  geom_point(colour = "black")+
  #Esto calcula la distancia desde el punto hasta la grafica
  #con lineas azules de 90°
  geom_linerange(aes(ymin = y, ymax = pred), colour = "blue")+
  labs(title = "Puntos proyectados a un modelo lineal al azar (Distancias)")

dist1
dev.off()

#La distancia de cada punto a la recta es la diferencia entre lo que predice nuestro modelo y el valor real

"Para computar la distancia, primero necesitamos una función que represente a nuestro modelo"
```
```{r}
# Para eso, vamos a crear una función que reciba un vector con los parámetros del modelo, y el set de datos, y genere la predicción:

model1 <- function(i, data){
  "Esta funcion genera predicciones, es decir, calcula la funcion y = a + b*x"
  "recibe en i un vector de que contenga c(a,b)., vamos a ver los puntos de proyeccions sobre los modelos lineales mostrados"
  md <- i[1] + data$x * i[2]
  return(md)
}

# Probamos la funcion y como vemos esos son los valores proyecctos sobre los puntos dados en sim1:

model1(c(a, b), sim1)
```

```{r}
#Ahora, necesitamos una forma de calcular los residuos y agruparlos. Esto lo vamos a hacer con el "error cuadrático medio" ECM

# Cremaos la funcion:
ECM <- function(mod, data){
  "ESta funcion calcular el error cuadratico medio en este caso recibiendo un vector (mod) y un dataframe, retorna ECM, es ddecir la raiz cuadrada dela media de la diferencia."
  gap <- data[[2]] - model1(mod, data)
  return(sqrt(mean(gap^2)))
}

# Probamos la funcion:
ECM(c(a,b), sim1)
```

#Evaluando los modelos aleatorios

Ahora podemos calcular el ECM para todos los modelos del dataframe models. Para eso utilizamos el paquete purrr, para ejecutar varias veces la misma función sobre varios elementos.

Tenemos que pasar los valores de a y b (dos parámetros –> map2), pero como nuestra función toma sólo uno (el vector a), nos armamos una función de ayuda para __wrapear__ "a" y "b"


```{r}
wrapear <- function(r,t){
  "ECM para wrapear a y b o en este caso recibe a sim1, returnara la solucion de a y b"
  ECM(c(r,t), sim1)
}

# AHora vamos añadirle a la base de datos de "m":
# la variable dist, hace algo como un map, para aplicar valor por valor a ECM

m <- m %>%
  mutate(dist = purrr::map2_dbl(a, b, wrapear))

"A continuación, con `fill` de ggplot los 10 mejores modelos a los datos. Coloreamos los modelos por -dist: esta es una manera fácil de asegurarse de que los mejores modelos (es decir, los que tienen la menor distancia) obtengan los colores más brillantes."
```


```{r}

png(filename = "mejores10modelos.png", height = 720, width = 720)

# Creamos un grafico que nos saque los diez mejores modelos hasta el momento, degradados por la variable o columna "dist", es decir a nenor distancia menos opacidad de la linea

mejor_modelo <- ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, colour = "grey30") +
  geom_abline(aes(intercept = a, slope = b, color = -dist),
              data = filter(m, rank(dist) <= 10))+
  labs(title = "Mejores 10 modelos por medio de `-dist`",
       subtitle = "a menor distancia más brillante la linea o función")

mejor_modelo
dev.off()
```


También podemos pensar en estos modelos como observaciones y visualizar con un gráfico de dispersión de a vs b, nuevamente coloreado por -dist.

Ya no podemos ver directamente cómo se compara el modelo con los datos, pero podemos ver muchos modelos a la vez. Nuevamente, destacamos los 10 mejores modelos, esta vez dibujando círculos rojos debajo de ellos. 


```{r}
png(filename = "GraficodeDIspercionRojo.png", height = 720, width = 720)

# Creamos un grafico de dispersion donde con un delineado rojo, muestre los 10 mejores modelos
dispersion <- ggplot(m, aes(a, b)) +
  geom_point(data = filter(m, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))+
  labs(title = "Grafico de Dispersión \"a\" vs \"b\"", subtitle = "Los 10 mejores modelos estan delineados con circulos rojos")

dispersion
dev.off()
```

# Busqueda de Cuadricula o Grid Search

En lugar de probar muchos modelos aleatorios, podríamos ser más sistemáticos y generar una cuadrícula de puntos uniformemente espaciada (esto se denomina grid search). Elegimos los parámetros del cuadro aproximadamente mirando dónde estaban los mejores modelos en el gráfico anterior.

```{r}
# Creamos un data frame  donde como expliqeu anterior mente vamos a graficarlo connel metodo de "busqueda de cuadricula"

grilla <- expand.grid(
  s1 = seq(-5, 20, length = 25),
  s2 = seq(1, 3, length = 25)
  ) %>% 
  mutate(dist = purrr::map2_dbl(s1, s2, wrapear))

# Con el operados "pipe" accedemos a los datos de grilla (cuadricula), para hacer un grafico donde muestre los 10 mejeres modelos, habiendo normalizado la tabla.

# algo asi, como exponer todos los datos de grilla y buscar los más claros en otras maneras.

png(filename = "GridSearch.png", height = 720, width = 720)

gridsearch <- grilla %>%
  ggplot(aes(s1, s2)) +
  geom_point(data = filter(grilla, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))+
  labs(title = "Por medio del metodo Grid Search buscamos los 10 Mejores Modelos",
       subtitle = "Estos modelos estan seleccionados con delineador rojo")

gridsearch
dev.off()
```

```{r}
#Cuando superponemos los 10 mejores modelos en los datos originales 
#todo se deberia ver bien:

"Graficamos los datos anteriores de plot1.png para compararlos con los modelos lineales sacados hasta ahora (los 10 mejores)"

png(filename = "Modelossobrepuestos.png", height = 720, width = 720)

sobrepuesto <- ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(aes(intercept = s1, slope = s2, colour = -dist),
              data = filter(grilla, rank(dist) <= 10))+
  labs(title = "Los 10 Mejores Modelos sobrepuestos sobre los datos originales",
       subtitle = "Entre más claro mejor es la aproximanción (una menor distancia)")

sobrepuesto
dev.off()
```
