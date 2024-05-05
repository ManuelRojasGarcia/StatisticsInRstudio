---
title: "PEC2 - MANUEL ROJAS GARCÍA"
output:
  pdf_document: default
---

# Pregunta 1:
Los datos del archive LasVegas.csv, obtenidas del repositorio UCI (https://archivebeta.
ics.uci.edu/ml/datasets) contienen información sobre las reseñas redactadas en
TripAdvisor por clientes de 21 hoteles de Las Vegas. Importe los datos al programa R y
observe los nombres de las variables.

Importamos los datos y creamos la variable dataset:

```{r}
dataset <- read.csv("C:/Users/Manuel/Desktop/UOC/SEMESTRE 3 (Sep 2023 - Feb 2024)/Estadistica/PEC 2/LasVegas.csv", sep = ";")
head(dataset)
```

a) La variable Traveler.type indica el tipo de viajero clasificado en Business, Couples,
Families, Friends, Solo (según si se han hospedado en el hotel por negocios, en pareja,
en familia, con amigos o solos). La variable Hotel.stars indica el número de estrellas
del hotel que pueden ser 3, 3.5, 4, 4.5 o 5. Elabore una tabla de contingencia entre las
variables Traveler.type y Hotel.stars:

Creamos la variable tabla que contiene una tabla de contigencia entre los valores indicados. Para tener una
visualización mejor usamos addmargins e incluimos los sumatorios.

*https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/addmargins*

```{r}
Tabla <- table(dataset$Traveler.type, dataset$Hotel.stars)
Tablasconsum <- addmargins(Tabla)
head(Tablasconsum)
```

b) Si escogemos un individuo de la base de datos al azar ¿cuál es la probabilidad que corresponda a un cliente que se ha hospedado con amigos y en un hotel de 5 estrellas?

Filtramos la tabla anterior en amigos y 5 estrellas. Contamos el total individuos y dividimos para obtener la probabilidad. 

28/504 = 0.05555556 

Realizamos la operación en R.


```{r}
Hotel5 <- subset(dataset, Hotel.stars >= 5 & Traveler.type == "Friends")
Amigo5 <- nrow(Hotel5)
filas <- nrow(dataset)
resultado <-Amigo5/filas
head(resultado)
```
c) ¿Cuál es la probabilidad que un cliente que viaja por negocios se haya hospedado en un hotel de 3 estrellas?

Realizamos la misma operación pero solo seleccionando los sumatorios de "business.
Realizamos la división para obtener el resultado.

15/74 = 0.02027

Realizamos la operación en R.

```{r}
Hotel3 <- sum(dataset$Hotel.stars == 3 & dataset$Traveler.type == "Business")
Negocios <- sum(dataset$Traveler.type == "Business")
Resultado = Hotel3/Negocios
head(Resultado)
```
d) ¿Cuál es la probabilidad que un cliente que se ha hospedado en un hotel de 4 estrellas viaje en pareja?

Realizamos la misma operación anterior pero el operador de 4 estrellas.
Realizamos la división para obtener el resultado.

58/214 = 0.2710

Realizamos la operación en R

```{r}
Hotel4 <- sum(dataset$Hotel.stars == 4 & dataset$Traveler.type == "Couples")
Parejas <- sum(dataset$Traveler.type == "Couples")
Resultado = Hotel4/Parejas
head(Resultado)
```

# Pregunta 2:

a) Si escogemos un cliente de la base de datos al azar. ¿Cuál es la probabilidad que se hospede en un hotel de 5 estrellas?

Hacemos un sumatorio de todos los clientes de 5 estrellas y lo dividimos por el total para poder obtener la probabilidad de hospedarse en dichas condiciones.

192/504 = 0.3809524

```{r}
Cliente5 <- subset(dataset, Hotel.stars == 5)
Totales5 <- nrow(Cliente5)
Totaldataset <- nrow(dataset)
resultado <- Totales5/Totaldataset
resultado
```
b) Escogemos al azar 10 clientes de la base de datos, con reposición. Consideramos la
variable que nos indica el número de clientes, entre los 10, que se hospedan en un hotel
de 5 estrellas.

  i.) ¿Qué distribución sigue esta variable? ¿De qué parámetros?
  
La distribución que sigue está variable es Binomial, que tiene una distribución B(n,p) donde
n será el número de clientes 10 y p la probabilidad de éxito de la probabilidad de hotel de 5 estrellas,
es decir, 192/504 = 0,3809.

B(10, 0,3809)

  ii.) ¿Cuál es la probabilidad que exactamente 3 de los 10 se hospeden en un hotel de 5
estrellas?

*https://r-coder.com/distribucion-binomial-r/*

Realizamos los cálculos con la fórmula.

P(Y = 3)= (10!/3!7!)* 0,3809^3 * (1-0.3809)^7

Utilizamos la bibliografia superior para realizar los cálculos en R


```{r}
dbinom(x = 3, size = 10, prob = 0.3809)
```
c) Supongamos ahora que sabemos que entre los que se hospedan en un hotel de 5 estrellas
el 25% usa el pàrquing del hotel. Sabemos también que un 30% de los clientes de la
base de datos que han usado el pàrquing de su hotel se han hospedado en un hotel de 5
estrellas. ¿Cuál es la probabilidad que un cliente use el pàrquing del hotel?

*https://rpubs.com/maarnuro/594459*

En este caso tenemos que resolver con el teorema de bayes. Se realiza un árbol de probabilidades, donde
el 25% de los clientes de 5 estrellas usan parking y un 30% del total también. 

0,25*0.30 = 0.075

```{r}
0.25*0.30
```

En segundo lugar, el árbol de decisión tiene un 75% de no usar parking y un 30% de si usar el parking aún no siendo de 5 estrellas. 

0.75*0.30 = 0.225

```{r}
0.75*0.3
```

Aplicamos la fórmula

0.25/0.075+0.225 = 0.833

```{r}
0.25/(0.075+0.225)
```
# Problema 3
Supongamos ahora que sabemos que la edad de los clientes de uno de estos hoteles
se distribuye siguiendo una distribución normal de media 52 y desviación típica 11. Escogemos
un cliente al azar:
a) ¿Cuál es la probabilidad que tenga más de 60 años?

*https://r-coder.com/distribucion-normal-r/*

```{r}
pnorm(60, mean = 52, sd = 11, log = FALSE, lower.tail = FALSE)
```
b) ¿Cuál es la probabilidad que tenga menos de 40 años?

```{r}
pnorm(40, mean = 52, sd = 11, log = FALSE, lower.tail = TRUE)
```

c) Encuentre una edad de forma que el 75% de los clientes de este hotel sean menores que
ese valor y el 25% de los clientes sean mayores.

Seguimos usando la web superior para encontral el cuartil con qnorm

```{r}
qnorm(0.75, mean = 52, sd = 11, lower.tail = TRUE, log.p = FALSE) 
```
