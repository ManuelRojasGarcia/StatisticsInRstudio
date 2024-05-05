---
title: "PEC1 Manuel Rojas García"
output: pdf_document
date: "2023-10-08"
---

Con la libraria ya instalada, seleccionamos el dataset con el que vamos a trabajar y su ruta

```{r}
library(readr)
file.choose()
```
Creamos una varible donde almacenaremos la ruta donde se encuentra el csv

```{r}
ruta_csv <- "C:\\Users\\Manuel\\Desktop\\UOC\\SEMESTRE 3 (Sep 2023 - Feb 2024)\\Estadistica\\PEC 1\\archive\\supervillain_data_3.csv"
```

Creamos una nueva variable donde almacenamos el dataset. 
Imprimimos por pantalla la variable con 100 valores.

```{r}
villanos <- read_csv(ruta_csv)
head(villanos, 100)
```

# Pregunta 1:

Supercebolla sabe que hay que entrenar mucho y aparecer en muchas películas / series para ser un verdadero
superhéroe. Se requiere analizar los siguientes puntos para ayudarle:
a) ¿De qué tipo es la variable No_Feature_Film? Haga un resumen numérico (media, mediana, cuartiles,
desviación típica, mínimo y máximo) de dicha variable. (1 punto).

La variable es una variable cuantitativa discreta, ya que solo contiene valores enteros.

Calculamos la media

```{r}
media <- mean(villanos$No_Feature_Films)

media
```
Calculamos la mediana


```{r}
mediana <- median(villanos$No_Feature_Films)
mediana

```
Calculamos los dos cuartines Q1, Q3 ya que el Q2 es la mediana.
```{r}
Q1 <- quantile(villanos$No_Feature_Films,0.25)
Q1

```
```{r}
Q3 <- quantile(villanos$No_Feature_Films,0.75)
Q3
```
Calculamos la desviación típica.

```{r}
desviacion <- sd(villanos$No_Feature_Films)
desviacion
```

Calculamos el mínimo y máximo.

```{r}
minimo <- min(villanos$No_Feature_Films) 
minimo
```

```{r}
maximo <- max(villanos$No_Feature_Films)
maximo
```
b) Realice un histograma para representar los datos de la variable No_Feature_Films y comente el resultado.
(1 punto).


```{r}
hist(villanos$No_Feature_Films)
```

Podemos interpretar que el histograma tiene un pico en los primeros valores y ausencia en uno de los valores entre 20/25. Se podría determinar que tiene una asimetria a la derecha pero los valores de 25/30 son mayores que de 15/20. Habría que determinar si los valores ausentes son valores átipicos.


c) Si se añade un nuevo villano con No_Feature_Films de 50 a la lista original. ¿Qué cambiará más, la
media o la mediana? Razona y desarrolla la respuesta. (1.5 puntos)


Afectaría sobre todo a la media, ya que se condideraría un valor atípico puesto que es el número máximo es 34. En resumen, afectaría más a la media que a la mediana ya que los valores alejados afectan más a la media y se dice que es poco resistente a los valores extremos.


# Pregunta 2:

Supercebolla necesita más información sobre los perfiles de los villanos. Responde a las siguientes preguntas:

a) Realice un boxplot entre la variable “No_Feature_Films” y los distintos grupos de la variable “Gender”.
Comente el resultado. (1 punto).

```{r}
boxplot(villanos$No_Feature_Films ~ villanos$Gender)
```
Podemos determinar que los valores femininos y masculinos comparten el mismo mínimo. La mediana está muy cerca de Q1 en ambos generos, pero sobre todo en el masculino. El máximo y los valores más atipicos pertenecen al genero másculino.
En cuanto a los valores otros, podemos determinar que no tiene valores atipicos, que los valores máximos y mínimos coinciden con los cuartiles y que sus valores son asimétricos.
En conceptos generales podemos determinar que el genero femenino tiene la mediana más alta ya que la caja está en una posición más elevada.

b) Haced una tabla de frecuencias relativas de la variable “Gender”. Después usa la tabla para hacer un
diagrama de barras. Comentad los resultados. (1 punto).


Calculamos la frecuencia relativa por genero y generamos la tabla.

```{r}
frecuencia <- prop.table(table(villanos$Gender)) 
frecuencia
```

```{r}
barplot(frecuencia)
```

Podemos determinar que la frecuencia de que un villano sea hombre es muy elevada respecto a los demás valores.

c) ¿Cuáles son los villanos que han ganado más de 80 “Premios de villanos” (variable Award_Winds)?
Mostrar en la salida únicamente la variable del nombre del villano (Name), Award_Wins y Gender.
(1.5 punto)
**fuente https://rpubs.com/hllinas/R_Filtrar_DataFrames**

```{r}
masde80 <- subset(villanos, Award_Wins > 80, select = c("Name", "Award_Wins", "Gender"))
masde80
```

# Pregunta 3:

Supercebolla ha realizado una encuesta, para entre otras cosas, que los villano/as de Marvel y DC indicaran
el año en que decidieron convertirse en villanos por primera vez. Se encuestó en total a 30 villanos de Marvel
y DC.

a) ¿Los resultados de esta encuesta son datos de población o datos de muestra? Razona la respuesta.


Teniendo en cuenta que podemos acceder al número total de villanos en el dataset y solo vamos a seleccionar 30, los resultados deben ser datos de muestra sobre un subconjunto de la población.

b) ¿Cuál es la variable de estudio de la encuesta? ¿Qué tipo de variable es, cuantitativa o cualitativa?

Será  una variable cuantitativa, ya que se refiere a una cantidad numérica entera (el año) que puede medirse y cuantificarse.

c) Si se selecciona para otra encuesta 10 comarcas al azar y seleccionamos al azar 3 villanos de estas
comarcas a los cuales llamamos por teléfono, ¿qué tipo de muestreo sería?

Al seleccionar 10 comarcas al azar podemos determinar que es un muestreo simple (aún sin saber su proceso de selección), pero al seleccionar 3 villanos de dichas comarcas estamos haciendo un estrato, por consiguiente, sería un muestreo de estratificación.
Como indica los apuntes "La muestra se obtiene seleccionando una muestra aleatoria dentro de cada estrato".
Pero después de como se ha resuelto un ejercicio del moodle debemos considerar que es un muestreo por conglomerado, aún sin conocer si los conglomerados son similares los unos a los otros.





