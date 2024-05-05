---
title: "PEC 5- MANUEL ROJAS GARCÍA"
author: "UOC"
output:
  word_document: default
  html_document: default
  pdf_document: default
---

**NOMBRE: MANUEL ROJAS GARCÍA**

# Introducción

En esta PEC utilizaremos el conjunto de datos 'winequality-red.csv' que contiene información técnica y gustativa de distintos tipos de vino tinto.

Se pueden consultar en 
[https://archive.ics.uci.edu/ml/datasets/Wine+Quality](https://archive.ics.uci.edu/ml/datasets/Wine+Quality)

Las variables que contiene son las siguientes: 

- acidez fija
- acidez volátil
- ácido cítrico
- azúcar residual
- cloruros
- dióxido de azufre libre
- dióxido de azufre total
- densidad
- pH
- sulfatos
- alcohol
- calidad


Os puede ser útil consultar el siguiente material:

- Módulos teóricos de Regresión lineal simple, múltiple y ANOVA.
- Actividades resueltas del Reto 5 (regresión lineal simple, múltiple y ANOVA).

Hay que entregar la práctica en fichero pdf o html (exportando el resultado final a pdf o html por ejemplo). Se recomienda generar el informe con Rmarkdown que genera automáticamente el pdf/html a entregar.

NOTA 1: no es necesario ni limpiar ni preprocesar los datos para este ejercicio

NOTA 2: comprobar que el dataset ha cargado correctamente (vigilar con la separación que se utiliza en el csv)

```{r}
dataset <- read.csv("C:/Users/Manuel/Desktop/UOC/SEMESTRE 3 (Sep 2023 - Feb 2024)/Estadistica/PEC 5/ENTREGA/data_pac5.csv", sep = ";", header = TRUE)
```


# Pregunta 1. (resolver con R). (3 puntos)

La empresa especializada en la creación de vinos de alta calidad está buscando comprender mejor las variables que influyen en la calidad del vino para optimizar sus estrategias de producción y marketing. Se realizará un análisis para identificar las características clave que contribuyen a la calidad del vino y determinar el enfoque para futuras campañas publicitarias.


a) Realiza un gráfico de dispersión entre la variable *citric.acid* y la variable *residual.sugar*. ¿Cuál es el coeficiente de correlación? Interpretad el resultado (1 punto).

*https://rpubs.com/osoramirez/316691*

**Solución:**

```{r,eval=TRUE,echo=TRUE}
plot(dataset$citric.acid, dataset$residual.sugar,
     main = "Gráfico de Dispersión: Citric Acid vs. Residual Sugar",
     xlab = "Ácido Cítrico",
     ylab = "Azúcar Residual")
```

```{r}
modelo <- lm(citric.acid ~ residual.sugar, data = dataset)

summary(modelo)
```
La recta de regresión es:
ŷ = 0.220613 + 0.019837x

La pendiente, de valor 0.019837 nos indica que por cada aumento de 1, el aumento del azucar residual es de 0.019837.

El coeficiente de determinación R2 es de 0.02061 y el de coeficiente de correlación lineal es: √0.2061

```{r}
correlacion <- sqrt(0.02061)

cat ("El coeficiente de correlacion es de:", correlacion)
```
Hacemos los cálculos de forma automática.

```{r}
correlacionauto <- cor(dataset$citric.acid, dataset$residual.sugar)
cat("Coeficiente de Correlación:", correlacionauto)
```

El coeficiente de determinación (R^2) es de 0.02061 lo que indica que el modelo de regresión lineal solo explica el 2.06% de la variabilidad en la variable de respuesta (citric.acid) en función de la variable predictora (residual.sugar).
Un coeficiente de determinacion bajo indica que el modelo no explica una gran proporción de la variabilidad en la variable dependiente. Además, la correlación también es baja, con un valor de 0.1435 lo que determina que hay una relación débil entre las dos variables analizadas: citric.acid y residual.sugar, es decir, los cambios en residual.sugar no están asociados con cambios en citric.acid.


b) Encontrad los siguientes dos parámetros del modelo de regresión lineal a estudiar: el intercepto ($B_0$) y la pendiente ($B_1$) (1 punto).

**Solución:**

La recta de regresión es:  ŷ = 0.220613 + 0.019837x

Como ya lo habiamos obtenido en el ejecicio anterior. Como conocemos que la recta de la regresión lineal es:

ŷ = β0 + β1 x 

β0 = 0.220613
β1 = 0.19837

Podemos encontrar los resultados en la misma recta de regresion.

Realizamos el cálculo automático de los coeficientes

```{r,eval=TRUE,echo=TRUE}
modelo <- lm(citric.acid ~ residual.sugar, data = dataset)

coefficients(modelo)
```

c) ¿Qué porcentaje de la variación en la calidad del vino no puede ser explicado por los azucares residuales? (1 punto)


```{r}
azucares <- lm(citric.acid ~ residual.sugar, data = dataset)
summary(azucares)
```
Como hicimos en el ejercicio anterior, obtenemos el valor del modelo:

Multiple R-squared:  0.02061,	Adjusted R-squared:  0.02 

Para obtener el porcentaje de variación no explicada, restamos 1 al coeficiente de determinación y multiplicamos por 100 para obtener el porcentaje.

```{r}
Porcentaje <- (1 - 0.02061)*100
cat ("Porcentaje de variación no explicada:", Porcentaje, "%") 
```

El 97.939 de la variabilidad en solo enla calidad del vino no está siendo explicada por la variable residual.sugar.Deberiamos incluir otros valores u actores externos como podría ser el clima, sequia, etc.


# Pregunta 2. (resolver con R). (3 puntos)

Para la creación de la próxima versión mejorada de vinos tintos, se han seleccionado distintos vinos y se han sometido a diversas catas.

Se procederá inicialmente a analizar los datos obtenidos de la evaluación de la influencia de la cantidad de sulfatos en la calidad del vino. Se buscará determinar si existen diferencias significativas entre las cantidades de la variable *sulphates*  para distintos grupos definidos por la calidad (variable Quality_group **no disponible en el dataset**).

Si miramos la salida del modelo creado, contestad las preguntas siguientes:


a) ¿Cuántos grupos y cuántas observaciones hay en el dataset? (1 punto)

**Solución:**

Hago un modelo parecido para mi propio dataset para analizar y comprender los datos con la siguiente web

*https://rpubs.com/Joaquin_AR/219148*



```{r}
modelo_anova <- aov(quality ~ sulphates , data = dataset)
summary(modelo_anova)
```

```{r}
plot(modelo_anova)
```

Una vez comprendidos los datos anova, usamos los que indica el ejercicio

## Df Sum Sq Mean Sq F value Pr(>F)
## Quality_group 3 2.98 0.9942 36.94 <2e-16 ***
## Residuals 1595 42.93 0.0269
## ---
## Signif. codes: 0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1


Podemos determinar que tiene 3 grados de libertad (DF) para los grupos "Quality_group" y 1595 grados de libertad (DF) para "Residuals"
Hay 4 grupos ya que en Anova se resta 1 a los grados de libertad (k -1) y el número total de observaciones en el dataset es la suma de los grados de libertad para los grupos y los residuos: 3 + 1595 =1598



b) Si se utiliza el nivel de significación $\alpha = 0.05$, ¿qué valor crítico se debe utilizar para realizar el análisis de la varianza? (1 punto)

**Solución:**

En primer lugar, determinamos que rechazamos la hipótesis nula ya que el pvalor (que es prácticamente 0) es menor que el valor de significación (Aunque en el siguiente apartado descubriremos la misma comparativa pero con el valor crítico)

En segundo lugar, para poder obtener el valor crítico necesitamos los grados de libertad ya obtenidos en el apartado anterior  y usar qf para obtener la distribución F.

*https://statologos.com/f-valor-critico-r/*

```{r}

alpha <- 0.05

valorcritico <- qf(1 - alpha, df1 = 3, df2 = 1595)

cat("Valor crítico de la distribución F:", valorcritico)


```

c) ¿Cuál es la conclusión del análisis de la varianza (con un nivel de significación del 5%) en función del valor crítico? (1 punto)



```{r}
modeloanova <- aov(quality ~ sulphates, data = dataset)
summary(modeloanova)
```


El valor F es de 107,7  si  este valor es superior al valor crítico (2,61) debemos rechazar la hipótesis nula y afirmamos que hay diferencias significativas entre los grupos en términos de la variable "Quality_group"


# Pregunta 3 (resolver con R). (4 puntos)

Exploraremos un modelo predictivo sobre la calidad del vino utilizando múltiples variables:

a) Escribe la ecuación que se obtiene del modelo de regresión múltiple para predecir la calidad del vino utilizando las variables de pH, contenido de azúcar residual y sulfatos. (1 punto)

**Solución:**


Quality = β0 + β1(pH) + β2(Residualsugar) + β3(sulphates) + error.

Sacamos el modelo como en regresion simple pero ahora multiple con las 3 variables que nos indica el ejercicio.

```{r}
modelo2 <- lm(quality ~ pH + `residual.sugar` + sulphates, data = dataset)

summary(modelo2)

```
Siendo la formula con los datos la siguiente:

Calidad del vino  = 4.967242 − 0.039750(Ph) + 0.006701 (residual sugar) + 1.190285 (sulphates) + error.

Quality = β0 + β1(pH) + β2(Residualsugar) + β3(sulphates) + error.

b) ¿El modelo en su conjunto es significativo con un nivel del 5%? Además, ¿cuál es el coeficiente de determinación obtenido para este modelo? (1 punto)


F-statistic: 35.99 on 3 and 1595 DF,  p-value: < 2.2e-16


Multiple R-squared:  0.06341,	Adjusted R-squared:  0.06165

El modelo de determinación es 0.06341, y el de coeficiente de correlación lineal es: √0.06341 = 0.2518134

Volvemos ha realizar el valor critico como un ejercicio anterior que ya obteniamos el valor critico

```{r}
alpha <- 0.05

valorcritico <- qf(1 - alpha, df1 = 3, df2 = 1595)

cat("Valor crítico de la distribución F:", valorcritico)
```

Seguimos determinando que como el pvalor es tan pequeño  que sigue significando que rechazamos la hipótesis nula


c) Dado un vino con un pH de 3.5, un contenido de azúcar residual de 2.5 y sulfatos de 0.6, ¿cuál sería su
calidad según el modelo establecido?

Como ya tenemos la fórmula creada solo debemos sustituir los valores

Calidad del vino  = 4.967242 − 0.039750(Ph) + 0.006701 (residual sugar) + 1.190285 (sulphates) + error.

Calidad del vino  = 4.967242 − 0.039750(3.5) + 0.006701 (2.5) + 1.190285 (0.6)

```{r}
Quality <- 4.967242 - 0.039750 * (3.5) + 0.006701 * (2.5) + 1.190285 * (0.6)
Quality
```

d) Si tuvieras que eliminar alguna variable del modelo del apartado a), considerando un nivel de significación del 5%, ¿cuál eliminarías y por qué? (1 punto).

**Solución:**


```{r}
matrizcorrelacion <- cor(dataset[c("quality", "pH", "residual.sugar", "sulphates")])

print(matrizcorrelacion)

```


Quality vs. pH: La correlación entre "quality" y "pH" es -0.0577. Esta correlación es bastante baja y cercana a cero, lo que sugiere una relación débil o nula entre estas dos variables.

Quality vs. Residual Sugar: La correlación entre "quality" y "residual.sugar" es 0.0137. Similar a la correlación con pH, es cercana a cero, indicando una relación débil o nula.

Quality vs. Sulphates: La correlación entre "quality" y "sulphates" es 0.2514. Esta correlación es más fuerte en comparación con las anteriores, pero aún así no es extremadamente alta. Indica una relación positiva moderada entre la calidad del vino y la cantidad de sulfatos.

Basándonos en estos resultados, podríamos considerar eliminar la variable pH del modelo. Sin embargo, la decisión también dependerá del contexto del problema y de consideraciones teóricas sobre qué variables son importantes para tu análisis específico.