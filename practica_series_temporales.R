########################################################################
#  PRACTICA: SERIES TEMPORALES EN BIOMEDICINA
#
#  Datasets:
#    - admissions.csv          (ingresos hospitalarios por COVID-19)
#    - breast_cancer_*.csv     (muertes por cancer de mama)
#    - Rmissing.csv            (serie con datos faltantes)
#
#  Antes de empezar:
#    1. Descarga todos los archivos del repositorio GitHub del curso
#    2. Establece el directorio de trabajo con setwd()
#    3. Guarda tu practica como: practica_ST_TuNombre.R
########################################################################


# ─────────────────────────────────────────────────────────────────────
# PASO 0: Instalar y cargar paquetes
# (ejecuta install.packages solo la primera vez)
# ─────────────────────────────────────────────────────────────────────
# install.packages(c("lubridate", "tseries", "forecast",
#                    "ggplot2", "zoo", "xts", "dygraphs"))

library(lubridate)   # manejo de fechas
library(tseries)     # test de estacionariedad (adf.test)
library(forecast)    # auto.arima, checkresiduals, forecast
library(ggplot2)     # graficos
library(zoo)         # series temporales con fechas reales
library(xts)         # extension de zoo para datos financieros/temporales
library(dygraphs)    # graficos interactivos


########################################################################
# BLOQUE 1: MANEJO DE FECHAS CON LUBRIDATE (20 min)
########################################################################
# Concepto clave: En series temporales el TIEMPO es el eje principal.
# Antes de crear cualquier serie debemos asegurarnos de que las fechas
# esten bien formateadas. El paquete lubridate simplifica este trabajo.
########################################################################

# ─────────────────────────────────────────────────────────────────────
# EJERCICIO 1.1 — Crear y manipular fechas
# ─────────────────────────────────────────────────────────────────────

# lubridate lee fechas en distintos formatos con funciones intuitivas:
#   ymd()   -> "2020-03-14"  (año-mes-dia, formato ISO)
#   dmy()   -> "14-03-2020"  (dia-mes-año, formato europeo)
#   mdy()   -> "03-14-2020"  (mes-dia-año, formato americano)

# Ejemplo: inicio del estado de alarma por COVID-19 en España
inicio_pandemia <- ymd("2020-03-14")
inicio_pandemia
class(inicio_pandemia)  # debe ser "Date"

# Tambien podemos incluir hora y zona horaria
primer_caso_oms <- ymd_hm("2019-12-31 12:00", tz = "Asia/Shanghai")
primer_caso_oms

# Extraer componentes individuales de una fecha
year(inicio_pandemia)
month(inicio_pandemia)
day(inicio_pandemia)
wday(inicio_pandemia, label = TRUE, abbr = FALSE)  # dia de la semana

# ---
# PREGUNTA 1.1a:
# ¿Que dia de la semana fue el 14 de marzo de 2020 (inicio del confinamiento)?
# Escribe tu respuesta como comentario:
# RESPUESTA: 

# ---
# PREGUNTA 1.1b:
# Crea una fecha llamada "fin_estado_alarma" con el valor "2020-06-21".
# Luego calcula cuantos dias duro el estado de alarma.

fin_estado_alarma <- _______________
dias_alarma       <- _______________
dias_alarma


########################################################################
# BLOQUE 2: IMPORTAR Y PREPARAR LA SERIE TEMPORAL (15 min)
########################################################################
# Concepto clave: Los objetos zoo y xts permiten asociar cada valor
# a su fecha real, respetando el orden cronologico. Son mas potentes
# que el objeto ts base cuando trabajamos con fechas irregulares.
########################################################################

# ─────────────────────────────────────────────────────────────────────
# EJERCICIO 2.1 — Cargar y preparar datos de ingresos COVID
# ─────────────────────────────────────────────────────────────────────

# Establece el directorio donde tienes los archivos CSV:
# setwd("ruta/a/tu/carpeta")

admissions <- read.csv("admissions.csv")

# Explorar la estructura del dataset
head(admissions)
str(admissions)
unique(admissions$HospitalID)  # hospitales disponibles: A, B, C

# ---
# PREGUNTA 2.1a:
# ¿Cuantas filas y columnas tiene el dataset?
dim(admissions)
# RESPUESTA:
# Filas:     Columnas: 

# ---
# Filtramos solo el Hospital A y sumamos ingresos por fecha
# (puede haber varios codigos postales por dia)
admissions_A <- admissions[admissions$HospitalID == "A", ]

data_A <- aggregate(admissions_A$NumberOfAdmissions,
                    by  = list(admissions_A$HospitalAdmitDate),
                    FUN = sum)
names(data_A) <- c("Dates", "Admissions")

# Convertir a formato Date (imprescindible para crear la serie temporal)
data_A$Dates <- as.Date(data_A$Dates, format = "%Y-%m-%d")
head(data_A)

# Crear objeto zoo: serie temporal con fechas reales como indice
ts_A <- zoo(x = data_A$Admissions, order.by = data_A$Dates)

# Ver inicio y fin de la serie
start(ts_A)
end(ts_A)
length(ts_A)  # numero total de observaciones

# ---
# PREGUNTA 2.1b:
# ¿Cuando empieza y cuando termina la serie del Hospital A?
# ¿Cuantas observaciones tiene?
# RESPUESTA:
# Inicio:         Fin:          N observaciones: 


########################################################################
# BLOQUE 3: EXPLORACION VISUAL DE LA SERIE (15 min)
########################################################################
# Concepto clave: Antes de modelar SIEMPRE visualizamos.
# Buscamos tendencia, estacionalidad y valores atipicos.
# La descomposicion separa la serie en sus tres componentes.
########################################################################

# ─────────────────────────────────────────────────────────────────────
# EJERCICIO 3.1 — Visualizar la serie temporal
# ─────────────────────────────────────────────────────────────────────

# Grafico estatico basico
plot(ts_A,
     col  = "steelblue",
     lwd  = 2,
     main = "Ingresos diarios por COVID-19 — Hospital A",
     ylab = "Numero de ingresos",
     xlab = "Fecha")

# Grafico interactivo con dygraphs (zoom con el raton)
dygraph(ts_A, main = "Ingresos diarios COVID-19 — Hospital A") %>%
  dyAxis("y", label = "Numero de ingresos") %>%
  dyOptions(strokeWidth = 1.5, colors = "steelblue")

# ---
# PREGUNTA 3.1a:
# Mirando el grafico, ¿observas alguna tendencia general?
# ¿Hay periodos con mas ingresos que otros? ¿Cuantas olas distingues?
# RESPUESTA:

# ─────────────────────────────────────────────────────────────────────
# EJERCICIO 3.2 — Descomposicion de la serie
# ─────────────────────────────────────────────────────────────────────
# La funcion decompose() separa la serie en:
#   - Tendencia (trend): direccion general a largo plazo
#   - Componente estacional (seasonal): patron ciclico repetido
#   - Residual: lo que no explican los anteriores (debe ser ruido blanco)

# Para descomponer necesitamos especificar la frecuencia.
# Con datos diarios usamos frequency=7 (patron semanal)
ts_A_weekly <- ts(data_A$Admissions, frequency = 7)
decomp_A    <- decompose(ts_A_weekly)
autoplot(decomp_A) +
  ggtitle("Descomposicion de la serie — Hospital A") +
  theme_minimal()

# ---
# PREGUNTA 3.2a:
# Tras la descomposicion:
# - ¿La serie tiene tendencia? ¿Crece o decrece?
# - ¿Hay estacionalidad semanal? ¿Que dias tienen mas ingresos?
# RESPUESTA:

# ─────────────────────────────────────────────────────────────────────
# EJERCICIO 3.3 — Comparar dos hospitales en el mismo grafico
# ─────────────────────────────────────────────────────────────────────

# Preparamos los datos del Hospital C de la misma manera
admissions_C <- admissions[admissions$HospitalID == "C", ]
data_C <- aggregate(admissions_C$NumberOfAdmissions,
                    by  = list(admissions_C$HospitalAdmitDate),
                    FUN = sum)
names(data_C) <- c("Dates", "Admissions")
data_C$Dates  <- as.Date(data_C$Dates)
ts_C <- zoo(x = data_C$Admissions, order.by = data_C$Dates)

# Grafico comparativo interactivo
dygraph(cbind(ts_A, ts_C),
        main = "Ingresos COVID-19: Hospital A vs Hospital C") %>%
  dySeries("ts_A", label = "Hospital A", color = "steelblue") %>%
  dySeries("ts_C", label = "Hospital C", color = "tomato")  %>%
  dyOptions(strokeWidth = 1.5)

# ---
# PREGUNTA 3.3a:
# ¿Cual de los dos hospitales tuvo mas ingresos acumulados en total?
# Usa sum() sobre los valores de cada serie:

total_A <- sum(coredata(ts_A), na.rm = TRUE)
total_C <- sum(coredata(ts_C), na.rm = TRUE)
total_A
total_C

# RESPUESTA:
# Total Hospital A:     Total Hospital C: 


########################################################################
# BLOQUE 4: DATOS FALTANTES Y LIMPIEZA (10 min)
########################################################################
# Concepto clave: Las series temporales requieren periodos IGUALES
# y CONTINUOS. Los NA rompen esa continuidad y hay que imputarlos
# antes de cualquier analisis o modelado.
########################################################################

# ─────────────────────────────────────────────────────────────────────
# EJERCICIO 4.1 — Detectar y tratar valores faltantes
# ─────────────────────────────────────────────────────────────────────

mydata <- read.csv("Rmissing.csv")
myts   <- ts(mydata$mydata)

# Cuantos NAs tiene la serie?
sum(is.na(myts))
plot(myts, main = "Serie con valores faltantes y outliers",
     col = "steelblue", lwd = 1.5)

# ---
# PREGUNTA 4.1a:
# ¿Cuantos valores faltantes tiene la serie?
# RESPUESTA: 

# Tres metodos de imputacion:

# Metodo 1: LOCF — Last Observation Carried Forward
# Copia el ultimo valor conocido hacia adelante.
# Simple pero puede distorsionar la varianza.
myts_locf   <- na.locf(myts)

# Metodo 2: Interpolacion lineal (RECOMENDADO para series temporales)
# Estima los NAs trazando una linea recta entre los valores conocidos.
# Respeta la continuidad temporal mejor que LOCF.
myts_interp <- na.interp(myts)

# Metodo 3: Rellenar con la media global
# Sencillo pero ignora la estructura temporal.
# Solo recomendado si los NAs son pocos y aleatorios.
media_serie <- mean(myts, na.rm = TRUE)
myts_fill   <- na.fill(myts, media_serie)

# Comparacion visual de los tres metodos
par(mfrow = c(3, 1), mar = c(3, 4, 2, 1))
plot(myts_locf,   main = "Metodo 1: LOCF",                 col = "steelblue",  lwd = 1.5)
plot(myts_interp, main = "Metodo 2: Interpolacion lineal", col = "darkorange", lwd = 1.5)
plot(myts_fill,   main = "Metodo 3: Media global",         col = "darkgreen",  lwd = 1.5)
par(mfrow = c(1, 1))

# tsclean: elimina outliers Y NAs a la vez (muy util en la practica)
myts_clean <- tsclean(myts)
plot(myts_clean,
     main = "Serie limpia con tsclean()",
     col  = "purple", lwd = 2)

# ---
# PREGUNTA 4.1b:
# ¿Cual de los tres metodos crees que es mas adecuado para series
# temporales y por que? ¿Cuando usarias tsclean en lugar de los otros?
# RESPUESTA:


########################################################################
# BLOQUE 5: ESTACIONARIEDAD Y TEST DICKEY-FULLER (10 min)
########################################################################
# Concepto clave: La mayoria de modelos ARIMA requieren que la serie
# sea estacionaria (media y varianza constantes, sin tendencia).
# El test ADF (Augmented Dickey-Fuller) nos dice si lo es.
#
#   H0: la serie NO es estacionaria
#   H1: la serie ES estacionaria
#   p-valor < 0.05 -> rechazamos H0 -> serie estacionaria
########################################################################

# ─────────────────────────────────────────────────────────────────────
# EJERCICIO 5.1 — Test de Dickey-Fuller en la serie del Hospital A
# ─────────────────────────────────────────────────────────────────────

adf_resultado <- adf.test(data_A$Admissions)
adf_resultado

# ---
# PREGUNTA 5.1a:
# ¿Cual es el p-valor? ¿Es estacionaria la serie del Hospital A?
# Interpreta el resultado en el contexto clinico.
# RESPUESTA:
# p-valor: 
# Estacionaria: SI / NO
# Interpretacion:

# ─────────────────────────────────────────────────────────────────────
# EJERCICIO 5.2 — Diferenciar para conseguir estacionariedad
# ─────────────────────────────────────────────────────────────────────
# La diferenciacion (d en ARIMA) elimina la tendencia:
# x_t' = x_t - x_{t-1}
# El parametro d de ARIMA indica cuantas diferenciaciones se aplicaron.

ts_A_diff <- diff(data_A$Admissions)
adf.test(ts_A_diff)

# Comparacion visual: original vs diferenciada
par(mfrow = c(2, 1), mar = c(3, 4, 2, 1))
plot(data_A$Admissions, type = "l",
     main = "Serie original — Hospital A",    col = "steelblue", lwd = 1.5)
plot(ts_A_diff, type = "l",
     main = "Serie diferenciada (d=1)",        col = "tomato",    lwd = 1.5)
par(mfrow = c(1, 1))

# ---
# PREGUNTA 5.2a:
# Tras diferenciar, ¿la serie se vuelve estacionaria?
# ¿Que p-valor obtienes? ¿Que valor de d se usara en ARIMA?
# RESPUESTA:
# p-valor tras diferenciar: 
# Estacionaria: SI / NO
# Valor de d para ARIMA: 


########################################################################
# BLOQUE 6: ACF Y PACF — IDENTIFICACION DEL MODELO (10 min)
########################################################################
# Concepto clave:
#   ACF  (Funcion de Autocorrelacion): muestra la correlacion de la
#        serie con sus propios retardos. Picos significativos -> q (MA).
#   PACF (Funcion de Autocorrelacion Parcial): correlacion directa
#        con cada retardo, eliminando el efecto de los intermedios.
#        Picos significativos -> p (AR).
#
#   Las lineas azules discontinuas marcan el umbral de significacion.
########################################################################

# ─────────────────────────────────────────────────────────────────────
# EJERCICIO 6.1 — Leer ACF y PACF para identificar p y q
# ─────────────────────────────────────────────────────────────────────

par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
acf(ts_A_diff,  main = "ACF — Serie diferenciada Hospital A",  lag.max = 30)
pacf(ts_A_diff, main = "PACF — Serie diferenciada Hospital A", lag.max = 30)
par(mfrow = c(1, 1))

# ---
# PREGUNTA 6.1a:
# - ¿Cuantos retardos salen significativos en el ACF? -> valor de q
# - ¿Cuantos retardos salen significativos en el PACF? -> valor de p
# - ¿Que modelo ARIMA(p,d,q) sugieren los graficos?
# RESPUESTA:
# Retardos significativos ACF  (q): 
# Retardos significativos PACF (p): 
# Modelo sugerido: ARIMA( , 1, )


########################################################################
# BLOQUE 7: MODELADO ARIMA Y PREDICCION (15 min)
########################################################################
# Concepto clave: Auto ARIMA prueba automaticamente combinaciones de
# (p,d,q) y selecciona la que minimiza el AIC (criterio de Akaike).
# Maximiza el ajuste penalizando la complejidad del modelo.
########################################################################

# ─────────────────────────────────────────────────────────────────────
# EJERCICIO 7.1 — Ajustar Auto ARIMA
# ─────────────────────────────────────────────────────────────────────

# Creamos objeto xts para usar con forecast
df_A.ts <- xts(data_A[, -1], order.by = data_A$Dates)
names(df_A.ts) <- "Admissions"

# Auto ARIMA: stepwise=FALSE y approximation=FALSE prueban mas modelos
# (mas lento pero mas preciso)
modelo_arima <- auto.arima(df_A.ts,
                           stepwise      = FALSE,
                           approximation = FALSE,
                           trace         = TRUE)

# Resumen del modelo seleccionado
modelo_arima
summary(modelo_arima)

# ---
# PREGUNTA 7.1a:
# ¿Que modelo ARIMA(p,d,q) ha seleccionado Auto ARIMA?
# ¿Cual es el valor de AIC? (menor AIC = mejor modelo)
# RESPUESTA:
# Modelo: ARIMA( ,  ,  )
# AIC: 

# ─────────────────────────────────────────────────────────────────────
# EJERCICIO 7.2 — Diagnostico de residuos
# ─────────────────────────────────────────────────────────────────────
# Un buen modelo tiene residuos que se comportan como RUIDO BLANCO:
# - Media cero y varianza constante
# - Sin autocorrelacion (ACF plano)
# - Distribucion aproximadamente normal
# El test de Ljung-Box contrasta si hay autocorrelacion en los residuos.
# p-valor > 0.05 -> no hay autocorrelacion -> residuos son ruido blanco

checkresiduals(modelo_arima)

# ---
# PREGUNTA 7.2a:
# ¿Los residuos son ruido blanco? ¿Como lo determines?
# RESPUESTA:
# p-valor test Ljung-Box: 
# ¿Ruido blanco?: SI / NO
# Interpretacion:

# ─────────────────────────────────────────────────────────────────────
# EJERCICIO 7.3 — Prediccion a 30 dias
# ─────────────────────────────────────────────────────────────────────
# h = numero de periodos a predecir hacia el futuro
# El grafico muestra el valor predicho + intervalos de confianza al 80% y 95%

prediccion_arima <- forecast(modelo_arima, h = 30)

autoplot(prediccion_arima) +
  ggtitle("Prediccion 30 dias — Hospital A (Auto ARIMA)") +
  ylab("Ingresos diarios") +
  xlab("Tiempo") +
  theme_minimal()

# Ver los valores numericos de la prediccion
print(prediccion_arima)

# ---
# PREGUNTA 7.3a:
# ¿Cuantos ingresos diarios predice el modelo para los primeros 7 dias?
# ¿Los intervalos de confianza son amplios o estrechos?
# ¿Que implica eso sobre la certeza de la prediccion?
# RESPUESTA:

# ─────────────────────────────────────────────────────────────────────
# EJERCICIO 7.4 — Comparar valores reales vs estimados del modelo
# ─────────────────────────────────────────────────────────────────────

dygraph(cbind(prediccion_arima$x, fitted(prediccion_arima))) %>%
  dySeries("prediccion_arima$x", label = "Real", color = "steelblue") %>%
  dySeries("fitted(prediccion_arima)", label = "Estimado", color = "tomato") %>%
  dyOptions(strokeWidth = 1.5)


# dygraph(cbind(Real = prediccion_arima$x, Estimado = fitted(prediccion_arima)),
#         main = "Real vs Estimado — Hospital A") %>%
#   dySeries("Real",     label = "Real",     color = "steelblue") %>%
#   dySeries("Estimado", label = "Estimado", color = "tomato") %>%
#   dyOptions(strokeWidth = 1.5)



# g <- dygraph(cbind(prediccion_arima$x, fitted(prediccion_arima))) %>%
#   dySeries("prediccion_arima$x", label = "Real", color = "steelblue") %>%
#   dySeries("fitted(prediccion_arima)", label = "Estimado", color = "tomato") %>%
#   dyOptions(strokeWidth = 1.5)
# htmlwidgets::saveWidget(g, "grafico.html", selfcontained = TRUE)
# browseURL("grafico.html")




# ---
# PREGUNTA 7.4a:
# ¿En que periodos el modelo sobreestima o subestima los ingresos?
# ¿Coinciden los picos reales con los picos estimados?
# RESPUESTA:




########################################################################
# BLOQUE 8: EJERCICIO INTEGRADOR — Cancer de mama (OPCIONAL, 15 min)
########################################################################

# ─────────────────────────────────────────────────────────────────────
# EJERCICIO 8.1 — Preparar la serie de España y Francia
# ─────────────────────────────────────────────────────────────────────

df_cancer <- read.csv("breast_cancer_number_of_female_deaths.csv",
                      check.names = FALSE,
                      row.names    = 1)

# España: valores numéricos normales
ts_spain <- ts(as.numeric(df_cancer["Spain", ]),
               start = 1990, frequency = 1)

# Francia: valores con "k" (ej: "12.5k") -> eliminar "k" y multiplicar x1000
france_raw <- as.character(unlist(df_cancer["France", ]))
france_num <- as.numeric(gsub("k", "", france_raw)) * 1000
ts_france  <- ts(france_num, start = 1990, frequency = 1)

# Verificar que no hay NAs — ambos deben devolver 0
sum(is.na(ts_spain))
sum(is.na(ts_france))

# Vista previa
head(ts_spain)
head(ts_france)   # debe mostrar 12500, 12700, 12800...


# ─────────────────────────────────────────────────────────────────────
# TAREA 8.1 — Análisis completo para España
# ─────────────────────────────────────────────────────────────────────

# 1. VISUALIZACIÓN
autoplot(ts_spain) +
  ggtitle("Muertes anuales por cancer de mama — España") +
  ylab("Numero de muertes") +
  xlab("Año") +
  theme_minimal()

# 2. TEST DE ESTACIONARIEDAD
adf.test(ts_spain)
# RESPUESTA: p-valor = 0.8947 -> NO estacionaria (p > 0.05)
# auto.arima usará d=1 o d=2

# 3. AUTO ARIMA
modelo_spain <- auto.arima(ts_spain,
                           stepwise      = FALSE,
                           approximation = FALSE,
                           trace         = TRUE)
modelo_spain
summary(modelo_spain)
# RESPUESTA: Modelo: ARIMA( , , )   AIC:

# 4. DIAGNÓSTICO DE RESIDUOS
checkresiduals(modelo_spain)
# RESPUESTA: p-valor Ljung-Box:    ¿Ruido blanco?: SI / NO

# 5. PREDICCIÓN para los próximos 5 años
pred_spain <- forecast(modelo_spain, h = 5)
autoplot(pred_spain) +
  ggtitle("Prediccion 5 años — Muertes cancer de mama España") +
  ylab("Numero de muertes") +
  xlab("Año") +
  theme_minimal()
print(pred_spain)
# RESPUESTA: Tendencia:    Intervalos amplios/estrechos:

# 6. INTERPRETACIÓN FINAL
# RESPUESTA:
# Modelo seleccionado:
# ¿Estacionaria?: NO (p=0.8947)
# Tendencia observada:
# Implicación sanitaria: el descenso desde ~2000 refleja mejoras
# en diagnóstico precoz (mamografía) y tratamientos adyuvantes


# ─────────────────────────────────────────────────────────────────────
# TAREA 8.2 — Comparar España y Francia en el mismo gráfico
# ─────────────────────────────────────────────────────────────────────

ts_ambos <- cbind(ts_spain, ts_france)

# Verificar que las dos columnas tienen datos
head(ts_ambos)
sum(is.na(ts_ambos))   # debe ser 0

# Gráfico comparativo interactivo
dygraph(ts_ambos,
        main = "Muertes por cancer de mama: España vs Francia") %>%
  dySeries("ts_spain",  label = "España",  color = "steelblue") %>%
  dySeries("ts_france", label = "Francia", color = "tomato")   %>%
  dyOptions(strokeWidth = 2)

# Totales acumulados
total_spain  <- sum(coredata(ts_spain),  na.rm = TRUE)
total_france <- sum(coredata(ts_france), na.rm = TRUE)
total_spain
total_france

# RESPUESTA 8.2a:
# ¿Qué país tiene mayor número de muertes absolutas?
# Total España:       Total Francia:
# ¿Tendencias similares o divergentes?:
# ¿Cómo influyen las diferencias demográficas?:
# Francia tiene ~2x la población de España, por eso más muertes
# absolutas. La comparación correcta requiere tasas por 100.000
# mujeres ajustadas por edad, no números absolutos.


########################################################################
# FIN DE LA PRÁCTICA
# Guarda tu script con tus respuestas: practica_ST_TuNombre.R
########################################################################

