#DATOS CUALITATIVOS
##Tablas de frecuencia (UNIDIMENSIONAL)
estado_civil<-scan(what = "character")
estado_civil
table(estado_civil) #Tabla de frecuencias absolutas (fi)
#La primera fila indica los niveles de la variable
#La segunda son las f. absolutas
names(table(estado_civil)) #Nombre de los niveles
table(estado_civil)[4] #Nivel de la posiciÃ³n 4
table(estado_civil)["C"]
prop.table(table(estado_civil)) #Frecuencias relativas (fr)
sum(prop.table(table(estado_civil))) #Todo igual a 1
names(which(table(x) == max(table(x)))) #Hayamos el nivel donde se encuentra la moda
##Tablas de frecuencia (BIDIMENSIONAL)
respuestas<-c("No","No","Si","No","Si","No","No","Si")
sexo<-c("M","M","M","F","F","F","F","F")
table(respuestas,sexo)
table(respuestas,sexo)[1,2] #Se trabaja igual que matrices
table(respuestas,sexo)["Si","M"]
prop.table(table(respuestas,sexo)) #Frecuencias relativas globales
sum(prop.table(table(respuestas,sexo)))
prop.table(table(respuestas,sexo), margin = 1) #F. relativas marginales (Respuesta)
prop.table(table(respuestas,sexo), margin = 2) #F relativas marginales (Sexo)
##Tablas de Frecuencias (MULTIDIMENSIONALES)
pais<-c("Fra","Ale","Ita","Ita","Ita","Ita","Ale","Fra")
table(respuestas,sexo,pais) #Separa por pais
ftable(respuestas,sexo,pais)
ftable(respuestas,sexo,pais, col.vars = c("sexo","respuestas"))
table(sexo,respuestas,pais)["F","No","Ita"]
table(sexo,respuestas,pais)[,"No","Ita"] #Sin distincion del sexo
table(sexo,respuestas,pais)[ , ,"Ita"] #Sin distincion del sexo y las respuestas
prop.table(table(respuestas,sexo,pais))
prop.table(table(respuestas,sexo,pais), margin = 3)
prop.table(table(respuestas,sexo,pais), margin = c(2,3))
##Tablas de frecuencia (DATAFRAME)
#Color de cabello, ojos y sexo de 529 alumnos
tabla<-read.table("http://aprender.uib.es/Rdir/bebenerg.txt", header = TRUE, encoding = "UTF-8")
tabla
summary(tabla) #Resumen de los datos
sapply(tabla,FUN = table) #Frecuencia de cada una de la variables
ftable(tabla)
##Diagrama de barras
barplot(table(tabla$sexo), col = c("blue","pink"), main = "Diagrama de barras de las frecuencias absolutas de la variable sexo")
barplot(prop.table(table(tabla$sexo)), col = c("blue","pink"), main = "Diagrama de barras de las frecuencias relativas de la variable sexo")
barplot(table(tabla$sexo,tabla$bebe))
barplot(table(tabla$sexo,tabla$bebe), beside = TRUE) #Barras sin apilar
barplot(table(tabla$sexo,tabla$bebe), beside = TRUE, legend.text = TRUE) #Legenda que indica cada nivel
##Diagrama circular
pie(table(tabla$estudio), main = "Diagrama circular de la variable estudio")
##Grafico de mosaico
plot(table(tabla$estudio,tabla$bebe), main = "GrÃ¡fico de mosaicos de las variables estudio y bebe")
library(vcd)
library(grid)
mosaic(HairEyeColor, dir = c("v","h","v"), highlighting = "Sex",
       highlighting_fill = c("pink","blue"),
       main = "Gráfico de mosaico de la tabla HairEyeColor")
##EJEMPLO
H_E<-as.table(apply(HairEyeColor, MARGIN = c(1, 2), FUN = sum))
H_E

dimnames(H_E)
dimnames(H_E)<-list(Cabello = c("Negro","Castaño","Rojo","Rubio"),
                    Ojos = c("Marrones","Azules","Pardos","Verdes"))
H_E
plot(H_E, col = c("green"), main = "Diagrama de mosaico de la tabla bidimensional de frecuencias del color de cabello y de ojos")

colSums(H_E) #F. absoluta: Color de ojos
rowSums(H_E) #F. absoluta: Color de cabello
round(prop.table(colSums(H_E)), 3) #F. relativa: Color de ojos
round(prop.table(rowSums(H_E)), 3) #F. relativa: Color de cabello

barplot(prop.table(colSums(H_E)),
        col = c("chocolate4","royalblue1","peru","lawngreen"),
        ylim = c(0,0.5), main = "Frecuencias relativas de colores de ojos")

barplot(prop.table(rowSums(H_E)),
        col = c("black","orange4","red","gold"),
        ylim = c(0,0.5), main = "Frecuencias relativas de colores de cabello")

round(prop.table(H_E), 3) #Frecuencias relativas globales
round(prop.table(H_E, margin = 1), 3) #Frecuencias relativas marginales por color de cabello (Filas)
round(prop.table(H_E, margin = 2), 3) #Frecuencias relativas marginales por color de ojos (Columnas)

barplot(prop.table(H_E, margin = 1), beside = TRUE, legend.text = TRUE,
        col = c("black","orange4","red","gold"),
        ylim = c(0,0.8), main = "Frecuencias relativas de colores de cabello \n según el color de ojos")

barplot(t(prop.table(H_E, margin = 2)), beside = TRUE, legend.text = TRUE,
        col = c("chocolate4","royalblue1","peru","lawngreen"),
        ylim = c(0,0.6), main = "Frecuencias relativas de colores de ojos \n según el color de cabello")

mosaic(HairEyeColor, dir = c("v","h","v"),highlighting = "Sex",
       highlighting_fill = c("pink","lightblue"),
       main = "Gráfico de mosaico de la tabla HairEyesColor")

#DATOS CUANTITATIVOS
pases<-scan()
table(pases) #Frecuencias absolutas
cumsum(table(pases)) #F. absolutas acumuladas
prop.table(table(pases)) #F. relativas
cumsum(prop.table(table(pases))) #F. relativas acumuladas
##Medidas de tendencia central y posición
#Moda: máxima frecuencia
#Media: punto de equilibrio de los datos
#Mediana: dato central
as.numeric(names(which(table(pases) == max(table(pases)))))
table(pases)
mean(pases)
median(pases)
#Mediana: es el cuantil 0.5 (Divide a los datos 50 - 50)
#Cuartiles: cuantil 0.25, 05, 0.75
#Deciles:
#Percentiles:
quantile(pases, 0.25)
#26 es el primero en superar a 0.05
quantile(pases, 0.5)
#29 es el primero en superar a 0.5
quantile(pases, 0.75)
#31 es el primero en superar a 0.75
cumsum(prop.table(table(pases)))
##Medidas de dispersión
#Rango: Diferencia entre el máx y el min de las observaciones
#Rango intercuartilico: Es la diferencia entre los cuartiles 0.75 y 0.25
#Varianza: distancia de los datos respecto a la media
#Varianza muestral: se aproxima significativamente mejor a la varianza real de la población
#D.E.: indica que tan dispersos están los datos alrededor de la media
#Coef. de variaciín: describe la variación de los datos (escala sin unidades)
pesos<-(ChickWeight$weight) #Pesos de pollos con 4 dietas diferentes
pesos
##ESTADISTICOS PARA DATOS SIN AGRUPAR
diff(range(pesos))
IQR(pesos)
var_muestral<-var(pesos)
var<-var(pesos)*(length(pesos)-1)/length(pesos)
c(var_muestral,var)
DE_muestral<-sqrt(var_muestral)
DE<-sqrt(var)
c(DE_muestral,DE)
sd(pesos)
sd(pesos)*sqrt((length(pesos)-1)/length(pesos))
#Summary: Resumen estadistico (Max, Min, 3 Cuartiles, Media)
summary(pesos)
summary(ChickWeight)
#Con by hacemos un resumen según los niveles
#Saber si la dieta influye en el peso (Diet es el factor)
by(pesos,ChickWeight$Diet, FUN = summary)
##Diagrama Box Plot (Caja)
#5 valores (Superior e inferior: 1er y 3er cuartil, Altura: Rango intercuartilico, 
#L. gruesa: Mediana, Bigotes: Min y Max de la variable y valores atípicos)
boxplot(pesos, main = "Diagrama de caja de peso", ylab = "peso", col = "cyan1")
boxplot(pesos~ChickWeight$Diet, main = "Diagrama de caja de peso según dieta", ylab = "peso", xlab = "dieta", col = "cornflowerblue")
boxplot(pesos~ChickWeight$Diet, main = "Diagrama de caja de peso según dieta", ylab = "peso", xlab = "dieta", col = c("cornflowerblue","red","blue","yellow"))
boxplot(pesos~ChickWeight$Diet, plot = FALSE)$stats #5 valores por dieta
boxplot(pesos~ChickWeight$Diet, plot = FALSE)$out #Observaciones atipicas
boxplot(pesos~ChickWeight$Diet, plot = FALSE)$group #Diagrama al que pertenecen
#AGRUPAMIENTO DE DATOS
##Datos discretos con muchos posibles valores
##Datos continuos en gran cantidad
long_petalo<-iris$Petal.Length #Longitud del pétalo de 150 flores
long_petalo
#Número de intervalos
nclass.scott(long_petalo)
nclass.FD(long_petalo)
k = nclass.Sturges(long_petalo)
k
#Amplitud de clase
A = diff(range(long_petalo))/k
A
A = 0.66
#Extremos
m = min(long_petalo)
m
L = m + A*(0:k)
L
#Marcas de clase
marca = (L[0:k] + L[1:k+1])/2
marca
#Usando cut: Lista codificada
##breaks: vector con los extremos
##right: extremo derecho de los intervalos
lista_codif<-cut(long_petalo, breaks = L, right = FALSE)
lista_codif
intervalos <- levels(lista_codif)
intervalos
f_abs = as.vector(table(lista_codif))
f_abs_acum = cumsum(table(lista_codif))
f_rel = as.vector(prop.table(table(lista_codif)))
f_rel_acum = cumsum(prop.table(table(lista_codif)))
#Hacemos la tabla de frecuencias
tabla_fre = data.frame(marca,f_abs, f_abs_acum, f_rel, f_rel_acum)
tabla_fre
##ESTADISTICOS PARA DATOS AGRUPADOS
total = sum(f_abs)
total
media = sum(f_abs*marca)/total
media
mean(long_petalo)
varianza = sum(f_abs*(marca^2))/total -  media^2
varianza
var(long_petalo)
DE = sqrt(varianza)
DE
sd(long_petalo)
#Para el intervalo modal
int_modal = intervalos[which(f_abs == max(f_abs))]
int_modal
tabla_fre
#Para la mediana
total #Número de datos pares, caso contrario se suma 1 antes de dividir
total/2
#Dado que el n está en las f.abs. acum. el limite superior es la mediana
#Intervalo a trabajar
intervalos[which(f_abs_acum >= n)][1]
L
Ls = L[which(f_abs_acum >= n)[1] + 1]
Ls
Li = L[which(f_abs_acum >= n)[1]]
Li
Ai = Ls - Li
Ai
Fi_ant = f_abs_acum[which(f_abs_acum >= n)[1] - 1]
Fi_ant
fi = f_abs[which(f_abs_acum >= n)[1]]
fi
mediana = Li + Ai*((total/2)-Fi.ant)/fi
mediana
#Media de los datos
median(long_petalo)

#HISTOGRAMAS
##Parametros
##breaks: valores de los extremos, númerode intervalos o el metodo
##freq: f. absolutas (TRUE) f. relativas (FALSE)
##right: intervalos abiertos por la derecha (FALSE)
##Componentes
##mids: puntos medios de los intervalos
##counts: f. absolutas de los intervalos
##density: f. relativas entre la amplitud
hist(long_petalo, breaks = L, right = FALSE, labels = c(intervalos), 
     ylim = c(0,50),
     main = "Histograma de frecuencias absolutas",
     xlab = "Intervalos", ylab = "Frecuencias absolutas")

hist(long_petalo, breaks = L, right = FALSE, plot = FALSE)$breaks
hist(long_petalo, breaks = L, right = FALSE, plot = FALSE)$mids
hist(long_petalo, breaks = L, right = FALSE, plot = FALSE)$counts
