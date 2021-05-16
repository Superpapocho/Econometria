#=====================================================
#Limpieza de datos
#=====================================================


# install.packages("xlsx")

library(xlsx)
library(dplyr)
library(readxl)

# Direcci?n en la que quiere guardarse el archivo
setwd("D:/Windows/Desktop")

# Base de datos se encuentra en el Repositorio
url <- "https://github.com/Superpapocho/Econometria/blob/main/fake_news_project.xlsx?raw=true"
destfile <- "fake_news_project.xlsx"
curl::curl_download(url, destfile)
data_project <- read_excel(destfile)

# Se eliminan filas innecesarias
data_project=data_project[-1,]
data_project=data_project[,-c(1:5,7:17)]

# Me parecio que la variable duraci?n nos puede dar una idea de la concentraci?n del encuestado
names(data_project)[names(data_project) == "Duration (in seconds)"] <- "duration"

# Remplazar NA con 0
data_project <- mutate_all(data_project, ~replace(., is.na(.), 0))

# Se crea la variable tratamiento: "Tratamiento" y "Control"
data_project$tratamiento <- "Prueba"
data_project$tratamiento <- ifelse(data_project$Tb1percepcion_1>0, "Tratamiento", data_project$tratamiento)
data_project$tratamiento <- ifelse(data_project$Cb1percepcion_1>0, "Control", data_project$tratamiento)
data_project = data_project[data_project$tratamiento=="Tratamiento" | data_project$tratamiento=="Control",]

# Se unen Tb#percepcion_# con Cb#percepcion_# en una sola variable seg?n sea tratamiento o control 
data_project$b1percepcion_1 <- ifelse(data_project$tratamiento=="Tratamiento",data_project$Tb1percepcion_1,data_project$Cb1percepcion_1)
data_project$b1percepcion_2 <- ifelse(data_project$tratamiento=="Tratamiento",data_project$Tb1percepcion_2,data_project$Cb1percepcion_2)
data_project$b1percepcion_3 <- ifelse(data_project$tratamiento=="Tratamiento",data_project$Tb1percepcion_3,data_project$Cb1percepcion_3)
data_project$b1percepcion_4 <- ifelse(data_project$tratamiento=="Tratamiento",data_project$Tb1percepcion_4,data_project$Cb1percepcion_4)

data_project$b2percepcion_1 <- ifelse(data_project$tratamiento=="Tratamiento",data_project$Tb2percepcion_1,data_project$Cb2percepcion_1)
data_project$b2percepcion_2 <- ifelse(data_project$tratamiento=="Tratamiento",data_project$Tb2percepcion_2,data_project$Cb2percepcion_2)
data_project$b2percepcion_3 <- ifelse(data_project$tratamiento=="Tratamiento",data_project$Tb2percepcion_3,data_project$Cb2percepcion_3)
data_project$b2percepcion_4 <- ifelse(data_project$tratamiento=="Tratamiento",data_project$Tb2percepcion_4,data_project$Cb2percepcion_4)

# Se unen Tb#verdad_# con Cb#verdad_# en una sola variable seg?n sea tratamiento o control 
data_project$b1verdad_1 <- ifelse(data_project$tratamiento=="Tratamiento",data_project$Tb1verdad_1,data_project$Cb1verdad_1)
data_project$b1verdad_2 <- ifelse(data_project$tratamiento=="Tratamiento",data_project$Tb1verdad_2,data_project$Cb1verdad_2)
data_project$b1verdad_3 <- ifelse(data_project$tratamiento=="Tratamiento",data_project$Tb1verdad_3,data_project$Cb1verdad_3)
data_project$b1verdad_4 <- ifelse(data_project$tratamiento=="Tratamiento",data_project$Tb1verdad_4,data_project$Cb1verdad_4)

data_project$b2verdad_1 <- ifelse(data_project$tratamiento=="Tratamiento",data_project$Tb2verdad_1,data_project$Cb2verdad_1)
data_project$b2verdad_2 <- ifelse(data_project$tratamiento=="Tratamiento",data_project$Tb2verdad_2,data_project$Cb2verdad_2)
data_project$b2verdad_3 <- ifelse(data_project$tratamiento=="Tratamiento",data_project$Tb2verdad_3,data_project$Cb2verdad_3)
data_project$b2verdad_4 <- ifelse(data_project$tratamiento=="Tratamiento",data_project$Tb2verdad_4,data_project$Cb2verdad_4)

# Se eliminan variables innecesarias
data_project=data_project[,-c(14:45)]

# Vector de variables seg?n el tipo al que se va a convertir
vector = c("sexo","edad","nivel_edu","laboral","superior","elecciones_primera",
           "perc_gobierno","dispuesto_vacunarse","perc_vacunas","tipo_vacuna", "perc_vacunacion",
           "b1percepcion_1","b1percepcion_2","b1percepcion_3","b1percepcion_4",
           "b2percepcion_1","b2percepcion_2","b2percepcion_3","b2percepcion_4",
           "b1verdad_1","b1verdad_2","b1verdad_3","b1verdad_4", "tratamiento",
           "b2verdad_1","b2verdad_2","b2verdad_3","b2verdad_4")
numerico = c("duration","espectro_1")

# Volver variables de vector a factor
data_project[,vector]=lapply(data_project[,vector],factor) 
# Vover variables de numerico a num
data_project[,numerico]=lapply(data_project[,numerico],as.numeric)

# Armando la variable precision - Se tiene en cuenta si la noticia es verdadera o falsa al momento de asignarle un valor
b1p1 = ifelse(data_project$b1verdad_1=="Verdadera",1,0)
b1p2 = ifelse(data_project$b1verdad_2=="Verdadera",1,0)
b1p3 = ifelse(data_project$b1verdad_3=="Verdadera",0,1)
b1p4 = ifelse(data_project$b1verdad_3=="Verdadera",0,1)

b2p1 = ifelse(data_project$b2verdad_1=="Verdadera",1,0)
b2p2 = ifelse(data_project$b2verdad_2=="Verdadera",1,0)
b2p3 = ifelse(data_project$b2verdad_3=="Verdadera",0,1)
b2p4 = ifelse(data_project$b2verdad_3=="Verdadera",1,0)

# N?mero de respuestas correctas
suma = b1p1+b1p2+b1p3+b1p4+b2p1+b2p2+b2p3+b2p4

# Proporci?n de respuestas contestadas correctamente
data_project = data_project %>% mutate(precision=suma/8)
mean(data_project$precision)

# Contando el n?mero de veces que las personas seleccionaron percepcion "Positiva"
positivob1p1 = ifelse(data_project$b1percepcion_1=="Positiva",1,0)
positivob1p2 = ifelse(data_project$b1percepcion_2=="Positiva",1,0)
positivob1p3 = ifelse(data_project$b1percepcion_3=="Positiva",1,0)
positivob1p4 = ifelse(data_project$b1percepcion_3=="Positiva",1,0)

positivob2p1 = ifelse(data_project$b2percepcion_1=="Positiva",1,0)
positivob2p2 = ifelse(data_project$b2percepcion_2=="Positiva",1,0)
positivob2p3 = ifelse(data_project$b2percepcion_3=="Positiva",1,0)
positivob2p4 = ifelse(data_project$b2percepcion_3=="Positiva",1,0)

# Contando el n?mero de veces que las personas seleccionaron percepcion "Ni positiva ni negativa"
neutralb1p1 = ifelse(data_project$b1percepcion_1=="Ni positiva ni negativa",1,0)
neutralb1p2 = ifelse(data_project$b1percepcion_2=="Ni positiva ni negativa",1,0)
neutralb1p3 = ifelse(data_project$b1percepcion_3=="Ni positiva ni negativa",1,0)
neutralb1p4 = ifelse(data_project$b1percepcion_3=="Ni positiva ni negativa",1,0)

neutralb2p1 = ifelse(data_project$b2percepcion_1=="Ni positiva ni negativa",1,0)
neutralb2p2 = ifelse(data_project$b2percepcion_2=="Ni positiva ni negativa",1,0)
neutralb2p3 = ifelse(data_project$b2percepcion_3=="Ni positiva ni negativa",1,0)
neutralb2p4 = ifelse(data_project$b2percepcion_3=="Ni positiva ni negativa",1,0)

# Contando el n?mero de veces que las personas seleccionaron percepcion "Negativa"
negativab1p1 = ifelse(data_project$b1percepcion_1=="Negativa",1,0)
negativab1p2 = ifelse(data_project$b1percepcion_2=="Negativa",1,0)
negativab1p3 = ifelse(data_project$b1percepcion_3=="Negativa",1,0)
negativab1p4 = ifelse(data_project$b1percepcion_3=="Negativa",1,0)

negativab2p1 = ifelse(data_project$b2percepcion_1=="Negativa",1,0)
negativab2p2 = ifelse(data_project$b2percepcion_2=="Negativa",1,0)
negativab2p3 = ifelse(data_project$b2percepcion_3=="Negativa",1,0)
negativab2p4 = ifelse(data_project$b2percepcion_3=="Negativa",1,0)

# Suma de las variables anteriores, n?mero de veces que contestan "Positiva", "Ni positiva ni negativa", "Negativa
positiva = positivob1p1+positivob1p2+positivob1p3+positivob1p4+positivob2p1+positivob2p2+positivob2p3+positivob2p4
neutral = neutralb1p1+neutralb1p2+neutralb1p3+neutralb1p4+neutralb2p1+neutralb2p2+neutralb2p3+neutralb2p4
negativa = negativab1p1+negativab1p2+negativab1p3+negativab1p4+negativab2p1+negativab2p2+negativab2p3+negativab2p4

# Se incluye en la base
data_project = data_project %>% mutate(num_positiva=positiva,num_neutral=neutral,num_negativa=negativa)

# Se eliminan variables innecesarias
data_project=data_project[,-c(15:30)]
str(data_project)

# Se escribe un nuevo archivo
write.xlsx(data_project, file="base_limpia.xlsx", sheetName = "Base", append = TRUE)




