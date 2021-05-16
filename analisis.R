#==================================================================================
#Análisis de datos
#==================================================================================

library(stargazer);library(tidyverse);library(readxl);library(car)
library(lmtest);library(sandwich)
base<-read_excel("../Econometria/base_limpia.xlsx")

# ******************************************************************
#Análisis teniendo como controles las variables sociodemográficas
# ******************************************************************

#Convertir en factor las variables categóricas sociodemográficas
base$sexo<-as.factor(base$sexo) #Categoría base: hombre
base$laboral<-as.factor(base$laboral) #Categoría base: desempleado(a)
base$nivel_edu<-as.factor(base$nivel_edu)
base$nivel_edu<-relevel(base$nivel_edu, ref="Primaria") #Categoría base: primaria
base$edad<-as.factor(base$edad)
base$edad<-relevel(base$edad, ref="18 - 25 años") #Categoría base: 18-25 años
base$tratamiento<-as.factor(base$tratamiento) #Categoría base: control

#Regresión con variable independiente únicamente: tratamiento
reg1<-lm(precision~tratamiento, data=base)
clas1<-coeftest(reg1)
rob1<-coeftest(reg1, vcov = vcovHC(reg1, "HC1"))
stargazer(reg1, type="text", se=list(rob1[,"Std. Error"]))

#Regresión con variables de percepción y tratamiento
reg2<-lm(precision~tratamiento+num_neutral+num_negativa+num_positiva, data=base) #NO CORRE NUM_POSITIVA
rob2<-coeftest(reg2, vcov = vcovHC(reg2, "HC1"))
stargazer(reg2, type="text", se=list(rob2[,"Std. Error"]))

#Interacción entre sexo y tratamiento
reg3<-lm(precision~tratamiento+sexo+tratamiento:sexo,data=base)
rob3<-coeftest(reg3, vcov = vcovHC(reg3, "HC1"))

#Regresión con variables: sociodemográficas y tratamiento 
reg4<-lm(precision~tratamiento + sexo + laboral + nivel_edu + edad +
        sexo*tratamiento + laboral*tratamiento + tratamiento*nivel_edu +
        tratamiento*edad, data=base)
rob4<-coeftest(reg4, vcov = vcovHC(reg4, "HC1"))

stargazer(reg3,reg4,type="text",se=list(rob3[,"Std. Error"], rob4[,"Std. Error"]))

# ******************************************************************
#Análisis teniendo como controles las variables de política
# ******************************************************************

#Convertir en factor las variables políticas
base$elecciones_primera<-as.factor(base$elecciones_primera)
base$elecciones_primera<-relevel(base$elecciones_primera, ref="Iván Duque Márquez") #Categoría base: Duque
base$perc_gobierno<-as.factor(base$perc_gobierno) #Categoría base: negativa
base$perc_vacunacion<-as.factor(base$perc_vacunacion) #Categoría base: negativa
base$dispuesto_vacunarse<-as.factor(base$dispuesto_vacunarse) #Categoría base: No
base$perc_vacunas<-as.factor(base$perc_vacunas) #Categoría base:negativa
base$tipo_vacuna<-as.factor(base$tipo_vacuna) #Categoría base: No

#Regresión con variables de percepción política
reg5<-lm(precision~tratamiento + espectro_1 + perc_gobierno + perc_vacunacion+
        dispuesto_vacunarse + perc_vacunas + tipo_vacuna,data=base)
rob5<-coeftest(reg5,vcov=vcovHC(reg5,"HC1"))
stargazer(reg5,type="text",se=list(rob5[,"Std. Error"]))

#Regresión larga con todos los controles
reg5<-lm(precision~tratamiento + sexo + laboral + nivel_edu + edad +
     sexo*tratamiento + laboral*tratamiento + tratamiento*nivel_edu +
     tratamiento*edad+espectro_1 + perc_gobierno + perc_vacunacion+
     dispuesto_vacunarse + perc_vacunas + tipo_vacuna, data=base)


#FALTA HACER UNA REGRESIÓN CON LAS INTERACCIONES EN POLÍTICA Y REVISAR TODO
#FALTA HACER LA TABLA