# Creador: Stheinna Rius
# Pregunta 2

# 1) Instalar paquetes y llamar a la base de datos

install.packages("pacman")
library(pacman)

p_load("readr",
       "ggplot2",
       "ggrepel",
       "dplyr",
       "matrixTests")

datos <- read_csv(file="https://raw.githubusercontent.com/ManuelLaraMVZ/Transcript-mica/main/examen2")
head(datos)
View(datos)

#############################################################

# 2) Identificar los miRNA de referencia

mirna_referencia <- datos %>%
  filter(Condicion=="Control")
head(mirna_referencia)

# 3) Sacar promedios CT de miRNA de referencia

promedio_referencia <- mirna_referencia %>%
  summarise(Mean_C1=mean(Cx1),
            Mean_C2=mean(Cx2),
            Mean_C3=mean(Cx3),
            Mean_T1=mean(T1),
            Mean_T2=mean(T2),
            Mean_T3=mean(T3),) %>%
  mutate(miRNA="Promedio_referencia") %>%
  select(7,1,2,3,4,5,6)
promedio_referencia  

############################################

# 4) Identificar los miRNA de interés 

mirna_interes <- datos %>%
  filter(Condicion=="Target") %>%
  select(-2)
head(mirna_interes)

############################################

# 5) Sacar el DCT

DCT <- mirna_interes %>%
  mutate(DCT_C1=(Cx1-promedio_referencia$Mean_C1),
         DCT_C2=(Cx2-promedio_referencia$Mean_C2),
         DCT_C3=(Cx3-promedio_referencia$Mean_C3),
         DCT_T1=(T1-promedio_referencia$Mean_T1),
         DCT_T2=(T2-promedio_referencia$Mean_T2),
         DCT_T3=(T3-promedio_referencia$Mean_T3)) %>%
  select(-2,-3,-4,-5,-6,-7)
DCT

# 6) Sacar el promedio de DCT

promedio_mirna_interes <- DCT %>%
  mutate(Mean_DCT_Cx=((DCT_C1+DCT_C2+DCT_C3)/3),
         Mean_DCT_Tx=((DCT_T1+DCT_T2+DCT_T3)/3)) %>%
  select(-2,-3,-4,-5,-6,-7)
promedio_mirna_interes

# 7) Sacar el 2^-DDCT

DosDDCT <- promedio_mirna_interes %>%
  mutate(DosDDCT=(2^-(Mean_DCT_Tx-Mean_DCT_Cx))) %>%
  select(-2,-3)
DosDDCT

############################################
