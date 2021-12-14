#Base de datos para estimar la prevalencia mensula de infección por cirugías 
# de reemplazos articulares (cadera, hombro, rodilla). Se realizaron 123 remplazos
# de 16932, de enero a noviembre de 2021

library(tidyverse)
library(readxl)
rem <- read_excel("Datos/Reemplazos.xls")

rem <- rem %>% 
  mutate_if(is.character, str_trim)

rem$edad <- as.numeric(rem$edad)

#frecuencia de realización de procedimientos
rem %>% 
  group_by(nompro) %>% 
  mutate(conteo = n()) %>%
  ggplot(aes(x=reorder(nompro,conteo))) +
  geom_bar() +
  coord_flip() +
  labs( title = "Remplazos articulares enero - noviembre 2021",x="", y= "", 
        caption = "Fuente: datos de C.A.S.A")


#frecuencia de realización de procedimientos por profesional
rem %>% 
  group_by(nompro) %>% 
  mutate(conteo = n()) %>%
  ggplot(aes(x=reorder(nompro,conteo), fill=codmed)) +
  geom_bar() +
  coord_flip() +
  labs( title = "Remplazos articulares enero - noviembre 2021",x="", y= "", 
        caption = "Fuente: datos de C.A.S.A")

#Estadistica descriptiva

rem %>% 
  summarise(media=mean(edad), mediana = median(edad), sd=sd(edad), min=min(edad), 
            max=max(edad))

ggplot(rem, aes(x=factor(nompro), y=edad)) +
  geom_boxplot() +
  coord_flip() +
  labs( title = "Remplazos articulares enero - noviembre 2021",x="", y= "Edad", 
        caption = "Fuente: datos de C.A.S.A")

rem %>% 
  group_by(eps) %>% 
  mutate(conteo = n()) %>%
  ggplot(aes(x=reorder(eps,conteo))) +
  geom_bar() +
  coord_flip() +
  labs( title = "Remplazos articulares enero - noviembre 2021", subtitle = "Asegurador"
        ,x="", y= "",caption = "Fuente: datos de C.A.S.A")


rem %>% 
  group_by(quirofano) %>% 
  mutate(conteo = n()) %>%
  ggplot(aes(x=reorder(quirofano,conteo))) +
  geom_bar() +
  coord_flip() +
  labs( title = "Remplazos articulares enero - noviembre 2021", subtitle = "Quirófano"
        ,x="", y= "",caption = "Fuente: datos de C.A.S.A")

  
ggplot(rem, aes(x=factor(nompro), y=dur)) +
  geom_boxplot() +
  coord_flip() +
  labs( title = "Remplazos articulares enero - noviembre 2021", subtitle = "Duración del procedimiento", 
          x="", y= "Duración (horas)", 
        caption = "Fuente: datos de C.A.S.A")
