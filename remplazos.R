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

tabla1 <- rem %>% 
  group_by(nompro) %>% 
  count() %>% 
  arrange(desc(n))

write.table(tabla1, file="Tabla1.xlsx")


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
        caption = "Fuente: datos de C.A.S.A", tag = "Figura 1")

rem %>% 
  group_by(eps) %>% 
  mutate(conteo = n()) %>%
  ggplot(aes(x=reorder(eps,conteo))) +
  geom_bar() +
  coord_flip() +
  labs( title = "Remplazos articulares enero - noviembre 2021", subtitle = "Asegurador"
        ,x="", y= "",caption = "Fuente: datos de C.A.S.A", tag = "Figura 2")


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
        caption = "Fuente: datos de C.A.S.A", tag = "Figura 3")

rem %>% 
  summarise(media=mean(dur), mediana = median(dur), sd=sd(dur), min=min(dur), 
            max=max(dur))




#Creando vector solo con procedimientos primarios

rem_2 <- rem %>%
  filter(!nompro %in% c("REVISION REEMPLAZO TOTAL DE RODILLA CON RECONSTRUCION DE LOS TRES COMPONENTES (FEMORAL TIBIAL Y PATELAR)",
                     "REVISION REEMPLAZO TOTAL DE RODILLA CON RECONSTRUCCION DE COMPONENTE TIBIAL", 
                     "REVISION REEMPLAZO TOTAL DE CADERA"))

rem_2 <- rem_2 %>% 
  mutate(articulación= ifelse(nompro %in% c("REEMPLAZO PARCIAL PROTESICO  DE CODO SOD",
                                            "REEMPLAZO TOTAL PROTESICO  DE CODO SOD"),"codo", nompro))

rem_2$articulación <- ifelse(rem_2$articulación %in% c("REEMPLAZO PROTESICO TOTAL PRIMARIO SIMPLE DE CADERA",
                                                       "REEMPLAZO PROTESICO TOTAL PRIMARIO COMPLEJO DE CADERA (ARTROSIS SECUNDARIA)",
                                                       "REEMPLAZO PARCIAL DE CADERA","REEMPLAZO PROTESICO TOTAL CON ARTRODESIS DE CADERA"),"cadera", rem_2$articulación)

rem_2$articulación <- ifelse(rem_2$articulación %in% c("REEMPLAZO PROTESICO TOTAL PRIMARIO TRICOMPARTIMENTAL SIMPLE DE RODILLA",                        
                                                       "REEMPLAZO PROTESICO TOTAL PRIMARIO TRICOMPARTIMENTAL COMPLEJO DE RODILLA (ARTROSIS SECUNDARIA)",
                                                       "REEMPLAZO  TOTAL DE RODILLA UNICOMPARTIMENTAL (HEMIARTICULACION)",                              
                                                       "REEMPLAZO  TOTAL DE RODILLA BICOMPARTIMENTAL"),"rodilla", rem_2$articulación)

rem_2$articulación <- ifelse(rem_2$articulación %in% c("REEMPLAZO PROTESICO PRIMARIO TOTAL DE HOMBRO"),"hombro", rem_2$articulación)



#procedimientos por articulación
rem_2 %>% 
  group_by(articulación) %>% 
  count() %>% 
  arrange(desc(n))

rem_2 %>% 
  group_by(articulación) %>% 
  count() %>% 
  arrange(desc(n)) %>%
  ungroup() %>% 
  summarise(suma=sum(n))

#Cálculo de tamaño muestral: Tamaño de la población:	103, proporción esperada:	2,000%,	
# Nivel de confianza:	95,0%, Efecto de diseño:	1,5 (muestreo aleatorio estratificado por mes), presición:3.5%. Uso de Epidat.
#44 casos

rem_2 <- rem_2 %>% 
  mutate(mes=str_sub(fechacx,4,5))

rem_2$mes <- factor(rem_2$mes, levels = c("01","02","03","04","05","06","07","08",
                                      "09","10","11"), labels = c("Ene", "Feb", "Mar",
                                                                  "Abr", "May", "Jun", "Jul",
                                                                  "Ago", "Sep", "Oct", "Nov"))

rem_2 %>% 
  group_by(mes) %>% 
  count()

#ejemplo de muestreo estratificado con afijaciòn proporcional, los estratos son los meses

rem_2 %>% 
  group_by(mes) %>% 
  summarise(frec=n()) %>% 
  mutate(prop=frec/nrow(rem_2), nj=round(prop*44,))

muestra_estratificada <- bind_rows(slice_sample(rem_2[rem_2$mes=="Ene",], n=6),
slice_sample(rem_2[rem_2$mes=="Feb",], n=7),
slice_sample(rem_2[rem_2$mes=="Mar",], n=3),
slice_sample(rem_2[rem_2$mes=="Abr",], n=0),
slice_sample(rem_2[rem_2$mes=="May",], n=6),
slice_sample(rem_2[rem_2$mes=="Jun",], n=1),
slice_sample(rem_2[rem_2$mes=="Jul",], n=4),
slice_sample(rem_2[rem_2$mes=="Ago",], n=5),
slice_sample(rem_2[rem_2$mes=="Sep",], n=4),
slice_sample(rem_2[rem_2$mes=="Oct",], n=5),
slice_sample(rem_2[rem_2$mes=="Nov",], n=4))


#muestreo estratificado usando tablas anidadas del paquete dplyr


muestra_estrat <- rem_2 %>% 
  group_nest(mes) %>% 
  rowwise() %>% 
  mutate(samplesz = round(44*nrow(data)/nrow(rem_2)), sample = list(sample_n(data, size = samplesz))) %>% 
  select(-c(data, samplesz)) %>% 
  unnest(sample)




# Muestreo aleatorio, exculyendo codo y rodillas por baja frecuencia

#muestra <- rem_2 %>% 
  #filter(articulación %in% c("cadera", "rodilla")) %>% 
  #sample_n(size = 44) (luego exporto para terminar de introducir los datos.)

#library(writexl)
#write_xlsx(muestra,"muestra.xlsx")

#cargando nueva de base de datos "muestra"

muestra <- read_excel("datos/muestra.xlsx")

muestra <- muestra %>% 
  mutate(mes=str_sub(fechacx,4,5))

muestra$mes <- as.numeric(muestra$mes)


muestra$mes <- factor(muestra$mes, levels = c(1:11),
                      labels = c("Ene", "Feb", "Mar","Abr", "May", "Jun", "Jul",
                                 "Ago", "Sep", "Oct", "Nov"), 
                      ordered = TRUE)

library(janitor)

muestra %>% 
 tabyl(iso) %>% 
  adorn_totals("row")
  
library(DescTools)
BinomCI(4,44,conf.level=0.95)*100

muestra %>% 
  filter(iso=="si") %>% 
  ggplot(aes(x=mes, y=iso, fill=tipoiso)) +
  geom_col() +
  labs(title = "Infecciones sitio operatorio", subtitle= "Reemplazos primarios de cadera y rodilla", caption= "fuente: datos de C.A.S.A",
       tag = "Figura 4") +
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(hjust = 0.5))

muestra %>%
  filter(iso=="si") %>% 
  tabyl(tipoiso) %>% 
  adorn_totals("row")
