library(dplyr)
library(tidyverse)
library(forcats)

#Carga de las bases
victimas<-read.csv("fuentes_de_info/victimas.csv",encoding = "latin1",dec=".")

victimas<-victimas %>% #este mutate es para generar el input en la tabla dinámica de mi aplicación Shiny
  mutate(
    INCIDENCIA = ifelse(NOM_MUN=="Chemax"|
                          NOM_MUN=="Progreso"|
                          NOM_MUN=="Halachó"|
                          NOM_MUN=="Umán"|
                          NOM_MUN=="Kanasín","Municipios del proyecto","Otros municipios"))

muj_mun_2023<-muj_mun_2023%>% #este mutate es para generar el input en la tabla dinámica de mi aplicación Shiny
  mutate(
    INCIDENCIA = ifelse(NOM_MUN=="Chemax"|
                          NOM_MUN=="Progreso"|
                          NOM_MUN=="Halachó"|
                          NOM_MUN=="Umán"|
                          NOM_MUN=="Kanasín","Municipios del proyecto","Otros municipios"))

#Tabla para violencia en el ámbito familiar para niñas y adolescentes entre 10 y 19 años

tbl_violencia_fam<-victimas %>% 
  filter(ANO_HECHO==2023,Edad>=10,Edad<=19,estadohecho=="Yucatán"|
           estadohecho=="",modalidad=="Familiar") %>% 
  distinct(fk_euv, .keep_all = TRUE) %>% 
  right_join(muj_mun_2023, by="NOM_MUN") %>% 
  replace(is.na(.), 0) %>%
  mutate(POB_10A19=POB_10_14+POB_15_19) %>% 
  group_by(Municipio=NOM_MUN) %>% 
  summarise(Víctimas=n(),
            Tasa=round(n()/unique(POB_10A19)*1000,digits = 1))


tbl_violencia_fam %>% 
  mutate(INCIDENCIA = ifelse(Municipio=="Chemax"|
                                    Municipio=="Progreso"|
                                    Municipio=="Halachó"|
                                    Municipio=="Umán"|
                                    Municipio=="Kanasín",
                                          "Municipios del proyecto",
                                          "Otros municipios")) %>% 
  filter(INCIDENCIA=="Municipios del proyecto") %>% 
  select(Municipio,Víctimas,Tasa) %>% 
  gt()

#Tabla para violencia sexual hacia niñas y adolescentes entre 10 y 19 años

tbl_violencia_sexual<-victimas %>% 
  filter(ANO_HECHO==2023,Edad>=10,Edad<=19,estadohecho=="Yucatán"|
           estadohecho=="",str_detect(tipos,"Sexual")|
           str_detect(descripcion_otro_tipos,"sex")|
           str_detect(descripcion_otro_tipos,"SEX")|
           str_detect(descripcion_otro_tipos,"Sex")) %>% 
  distinct(fk_euv, .keep_all = TRUE) %>%  
  right_join(muj_mun_2023, by="NOM_MUN") %>% 
  replace(is.na(.), 0) %>%
  mutate(POB_10A19=POB_10_14+POB_15_19) %>% 
  group_by(Municipio=NOM_MUN) %>% 
  summarise(Víctimas=n(),
            Tasa=round( n()/unique(POB_10A19)*1000,digits = 1)) 

write.csv(tbl_violencia_sexual,"Violencia sexual adolescentes.csv", fileEncoding = "latin1")

tbl_violencia_sexual%>% 
  mutate(INCIDENCIA = ifelse(Municipio=="Chemax"|
                               Municipio=="Progreso"|
                               Municipio=="Halachó"|
                               Municipio=="Umán"|
                               Municipio=="Kanasín",
                             "Municipios del proyecto",
                             "Otros municipios")) %>% 
  filter(INCIDENCIA=="Municipios del proyecto") %>% 
  select(Municipio,Víctimas,Tasa) %>% 
  gt()

 #Gráficas---
 

tbl_violencia_fam %>% 
  mutate(INCIDENCIA = ifelse(Municipio=="Chemax"|
                               Municipio=="Progreso"|
                               Municipio=="Halachó"|
                               Municipio=="Umán"|
                               Municipio=="Kanasín",
                          "Municipios del proyecto",
                          "Otros municipios")) %>% 
  filter(INCIDENCIA=="Municipios del proyecto") %>% 
  arrange(-Tasa) %>% 
  mutate(Municipio=factor(Municipio, levels=Municipio)) %>%
  ggplot()+
  aes(x=Municipio,y=Tasa)+
  geom_segment( aes(xend=Municipio, yend=0), lwd = 1) +
  geom_point( size=7, color="pink") +
  geom_text(aes(label = Tasa), color = "black", size = 3,fontface = "bold") +
  coord_flip() +
  theme_minimal(base_size = 11)+
  xlab("") +
  ylab("Tasa")+
  theme(plot.margin = margin(t = 20,  # Margen superior
                             r = 50,  # Margen derecho
                             b = 30,  # Margen inferior
                             l = 10)) # Margen izquierdo



tbl_violencia_sexual %>% #gráfica para violencia sexual en municipios del proyecto
  mutate(INCIDENCIA = ifelse(Municipio=="Chemax"|
                               Municipio=="Progreso"|
                               Municipio=="Halachó"|
                               Municipio=="Umán"|
                               Municipio=="Kanasín",
                             "Municipios del proyecto",
                             "Otros municipios")) %>% 
  filter(INCIDENCIA=="Municipios del proyecto") %>% 
  arrange(-Tasa) %>% 
  mutate(Municipio=factor(Municipio, levels=Municipio)) %>%
  ggplot()+
  aes(x=Municipio,y=Tasa)+
  geom_segment( aes(xend=Municipio, yend=0), lwd = 1) +
  geom_point( size=7, color="#E2BC24") +
  geom_text(aes(label = Tasa), color = "black", size = 3,fontface = "bold") +
  coord_flip() +
  theme_minimal(base_size = 11)+
  xlab("") +
  ylab("Tasa")+
  theme(plot.margin = margin(t = 20,  # Margen superior
                             r = 50,  # Margen derecho
                             b = 30,  # Margen inferior
                             l = 10)) # Margen izquierdo

tbl_violencia_sexual %>% #gráfica para violencia sexual en municipios arriba de la media estatal
  filter(Tasa>=4,Tasa<=14) %>% 
  arrange(Tasa) %>% 
  mutate(Municipio=factor(Municipio, levels=Municipio)) %>%
  ggplot()+
  aes(x=Municipio,y=Tasa)+
  geom_segment( aes(xend=Municipio, yend=0), lwd = 1) +
  geom_point( size=7, color="#F2044B") +
  geom_text(aes(label = Tasa), color = "white", size = 3,fontface = "bold") +
  coord_flip() +
  theme_minimal(base_size = 9)+
  xlab("") +
  ylab("Tasa")+
  theme(plot.margin = margin(t = 10,  # Margen superior
                             r = 50,  # Margen derecho
                             b = 10,  # Margen inferior
                             l = 10)) # Margen izquierdo

tbl_violencia_sexual %>% #gráfica para violencia sexual en municipios arriba de la media estatal
  filter(Tasa<=2.4) %>% 
  arrange(Tasa) %>% 
  mutate(Municipio=factor(Municipio, levels=Municipio)) %>%
  ggplot()+
  aes(x=Municipio,y=Tasa)+
  geom_segment( aes(xend=Municipio, yend=0), lwd = 1) +
  geom_point( size=7, color="#93F6BB") +
  geom_text(aes(label = Tasa), color = "white", size = 3,fontface = "bold") +
  coord_flip() +
  theme_minimal(base_size = 9)+
  xlab("") +
  ylab("Tasa")+
  theme(plot.margin = margin(t = 10,  # Margen superior
                             r = 50,  # Margen derecho
                             b = 10,  # Margen inferior
                             l = 10)) # Margen izquierdo

tbl_violencia_sexual %>% #gráfica para violencia sexual en municipios abajo de la media estatal
  filter(Tasa>=2.5,Tasa<=3.9) %>% 
  arrange(Tasa) %>% 
  mutate(Municipio=factor(Municipio, levels=Municipio)) %>%
  ggplot()+
  aes(x=Municipio,y=Tasa)+
  geom_segment( aes(xend=Municipio, yend=0), lwd = 1) +
  geom_point( size=7, color="#F2044B") +
  geom_text(aes(label = Tasa), color = "white", size = 3,fontface = "bold") +
  coord_flip() +
  theme_minimal(base_size = 9)+
  xlab("") +
  ylab("Tasa")+
  theme(plot.margin = margin(t = 10,  # Margen superior
                             r = 50,  # Margen derecho
                             b = 10,  # Margen inferior
                             l = 10)) # Margen izquierdo

##--2024---------------------------

tbl_violencia_sexual2024<-victimas %>% 
  filter(ANO_HECHO==2024,Edad>=10,Edad<=19,estadohecho=="Yucatán"|
           estadohecho=="",str_detect(tipos,"Sexual")|
           str_detect(descripcion_otro_tipos,"sex")|
           str_detect(descripcion_otro_tipos,"SEX")|
           str_detect(descripcion_otro_tipos,"Sex")) %>% 
  distinct(fk_euv, .keep_all = TRUE) %>%  
  right_join(muj_mun_2023, by="NOM_MUN") %>% 
  replace(is.na(.), 0) %>%
  mutate(POB_10A19=POB_10_14+POB_15_19) %>% 
  group_by(Municipio=NOM_MUN) %>% 
  summarise(Víctimas=n()) 

write.csv(tbl_violencia_sexual2024,"Violencia sexual adolescentes 2024.csv", fileEncoding = "latin1")



victimas %>% 
  filter(ANO_HECHO==2024,Edad>=10,Edad<=19,estadohecho=="Yucatán"|
           estadohecho=="",str_detect(tipos,"Sexual")|
           str_detect(descripcion_otro_tipos,"sex")|
           str_detect(descripcion_otro_tipos,"SEX")|
           str_detect(descripcion_otro_tipos,"Sex")) %>% 
  distinct(fk_euv, .keep_all = TRUE) %>%  
  right_join(muj_mun_2023, by="NOM_MUN") %>% 
  replace(is.na(.), 0) %>%
  mutate(POB_10A19=POB_10_14+POB_15_19) %>%  
  summarise(Víctimas=n()) 

#----- análisis violencia mujeres en Mérida

victimas %>% 
  filter(ANO_HECHO==2024,NOM_MUN=="Mérida") %>% 
  #distinct(fk_euv, .keep_all = TRUE) %>%
  replace(is.na(.), 0) %>%
  summarise(Víctimas=n()) 


victimas %>%
  filter(ANO_HECHO==2021,estadohecho=="Yucatán"|
           estadohecho=="",str_detect(tipos,"Sexual")|
           str_detect(descripcion_otro_tipos,"sex")|
           str_detect(descripcion_otro_tipos,"SEX")|
           str_detect(descripcion_otro_tipos,"Sex"),
         NOM_MUN=="Mérida") %>% 
  #distinct(fk_euv, .keep_all = TRUE) %>%  
  replace(is.na(.), 0) %>%
  summarise(Víctimas=n()) 


##Tipos de violencia Mérida---

z<-victimas %>% 
  
  mutate(descripcion_otro_tipos=case_when(
    str_detect(descripcion_otro_tipos, "FEM|FAM|HOMICIDIO|MUERTE|VITAL|TRATA|ARMA|DROGAS|CRIMEN|FEMINICIDA")~ "FEMINICIDA",
    str_detect(descripcion_otro_tipos, "VERBAL|NEGLIGENCIA|ABANDONO|AMENAZA|AMANEZ|AME|PSIC|OMIS|EMOC|DISCRIMINACIÓN|PSICOLOGICA")~"PSICOLOGICA",
    str_detect(descripcion_otro_tipos, "DIG|CIBER|ESPECIF|NO|OTRO|DESCONOCE|DDSCONOCE|X|ESCOLAR|REDES SOCIALES|ASESOR|ADICCIONES|CUSTODIA|NINGUNA|COMUNITAR")~"NA",
    str_detect(descripcion_otro_tipos, "IMAG|EST|ESTETICA")~"ESTETICA",
    str_detect(descripcion_otro_tipos, "OBS|OBSTETRICA")~"OBSTETRICA",
    str_detect(descripcion_otro_tipos, "ABORTO|ACOSO|SEX|ABUSO|EXHIBE FOTOGRAFIAS INTIMAS|INSINUACIONES|HOSTI|ACOS|SEXUAL")~"SEXUAL",
    str_detect(descripcion_otro_tipos, "GOLPES|FIS|FISICA")~"FISICA",
    str_detect(descripcion_otro_tipos, "PENSION|INCUMPLIMIENTO|DIVORCIO|ECONOMICA")~"ECONOMICA",
    str_detect(descripcion_otro_tipos, "VICARIA|VICAR?A")~"VICARIA",
    TRUE ~ "NA"
  )) %>% 
  mutate(tipo_feminicida= case_when(tipos=="Feminicida"|
                                      descripcion_otro_tipos=="FEMINICIDA"~ 1,
                                    TRUE~ 0),
         tipo_economica= case_when(tipos=="Económica"|
                                     descripcion_otro_tipos=="ECONOMICA"~ 1,
                                   TRUE~ 0),
         tipo_fisica= case_when(tipos=="F?sica"|
                                  descripcion_otro_tipos=="FISICA" ~ 1,
                                TRUE~ 0),
         tipo_patrim= case_when(tipos=="Patrimonial"|
                                  descripcion_otro_tipos=="PATRIMONIAL" ~ 1,
                                TRUE~ 0),
         tipo_psico= case_when(tipos=="Psicológica"|
                                 descripcion_otro_tipos=="PSICOLOGICA" ~ 1,
                               TRUE~ 0),
         tipo_sexual= case_when(tipos=="Sexual"|
                                  descripcion_otro_tipos=="SEXUAL" ~ 1,
                                TRUE~ 0),
         tipo_obstetrica= case_when(tipos=="Obstétrica"|
                                      descripcion_otro_tipos=="OBSTETRICA" ~ 1,
                                    TRUE~ 0),
         tipo_estetica= case_when(tipos=="Estética"|
                                    descripcion_otro_tipos=="ESTETICA" ~ 1,
                                  TRUE~ 0),
         tipo_vicaria= case_when(tipos=="Vicaria"|
                                   descripcion_otro_tipos=="VICARIA" ~ 1,
                                 TRUE~ 0),
         tipo_politica= case_when(tipos=="Política"|
                                    descripcion_otro_tipos=="POLITICA" ~ 1,
                                  TRUE~ 0),
         
         tipo_economica= case_when(str_detect(tipos,"Económica")  ~ 1,
                                   TRUE~ 0),
         tipo_fisica= case_when(str_detect(tipos,"Física")  ~ 1,
                                TRUE~ 0),
         tipo_psico= case_when(str_detect(tipos,"Psicológica")  ~ 1,
                               TRUE~ 0),
         tipo_patrim= case_when(str_detect(tipos,"Patrimonial")  ~ 1,
                                TRUE~ 0),
         tipo_sexual= case_when(str_detect(tipos,"Sexual")  ~ 1,
                                TRUE~ 0),
         tipo_obstetrica= case_when(str_detect(tipos,"Obstétrica")  ~ 1,
                                    TRUE~ 0)
  )



Casos_violencia_MID<-z %>% 
  filter(ANO_HECHO>=2021&ANO_HECHO<=2024,NOM_MUN=="Mérida") %>%
  group_by(NOM_MUN) %>% 
  summarise(tipo_economica=sum(tipo_economica),
            tipo_fisica=sum(tipo_fisica),
            tipo_psico=sum(tipo_psico),
            tipo_patrim=sum(tipo_patrim),
            tipo_sexual=sum(tipo_sexual),
            tipo_obstetrica=sum(tipo_obstetrica),
            tipo_estetica=sum(tipo_estetica),
            tipo_vicaria=sum(tipo_vicaria),
            tipo_feminicida=sum(tipo_feminicida),
            tipo_politica=sum(tipo_politica)
  ) %>% 
  
  ungroup()

write.csv(Casos_violencia_MID,"Casos_violencia_MID.csv", fileEncoding = "latin1")
