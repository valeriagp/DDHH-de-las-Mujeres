
library(dplyr)
library(gt)
library(tidyverse)
library(forcats)

## importar los archivos

### Base 1: Registros de nacimientos 2023

datos2023<-read.csv("fuentes_de_info/Nacimientos_2023.csv")
catmpo2020<-read.csv("catalogos/MUNICIPIOS.csv")
muj_est_2023<-read.csv("fuentes_de_info/proyeccion2023mujeres.csv",encoding = "latin1")
muj_mun_2023<-read.csv("fuentes_de_info/proyecciones_municipales_2023_mujeres.csv",encoding = "latin1")
tff_12a14_2024<-read.csv("fuentes_de_info/2024_tff_12a14.csv",encoding = "latin1")
rff_10a14_2024<-read.csv("fuentes_de_info/2024_rff_10a14.csv",encoding = "latin1")
tfa_15a19_2024<-read.csv("fuentes_de_info/2024_tfa_15a19.csv",encoding = "latin1")
cat_ent<-read.csv("fuentes_de_info/cat_entidades_dgis.csv",encoding = "latin1")
muj_capitales<-read.csv("fuentes_de_info/proyecciones_muj_capitales_2023.csv",encoding = "latin1")
datos2020<-read.csv("fuentes_de_info/sinac_2020.csv",encoding = "latin1")
datos2021<-read.csv("fuentes_de_info/Nacimientos_2021.csv",encoding = "latin1")
datos2022<-read.csv("fuentes_de_info/Nacimientos_2022.csv",encoding = "latin1")

#Para este sript, se trabajará con las variables ENT_RES, MPO_RES, EDADM, 
#EDOCIVIL, COND_INDM (dicrepancia entre catálogo y base).
#ENTIDADRESIDENCIA, MUNICIPIORESIDENCIA, LOCALIDADRESIDENCIA, NUMEROEMBARAZOS, EDAD

#Crear base para Yucatán: Filtrar base por entidad de residencia de la madre, 
#Yucatán (nomenclatura 31)

nacimientos2023<-datos2023 %>%
  filter(ENTIDADRESIDENCIA==31) %>%
  select(ENTIDADRESIDENCIA,
         MUNICIPIORESIDENCIA,
         LOCALIDADRESIDENCIA,
         NUMEROEMBARAZOS,
         EDAD,SECONSIDERAINDIGENA) %>% 
  rename(CVE_MUN=MUNICIPIORESIDENCIA)%>%
  inner_join(tempocatmpo2020,by="CVE_MUN") %>% 
  inner_join(muj_mun_2023,by="CVE_MUN") %>% 
  select(-NOM_MUN.x,-CVE_ENT) %>% 
  rename(NOM_MUN=NOM_MUN.y) %>% 
  mutate(
    INCIDENCIA = ifelse(NOM_MUN=="Chemax"|
                               NOM_MUN=="Progreso"|
                               NOM_MUN=="Halachó"|
                               NOM_MUN=="Umán"|
                               NOM_MUN=="Kanasín","Municipios del proyecto","Otros municipios")
  )


tempocatmpo2020<- catmpo2020 %>%
  filter(CVE_ENT==31,CVE_MUN!=999) %>%
  select(-ESTATUS)


#Crear las tablas a nivel municipal:

tbl_emb_15<-nacimientos2023 %>% 
  filter(ENTIDADRESIDENCIA==31,EDAD>=15,EDAD<=19) %>%  
  group_by(Municipio=NOM_MUN) %>% 
  summarise(Nacimientos=n(),
            Tasa=round(n()/unique(POB_15_19)*1000,digits = 1))%>% 
  mutate(INCIDENCIA = ifelse(Municipio=="Chemax"|
                               Municipio=="Progreso"|
                               Municipio=="Halachó"|
                               Municipio=="Umán"|
                               Municipio=="Kanasín","Municipios del proyecto","Otros municipios")
    
  )


tbl_emb_15 %>% 
  filter(INCIDENCIA=="Municipios del proyecto") %>% 
  select(Municipio,Nacimientos, Tasa)

tbl_emb_15 %>% #Gráfica para nacimientos 15a19
  filter(INCIDENCIA=="Municipios del proyecto") %>% 
  select(Municipio,Nacimientos, Tasa) %>% 
  ggplot( aes(x=Municipio, y=Tasa)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  geom_text(aes(label = Tasa), color = "#464542", size = 3,fontface = "bold")+
  coord_flip() +
  xlab("") +
  ylab("")+
  theme(plot.margin = margin(t = 20,  # Margen superior
                             r = 50,  # Margen derecho
                             b = 30,  # Margen inferior
                             l = 10))+ # Margen inferior
  theme_bw(base_size = 12)



tbl_sub_15<-nacimientos2023 %>%  #embarazo subsecuente 15a19
  filter(EDAD>=15,EDAD<=19) %>%
  mutate(Subsecuente=case_when(NUMEROEMBARAZOS>=2~"Si",
                               TRUE~"No")) %>% 
  select(NOM_MUN,Subsecuente) %>% 
  group_by(Municipio=NOM_MUN) %>% 
  summarise(Subsecuentes=sum(Subsecuente == "Si"),
            Porcentaje=round(sum(Subsecuente == "Si")/n()*100, digits = 2)) %>% 
  mutate(INCIDENCIA = ifelse(Municipio=="Chemax"|
                               Municipio=="Progreso"|
                               Municipio=="Halachó"|
                               Municipio=="Umán"|
                               Municipio=="Kanasín","Municipios del proyecto","Otros municipios"))


tbl_sub_15 %>% 
  filter(INCIDENCIA=="Municipios del proyecto") %>% 
  select(Municipio,Subsecuentes,Porcentaje)

sum(tbl_sub_15$Subsecuentes)


tbl_sub_15 %>% #Gráfica embarazo subsecuente 15a19
  filter(INCIDENCIA=="Municipios del proyecto") %>% 
  select(Municipio,Subsecuentes,Porcentaje) %>% 
  arrange(-Porcentaje) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(NOM_ENT=factor(Municipio, levels=Municipio)) %>% 
  ggplot( aes(x=Municipio, y=Porcentaje)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  geom_text(aes(label = Porcentaje), color = "#464542", size = 3,fontface = "bold")+
  coord_flip() +
  xlab("") +
  ylab("")+
  theme(plot.margin = margin(t = 20,  # Margen superior
                             r = 50,  # Margen derecho
                             b = 30,  # Margen inferior
                             l = 10))+ # Margen inferior
  theme_bw(base_size = 12)



tbl_sub_10<-nacimientos2023 %>%  #embarazo subsecuente 10a14
  filter(EDAD>=10,EDAD<=14) %>%
  mutate(Subsecuente=case_when(NUMEROEMBARAZOS>=2~"Si",
                               TRUE~"No")) %>% 
  select(NOM_MUN,Subsecuente) %>% 
  group_by(Municipio=NOM_MUN) %>% 
  summarise(Subsecuentes=sum(Subsecuente == "Si"),
            Porcentaje=round(sum(Subsecuente == "Si")/n()*100, digits = 2)) %>% 
  mutate(INCIDENCIA = ifelse(Municipio=="Chemax"|
                               Municipio=="Progreso"|
                               Municipio=="Halachó"|
                               Municipio=="Umán"|
                               Municipio=="Kanasín","Municipios del proyecto","Otros municipios"))

tbl_sub_10 %>% 
  filter(INCIDENCIA=="Municipios del proyecto") %>% 
  select(Municipio,Subsecuentes, Porcentaje) %>% 
  gt()

tbl_sub_10A19<-nacimientos2023 %>%  #embarazo subsecuente 10a14
  filter(EDAD>=10,EDAD<=19) %>%
  mutate(Subsecuente=case_when(NUMEROEMBARAZOS>=2~"Si",
                               TRUE~"No")) %>% 
  select(NOM_MUN,Subsecuente) %>% 
  group_by(Municipio=NOM_MUN) %>% 
  summarise(Subsecuentes=sum(Subsecuente == "Si"),
            Porcentaje=round(sum(Subsecuente == "Si")/n()*100, digits = 2))

#Crear las tablas a nivel Estatal:


datos2023 %>% 
  filter(EDAD>=10,EDAD<=19) %>%
  summarise(n())


tbl_emb_nac_15<-datos2023 %>% 
  select(ENTIDADRESIDENCIA,
         NUMEROEMBARAZOS,
         EDAD,SECONSIDERAINDIGENA) %>% 
  rename(CVE_ENT=ENTIDADRESIDENCIA) %>% 
  inner_join(muj_est_2023,by="CVE_ENT") %>% 
  filter(EDAD>=15,EDAD<=19) %>% #adolescentes de 15 a 19
  group_by(NOM_ENT) %>% 
  summarise(Nacimientos=n(),
            "Tasa de nacimientos"=n()/unique(pob_15a19)*1000)


tbl_emb_nac_10<-datos2023 %>% 
  select(ENTIDADRESIDENCIA,
         NUMEROEMBARAZOS,
         EDAD,SECONSIDERAINDIGENA) %>% 
  rename(CVE_ENT=ENTIDADRESIDENCIA) %>% 
  inner_join(muj_est_2023,by="CVE_ENT") %>% 
  filter(EDAD>=10,EDAD<=14) %>% #adolescentes de 10 a 14
  group_by(NOM_ENT) %>% 
  summarise(Nacimientos=n(),
            "Tasa de nacimientos"=n()/unique(pob_15a19)*1000)


##Gráficas

tff_12a14_2024 %>%
  arrange(-tff_12a14) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(NOM_ENT=factor(NOM_ENT, levels=NOM_ENT)) %>%  
  ggplot( aes(x=NOM_ENT, y=tff_12a14)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  geom_text(aes(label = tff_12a14), color = "#464542", size = 3,fontface = "bold")+
  coord_flip() +
  xlab("") +
  ylab("")+
  theme(plot.margin = margin(t = 20,  # Margen superior
                             r = 50,  # Margen derecho
                             b = 30,  # Margen inferior
                             l = 10))+ # Margen inferior
  theme_bw(base_size = 18)




rff_10a14_2024 %>%
  arrange(-rff_10a14) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(NOM_ENT=factor(NOM_ENT, levels=NOM_ENT)) %>%  
  ggplot( aes(x=NOM_ENT, y=rff_10a14)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  geom_text(aes(label = rff_10a14), color = "#464542", size = 3,fontface = "bold")+
  coord_flip() +
  xlab("") +
  ylab("")+
  theme(plot.margin = margin(t = 20,  # Margen superior
                             r = 50,  # Margen derecho
                             b = 30,  # Margen inferior
                             l = 10))+ # Margen inferior
  theme_bw(base_size = 18)


tfa_15a19_2024 %>%
  arrange(-X2024_tfa_15a19) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(NOM_ENT=factor(NOM_ENT, levels=NOM_ENT)) %>%  
  ggplot( aes(x=NOM_ENT, y=X2024_tfa_15a19)) +
  geom_bar(stat="identity", fill="#41C2B9", alpha=.6, width=.4) +
  geom_text(aes(label = X2024_tfa_15a19), color = "#464542", size = 3,fontface = "bold")+
  coord_flip() +
  xlab("") +
  ylab("")+
  theme(plot.margin = margin(t = 20,  # Margen superior
                             r = 50,  # Margen derecho
                             b = 30,  # Margen inferior
                             l = 10))+ # Margen inferior
  theme_bw(base_size = 18)



#-------------- Para elaborar table de capitales de entidades federativas----

catmpo2020$CVE_MUN<-as.character(catmpo2020$CVE_MUN)

tempo<-datos2023 %>%
  select(ENTIDADRESIDENCIA,
         MUNICIPIORESIDENCIA,
         LOCALIDADRESIDENCIA,
         NUMEROEMBARAZOS,
         EDAD,SECONSIDERAINDIGENA) %>% 
  mutate(CVE_MUN=paste0(ENTIDADRESIDENCIA,MUNICIPIORESIDENCIA),
         CVE_LOC=paste0(ENTIDADRESIDENCIA,MUNICIPIORESIDENCIA,LOCALIDADRESIDENCIA)) %>% 
  inner_join(catmpo2020,by="CVE_MUN")


      
      
emb_capitales<- tempo %>% 
  inner_join(muj_capitales,by="NOM_MUN") 


tbl_emb_capitales<-emb_capitales %>%
  filter(EDAD>=10,EDAD<=19) %>% 
  group_by(NOM_MUN_2) %>% 
  summarise(Nacimientos=n(),
            Tasa=round(n()/unique(MUJ_10_19)*1000,digits = 1)) %>% 
  arrange(Tasa)
  

write.csv(tbl_emb_capitales,"tbl_emb_capitales.csv",fileEncoding="latin1")


datos2020 %>% 
  filter(EDAD>=10,EDAD<=19,ENTIDADRESIDENCIA==31,MUNICIPIORESIDENCIA==50) %>% 
  summarise(Nacimientos=n())


datos2021 %>% 
  filter(EDAD>=10,EDAD<=19,ENTIDADRESIDENCIA==31,MUNICIPIORESIDENCIA==50) %>% 
  summarise(Nacimientos=n())

datos2022 %>% 
  filter(EDAD>=10,EDAD<=19,ENTIDADRESIDENCIA==31,MUNICIPIORESIDENCIA==50) %>% 
  summarise(Nacimientos=n())

datos2023 %>% 
  filter(EDAD>=10,EDAD<=19,ENTIDADRESIDENCIA==31,MUNICIPIORESIDENCIA==50) %>% 
  summarise(Nacimientos=n())


datos2020 %>% 
  filter(EDAD>=10,EDAD<=19) %>% 
  summarise(Nacimientos=n())
datos2021 %>% 
  filter(EDAD>=10,EDAD<=19) %>% 
  summarise(Nacimientos=n())
datos2022 %>% 
  filter(EDAD>=10,EDAD<=19) %>% 
  summarise(Nacimientos=n())
datos2023 %>% 
  filter(EDAD>=10,EDAD<=19) %>% 
  summarise(Nacimientos=n())







#%>% 
  mutate(
    CAPITALES=ifelse(NOM_MUN_2=="Tlaxcala"|
                       NOM_MUN_2=="Coyoacán"|
                       NOM_MUN_2=="Tuxtla Gutiérrez"|
                       NOM_MUN_2=="Mérida"|
                       NOM_MUN_2=="La Paz"|
                       NOM_MUN_2=="Guadalajara"|
                       NOM_MUN_2=="Saltillo"|
                       NOM_MUN_2=="Culiacán"|
                       NOM_MUN_2=="Villahermosa"|
                       NOM_MUN_2=="Oaxaca de Juárez"|
                       NOM_MUN_2=="Zacatecas"|
                       NOM_MUN_2=="Chihuahua"|
                       NOM_MUN_2=="Monterrey"|
                       NOM_MUN_2=="Xalapa"|
                       NOM_MUN_2=="Querétaro"|
                       NOM_MUN_2=="Mexicali"|
                       NOM_MUN_2=="Campeche"|
                       NOM_MUN_2=="Victoria"|
                       NOM_MUN_2=="Othón P. Blanco"|
                       NOM_MUN_2=="Chilpancingo de los Bravo"|
                       NOM_MUN_2=="Aguascalientes"|
                       NOM_MUN_2=="Colima"|
                       NOM_MUN_2=="Cuernavaca"|
                       NOM_MUN_2=="Durango"|
                       NOM_MUN_2=="Puebla"|
                       NOM_MUN_2=="La Paz"|
                       NOM_MUN_2=="Morelia"|
                       NOM_MUN_2=="Pachuca de Soto"|
                       NOM_MUN_2=="Saltillo"|
                       NOM_MUN_2=="San Luis Potosí"|
                       NOM_MUN_2=="Tepic"|
                       NOM_MUN_2=="Zacatecas","Capital","Otro"))
    