
incidencia_yuc<-read.csv("fuentes_de_info/IDM_NM_dic24.csv",encoding = "latin1") %>% 
  filter(Entidad=="Yucatán")


incidencia_yuc %>% 
  mutate(Inseguridad=ifelse(Subtipo.de.delito=="Homicidio doloso"|
                              ifelse(Subtipo.de.delito=="Lesiones dolosas"|
                                       ifelse(Subtipo.de.delito=="Feminicidio"|
                                                ifelse(Subtipo.de.delito=="Violación simple"|
                                                         ifelse(Subtipo.de.delito=="Violencia familiar"|
                                                                  ifelse(Subtipo.de.delito=="Abuso sexual"|
                                                                           ifelse(Subtipo.de.delito=="Violación equiparada",1,0)))))))) %>%
  group_by(Municipio)


incidencia_yuc$Total_delitos = rowSums (incidencia_yuc[ , 10:21])

incidencia_mun<-incidencia_yuc %>% 
  filter(Año==2024) %>% 
  group_by(Municipio) %>% 
  summarise(sum(Total_delitos))

write.csv(incidencia_mun,"incidencia_mun.csv", fileEncoding = "latin1" )  

##

incidencia$Total_delitos = rowSums (incidencia[ , 10:21])

corrupcion_menores<-incidencia %>% 
  filter(Año==2024,Tipo.de.delito=="Corrución de menores") %>% 
  group_by(Entidad) %>% 
  summarise(sum(Total_delitos))

trata<-incidencia %>% 
  filter(Año==2024,Tipo.de.delito=="Trata de personas") %>% 
  group_by(Entidad) %>% 
  summarise(sum(Total_delitos))
    
    
  