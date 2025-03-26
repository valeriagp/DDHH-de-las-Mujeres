

library(dplyr)
library(stringr)

#Carga de las bases

planificacion2023<-read.csv("fuentes_de_info/DA_PF_SIS_2023.csv",encoding = "latin1")
metodos2023<-read.csv("fuentes_de_info/DA_MPF_SIS_2023.csv",encoding = "latin1")

planificacion2023<-planificacion2023 %>% #para crear el input en el shiny app
  mutate(
    INCIDENCIA = ifelse(MUNICIPIO=="Chemax"|
                          MUNICIPIO=="Progreso"|
                          MUNICIPIO=="Halachó"|
                          MUNICIPIO=="Umán"|
                          MUNICIPIO=="Kanasín","Municipios del proyecto","Otros municipios"))

planificacion2023$MUNICIPIO<-stringr::str_to_title(planificacion2023$MUNICIPIO)

#Tablas para servicios a usuarios de planificación familiar: se usa la base planificacion 2023

planificacion2023 %>% 
  filter(ENTIDAD=="YUCATAN") %>% 
  mutate(Menores=PFU01+ #USUARIOS ACTIVOS < 20 AÑOS ORAL
           PFU02+ #USUARIOS ACTIVOS < 20 AÑOS INYECTABLE MENSUAL
           PFU03+ #USUARIOS ACTIVOS < 20 AÑOS INYECTABLE BIMESTRAL
           PFU04+ #USUARIOS ACTIVOS < 20 AÑOS IMPLANTE SUBDÉRMICO
           PFU05+ #USUARIOS ACTIVOS < 20 AÑOS DIU
           PFU06+ #USUARIOS ACTIVOS < 20 AÑOS QUIRÚRGICO
           PFU07+ #USUARIOS ACTIVOS < 20 AÑOS PRESERVATIVO
           PFU08+ #USUARIOS ACTIVOS < 20 AÑOS OTRO MÉTODO
           PFU17+ #USUARIOS ACTIVOS < 20 AÑOS DIU MEDICADO
           PFU18+ #USUARIOS ACTIVOS < 20 AÑOS PARCHE DÉRMICO
           PFU21,
         Total=PFU01+
         PFU02+
         PFU03+
         PFU04+
         PFU05+
         PFU06+
         PFU07+
         PFU08+
         PFU09+
         PFU10+
         PFU11+
         PFU12+
         PFU13+
         PFU14+
         PFU15+
         PFU16+
         PFU17+
         PFU18+
         PFU19+
         PFU20+
         PFU21+
         PFU22) %>% 
  group_by(ENTIDAD) %>% 
  summarise("Servicios totales"=sum(Menores),"Porcentaje"=sum(Menores)/sum(Total)*100)


usuarios_anticonceptivos2023<-planificacion2023 %>% 
  filter(ENTIDAD=="YUCATAN") %>% 
  mutate(Menores=PFU01+ #USUARIOS ACTIVOS < 20 AÑOS ORAL
           PFU02+ #USUARIOS ACTIVOS < 20 AÑOS INYECTABLE MENSUAL
           PFU03+ #USUARIOS ACTIVOS < 20 AÑOS INYECTABLE BIMESTRAL
           PFU04+ #USUARIOS ACTIVOS < 20 AÑOS IMPLANTE SUBDÉRMICO
           PFU05+ #USUARIOS ACTIVOS < 20 AÑOS DIU
           PFU06+ #USUARIOS ACTIVOS < 20 AÑOS QUIRÚRGICO
           PFU07+ #USUARIOS ACTIVOS < 20 AÑOS PRESERVATIVO
           PFU08+ #USUARIOS ACTIVOS < 20 AÑOS OTRO MÉTODO
           PFU17+ #USUARIOS ACTIVOS < 20 AÑOS DIU MEDICADO
           PFU18+ #USUARIOS ACTIVOS < 20 AÑOS PARCHE DÉRMICO
           PFU21, #USUARIOS ACTIVOS < 20 AÑOS PRESERVATIVO FEMENINO
         Total=PFU01+
           PFU02+
           PFU03+
           PFU04+
           PFU05+
           PFU06+
           PFU07+
           PFU08+
           PFU09+
           PFU10+
           PFU11+
           PFU12+
           PFU13+
           PFU14+
           PFU15+
           PFU16+
           PFU17+
           PFU18+
           PFU19+
           PFU20+
           PFU21+
           PFU22) %>% 
  group_by(Municipio=MUNICIPIO) %>% 
  summarise("Servicios totales"=sum(Menores),
            "Total"=sum(Total),
            "Porcentaje"=round(sum(Menores)/sum(Total)*100,digits = 2)) %>% 
  mutate(INCIDENCIA = ifelse(Municipio=="Chemax"|
                               Municipio=="Progreso"|
                               Municipio=="Halachó"|
                               Municipio=="Umán"|
                               Municipio=="Kanasín",
                                  "Municipios del proyecto",
                                  "Otros municipios"))

planificacion2023 %>% 
  filter(ENTIDAD=="YUCATAN") %>% 
  mutate(Menores=PFU01+ #USUARIOS ACTIVOS < 20 AÑOS ORAL
           PFU02+ #USUARIOS ACTIVOS < 20 AÑOS INYECTABLE MENSUAL
           PFU03+ #USUARIOS ACTIVOS < 20 AÑOS INYECTABLE BIMESTRAL
           PFU04+ #USUARIOS ACTIVOS < 20 AÑOS IMPLANTE SUBDÉRMICO
           PFU05+ #USUARIOS ACTIVOS < 20 AÑOS DIU
           PFU06+ #USUARIOS ACTIVOS < 20 AÑOS QUIRÚRGICO
           PFU07+ #USUARIOS ACTIVOS < 20 AÑOS PRESERVATIVO
           PFU08+ #USUARIOS ACTIVOS < 20 AÑOS OTRO MÉTODO
           PFU17+ #USUARIOS ACTIVOS < 20 AÑOS DIU MEDICADO
           PFU18+ #USUARIOS ACTIVOS < 20 AÑOS PARCHE DÉRMICO
           PFU21, #USUARIOS ACTIVOS < 20 AÑOS PRESERVATIVO FEMENINO
         Total=PFU01+
           PFU02+
           PFU03+
           PFU04+
           PFU05+
           PFU06+
           PFU07+
           PFU08+
           PFU09+
           PFU10+
           PFU11+
           PFU12+
           PFU13+
           PFU14+
           PFU15+
           PFU16+
           PFU17+
           PFU18+
           PFU19+
           PFU20+
           PFU21+
           PFU22) %>% 
  summarise("Servicios totales"=sum(Menores),
            Total=sum(Total))





usuarios_anticonceptivos2023 %>% 
  filter(INCIDENCIA=="Municipios del proyecto") %>%
  select(Municipio,"Servicios totales",Porcentaje) %>% 
  gt()

usuarios_anticonceptivos2023 %>% 
  filter(INCIDENCIA=="Municipios del proyecto") %>%
  select(Municipio,"Servicios totales",Porcentaje) %>%
  arrange(-Porcentaje) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(NOM_ENT=factor(Municipio, levels=Municipio)) %>%  
  ggplot( aes(x=Municipio, y=Porcentaje)) +
  geom_bar(stat="identity", fill="#8673E9", alpha=.6, width=.4) +
  geom_text(aes(label = Porcentaje), color = "#464542", size = 3,fontface = "bold")+
  coord_flip() +
  xlab("") +
  ylab("")+
  theme(plot.margin = margin(t = 20,  # Margen superior
                             r = 50,  # Margen derecho
                             b = 30,  # Margen inferior
                             l = 10))+ # Margen inferior
  theme_bw(base_size = 10)


#Tablas para Métodos entregados de planificación familiar: se usa la base metodos 2023


metodos2023 %>% 
  filter(ENTIDAD=="YUCATAN") %>% 
  mutate(Menores=PFM12+
         PFM13+
         PFM14+
         PFM15+
         PFM16+
         PFM17+
         PFM18+
         PFM19+
         PFM20+
         PFM21+
         PFM32) %>% 
  group_by(ENTIDAD) %>% 
  summarise(sum(Menores))

anticonceptivos_entregados2023<-metodos2023 %>% 
  filter(ENTIDAD=="YUCATAN") %>% 
  mutate(Menores=PFM12+ #ORAL < 20 AÑOS
           PFM13+ #INYECTABLE MENSUAL < 20 AÑOS
           PFM14+ #INYECTABLE BIMESTRAL < 20 AÑOS
           PFM15+ #IMPLANTE SUBDÉRMICO < 20 AÑOS
           PFM16+ #PARCHE DÉRMICO < 20 AÑOS
           PFM17+ #DIU < 20 AÑOS
           PFM18+ #DIU MEDICADO < 20 AÑOS
           PFM19+ #PRESERVATIVO FEMENINO < 20 AÑOS
           PFM20+ #PRESERVATIVO < 20 AÑOS
           PFM21+ #ANTICONCEPCIÓN DE EMERGENCIA < 20 AÑOS
           PFM32, #INYECTABLE TRIMESTRAL < 20 AÑOS
         Total=PFM12+
         PFM13+
         PFM14+
         PFM15+
         PFM16+
         PFM17+
         PFM18+
         PFM19+
         PFM20+
         PFM21+
         PFM22+
         PFM23+
         PFM24+
         PFM25+
         PFM26+
         PFM27+
         PFM28+
         PFM29+
         PFM30+
         PFM31+
         PFM32+
         PFM33) %>% 
  group_by(MUNICIPIO) %>% 
  summarise("Servicios totales"=sum(Menores),"Porcentaje"=sum(Menores)/sum(Total)*100)

