library(dplyr)
library(gt)

recursos<-read.csv("Salud_PED_Recursos/recursos_salud_sectorial.csv",encoding = "latin1") #%>% 
  #select(c(Nombre.Estado,Tipo.de.Establecimiento,Mastógrafos..analógico.y.digital.
  
#))

glimpse(recursos)

recursos[is.na(recursos)] <- 0

mastografos<-recursos %>% 
  group_by(Nombre.Estado) %>% 
  summarise(sum(Mastógrafos..analógico.y.digital.))

masto_tipo<-recursos %>% 
  filter(Nombre.Estado=="YUCATAN") %>% 
  mutate(tipo=ifelse(Institución=="IMSS"|
                       Institución=="ISSSTE"|
                       Institución=="SEMAR"|
                       Institución=="PEMEX"|
                       Institución=="SEDENA",
                     "Instituciones de salud y seguridad social",
                     ifelse(Institución=="SSA"|
                              Institución=="IMSS Bienestar"|
                              Institución=="UNIVERSITARIO"|
                              Institución=="ESTATAL"|
                              Institución=="MUNICIPAL",
                            "Servicios de salud para población sin seguridad social","NINGUNA"
                       
                     ))) %>% 
  filter(tipo!="NINGUNA") %>% 
  group_by("Tipo de institución de salud"=tipo) %>% 
  summarise("Mastógrafos"=sum(Mastógrafos..analógico.y.digital.),
            Porcentaje = round(sum(Mastógrafos..analógico.y.digital.)/21*100, digits = 2))

masto_tipo %>% 
  gt()


total_oncologos<-recursos %>% 
  filter(Nombre.Estado=="YUCATAN") %>% 
  mutate(tipo=ifelse(Institución=="IMSS"|
                       Institución=="ISSSTE"|
                       Institución=="SEMAR"|
                       Institución=="PEMEX"|
                       Institución=="SEDENA",
                     "Instituciones de salud y seguridad social",
                     ifelse(Institución=="SSA"|
                              Institución=="IMSS Bienestar"|
                              Institución=="UNIVERSITARIO"|
                              Institución=="ESTATAL"|
                              Institución=="MUNICIPAL",
                            "Servicios de salud para población sin seguridad social","NINGUNA"
                            
                     )),
         cuenta_con_oncologo=ifelse(Médicos.Oncólogos>0,"Si",
                                    ifelse(Médicos.Oncólogos==0,"No","ZA")
           
         )) %>% 
  filter(tipo!="NINGUNA") %>% 
  group_by("Tipo de institución de salud"=tipo) %>% 
  summarise("Total de Oncólogos"=sum(Médicos.Oncólogos),
            Porcentaje = round(sum(Médicos.Oncólogos)/48*100, digits = 2))

total_oncologos %>% 
  gt()

inst_con_oncologos<-recursos %>% 
  filter(Nombre.Estado=="YUCATAN") %>% 
  mutate(tipo=ifelse(Institución=="IMSS"|
                       Institución=="ISSSTE"|
                       Institución=="SEMAR"|
                       Institución=="PEMEX"|
                       Institución=="SEDENA",
                     "Instituciones de salud y seguridad social",
                     ifelse(Institución=="SSA"|
                              Institución=="IMSS Bienestar"|
                              Institución=="UNIVERSITARIO"|
                              Institución=="ESTATAL"|
                              Institución=="MUNICIPAL",
                            "Servicios de salud para población sin seguridad social","NINGUNA"
                            
                     )),
         cuenta_con_oncologo=ifelse(Médicos.Oncólogos>0,"Sí",
                                    ifelse(Médicos.Oncólogos==0,"No","ZA")
                                    
         )) %>% 
  filter(tipo!="NINGUNA") %>% 
  group_by("Con médico oncólogo"=cuenta_con_oncologo) %>% 
  summarise("Instituciones"=n(),
            Porcentaje = round(n()/330*100, digits = 2))
     
inst_con_oncologos %>% 
  gt()
