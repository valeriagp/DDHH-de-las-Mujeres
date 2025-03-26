
library(dplyr)
library(stringr)

#Carga de las bases
##Variables a utilizar

### Tabla Evento: ID,Clues, FechaAtencion, Edad, SEXO (2 mujer), CodEstado, CodMunicipio, CodLocalidad,CodIntencionalidad
### Tabla Atención: ID, Clues, CodTipoAtencion (7 PÍLDORA ANTICONCEPTIVA DE EMERGENCIA, 10 IVE (INTERRUPCIÓN VOLUNTARIA DEL EMBARAZO)
### Tabla Tipo Violencia: ID,Clues,CodTipoViolencia (7 VIOLENCIA SEXUAL)

evento_salud<-read.csv("Registros_atenciónViolenciaSalud/d_evento.csv",encoding = "latin1")%>% 
  #select(c(ID,Clues,FechaAtencion, Edad, SEXO, CodEstado, CodMunicipio, CodLocalidad,CodIntencionalidad)) %>% 
  mutate(capital=ifelse(Clues=="YNSMP000322"|
                          Clues=="YNSMP000346"|
                          Clues=="YNIMS000141"|
                          Clues=="YNSME000016"|
                          Clues=="YNIMS000310"|
                          Clues=="YNSSA013795"|
                          Clues=="YNSSA000606"|
                          Clues=="YNSSA000553"|
                          Clues=="YNIMS000071"|
                          Clues=="YNSMP000474"|
                          Clues== "YNSMP000462"|
                          Clues=="YNIMS000083"|
                          Clues=="YNSSA000565"|
                          Clues=="YNSMP000073"|
                          Clues=="YNSSA013836"|
                          Clues== "YNSSA000570"|
                          Clues=="YNSMP000492"|
                          Clues=="YNSMP000483"|
                          Clues=="YNIMS000136"|
                          Clues=="YNSSA000582"|
                          Clues== "YNSMP000276"|
                          Clues=="YNSMP000281"|
                          Clues=="YNSMP000293"|
                          Clues=="YNSSA013870"|
                          Clues=="YNSMP000020"|
                          Clues=="YNSMP000032"|
                          Clues=="YNSMP000044"|
                          Clues=="YNSMP000056"|
                          Clues=="YNSMP000061"|
                          Clues=="YNIMS000095"|
                          Clues== "YNSSA013882"|
                          Clues=="YNSMP000351"|
                          Clues=="YNSMP000363"|
                          Clues=="YNSMP000375"|
                          Clues=="YNSMP000240"|
                          Clues=="YNSMP000252"|
                          Clues=="YNSMP000264"|
                          Clues=="YNSMP000310"|
                          Clues=="YNSMP000102"|
                          Clues=="YNSMP000421"|
                          Clues== "YNSCT000011"|
                          Clues=="YNCRO000011"|
                          Clues== "YNSMP000450"|
                          Clues=="YNIST000030"|
                          Clues=="YNIST000136"|
                          Clues=="YNIMO000911"|
                          Clues=="YNIMS000100"|
                          Clues=="YNIMS000112"|
                          Clues=="YNSMP000433"|
                          Clues== "YNSMP000445"|
                          Clues== "YNDIF000143"|
                          Clues=="YNDIF000155"|
                          Clues== "YNSMP000085"|
                          Clues=="YNSMP000090"|
                          Clues=="YNSSA000594"|
                          Clues=="YNSSA013633"|
                          Clues== "YNSSA013650"|
                          Clues=="YNCRO000023"|
                          Clues=="YNIMO000894"|
                          Clues== "YNIMO000906"|
                          Clues== "YNSSA013981"|
                          Clues== "YNSMP000015"|
                          Clues=="YNSMP000235"|
                          Clues=="YNIMS000124"|
                          Clues=="YNIMO000923"|
                          Clues== "YNSSA013894"|
                          Clues=="YNSSA013766"|
                          Clues=="YNSDN000014"|
                          Clues=="YNSMP000380"|
                          Clues=="YNSMP000392"|
                          Clues=="YNSSA013423"|
                          Clues== "YNSSA013771"|
                          Clues=="YNSSA013783"|
                          Clues== "YNIST000112"|
                          Clues=="YNSSA000541"|
                          Clues=="YNSSA000611"|
                          Clues=="YNIMS000153"|
                          Clues== "YNIMO000993"|
                          Clues== "YNSSA014022"|
                          Clues==  "YNSSA000623"|
                          Clues=="YNIMS000165"|
                          Clues=="YNSSA014010"|
                          Clues=="YNIMO000405"|
                          Clues== "YNIMS000170"|
                          Clues=="YNIMS000182","Mérida","Otro"))




atencion_salud<-read.csv("Registros_atenciónViolenciaSalud/d_tipo_atencion.csv",encoding = "latin1")%>% 
  select(c(ID, Clues, CodTipoAtencion)) %>%  
  mutate(capital=ifelse(Clues=="YNSMP000322"|
                          Clues=="YNSMP000346"|
                          Clues=="YNIMS000141"|
                          Clues=="YNSME000016"|
                          Clues=="YNIMS000310"|
                          Clues=="YNSSA013795"|
                          Clues=="YNSSA000606"|
                          Clues=="YNSSA000553"|
                          Clues=="YNIMS000071"|
                          Clues=="YNSMP000474"|
                          Clues== "YNSMP000462"|
                          Clues=="YNIMS000083"|
                          Clues=="YNSSA000565"|
                          Clues=="YNSMP000073"|
                          Clues=="YNSSA013836"|
                          Clues== "YNSSA000570"|
                          Clues=="YNSMP000492"|
                          Clues=="YNSMP000483"|
                          Clues=="YNIMS000136"|
                          Clues=="YNSSA000582"|
                          Clues== "YNSMP000276"|
                          Clues=="YNSMP000281"|
                          Clues=="YNSMP000293"|
                          Clues=="YNSSA013870"|
                          Clues=="YNSMP000020"|
                          Clues=="YNSMP000032"|
                          Clues=="YNSMP000044"|
                          Clues=="YNSMP000056"|
                          Clues=="YNSMP000061"|
                          Clues=="YNIMS000095"|
                          Clues== "YNSSA013882"|
                          Clues=="YNSMP000351"|
                          Clues=="YNSMP000363"|
                          Clues=="YNSMP000375"|
                          Clues=="YNSMP000240"|
                          Clues=="YNSMP000252"|
                          Clues=="YNSMP000264"|
                          Clues=="YNSMP000310"|
                          Clues=="YNSMP000102"|
                          Clues=="YNSMP000421"|
                          Clues== "YNSCT000011"|
                          Clues=="YNCRO000011"|
                          Clues== "YNSMP000450"|
                          Clues=="YNIST000030"|
                          Clues=="YNIST000136"|
                          Clues=="YNIMO000911"|
                          Clues=="YNIMS000100"|
                          Clues=="YNIMS000112"|
                          Clues=="YNSMP000433"|
                          Clues== "YNSMP000445"|
                          Clues== "YNDIF000143"|
                          Clues=="YNDIF000155"|
                          Clues== "YNSMP000085"|
                          Clues=="YNSMP000090"|
                          Clues=="YNSSA000594"|
                          Clues=="YNSSA013633"|
                          Clues== "YNSSA013650"|
                          Clues=="YNCRO000023"|
                          Clues=="YNIMO000894"|
                          Clues== "YNIMO000906"|
                          Clues== "YNSSA013981"|
                          Clues== "YNSMP000015"|
                          Clues=="YNSMP000235"|
                          Clues=="YNIMS000124"|
                          Clues=="YNIMO000923"|
                          Clues== "YNSSA013894"|
                          Clues=="YNSSA013766"|
                          Clues=="YNSDN000014"|
                          Clues=="YNSMP000380"|
                          Clues=="YNSMP000392"|
                          Clues=="YNSSA013423"|
                          Clues== "YNSSA013771"|
                          Clues=="YNSSA013783"|
                          Clues== "YNIST000112"|
                          Clues=="YNSSA000541"|
                          Clues=="YNSSA000611"|
                          Clues=="YNIMS000153"|
                          Clues== "YNIMO000993"|
                          Clues== "YNSSA014022"|
                          Clues==  "YNSSA000623"|
                          Clues=="YNIMS000165"|
                          Clues=="YNSSA014010"|
                          Clues=="YNIMO000405"|
                          Clues== "YNIMS000170"|
                          Clues=="YNIMS000182","Mérida","Otro"),
         Atencion=ifelse(CodTipoAtencion==7,"Píldora anticonceptiva de emergencia",
                         ifelse(CodTipoAtencion==10,"IVE","Ninguno")))
           
           
        
atencion_viol_salud<-read.csv("Registros_atenciónViolenciaSalud/d_tipo_violencia.csv",encoding = "latin1")%>% 
  select(c(ID,Clues,CodTipoViolencia)) %>% 
  mutate(capital=ifelse(Clues=="YNSMP000322"|
                          Clues=="YNSMP000346"|
                          Clues=="YNIMS000141"|
                          Clues=="YNSME000016"|
                          Clues=="YNIMS000310"|
                          Clues=="YNSSA013795"|
                          Clues=="YNSSA000606"|
                          Clues=="YNSSA000553"|
                          Clues=="YNIMS000071"|
                          Clues=="YNSMP000474"|
                          Clues== "YNSMP000462"|
                          Clues=="YNIMS000083"|
                          Clues=="YNSSA000565"|
                          Clues=="YNSMP000073"|
                          Clues=="YNSSA013836"|
                          Clues== "YNSSA000570"|
                          Clues=="YNSMP000492"|
                          Clues=="YNSMP000483"|
                          Clues=="YNIMS000136"|
                          Clues=="YNSSA000582"|
                          Clues== "YNSMP000276"|
                          Clues=="YNSMP000281"|
                          Clues=="YNSMP000293"|
                          Clues=="YNSSA013870"|
                          Clues=="YNSMP000020"|
                          Clues=="YNSMP000032"|
                          Clues=="YNSMP000044"|
                          Clues=="YNSMP000056"|
                          Clues=="YNSMP000061"|
                          Clues=="YNIMS000095"|
                          Clues== "YNSSA013882"|
                          Clues=="YNSMP000351"|
                          Clues=="YNSMP000363"|
                          Clues=="YNSMP000375"|
                          Clues=="YNSMP000240"|
                          Clues=="YNSMP000252"|
                          Clues=="YNSMP000264"|
                          Clues=="YNSMP000310"|
                          Clues=="YNSMP000102"|
                          Clues=="YNSMP000421"|
                          Clues== "YNSCT000011"|
                          Clues=="YNCRO000011"|
                          Clues== "YNSMP000450"|
                          Clues=="YNIST000030"|
                          Clues=="YNIST000136"|
                          Clues=="YNIMO000911"|
                          Clues=="YNIMS000100"|
                          Clues=="YNIMS000112"|
                          Clues=="YNSMP000433"|
                          Clues== "YNSMP000445"|
                          Clues== "YNDIF000143"|
                          Clues=="YNDIF000155"|
                          Clues== "YNSMP000085"|
                          Clues=="YNSMP000090"|
                          Clues=="YNSSA000594"|
                          Clues=="YNSSA013633"|
                          Clues== "YNSSA013650"|
                          Clues=="YNCRO000023"|
                          Clues=="YNIMO000894"|
                          Clues== "YNIMO000906"|
                          Clues== "YNSSA013981"|
                          Clues== "YNSMP000015"|
                          Clues=="YNSMP000235"|
                          Clues=="YNIMS000124"|
                          Clues=="YNIMO000923"|
                          Clues== "YNSSA013894"|
                          Clues=="YNSSA013766"|
                          Clues=="YNSDN000014"|
                          Clues=="YNSMP000380"|
                          Clues=="YNSMP000392"|
                          Clues=="YNSSA013423"|
                          Clues== "YNSSA013771"|
                          Clues=="YNSSA013783"|
                          Clues== "YNIST000112"|
                          Clues=="YNSSA000541"|
                          Clues=="YNSSA000611"|
                          Clues=="YNIMS000153"|
                          Clues== "YNIMO000993"|
                          Clues== "YNSSA014022"|
                          Clues==  "YNSSA000623"|
                          Clues=="YNIMS000165"|
                          Clues=="YNSSA014010"|
                          Clues=="YNIMO000405"|
                          Clues== "YNIMS000170"|
                          Clues=="YNIMS000182","Mérida","Otro"))



#Crear base unida
base2023<-evento_salud %>% 
  filter(capital=="Mérida") %>% 
  inner_join(atencion_salud,by="Clues") %>% 
  inner_join(atencion_viol_salud,by="Clues")


##Casos de abuso sexual (T742) atendidos en 2024


tempo_NOM046<-evento_salud %>% 
  mutate(RangoEdad=ifelse(Edad>=10&Edad<=14,"10 a 14",
                          ifelse(Edad>=15&Edad<=19,"15 a 19",
                                 ifelse(Edad<=9,"Menores de 10 años",
                                        ifelse(Edad>=20,"Mayores de 19 años","Otro rango")))),
         Referencia=ifelse(CodReferido==1,"Unidad Médica",
                           ifelse(CodReferido==2,"Procuración de justicia",
                                  ifelse(CodReferido==3,"Secretaría de Educación Pública",
                                         ifelse(CodReferido==4,"Desarrollo Social",
                                                ifelse(CodReferido==5,"DIF",
                                                       ifelse(CodReferido==6,"Otras instituciones gubernamentales",
                                                              ifelse(CodReferido==7,"Instituciones no gubernamentales",
                                                                     ifelse(CodReferido==8,"Iniciativa propia",
                                                                            ifelse(CodReferido==9,"No especificado","Z"))))))))),
         Canalización=ifelse(CodDestino==1,	"Domicilio",
                             ifelse(CodDestino==2,"Traslado a otra unidad Médica",
                                    ifelse(CodDestino==3,"Servicio especializado en atención a la violencia",
                                           ifelse(CodDestino==4,"Consulta externa",
                                                  ifelse(CodDestino==5,"Defunción",
                                                         ifelse(CodDestino==6,"Refugio o albergue",
                                                                ifelse(CodDestino==7,"DIF",
                                                                       ifelse(CodDestino==8,"Hospitalización",
                                                                              ifelse(CodDestino==9,"Ministerio Público",
                                                                                     ifelse(CodDestino==10,"Grupo de ayuda mutua",
                                                                                            ifelse(CodDestino==11,"Otro",
                                                                                                   ifelse(CodDestino==99,"No especificado","Z")))))))))))),
         AgresionSexual=ifelse(AFECPRIN=="Y05"|  #NO incluye abuso sexual
                                 AFECPRIN=="Y050"|
                               AFECPRIN=="Y051"|
                               AFECPRIN=="Y052"|
                               AFECPRIN=="Y053"|
                               AFECPRIN=="Y054"|
                               AFECPRIN=="Y055"|
                               AFECPRIN=="Y056"|
                               AFECPRIN=="Y057"|
                               AFECPRIN=="Y058"|
                               AFECPRIN=="Y059","Agresión sexual con fuerza corporal","Otro"))



tbl_atn_abusoSex<-tempo_NOM046 %>% 
  filter(CodEstado==31 ,capital=="Mérida",CodMunicipio==50,AFECPRIN=="T742") %>% #CIE10: abuso sexual
  group_by(RangoEdad) %>% 
  summarise(Total = n(),
            Porcentaje = round(Total/191*100,digits = 2))

write.csv(tbl_atn_abusoSex,"Casos de abuso sexual atendidos por Salud.csv", fileEncoding = "latin1" )


tbl_referencia<-tempo_NOM046 %>% 
    filter(CodEstado==31 ,capital=="Mérida",CodMunicipio==50,AFECPRIN=="T742",Edad>=10&Edad<=14) %>% #CIE10: abuso sexual
    group_by(Referencia) %>% 
    summarise(Total = n(),
              Porcentaje = round(Total/47*100,digits = 2))
    
write.csv(tbl_referencia,"Referencias casos abuso sexual 2024.csv", fileEncoding = "latin1" )

tbl_canalizacion<-tempo_NOM046 %>% 
  filter(CodEstado==31 ,CodMunicipio==50, capital=="Mérida",AFECPRIN=="T742",Edad>=10&Edad<=14) %>% #CIE10: abuso sexual
  group_by(Canalización) %>% 
  summarise(Total = n(),
            Porcentaje = round(Total/47*100,digits = 2))

write.csv(tbl_canalizacion,"Canalización casos abuso sexual 2024.csv", fileEncoding = "latin1" )


nn_abusoSex<-tempo_NOM046 %>% 
  filter(AFECPRIN=="T742",Edad<=14) %>% #CIE10: abuso sexual
  group_by(CodEstado) %>% 
  summarise(Total = n())

write.csv(nn_abusoSex,"nn_abusoSex2024.csv", fileEncoding = "latin1" )

menores15<-tempo_NOM046 %>% 
  filter(Edad<=14) %>% 
  group_by(CodEstado) %>% 
  summarise(Total = n())
write.csv(menores15,"numero de niñes servicios salud.csv", fileEncoding = "latin1" )
