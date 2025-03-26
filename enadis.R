library(dplyr)
library(tidyverse)

enadis<-read.csv("Salud_PED_Recursos/enadis2022.csv",encoding = "latin1")%>%
  mutate(tamaño_localidad=ifelse(TLOC==1,"100 000 y más habitantes",
                                      ifelse(TLOC==2,"15 000 a 99 999 habitantes",
                                             ifelse(TLOC==3,"2 500 a 14 999 habitantes",
                                                    ifelse(TLOC==4,"menor a 2 500 habitantes","Z")))),
         discrim_color = ifelse(PO4_1_01==1,1,0),
         discrim_orientacion=ifelse(PO4_1_10==1,1,0),
         discrim_etnia=ifelse(PO4_1_11==1,1,0),
         discrim_discapacidad=ifelse(PO4_1_12==1,1,0),
         discrim_habla=ifelse(PO4_1_02==1,1,0),
         discrim_peso=ifelse(PO4_1_03==1,1,0),
         discrim_vestimenta=ifelse(PO4_1_04==1,1,0),
         discrim_clase=ifelse(PO4_1_05==1,1,0),
         discrim_lugar=ifelse(PO4_1_06==1,1,0),
         discrim_religion=ifelse(PO4_1_07==1,1,0),
         discrim_mujer=ifelse(PO4_1_08==1,1,0),
         discrim_edad=ifelse(PO4_1_09==1,1,0),
         discrim_enfermedad=ifelse(PO4_1_13==1,1,0),
         discrim_politicas=ifelse(PO4_1_14==1,1,0),
         discrim_edocivil=ifelse(PO4_1_15==1,1,0),
         discrim_otro=ifelse(PO4_1_16==1,1,0),
         tot_discriminades=ifelse(PO4_1_01==1|
                                    PO4_1_02==1|
                                    PO4_1_03==1|
                                    PO4_1_04==1|
                                    PO4_1_05==1|
                                    PO4_1_06==1|
                                    PO4_1_07==1|
                                    PO4_1_08==1|
                                    PO4_1_09==1|
                                    PO4_1_10==1|
                                    PO4_1_11==1|
                                    PO4_1_12==1|
                                    PO4_1_13==1|
                                    PO4_1_14==1|
                                    PO4_1_15==1|
                                    PO4_1_16==1,1,0)) %>% 
  inner_join(cat_ent_enadis,by="ENT")
cat_ent_enadis<-read.csv("Salud_PED_Recursos/cat_entidades_enadis.csv",encoding = "latin1") 
indigena_enadis<-read.csv("Salud_PED_Recursos/indigena_enadis2022.csv",encoding = "latin1")

summary(enadis$EDAD)

#tempo_enadis<-enadis %>%
  mutate(tamaño_localidad=ifelse(TLOC==1,"100 000 y más habitantes",
                                      ifelse(TLOC==2,"15 000 a 99 999 habitantes",
                                             ifelse(TLOC==3,"2 500 a 14 999 habitantes",
                                                    ifelse(TLOC==4,"menor a 2 500 habitantes","Z")))),
         discrim_color = ifelse(PO4_1_01==1,1,0),
         discrim_orientacion=ifelse(PO4_1_10==1,1,0),
         discrim_etnia=ifelse(PO4_1_11==1,1,0),
         discrim_discapacidad=ifelse(PO4_1_12==1,1,0),
         discrim_habla=ifelse(PO4_1_02==1,1,0),
         discrim_peso=ifelse(PO4_1_03==1,1,0),
         discrim_vestimenta=ifelse(PO4_1_04==1,1,0),
         discrim_clase=ifelse(PO4_1_05==1,1,0),
         discrim_lugar=ifelse(PO4_1_06==1,1,0),
         discrim_religion=ifelse(PO4_1_07==1,1,0),
         discrim_mujer=ifelse(PO4_1_08==1,1,0),
         discrim_edad=ifelse(PO4_1_09==1,1,0),
         discrim_enfermedad=ifelse(PO4_1_13==1,1,0),
         discrim_politicas=ifelse(PO4_1_14==1,1,0),
         discrim_edocivil=ifelse(PO4_1_15==1,1,0),
         discrim_otro=ifelse(PO4_1_16==1,1,0),
         tot_discriminades=ifelse(PO4_1_01==1|
                                    PO4_1_02==1|
                                    PO4_1_03==1|
                                    PO4_1_04==1|
                                    PO4_1_05==1|
                                    PO4_1_06==1|
                                    PO4_1_07==1|
                                    PO4_1_08==1|
                                    PO4_1_09==1|
                                    PO4_1_10==1|
                                    PO4_1_11==1|
                                    PO4_1_12==1|
                                    PO4_1_13==1|
                                    PO4_1_14==1|
                                    PO4_1_15==1|
                                    PO4_1_16==1,1,0)) %>% 
  inner_join(cat_ent_enadis,by="ENT")
         

         
  
#Percepción de discriminación por orientación sexual  

d_orientacion<-tempo_enadis %>% 
  group_by(Entidad=descrip) %>% 
   summarise(Total = sum(discrim_orientacion*FAC_P18, na.rm = T),
            Porcentaje = round(Total/sum(tot_discriminades*FAC_P18, na.rm = T)*100,digits = 1)) %>% 
  arrange(desc(Porcentaje))

write.csv(d_orientacion,"d_orientacion.csv",fileEncoding = "latin1")

#Percepción de discriminación de juventudes por orientación sexual

tempo_enadis %>% 
  filter(EDAD>=18,EDAD<=30) %>% 
  group_by(Entidad=descrip) %>% 
  summarise(Total = sum(discrim_orientacion*FAC_P18, na.rm = T),
            Porcentaje = round(Total/sum(tot_discriminades*FAC_P18, na.rm = T)*100,digits = 1)) %>% 
  arrange(desc(Porcentaje))

#Percepción de discriminación por tono de piel

d_tono_piel<-enadis %>% 
  group_by(Entidad=descrip) %>% 
  summarise(Total = sum(discrim_color*FAC_P18, na.rm = T),
            Porcentaje = round(Total/sum(tot_discriminades*FAC_P18, na.rm = T)*100,digits = 1)) %>% 
  arrange(desc(Porcentaje))

write.csv(d_tono_piel,"d_tono_piel.csv",fileEncoding = "latin1")

enadis %>% 
  filter(descrip=="Yucatán",SEXO==2) %>% #Mujeres discriminación por tono de piel
  group_by(tamaño_localidad) %>% 
  summarise(Total = sum(discrim_color*FAC_P18, na.rm = T),
            Porcentaje = round(Total/sum(tot_discriminades*FAC_P18, na.rm = T)*100,digits = 1)) %>% 
  arrange(desc(Porcentaje))

#Percepción de jóvenes entre 18 y 30 discriminación por edad 

d_edad_jovenes<-tempo_enadis %>% 
  filter(EDAD>=18,EDAD<=29) %>% 
  group_by(Entidad=descrip) %>% 
  summarise(Total = sum(discrim_edad*FAC_P18, na.rm = T),
            Porcentaje = round(Total/sum(tot_discriminades*FAC_P18, na.rm = T)*100,digits = 1)) %>% 
  arrange(desc(Porcentaje))

write.csv(d_edad_jovenes,"d_edad_jovenes.csv",fileEncoding = "latin1")

#Percepción de discriminación por etnia
      
d_etnia<-tempo_enadis %>% 
  group_by(Entidad=descrip) %>% 
  summarise(Total = sum(discrim_etnia*FAC_P18, na.rm = T),
            Porcentaje = round(Total/sum(tot_discriminades*FAC_P18, na.rm = T)*100,digits = 1)) %>% 
  arrange(desc(Porcentaje))

enadis %>% 
  filter(descrip=="Yucatán",SEXO==2) %>% #Mujeres discriminación por etnia
  group_by(tamaño_localidad) %>% 
  summarise(Total = sum(discrim_etnia*FAC_P18, na.rm = T),
            Porcentaje = round(Total/sum(tot_discriminades*FAC_P18, na.rm = T)*100,digits = 1)) %>% 
  arrange(desc(Porcentaje))

write.csv(d_etnia,"d_etnia.csv",fileEncoding = "latin1")


#----



tempo_indigena_enadis<-indigena_enadis %>%
  mutate(
         indigenas=ifelse(PVI_1==1,1,0),
         discrim_gobierno=ifelse(PM9_7_4==1,1,0))%>% 
  inner_join(cat_ent_enadis,by="ENT")

SEXO #Mujer=2
EDAD
PM1_6_05 #Persona indígena que le niegan información o no le explican en gobierno
PM9_7_4 #¿la(o) han discriminado o menospreciado (ignoraron, rechazaron o agredieron) en alguna oficina de gobierno?

tbl_ind_gob_muj<-tempo_indigena_enadis %>%
  filter(SEXO==2,PVI_1==1) %>%
  group_by(Entidad=descrip) %>% 
  summarise(Total = sum(discrim_gobierno*FAC_IND, na.rm = T),
            Porcentaje = round(Total/sum(indigenas*FAC_IND, na.rm = T)*100,digits = 1)) %>% 
  arrange(desc(Porcentaje)) 

tbl_ind_gob<-tempo_indigena_enadis %>%
  filter(PVI_1==1) %>%
  group_by(Entidad=descrip) %>% 
  summarise(Total = sum(discrim_gobierno*FAC_IND, na.rm = T),
            Porcentaje = round(Total/sum(indigenas*FAC_IND, na.rm = T)*100,digits = 1)) %>% 
  arrange(desc(Porcentaje))
  
## TABLA DE ADOLESCENTES ENADIS 2022

enadis_adolescentes<-read.csv("Salud_PED_Recursos/enadis_adolescentes2022.csv",encoding = "latin1") %>% 
  mutate(respeto_derechos_adol=ifelse(PM7_1==1,"Mucho",
                                      ifelse(PM7_1==2,"Algo",
                                             ifelse(PM7_1==3,"Poco",
                                                    ifelse(PM7_1==4,"Nada",
                                                           ifelse(PM7_1==9,"No sabe","Z"))))),
         problemas_adolescentes=ifelse(PM7_2==1,"Falta de empleo",
                                       ifelse(PM7_2==2,"Falta de oportunidades para seguir estudiando",
                                              ifelse(PM7_2==3,"Adicciones",
                                                     ifelse(PM7_2==4,"Violencia e inseguridad",
                                                            ifelse(PM7_2==5,"Embarazo en la adolescencia",
                                                                   ifelse(PM7_2==6,"Acoso escolar o bullying",
                                                                          ifelse(PM7_2==7,"Problemas personales o familiares",
                                                                                 ifelse(PM7_2==8,"Otro","Z")
                                                                          ))))))),
         Adolescentes=ifelse(EDAD>=12&EDAD<=19,1,0),
         Juventudes=ifelse(EDAD>=20&EDAD<=29,1,0))


enadis_adolescentes %>% #discriminación por orientación sexual
  filter(ENT==31,TLOC==1) %>% 
  group_by(PM9_6_10) %>% 
  summarise(Total=sum(Adolescentes*FAC_ADO, na.rm = T),
            Porcentaje = round(Total/109502*100,digits = 2))


enadis_adolescentes %>% #discriminación por opiniones políticas
  filter(ENT==31,TLOC==1) %>% 
  group_by(PM9_6_14) %>% 
  summarise(Total=sum(Adolescentes*FAC_ADO, na.rm = T),
            Porcentaje = round(Total/109502*100,digits = 2))


enadis_adolescentes %>% #principales problemas de las adolescencias
  filter(ENT==31,TLOC==1) %>% 
  group_by(problemas_adolescentes) %>% 
  summarise(Total=sum(Adolescentes*FAC_ADO, na.rm = T),
            Porcentaje = round(Total/109502*100,digits = 2)) %>% 
  arrange(-Porcentaje)

respeto_derechos_adol

enadis_adolescentes %>% #Porcentaje adolescentes según percepción de respeto a sus derechos
  filter(ENT==31,TLOC==1) %>% 
  group_by(respeto_derechos_adol) %>% 
  summarise(Total=sum(Adolescentes*FAC_ADO, na.rm = T),
            Porcentaje = round(Total/109502*100,digits = 2)) %>% 
  arrange(-Porcentaje)

enadis_adolescentes %>% #Porcentaje adolescentes según percepción de respeto a sus derechos
  filter(ENT==31,TLOC==1) %>% 
  summarise(Total=sum(Adolescentes*FAC_ADO, na.rm = T))
