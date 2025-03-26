library(dplyr)
library(tidyverse)
library(srvyr)
library(survey)
library (scales)
# Se instalan los paquetes y librerías a utilizar en el programa
if (!require(pacman)) install.packages("pacman")
library(pacman) ; p_load("data.table", "tidyverse", "gdata", "srvyr", "bit64")


censo_personas<-read.csv("Salud_PED_Recursos/Personas31.csv",encoding = "latin1")
#censo_personas[is.na(censo_personas)] = 0

ageb<-read.csv("embarazo_recursos/RESAGEBURB_31CSV20.csv",encoding = "UTF-8")



educacion_censo<-censo_personas %>% 
  mutate(asisten=ifelse(ASISTEN==1,1,0),
         no_asisten=ifelse(ASISTEN==3,1,0),
         nivel_estudio=ifelse(NIVACAD==00,"Ninguno",
                              ifelse(NIVACAD==01,"Preescolar",
                                     ifelse(NIVACAD==02,"Primaria",
                                            ifelse(NIVACAD==03,"Secundaria",
                                                   ifelse(NIVACAD==04,"Preparatoria",
                                                          ifelse(NIVACAD==05,"Bachillerato tecnológico",
                                                                 "Otro nivel"))))))) %>% 
  select(nivel_estudio,asisten,no_asisten,ASISTEN,EDAD,MUN,SEXO,LOC50K,HIJOS_NAC_VIVOS,HIJOS_FALLECIDOS,HIJOS_SOBREVIV,FACTOR,UPM,ESTRATO,NIVACAD)



educacion_censo %>% 
  group_by(ASISTEN) %>% 
  summarise(sum(FACTOR))


svy <- educacion_censo %>% 
  as_survey_design(weights = FACTOR)

adol_madres_asisten<-svy %>% #solo en el municipio de Mérida
  filter(EDAD>=18,EDAD<=24,HIJOS_NAC_VIVOS>=1,MUN==50) %>% 
  group_by(EDAD) %>%
  summarise(prop = survey_total(vartype = NULL))


svy %>% #solo en el municipio de Mérida
  filter(EDAD>=18,EDAD<=24,HIJOS_NAC_VIVOS>=1,MUN==50) %>% 
  group_by(EDAD) %>%
  summarise(prop = survey_total(vartype = NULL))



#POR SI ME SIRVEN DESPUÉS

mutate(asisten=ifelse(ASISTEN==1,1,0),
       no_asisten=ifelse(ASISTEN==3,1,0),
       ninguno_nivel_esc=ifelse(NIVACAD==00,1,0),
       preesc_nivel_esc=ifelse(NIVACAD==01,1,0),
       primaria_nivel_esc=ifelse(NIVACAD==02,1,0),
       secu_nivel_esc=ifelse(NIVACAD==03,1,10),
       prepa_nivel_esc=ifelse(NIVACAD==04,1,10),
       bachTec_nivel_esc=ifelse(NIVACAD==05,1,10),
       tec_prim_ter=ifelse(NIVACAD==06,1,10),
       tec_sec_ter=ifelse(NIVACAD==07,1,10),
       tec_prep_ter=ifelse(NIVACAD==08,1,10),
       normal_prim_sec=ifelse(NIVACAD==09,1,10))

#Diagnóstico del Embarazo en adolescentes:

ageb$P_8A14=as.numeric(ageb$P_8A14)
ageb$P_8A14[is.na(ageb$P_8A14)] = 0

ageb$P_15A17=as.numeric(ageb$P_15A17)
ageb$P_15A17[is.na(ageb$P_15A17)] = 0

ageb$P_18A24=as.numeric(ageb$P_18A24)
ageb$P_18A24[is.na(ageb$P_18A24)] = 0

ageb$P_8A14_F=as.numeric(ageb$P_8A14_F)
ageb$P_8A14_F[is.na(ageb$P_8A14_F)] = 0

ageb$P_15A17_F=as.numeric(ageb$P_15A17_F)
ageb$P_15A17_F[is.na(ageb$P_15A17_F)] = 0

ageb$P_18A24_F=as.numeric(ageb$P_18A24_F)
ageb$P_18A24_F[is.na(ageb$P_18A24_F)] = 0

ageb$POBTOT=as.numeric(ageb$POBTOT)
ageb$POBTOT[is.na(ageb$POBTOT)] = 0

pob_NNA<-ageb %>%
  filter(MUN==50) %>% 
  mutate(NNA=P_8A14+P_15A17+P_18A24,
         NNA_Muj=P_8A14_F +P_15A17_F +P_18A24_F) %>% 
  group_by(NOM_LOC) %>% 
  summarise(NNA=sum(NNA),
            Porcentaje_NNA=round(sum(NNA)/sum(POBTOT)*100,digits = 2),
            "NNA Mujeres"=sum(NNA_Muj),
            Porcentaje= round(sum(NNA_Muj)/sum(NNA)*100,digits = 2))
