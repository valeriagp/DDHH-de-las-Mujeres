library(dplyr)
library(tidyverse)
library(scales)



#Variables a utilizar:

UPM	#Unidad Primaria de Muestreo
ENT	#Entidad federativa
TAM_LOC	#Tamaño de localidad
P5_2_1	#Edad
P5_3	#Filtro asistencia escolar
P5_4	#Causa de abandono escolar
P5_22	#Condición de abortos
P5_23	#Número de abortos
P7_16	#Decisión número de hijas(os)
P7_17	#Más hijas(os) del ideal
p8_1_01
p8_1_02
p8_1_03
p8_1_04
p8_1_05
p8_1_06
p8_1_07
p8_1_08
p8_1_09
p8_1_10
p8_1_11
p8_1_12
p8_1_13

p8_2_03_1
p8_2_03_2
p8_2_04
p8_2_05
p8_2_06_1
p8_2_06_2
p8_2_07_1
p8_2_07_2
p8_2_08_1
p8_2_08_2
p8_2_08_3
p8_2_09_1
p8_2_09_2
p8_2_09_3
p8_2_10
p8_2_11
p8_2_12
p8_2_13
p8_10 #Condición de uso (actual) 
P8_3	
P8_12A	#Método que usa (actual) por posición
P8_40	#Consentimiento primera relación
P8_41_01	#Protección utilizada en la primera relación sexual	No usaron nada
p8_16
llave_muj 
ent
fac_mod 
upm_dis 
estrato

#Carga de las bases  fac_mod, upm_dis, estrato: necesarias para estimaciones
enadid<-read.csv("fuentes_de_info/TMUJER1.csv",encoding = "latin1")%>% 
  select(c("upm","viv_sel","hogar", "n_ren", "llave_viv", "llave_hog", "llave_muj",
            "ent","tam_loc","fac_mod","t_loc_ur", "t_loc_ag1","cond_act","p5_2_1",	
            "p5_3","p5_4","p5_22","p5_23","p7_16","p7_17","p8_1_01","p8_1_02","p8_1_03",
           "p8_1_04","p8_1_05","p8_1_06","p8_1_07","p8_1_08","p8_1_09","p8_1_10","p8_1_11",
           "p8_1_12","p8_1_13","p8_2_03_1","p8_2_03_2","p8_2_04","p8_2_05","p8_2_06_1",
           "p8_2_06_2","p8_2_07_1","p8_2_07_2","p8_2_08_1","p8_2_08_2", "p8_2_08_3",
           "p8_2_09_1","p8_2_09_2","p8_2_09_3","p8_2_10","p8_2_11","p8_2_12","p8_2_13","p8_10","p8_14","p8_16",
            "p8_3","p8_12a","p8_40","p8_41_01","niv_esc","edad_1ag","c_limdisc",
           "estrato","est_dis","upm_dis"))


# Conocimientos 2023----
anticon_2023 <- select(enadid,
                       llave_muj, ent,tam_loc,
                       fac_mod, upm_dis, estrato, # necesarias para estimaciones
                       p5_2_1, niv_esc, # sociodem (edad, escolaridad)
                       starts_with("p8_1"), # conocimiento general de anticoncepción
                       starts_with("p8_2")) %>% 
  mutate(edad = p5_2_1) %>% 
  select(-"p5_2_1") %>% 
  subset(edad<50) %>% 
  mutate(algun_anti = ifelse(p8_1_01>2 & 
                               p8_1_02>2 & 
                               p8_1_03>2 &
                               p8_1_04>2 & 
                               p8_1_05>2 & 
                               p8_1_06>2 & 
                               p8_1_07>2 & 
                               p8_1_08>2 & 
                               p8_1_09>2 & 
                               p8_1_10>2 &
                               p8_1_11>2 &
                               p8_1_12>2 &
                               p8_1_13>2,
                              0,1),
         func_anti_pastillas = ifelse(p8_2_03_1==1 & 
                              p8_2_03_2==1,
                              1,0),
         func_anti_inyecciones = ifelse(p8_2_04==1,
                                      1,0),
         func_anti_subdermico = ifelse(p8_2_05==1,
                                        1,0),
         func_anti_parche = ifelse(p8_2_06_1==1&
                                       p8_2_06_2==1,
                                       1,0),
         func_anti_diu = ifelse(p8_2_07_1==1&
                                  p8_2_07_2==1,
                                   1,0),
         func_anti_condon = ifelse(p8_2_08_1==1&
                                     p8_2_08_2==1&
                                     p8_2_08_3==1,
                                1,0),
         func_anti_condon_fem = ifelse(p8_2_09_1==1&
                                         p8_2_09_2==1&
                                         p8_2_09_3==1,
                                       1,0),
         func_anti_ovulos = ifelse(p8_2_10==1,
                                        1,0),
         func_anti_ritmo = ifelse(p8_2_11==1,
                                   1,0),
         func_anti_coito = ifelse(p8_2_12==1,
                                  1,0),
         func_anti_pildora = ifelse(p8_2_13==1,
                                  1,0),
         grupos_edad = ifelse(edad<20, "Adolescentes (15-19 años)",
                              ifelse(edad>19 & edad<30, "Jóvenes (20-29 años)",
                                     ifelse(edad>29 & edad<40, "Adultas 1 (30-39 años)",
                                            ifelse(edad>39, "Adultas 2 (40-49 años)", "ZETA")))),
         localidad = ifelse(tam_loc==1,"Localidad mayor de 100,000 habitantes",
                            ifelse(tam_loc==2,"Localidad de 15,000 a 99,999 habitantes",
                                   ifelse(tam_loc==3,"Localidad de 2,500 a 14,999 habitantes",
                                          ifelse(tam_loc==4,"Localidad menor de 2,500 habitantes","Ninguno")))))
                              


##Tablas para Yucatán sobre conocimiento de métodos---

tbl_conoce<-anticon_2023 %>% #general Yucatán
  filter(ent==31) %>% 
  select(fac_mod, grupos_edad, func_anti, algun_anti) %>%
  group_by("Grupos de edad"=grupos_edad) %>%
  summarise(Total = sum(algun_anti*fac_mod, na.rm = T),
            Porcentaje = round( 
              Total/sum(fac_mod, na.rm = T)*100,digits = 2))

tbl_conoce %>% 
  gt()


tbl_conoce_localidad<-anticon_2023 %>% #por tamaño de localidad solo para adolescentes
  filter(ent==31,grupos_edad=="Adolescentes (15-19 años)") %>% 
  select(fac_mod,localidad, grupos_edad, func_anti, algun_anti) %>%
  group_by(Localidad=localidad) %>%
  summarise(Total = sum(algun_anti*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100,digits = 2))

tbl_conoce_localidad %>% 
  gt()

##Tablas para Yucatán sobre conocimiento del funcionamiento de métodos

tbl_funcionamiento<-anticon_2023 %>% #general Yucatán
  filter(ent==31,grupos_edad=="Adolescentes (15-19 años)") %>%
  summarise(Pastillas_anticonceptivas = sum(func_anti_pastillas*fac_mod, na.rm = T),
            Porcentaje_pastillas = round(Pastillas_anticonceptivas/sum(fac_mod, na.rm = T)*100, digits = 2),
            
            Inyecciones = sum(func_anti_inyecciones*fac_mod, na.rm = T),
            P_inyecciones = round(Inyecciones/sum(fac_mod, na.rm = T)*100, digits = 2),
            
            Subdermico= sum(func_anti_subdermico*fac_mod, na.rm = T),
            P_subdermico = round(Subdermico/sum(fac_mod, na.rm = T)*100, digits = 2),
            
            Parche= sum(func_anti_parche*fac_mod, na.rm = T),
            P_parche = round(Parche/sum(fac_mod, na.rm = T)*100, digits = 2),
            
            DIU= sum(func_anti_diu*fac_mod, na.rm = T),
            P_DIU = round(DIU/sum(fac_mod, na.rm = T)*100, digits = 2),
            
            Condon= sum(func_anti_condon*fac_mod, na.rm = T),
            P_condon = round(Condon/sum(fac_mod, na.rm = T)*100, digits = 2),
            
            CondonFEM= sum(func_anti_condon_fem*fac_mod, na.rm = T),
            P_condonFEM = round(CondonFEM/sum(fac_mod, na.rm = T)*100, digits = 2),
            
            Ovulos= sum(func_anti_ovulos*fac_mod, na.rm = T),
            P_ovulos = round(Ovulos/sum(fac_mod, na.rm = T)*100, digits = 2),
            
            Ritmo= sum(func_anti_ritmo*fac_mod, na.rm = T),
            P_ritmo = round(Ritmo/sum(fac_mod, na.rm = T)*100, digits = 2),
            
            Coito= sum(func_anti_coito*fac_mod, na.rm = T),
            P_coito = round(Coito/sum(fac_mod, na.rm = T)*100, digits = 2),
            
            Pildora= sum(func_anti_pildora*fac_mod, na.rm = T),
            P_pildora = round(Pildora/sum(fac_mod, na.rm = T)*100, digits = 2))


write.csv(tbl_funcionamiento,"Conocimiento funcional metodos adolescentes.csv", fileEncoding = "latin1")


anticon_2023 %>% #funcionamiento pastillas anticonceptivas
  filter(ent==31) %>%
  select(fac_mod, grupos_edad, func_anti_pastillas, algun_anti) %>%
  group_by("Grupos de edad"=grupos_edad) %>%
  summarise(Total = sum(func_anti_pastillas*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100, digits = 2))


anticon_2023 %>% #funcionamiento inyecciones o ampolletas anticonceptivas
  filter(ent==31) %>%
  select(fac_mod, grupos_edad, func_anti_inyecciones, algun_anti) %>%
  group_by("Grupos de edad"=grupos_edad) %>%
  summarise(Total = sum(func_anti_inyecciones*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100, digits = 2))


anticon_2023 %>% #funcionamiento implante anticonceptivo (subdérmico) o Norplant
  filter(ent==31) %>%
  select(fac_mod, grupos_edad, func_anti_subdermico, algun_anti) %>%
  group_by("Grupos de edad"=grupos_edad) %>%
  summarise(Total = sum(func_anti_subdermico*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100, digits = 2))

anticon_2023 %>% #funcionamiento parche anticonceptivo
  filter(ent==31) %>%
  select(fac_mod, grupos_edad, func_anti_parche, algun_anti) %>%
  group_by("Grupos de edad"=grupos_edad) %>%
  summarise(Total = sum(func_anti_parche*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100, digits = 2))

anticon_2023 %>% #funcionamiento DIU, dispositivo o aparato (de cobre)
  filter(ent==31) %>%
  select(fac_mod, grupos_edad, func_anti_diu, algun_anti) %>%
  group_by("Grupos de edad"=grupos_edad) %>%
  summarise(Total = sum(func_anti_diu*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100, digits = 2))

anticon_2023 %>% #funcionamiento condon o  preservativo masculino
  filter(ent==31) %>%
  select(fac_mod, grupos_edad, func_anti_condon, algun_anti) %>%
  group_by("Grupos de edad"=grupos_edad) %>%
  summarise(Total = sum(func_anti_condon*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100, digits = 2))

anticon_2023 %>% #funcionamiento condon o preservativo femenino
  filter(ent==31) %>%
  select(fac_mod, grupos_edad, func_anti_condon_fem, algun_anti) %>%
  group_by("Grupos de edad"=grupos_edad) %>%
  summarise(Total = sum(func_anti_condon_fem*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100, digits = 2))

anticon_2023 %>% #funcionamiento ovulos, jaleas o espumas anticonceptivas
  filter(ent==31) %>%
  select(fac_mod, grupos_edad, func_anti_ovulos, algun_anti) %>%
  group_by("Grupos de edad"=grupos_edad) %>%
  summarise(Total = sum(func_anti_ovulos*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100, digits = 2))

anticon_2023 %>% #funcionamiento ¿Cuándo cree usted que sea más probable que una mujer se embarace si tiene relaciones sexuales? Ritmo, calendario, Billings o abstinencia periódica
  filter(ent==31) %>%
  select(fac_mod, grupos_edad, func_anti_ritmo, algun_anti) %>%
  group_by("Grupos de edad"=grupos_edad) %>%
  summarise(Total = sum(func_anti_ritmo*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100, digits = 2))

anticon_2023 %>% #funcionamiento Retiro o coito interrumpido
  filter(ent==31) %>%
  select(fac_mod, grupos_edad, func_anti_coito, algun_anti) %>%
  group_by("Grupos de edad"=grupos_edad) %>%
  summarise(Total = sum(func_anti_coito*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100, digits = 2))

anticon_2023 %>% #funcionamiento píldora del día siguiente o anticoncepción de emergencia
  filter(ent==31) %>%
  select(fac_mod, grupos_edad, func_anti_pildora, algun_anti) %>%
  group_by("Grupos de edad"=grupos_edad) %>%
  summarise(Total = sum(func_anti_pildora*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100, digits = 2))



tbl_funcionamiento %>% 
  gt()

tbl_funcionamiento_loc<-anticon_2023 %>% #por tamaño de localidad solo para adolescentes
  filter(ent==31,grupos_edad=="Adolescentes (15-19 años)") %>%
  select(fac_mod,localidad, grupos_edad, func_anti, algun_anti) %>%
  group_by(Localidad=localidad) %>%
  summarise(Total = sum(func_anti*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100,digits = 2))


tbl_funcionamiento_loc %>% 
  gt()

#base uso de métodos anticonceptivos

uso_2023<-select(enadid,
                  llave_muj, ent,tam_loc,
                  fac_mod, upm_dis, estrato, # necesarias para estimaciones
                  p5_2_1, niv_esc, # sociodem (edad, escolaridad)
                  p8_41_01,p8_10,p8_40) %>% 
  mutate(edad = p5_2_1) %>% 
  select(-"p5_2_1") %>% 
  subset(edad<50) %>% 
  mutate(grupos_edad = ifelse(edad<20, "Adolescentes (15-19 años)",
                              ifelse(edad>19 & edad<30, "Jóvenes (20-29 años)",
                                     ifelse(edad>29 & edad<40, "Adultas 1 (30-39 años)",
                                            ifelse(edad>39, "Adultas 2 (40-49 años)", "ZETA")))),
         localidad = ifelse(tam_loc==1,"Localidad mayor de 100,000 habitantes",
                            ifelse(tam_loc==2,"Localidad de 15,000 a 99,999 habitantes",
                                   ifelse(tam_loc==3,"Localidad de 2,500 a 14,999 habitantes",
                                          ifelse(tam_loc==4,"Localidad menor de 2,500 habitantes","Ninguno")))),
         uso_actual = ifelse(p8_10==1,1,0),
         consentimiento = ifelse(p8_40==1,1,0),
         no_uso_metodo = ifelse(p8_41_01==1,1,0))



##Tablas para Yucatán sobre condición de uso actual de método anticonceptivo----

tbl_uso_actual<-uso_2023 %>% 
  filter(ent==31) %>%
  group_by("Grupos de edad"=grupos_edad) %>%
  summarise(Total = sum(uso_actual*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100,digits = 2))

tbl_uso_actual %>% 
  gt()


tbl_uso_actual_loc<-uso_2023 %>% 
  filter(ent==31,grupos_edad=="Adolescentes (15-19 años)") %>%
  group_by(Localidad=localidad) %>%
  summarise(Total = sum(uso_actual*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100,digits = 2))

tbl_uso_actual_loc %>% 
  gt()



##Tablas para Yucatán sobre uso de método anticonceptivo en su primera relación sexual---

tbl_no_uso_metodo<-uso_2023 %>% 
  filter(ent==31) %>%
  group_by("Grupos de edad"=grupos_edad) %>%
  summarise(Total = sum(no_uso_metodo*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100,digits = 2))

tbl_no_uso_metodo %>% 
  gt()

tbl_no_uso_metodo_loc<-uso_2023 %>% 
  filter(ent==31,grupos_edad=="Adolescentes (15-19 años)") %>%
  group_by(Localidad=localidad) %>%
  summarise(Total = sum(no_uso_metodo*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100,digits = 2))

tbl_no_uso_metodo_loc %>% 
  gt()

##Tablas para Yucatán sobre consentimiento en su primera relación sexual---

tbl_consentimiento<-uso_2023 %>% 
  filter(ent==31) %>%
  group_by("Grupos de edad"=grupos_edad) %>%
  summarise(Total = sum(consentimiento*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100,digits = 2))

tbl_consentimiento %>% 
  gt()

tbl_consentimiento_loc<-uso_2023 %>% 
  filter(ent==31,grupos_edad=="Adolescentes (15-19 años)") %>%
  group_by(Localidad=localidad) %>%
  summarise(Total = sum(consentimiento*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100,digits = 2))

tbl_consentimiento_loc %>% 
  gt()

#Base para decisión de hijes y asistencia escolar----------


planificacion<-select(enadid,
       llave_muj, ent,tam_loc,
       fac_mod, upm_dis, estrato, # necesarias para estimaciones
       p5_2_1,p7_16,p7_17,p5_4, niv_esc, # sociodem (edad, escolaridad)
       ) %>% 
  mutate(edad = p5_2_1) %>% 
  select(-"p5_2_1") %>% 
  subset(edad<50) %>% 
  mutate(grupos_edad = ifelse(edad<20, "Adolescentes (15-19 años)",
                              ifelse(edad>19 & edad<30, "Jóvenes (20-29 años)",
                                     ifelse(edad>29 & edad<40, "Adultas 1 (30-39 años)",
                                            ifelse(edad>39, "Adultas 2 (40-49 años)", "ZETA")))),
         localidad = ifelse(tam_loc==1,"Localidad mayor de 100,000 habitantes",
                            ifelse(tam_loc==2,"Localidad de 15,000 a 99,999 habitantes",
                                   ifelse(tam_loc==3,"Localidad de 2,500 a 14,999 habitantes",
                                          ifelse(tam_loc==4,"Localidad menor de 2,500 habitantes","Ninguno")))),
         decision_hijes = ifelse(p7_16==1 |
                                   p7_16==3,1,0),
         embarazo_no_planeado = ifelse(p7_17>3,0,1),
         abandono_esc = ifelse(p5_4==01,1,0))


##Tablas para Yucatán sobre planeación de embarazo (decisión de tener hijes)----
#decidió ella o ambos

tbl_decision_hijes<-planificacion %>% 
  filter(ent==31) %>%
  group_by("Grupos de edad"=grupos_edad) %>%
  summarise(Total = sum(decision_hijes*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100,digits = 2))


tbl_decision_hijes %>% 
  gt()


tbl_dec_hijes_loc<-planificacion %>% 
  filter(ent==31,grupos_edad=="Adolescentes (15-19 años)") %>%
  group_by(Localidad=localidad) %>%
  summarise(Total = sum(decision_hijes*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100,digits = 2))

tbl_dec_hijes_loc %>% 
  gt()



##Tabla para más hijas(os) del ideal: 
#No utilizó métodos anticonceptivos, No conocía métodos anticonceptivos, Le falló el método anticonceptivo

tbl_emb_no_planeado<-planificacion %>% 
  filter(ent==31) %>%
  group_by("Grupos de edad"=grupos_edad) %>%
  summarise(Total = sum(embarazo_no_planeado*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100,digits = 2))

tbl_emb_no_planeado %>% 
  gt()


tbl_emb_no_planeado_loc<-planificacion %>% 
  filter(ent==31,grupos_edad=="Adolescentes (15-19 años)") %>%
  group_by(Localidad=localidad) %>%
  summarise(Total = sum(embarazo_no_planeado*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100,digits = 2))

tbl_emb_no_planeado_loc %>% 
  gt()

##Tabla para Yucatán sobre abandono escolar por embarazo o tuvo hije

tbl_abandono_esc<-planificacion %>% 
  filter(ent==31) %>%
  group_by("Grupos de edad"=grupos_edad) %>%
  summarise(Total = sum(abandono_esc*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100,digits = 2))

tbl_abandono_esc %>% 
  gt()


tbl_abandono_esc_loc<-planificacion %>% 
  filter(ent==31,grupos_edad=="Adolescentes (15-19 años)") %>%
  group_by(Localidad=localidad) %>%
  summarise(Total = sum(abandono_esc*fac_mod, na.rm = T),
            Porcentaje = round(Total/sum(fac_mod, na.rm = T)*100,digits = 2))

tbl_abandono_esc_loc %>% 
  gt()


#Base para necesidad insatisfecha de métodos anticonceptivos---

servicios_salud<-select(enadid,
       llave_muj, ent,tam_loc,
       fac_mod, upm_dis, estrato, # necesarias para estimaciones
       p5_2_1,p8_14,p8_16, niv_esc, # sociodem (edad, escolaridad)
) %>% 
  mutate(edad = p5_2_1) %>% 
  select(-"p5_2_1") %>% 
  subset(edad<50) %>% 
  mutate(grupos_edad = ifelse(edad<20, "Adolescentes (15-19 años)",
                              ifelse(edad>19 & edad<30, "Jóvenes (20-29 años)",
                                     ifelse(edad>29 & edad<40, "Adultas 1 (30-39 años)",
                                            ifelse(edad>39, "Adultas 2 (40-49 años)", "ZETA")))),
         localidad = ifelse(tam_loc==1,"Localidad mayor de 100,000 habitantes",
                            ifelse(tam_loc==2,"Localidad de 15,000 a 99,999 habitantes",
                                   ifelse(tam_loc==3,"Localidad de 2,500 a 14,999 habitantes",
                                          ifelse(tam_loc==4,"Localidad menor de 2,500 habitantes","Ninguno")))),
         necesidad_insatisfecha = ifelse(p8_16==4,1,0),
         lugar_obtencion = ifelse(p8_14==01|
                                     p8_14==02|
                                     p8_14==03|
                                     p8_14==04|
                                     p8_14==05|
                                     p8_14==06,"Institución o programa público","Institución o medio privado"))

##Tabla para necesidad insatisfecha de métodos anticonceptivos (no había el método solicitado)---

tbl_nec_insatis<-servicios_salud %>% 
  filter(ent==31,grupos_edad=="Adolescentes (15-19 años)") %>%
  group_by(localidad) %>%
  summarise(total_general = sum(necesidad_insatisfecha*fac_mod, na.rm = T),
            porc_general = total_general/sum(fac_mod, na.rm = T)*100)




##Tabla para necesidad insatisfecha de métodos anticonceptivos---PENDIENTE DE ARREGLAR

servicios_salud %>% 
  filter(ent==31,grupos_edad=="Adolescentes (15-19 años)") %>%
  group_by(localidad,lugar_obtencion) %>%
  summarise(total_general = sum(fac_mod, na.rm = T),
            porc_general = total_general/sum(fac_mod, na.rm = T)*100)
