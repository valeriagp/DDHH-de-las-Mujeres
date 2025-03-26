library(dplyr)
library(tidyverse)
library(scales)

#Carga de las bases

defunciones2023<-read.csv("conjunto_de_datos_defunciones_registradas_2023/defunciones2023.csv",encoding = "latin1") %>% 
  mutate(Suicidio=ifelse(causa_def=="X600"|
                         causa_def=="X610"|
                         causa_def=="X618"|
                         causa_def=="X619"|
                         causa_def=="X620"|
                         causa_def=="X624"|
                         causa_def=="X629"|
                         causa_def=="X640"|
                         causa_def=="X642"|
                         causa_def=="X644"|
                         causa_def=="X645"|
                         causa_def=="X647"|
                         causa_def=="X648"|
                         causa_def=="X649"|
                         causa_def=="X650"|
                         causa_def=="X654"|
                         causa_def=="X659"|
                         causa_def=="X660"|
                         causa_def=="X664"|
                         causa_def=="X668"|
                         causa_def=="X669"|
                         causa_def=="X670"|
                         causa_def=="X680"|
                         causa_def=="X681"|
                         causa_def=="X684"|
                         causa_def=="X687"|
                         causa_def=="X688"|
                         causa_def=="X689"|
                         causa_def=="X690"|
                         causa_def=="X691"|
                         causa_def=="X694"|
                         causa_def=="X695"|
                         causa_def=="X696"|
                         causa_def=="X697"|
                         causa_def=="X698"|
                         causa_def=="X699"|
                         causa_def=="X700"|
                         causa_def=="X701"|
                         causa_def=="X702"|
                         causa_def=="X703"|
                         causa_def=="X704"|
                         causa_def=="X705"|
                         causa_def=="X706"|
                         causa_def=="X707"|
                         causa_def=="X708"|
                         causa_def=="X709"|
                         causa_def=="X710"|
                         causa_def=="X714"|
                         causa_def=="X718"|
                         causa_def=="X719"|
                         causa_def=="X720"|
                         causa_def=="X724"|
                         causa_def=="X727"|
                         causa_def=="X729"|
                         causa_def=="X730"|
                         causa_def=="X737"|
                         causa_def=="X740"|
                         causa_def=="X742"|
                         causa_def=="X744"|
                         causa_def=="X745"|
                         causa_def=="X746"|
                         causa_def=="X747"|
                         causa_def=="X748"|
                         causa_def=="X749"|
                         causa_def=="X760"|
                         causa_def=="X764"|
                         causa_def=="X769"|
                         causa_def=="X780"|
                         causa_def=="X781"|
                         causa_def=="X783"|
                         causa_def=="X784"|
                         causa_def=="X785"|
                         causa_def=="X786"|
                         causa_def=="X787"|
                         causa_def=="X788"|
                         causa_def=="X789"|
                         causa_def=="X800"|
                         causa_def=="X802"|
                         causa_def=="X804"|
                         causa_def=="X805"|
                         causa_def=="X808"|
                         causa_def=="X809"|
                         causa_def=="X815"|
                         causa_def=="X818"|
                         causa_def=="X819"|
                         causa_def=="X824"|
                         causa_def=="X828"|
                         causa_def=="X830"|
                         causa_def=="X834"|
                         causa_def=="X839"|
                         causa_def=="X840"|
                         causa_def=="X841"|
                         causa_def=="X842"|
                         causa_def=="X843"|
                         causa_def=="X844"|
                         causa_def=="X845"|
                         causa_def=="X848"|
                         causa_def=="X849",1,0),
         Capital_residencia=ifelse(ent_resid==31&mun_resid==50,"Mérida", #residencia habitual
                                   ifelse(ent_resid==1&mun_resid==1,"Aguascalientes",
                                    ifelse(ent_resid==2&mun_resid==2,"Mexicali",
                                    ifelse(ent_resid==3&mun_resid==3,"La Paz",
                                    ifelse(ent_resid==4&mun_resid==2,"Campeche",
                                    ifelse(ent_resid==5&mun_resid==30,"Saltillo",
                                    ifelse(ent_resid==6&mun_resid==2,"Colima",
                                    ifelse(ent_resid==7&mun_resid==101,"Tuxtla Gutiérrez",
                                    ifelse(ent_resid==8&mun_resid==19,"Chihuahua",
                                    ifelse(ent_resid==9&mun_resid==3,"Coyoacán",
                                    ifelse(ent_resid==10&mun_resid==5,"Durango",
                                    ifelse(ent_resid==11&mun_resid==15,"Guanajuato",
                                    ifelse(ent_resid==12&mun_resid==29,"Chilpancingo de los Bravo",
                                    ifelse(ent_resid==13&mun_resid==48,"Pachuca",
                                    ifelse(ent_resid==14&mun_resid==39,"Guadalajara",
                                    ifelse(ent_resid==15&mun_resid==106,"Toluca",
                                    ifelse(ent_resid==16&mun_resid==54,"Morelia",
                                    ifelse(ent_resid==17&mun_resid==7,"Cuernavaca",
                                    ifelse(ent_resid==18&mun_resid==17,"Tepic",
                                    ifelse(ent_resid==19&mun_resid==39,"Monterrey",
                                    ifelse(ent_resid==20&mun_resid==67,"Oaxaca",
                                    ifelse(ent_resid==21&mun_resid==114,"Puebla",
                                    ifelse(ent_resid==22&mun_resid==14,"Querétaro",
                                    ifelse(ent_resid==23&mun_resid==4,"Othón P. Blanco",
                                    ifelse(ent_resid==24&mun_resid==28,"San Luis Potosí",
                                    ifelse(ent_resid==25&mun_resid==6,"Culiacán",
                                    ifelse(ent_resid==26&mun_resid==30,"Hermosillo",
                                    ifelse(ent_resid==27&mun_resid==4,"Villahermosa",
                                    ifelse(ent_resid==28&mun_resid==41,"Victoria",
                                    ifelse(ent_resid==29&mun_resid==33,"Tlaxcala",
                                    ifelse(ent_resid==30&mun_resid==87,"Xalapa",
                                    ifelse(ent_resid==32&mun_resid==56,"Zacatecas","Ninguno")
                                                                                                                                                                                                    ))))))))))))))))))))))))))))))))        
##Variables a utilizar: ent_regis,ent_resid,mun_resid,loc_resid,ent_ocurr,mun_ocurr,loc_ocurr,causa_def,sexo,edad)
         


suicidio_entidad_fed<-defunciones2023 %>% 
  group_by(Capital_residencia) %>% 
  summarise(sum(Suicidio))


write.csv(suicidio_entidad_fed,"Suicidios por ciudad capital 2023.csv", fileEncoding = "latin1" )


suicidio_adolesc_capitales<-defunciones2023 %>% 
  filter(edad_agru==7|edad_agru==8) %>% #personas de 10 a 19 años de edad
  group_by(Capital_residencia) %>% 
  summarise(sum(Suicidio))    

write.csv(suicidio_adolesc_capitales,"Suicidios adolescentes por capitales 2023.csv", fileEncoding = "latin1" )
        
#_______________

suicidios_mun_yuc<-defunciones2023 %>% #según municipio de ocurrencia
  filter(ent_resid==31) %>% 
  group_by(mun_resid) %>% 
  summarise(sum(Suicidio))



write.csv(suicidios_mun_yuc,"Suicidios por municipio Yucatán2023.csv", fileEncoding = "latin1" )

##suicidio en juventudes
suicidio_juven<-defunciones2023 %>% 
  filter(edad_agru==9|edad_agru==10) %>% #personas de 20 a 29 años de edad
  group_by(ent_resid) %>% 
  summarise(sum(Suicidio)) 

write.csv(suicidio_juven,"Suicidios juventudes entidad fed 2023.csv", fileEncoding = "latin1" )

##__Por si me sirve más tarde: aquí es una lógica para registro por entidad

Capital_residencia=ifelse(ent_regis==31&ent_resid==31&mun_resid==50,"Mérida",
                          ifelse(ent_regis==1&ent_resid==1&mun_resid==1,"Aguascalientes",
                                 ifelse(ent_regis==2&ent_resid==2&mun_resid==2,"Mexicali",
                                        ifelse(ent_regis==3&ent_resid==3&mun_resid==3,"La Paz",
                                               ifelse(ent_regis==4&ent_resid==4&mun_resid==2,"Campeche",
                                                      ifelse(ent_regis==5&ent_resid==5&mun_resid==30,"Saltillo",
                                                             ifelse(ent_regis==6&ent_resid==6&mun_resid==2,"Colima",
                                                                    ifelse(ent_regis==7&ent_resid==7&mun_resid==101,"Tuxtla Gutiérrez",
                                                                           ifelse(ent_regis==8&ent_resid==8&mun_resid==19,"Chihuahua",
                                                                                  ifelse(ent_regis==9&ent_resid==9&mun_resid==3,"Coyoacán",
                                                                                         ifelse(ent_regis==10&ent_resid==10&mun_resid==5,"Durango",
                                                                                                ifelse(ent_regis==11&ent_resid==11&mun_resid==15,"Guanajuato",
                                                                                                       ifelse(ent_regis==12&ent_resid==12&mun_resid==29,"Chilpancingo de los Bravo",
                                                                                                              ifelse(ent_regis==13&ent_resid==13&mun_resid==48,"Pachuca",
                                                                                                                     ifelse(ent_regis==14&ent_resid==14&mun_resid==39,"Guadalajara",
                                                                                                                            ifelse(ent_regis==15&ent_resid==15&mun_resid==106,"Toluca",
                                                                                                                                   ifelse(ent_regis==16&ent_resid==16&mun_resid==54,"Morelia",
                                                                                                                                          ifelse(ent_regis==17&ent_resid==17&mun_resid==7,"Cuernavaca",
                                                                                                                                                 ifelse(ent_regis==18&ent_resid==18&mun_resid==17,"Tepic",
                                                                                                                                                        ifelse(ent_regis==19&ent_resid==19&mun_resid==39,"Monterrey",
                                                                                                                                                               ifelse(ent_regis==20&ent_resid==20&mun_resid==67,"Oaxaca",
                                                                                                                                                                      ifelse(ent_regis==21&ent_resid==21&mun_resid==114,"Puebla",
                                                                                                                                                                             ifelse(ent_regis==22&ent_resid==22&mun_resid==14,"Querétaro",
                                                                                                                                                                                    ifelse(ent_regis==23&ent_resid==23&mun_resid==4,"Othón P. Blanco",
                                                                                                                                                                                           ifelse(ent_regis==24&ent_resid==24&mun_resid==28,"San Luis Potosí",
                                                                                                                                                                                                  ifelse(ent_regis==25&ent_resid==25&mun_resid==6,"Culiacán",
                                                                                                                                                                                                         ifelse(ent_regis==26&ent_resid==26&mun_resid==30,"Hermosillo",
                                                                                                                                                                                                                ifelse(ent_regis==27&ent_resid==27&mun_resid==4,"Villahermosa",
                                                                                                                                                                                                                       ifelse(ent_regis==28&ent_resid==28&mun_resid==41,"Victoria",
                                                                                                                                                                                                                              ifelse(ent_regis==29&ent_resid==29&mun_resid==33,"Tlaxcala",
                                                                                                                                                                                                                                     ifelse(ent_regis==30&ent_resid==30&mun_resid==87,"Xalapa",
                                                                                                                                                                                                                                            ifelse(ent_regis==32&ent_resid==32&mun_resid==56,"Zacatecas","Ninguno")
                                                                                                                                                                                                                                            