library(tidyverse)

#Erikoiskasvien lannoitusaineisto:
#Kasveittainen lannoitus per ha, huomioiden eri lannoitetyypit

#Datataulut sisään

library(readxl)
Lannoitus <- read_excel("Data/Ravinnelaskennan_aineisto/Apetit_erikoiskasvien_lannoitus.xlsx", 
                                              sheet = "Kasvulohkon lannoitusrivi")

Sopimuskasvit<-read_excel("Data/Ravinnelaskennan_aineisto/Apetit_erikoiskasvien_lannoitus.xlsx", 
                          sheet = "Kasvulohko otsikko")
Sopimuskasvit[22:24]<-NULL

#Muuttujien karsinta.
 
Lannoitus<-Lannoitus %>% select(2,7:9)

Sopimuskasvit<- Sopimuskasvit %>% select(1,3,5:6)
colnames(Sopimuskasvit)[4]<-"Sopimuskasvi_nimi"

#Lannoitustaulussa ei kasvulohkokoodia, ainoastaan peruslohko
x<-merge(Sopimuskasvit, Lannoitus, by=c("Peruslohkonumero"),all=T) 

#Poistetaan rivit joissa tyhjät lannoitetiedot (ei matchia lohkonumeroiden pohjalta)

y<-x %>% filter(!is.na(`Lannoitemäärä per hehtaari`))

#Keskimääräinen kunkin lannoitetyypin käyttö kg/ha,  kasvi-  ja lannoitetyypeittäin 

Aggre<-y %>% group_by(Sopimuskasvi, Sopimuskasvi_nimi, Lannoitekoodi, Lannoiteteksti) %>% summarise(Lannoitetta_per_ha_keskimaarin = mean(`Lannoitemäärä per hehtaari`))

#Poistetaan eö

