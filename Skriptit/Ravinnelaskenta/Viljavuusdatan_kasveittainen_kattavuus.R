library(tidyverse);library(here);library(varhandle)

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

#Vertaillaan kasvien tarkkuudella Viljavuusdatan kattavuutta peltolohkodataan (GTK-maalaji) nähden

#Prosessoidaan datat lohkotasolle asti, virheenkorjaukset yms, vaan ei aggregoida

#GTK-viljelyaladatan prosessointi
source_lines(here("Skriptit/Uudet skriptit/GTK_datan_aggregointi.R"), 1:260)
rm.all.but("GTKdata")

#Viljavuusdatan prosessointi. 
source_lines(here("Skriptit/Uudet skriptit/Viljavuus_esikasittely_aggregointi.R"),1:240)

rm.all.but(c("GTKdata","Viljavuus_aggregointi_multavuus"))

#Määritellään viljavuusdatan lohkojen eloperäisyys-mineraalisuus sen perusteella, mikä multavuusmäärityksen tulos niillä on. Turvemaa ja mm (multamaa) eloperäistä, muut mineraalia. 

source_lines(here("Skriptit/Uudet skriptit/Viljavuus_esikasittely_aggregointi.R"),250:266)

#Aggregoidaan kasvitasolle
#Tavoitteena verrata kasveittain, kuinka hyvin pienempi viljavuusdata kattaa kokonaisen viljelyala-aineiston. 


GTK_kasvit<-GTKdata %>% group_by(KASVIKOODI_lohkodata_reclass,KASVINIMI_reclass) %>% summarise(across(c("Maannossumma","Eloperaista","Mineraalia"),sum))

Viljavuus_kasvit<-Viljavuus_aggregointi_multavuus %>% group_by(KASVITUNNU_reclass,KASVINIMI_reclass, Elop_ineral) %>% summarise(across(c(Maalajisumma),sum)) 
