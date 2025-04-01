library(tidyverse);library(here);library(varhandle);library(openxlsx)

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}
data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAbElEQVR4Xs2RQQrAMAgEfZgf7W9LAguybljJpR3wEse5JOL3ZObDb4x1loDhHbBOFU6i2Ddnw2KNiXcdAXygJlwE8OFVBHDgKrLgSInN4WMe9iXiqIVsTMjH7z/GhNTEibOxQswcYIWYOR/zAjBJfiXh3jZ6AAAAAElFTkSuQmCC
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


#Kari Koppelmäen kertoimissa joilla lannoitteiden sisältämät ravinteet lasketaan, tulee luultavasti olemaan tarkan maalajoluokan vaativia eroja (esim. hiesumaat yms). Aggregointi pitää silloin tehdä käyttäen datan alkuperäisiä
#maalajiluokka-prosentteja (hiesu% ym). Multavuuden mukaan määrittelemällä ei päästä kuin erottamaan mineraalit mullasta&turpeesta. 

#Aggregoidaan kasvitasolle
#Tavoitteena verrata kasveittain, kuinka hyvin pienempi viljavuusdata kattaa kokonaisen viljelyala-aineiston. 

#Kasvinimikkeissä voi olla kirjoituseroja datojen kesken ("monivuotinen" VS "monivuot.")
#Siksi aggregoidaan koodilla, liitetään nimi

GTK_kasvit<-GTKdata %>% group_by(KASVIKOODI_lohkodata_reclass) %>% summarise(across(c("Maannossumma"),sum))
colnames(GTK_kasvit)[1:2]<-c("Kasvikoodi","Pinta_ala_gtk")

Viljavuus_kasvit<-Viljavuus_aggregointi_multavuus %>% group_by(KASVITUNNU_reclass) %>% summarise(across(c(Maalajisumma),sum)) 
colnames(Viljavuus_kasvit)[1:2]<-c("Kasvikoodi","Pinta_ala_viljavuus")


#Nämä yhdistetään, nollat sallitaan x-framesta 

kasveittainenKattavuus<-full_join(GTK_kasvit, Viljavuus_kasvit, by=c("Kasvikoodi"))
rm.all.but(c("kasveittainenKattavuus","GTKdata","Viljavuus_aggregointi_multavuus"))

kasveittainenKattavuus[is.na(kasveittainenKattavuus)]<-0

#Kasvien nimet

library(readxl)
Kasvikategoriat_avain <- read_excel("Data/Kasvikategoriat_avain.xlsx")
Kasvikategoriat_avain<-Kasvikategoriat_avain %>% select(Kasvikoodi, Kasvi)

kasveittainenKattavuus<-left_join(kasveittainenKattavuus, Kasvikategoriat_avain, by="Kasvikoodi")

sum(kasveittainenKattavuus$Pinta_ala_viljavuus)
sum(kasveittainenKattavuus$Pinta_ala_gtk)


#kattavuus%: kuinka iso osa gtk-datan kokonaismäärästä kasvia (kattavampi data) löytyy viljavuuspuolelta

kasveittainenKattavuus<-kasveittainenKattavuus %>% mutate(Viljavuusdatan_kattavuusprosentti = (Pinta_ala_viljavuus/Pinta_ala_gtk)*100 )

kasveittainenKattavuus<- kasveittainenKattavuus %>% select(Kasvikoodi, Kasvi, Pinta_ala_gtk, Pinta_ala_viljavuus, Viljavuusdatan_kattavuusprosentti)

Output<-createWorkbook()
Output %>% addWorksheet("Kattavuusdata_gtk_viljav_kasvit")
writeData(Output, "Kattavuusdata_gtk_viljav_kasvit", kasveittainenKattavuus)
saveWorkbook(Output,here("Output/Ravinnedata/Kattavuusvertailu_kasvit_gtk_viljavuus.xlsx"),overwrite = T)

#TUOTANTOSUUNNITTAINEN KATTAVUUS

#Samanlainen laskelma, mutta aggregoidaan datat tuotantosuunnan tasolle



GTK_ts<-GTKdata %>% group_by(Tuotantosuunta) %>% summarise(across(c("Maannossumma"),sum))
colnames(GTK_ts)[1:2]<-c("Tuotantosuunta","Pinta_ala_gtk")

Viljavuus_ts<-Viljavuus_aggregointi_multavuus %>% group_by(Tuotantosuunta) %>% summarise(across(c(Maalajisumma),sum)) 
colnames(Viljavuus_ts)[1:2]<-c("Tuotantosuunta","Pinta_ala_viljavuus")


#Nämä yhdistetään, nollat sallitaan x-framesta 

tuotsuuntaKattavuus<-full_join(GTK_ts, Viljavuus_ts, by=c("Tuotantosuunta"))
rm.all.but(c("tuotsuuntaKattavuus","GTKdata","Viljavuus_aggregointi_multavuus"))

tuotsuuntaKattavuus[is.na(tuotsuuntaKattavuus)]<-0

sum(tuotsuuntaKattavuus$Pinta_ala_viljavuus)
sum(tuotsuuntaKattavuus$Pinta_ala_gtk)


#kattavuus%: kuinka iso osa gtk-datan kokonaismäärästä kasvia (kattavampi data) löytyy viljavuuspuolelta

tuotsuuntaKattavuus<-tuotsuuntaKattavuus %>% mutate(Viljavuusdatan_kattavuusprosentti = (Pinta_ala_viljavuus/Pinta_ala_gtk)*100 )

tuotsuuntaKattavuus<- tuotsuuntaKattavuus %>% select(Tuotantosuunta, Pinta_ala_gtk, Pinta_ala_viljavuus, Viljavuusdatan_kattavuusprosentti)

Output<-createWorkbook()
Output %>% addWorksheet("Viljavdata_kattavuus_tuotsuunta")
writeData(Output, "Viljavdata_kattavuus_tuotsuunta", tuotsuuntaKattavuus)
saveWorkbook(Output,here("Output/Ravinnedata/Kattavuus_tuotantosuunta_gtk_viljavuus.xlsx"),overwrite = T)





