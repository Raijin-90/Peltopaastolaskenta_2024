library(varhandle);library(here);library(tidyverse)

#Typen jako peltolohkoille. 
#Jaetaan sekä lannan että mineraalilannoitteen typpi. 
#Lannan käsittelyssä ja levityksen jälkeen tapahtuu typen hävikkiä ilmaan, nämä huomioitava myös. 

#' Source specific lines in an R file
#'
#' @param file character string with the path to the file to source.
#' @param lines numeric vector of lines to source in \code{file}.

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

#Hävikit? Sisältyvätkö jo lantakertoimiin. Kysytty juha grönroosilta 19052025

#MINERAALILANNOITUS: ERILLISESTÄ SKRIPTISTÄ. 

#Mineraalilannoite: ei koske luomulohkoja. Mineraalilannoituksen skriptissä tehdään  jaottelu luomuun ja normaaliin.
#Lisäksi skripti laskee mineraalilannoitteiden käytön typpikuorman kg per lohko, käyttäen kasvikohtaisia kertoimia jotka lasketaan erikseen 
#eläin- ja kasvitiloille. 

source(here("Skriptit/Ravinnelaskenta/mineraaliLannoitusRavinnelaskut.R"))

rm.all.but(c("Lohkoittainen_min_lann_typpi_elaintilat","Lohkoittainen_min_lann_typpi_kasvitilat","lohkot_luomuviljelyssä","tavanomaisen_viljelyn_lohkot"))

sum(Lohkoittainen_min_lann_typpi_elaintilat$Maannossumma)+sum(Lohkoittainen_min_lann_typpi_kasvitilat$Maannossumma)+sum(lohkot_luomuviljelyssä$Maannossumma)
(sum(Lohkoittainen_min_lann_typpi_kasvitilat$Mineraalilannoitteen_typpi)+sum(Lohkoittainen_min_lann_typpi_elaintilat$Mineraalilannoitteen_typpi))/1000

#LANNAN RAVINTEET

#Lannan ravinteet. Tätä käytetään sekä luomu- että "normaaleilla" lohkoilla
#Ravinnekilot laskettu excelissä, otetaan sisään. 

Lannan_ravinnelaskenta <- read_excel("Data/Ravinnelaskennan_aineisto/Lannan_ravinnelaskenta.xlsx", sheet = "Jako viljelyaloille", range = "A1:F27") 
Lannan_ravinnelaskenta<-Lannan_ravinnelaskenta %>% select(Tuotantosuunta, `Lannan typpi kg`)

#Jaetaan ravinnekilot tuotantosuunnittain viljelyalalle, tuloksena joka tuotantosuunnalle oma kasvista riippumaton kerroin kg typpeä/ha. 
Kaikki_lohkot<-rbind(lohkot_luomuviljelyssä, tavanomaisen_viljelyn_lohkot)
sum(Kaikki_lohkot$Maannossumma)

alat<-Kaikki_lohkot %>% group_by(Tuotantosuunta) %>% summarise(viljelyala=sum(Maannossumma))

Lannan_ravinnelaskenta<-inner_join(Lannan_ravinnelaskenta, alat, by="Tuotantosuunta")

sum(Lannan_ravinnelaskenta$`Lannan typpi kg`)
sum(Lannan_ravinnelaskenta$viljelyala)

#Typpikerroin: kg typpea/ha kokonaisalaa kullekin tuotantosuunnalle.                                    
Lantakertoimet<-Lannan_ravinnelaskenta %>% mutate(Lannan_typpi_kg_ha = `Lannan typpi kg`/viljelyala) %>% select(Tuotantosuunta, Lannan_typpi_kg_ha) 
rm(Lannan_ravinnelaskenta)

#Normilanta-järjestelmä huomioi varastoinnin typenhävikin vaan ei levityksessä aiheutuvaa.
#Dokumentaation mukaan 1.2% kokonaistypestä häviää ilmapäästöinä. 
#Pudotetaan kutakin kerrointa tämän verran. 

Lantakertoimet<-Lantakertoimet %>% mutate(Lannan_typpi_kg_ha = Lannan_typpi_kg_ha*0.988)









#Liitetään lohkoihin, sekä tavallisiin että luomu. 

Elaintilalohkot_ravinnekertoimet_tavallinen<-inner_join(Lohkoittainen_min_lann_typpi_elaintilat,Lantakertoimet, by="Tuotantosuunta")
rm(Lohkoittainen_min_lann_typpi_elaintilat)

Kasvitilalohkot_ravinnekertoimet_tavallinen<-inner_join(Lohkoittainen_min_lann_typpi_kasvitilat,Lantakertoimet, by="Tuotantosuunta")
rm(Lohkoittainen_min_lann_typpi_kasvitilat)

Luomulohkot_ravinnekertoimet<-inner_join(lohkot_luomuviljelyssä, Lantakertoimet, by="Tuotantosuunta")
rm(lohkot_luomuviljelyssä)

#Kerrotaan pinta-alaa (tässä vaiheessa kokonaisalaa) lantakertoimella. Lantakerroin on laskettu jakamalla lannan ravinne koko viljelylalle tuotantosuunnittain. 
#Tavallisen viljelyn lohkoille on jo laskettu mineraalilannoitteen ravinnekilot sen omassa skriptissä.  

Elaintilalohkot_ravinnekertoimet_tavallinen<-Elaintilalohkot_ravinnekertoimet_tavallinen %>% mutate(typpi_lannasta_kg = Lannan_typpi_kg_ha*Maannossumma)

Kasvitilalohkot_ravinnekertoimet_tavallinen<-Kasvitilalohkot_ravinnekertoimet_tavallinen %>% mutate(typpi_lannasta_kg = Lannan_typpi_kg_ha*Maannossumma)

Luomulohkot_ravinnekertoimet<-Luomulohkot_ravinnekertoimet %>% mutate(typpi_lannasta_kg = Lannan_typpi_kg_ha*Maannossumma)

#yhdistetään tavanomaisen viljelyn lohkot eläin- ja kasvitiloilta
Tavallinen_viljely_ravinteet<-rbind(Elaintilalohkot_ravinnekertoimet_tavallinen, Kasvitilalohkot_ravinnekertoimet_tavallinen)

rm(Elaintilalohkot_ravinnekertoimet_tavallinen, Kasvitilalohkot_ravinnekertoimet_tavallinen)

sum(Luomulohkot_ravinnekertoimet$typpi_lannasta_kg)+sum(Tavallinen_viljely_ravinteet$typpi_lannasta_kg) #76 282 552 kg on toivottu lantatypen totaali

#Luomulohkot-aliaineistoon ei tule ollenkaan mineraalilannoitteita. Niille tarvitaan kuitenkin dummymuuttujat datan kokoamista varten

Luomulohkot_ravinnekertoimet<-Luomulohkot_ravinnekertoimet %>% mutate(typpikerroin_kg_ha = 0,
                                        typpikerroin_org_maa_kg_ha=0,
                                        Mineraalilannoitteen_typpi = 0,                                 
                                        Mineraalilannoitteen_typpi_eloper_maa = 0
                               
                                               )
#Luomulohkot lopussa kiinni tavallisiin lohkoihin -> yhtenäinen lohkodata jossa mineraali- ja lantatyppijaettu lohkoille. 
lohkot_kaikki<-rbind(Tavallinen_viljely_ravinteet, Luomulohkot_ravinnekertoimet)

#summien tarkistus
sum(lohkot_kaikki$Maannossumma)
sum(lohkot_kaikki$typpi_lannasta_kg)
sum(lohkot_kaikki$Mineraalilannoitteen_typpi)

rm.all.but("lohkot_kaikki")

#TYPEN TARPEEN KATTAMINEN LANNALLA KOTIELÄINTILOILLA

#Mineraalilannoitteiden tarjonnan ja käytön erotus, joka katetaan mineraalilannoitteilla. 
#Typen tarjonta on laskettu Yatran tuotantotiedoista mikrodatan pohjalta Exceliin. 
#Mineraalilannoitteiden typen käyttö on laskettu Karin kertoimien pohjalta tähän dataan.  
#Erotus n. 13 000 tn

Lannalla_katettava<-(sum(lohkot_kaikki$Mineraalilannoitteen_typpi)- 158901000)
Lannalla_katettava/1000 #tonnit

x<-lohkot_kaikki %>% group_by(Tuotantosuunta, KASVIKOODI_lohkodata_reclass, KASVINIMI_reclass) %>% summarise(Lannan_typpi_kg= sum(typpi_lannasta_kg), 
                                                                                                          Miner_lannoitteen_typpi_kg =sum(Mineraalilannoitteen_typpi))
sum(x$Miner_lannoitteen_typpi_kg)
sum(x$Lannan_typpi_kg)




#ELÄINTILAT: VÄHENNYS
#Lasketaan eläiintiloille mineraalilannoitteen käyttömäärän pohjalta osuus lannalla katettavaa erotusta. Paljonko kunkin tuotantosuunnan mineraalilannoitekäyttöä pudptetaan?
#Vähennetään tämän erotuksen verran mineraalilannoitteiden käyttöä.

Animalfarms<-c("Hevostilat","Lammas- ja vuohitilat","Maitotilat", "Munatilat","Muut nautakarjatilat","Siipikarjatilat","Sikatilat","Turkistilat")

Jaettava_erotus<-x %>% filter(Tuotantosuunta %in% Animalfarms) %>% group_by(Tuotantosuunta) %>% summarise(Mineraalilann_typpi_kg =sum(Miner_lannoitteen_typpi_kg))
Jaettava_erotus<-Jaettava_erotus %>% mutate(Pros = Mineraalilann_typpi_kg/sum(Mineraalilann_typpi_kg)) %>% mutate(osuus_erotuksesta = Pros*Lannalla_katettava) 

#Tämän verran vähennetään mineraalilannoitteiden käyttöä eri tuotantosuunnilla. 
#Seuraavaksi tämä jaetaan tuotantosuuntien sisällä viljelykasveille. 

#Käyttö kaseittain tuotantosuunnan sisällä
Jakauma_kasvit<- x %>% filter(Tuotantosuunta %in% Animalfarms) %>% group_by(Tuotantosuunta, KASVIKOODI_lohkodata_reclass) %>% summarise(Mineraalilann_typpi_kg =sum(Miner_lannoitteen_typpi_kg))
#Käyttö yhteensä tuotantosuunnalla
Jakauma_ts<- x %>% filter(Tuotantosuunta %in% Animalfarms) %>% group_by(Tuotantosuunta) %>% summarise(Mineraalilann_typpi_kg_ts =sum(Miner_lannoitteen_typpi_kg))
 
Jakauma<-inner_join(Jakauma_kasvit, Jakauma_ts, by="Tuotantosuunta")

#Jaetaan kasvikohtainen käyttö tuotantosuunnan sisällä sen tuotantosuunnan käyttötotaalilla -> Jakauma
Jakauma$Pros<-Jakauma$Mineraalilann_typpi_kg/Jakauma$Mineraalilann_typpi_kg_ts

Elaintilojen_mineraalilannoite<-inner_join(Jakauma, Jaettava_erotus, by="Tuotantosuunta") %>% select(-Mineraalilann_typpi_kg.y, -Pros.y, -Mineraalilann_typpi_kg_ts)

Elaintilojen_mineraalilannoite<-Elaintilojen_mineraalilannoite %>% mutate(Erotus_kg = osuus_erotuksesta*Pros.x)

Elaintilojen_mineraalilannoite<-Elaintilojen_mineraalilannoite %>% mutate(Alennettu_mineraalilann_kaytto_kg = Mineraalilann_typpi_kg.x - Erotus_kg)
colnames(Elaintilojen_mineraalilannoite)[3:4]<-c("Mineraalilann_typpi_kg_alkup","Pros")

#KASVITILAT: KOROTUS
#Vastaavasti kasvitiloilla korotetaan erotuksen verran mineraalilannoitteiden käyttöä 

Jaettava_erotus<-x %>% filter(!(Tuotantosuunta %in% Animalfarms)) %>% group_by(Tuotantosuunta) %>% summarise(Mineraalilann_typpi_kg =sum(Miner_lannoitteen_typpi_kg))
Jaettava_erotus<-Jaettava_erotus %>% mutate(Pros = Mineraalilann_typpi_kg/sum(Mineraalilann_typpi_kg)) %>% mutate(osuus_erotuksesta = Pros*Lannalla_katettava) 

#Käyttö kaseittain tuotantosuunnan sisällä
Jakauma_kasvit<- x %>% filter(!(Tuotantosuunta %in% Animalfarms)) %>% group_by(Tuotantosuunta, KASVIKOODI_lohkodata_reclass) %>% summarise(Mineraalilann_typpi_kg =sum(Miner_lannoitteen_typpi_kg))
#Käyttö yhteensä tuotantosuunnalla
Jakauma_ts<- x %>% filter(!(Tuotantosuunta %in% Animalfarms)) %>% group_by(Tuotantosuunta) %>% summarise(Mineraalilann_typpi_kg_ts =sum(Miner_lannoitteen_typpi_kg))

Jakauma<-inner_join(Jakauma_kasvit, Jakauma_ts, by="Tuotantosuunta")


#Jaetaan kasvikohtainen käyttö tuotantosuunnan sisällä sen tuotantosuunnan käyttötotaalilla -> Jakauma
Jakauma$Pros<-Jakauma$Mineraalilann_typpi_kg/Jakauma$Mineraalilann_typpi_kg_ts

Kasvitilojen_mineraalilannoite<-inner_join(Jakauma, Jaettava_erotus, by="Tuotantosuunta") %>% select(-Mineraalilann_typpi_kg.y, -Pros.y, -Mineraalilann_typpi_kg_ts)

Kasvitilojen_mineraalilannoite<-Kasvitilojen_mineraalilannoite %>% mutate(Erotus_kg = osuus_erotuksesta*Pros.x)

#Korotetaan kasvitilojen mineraalilannoitteen käyttöä erotuksen verran
Kasvitilojen_mineraalilannoite<-Kasvitilojen_mineraalilannoite %>% mutate(Korotettu_mineraalilann_kaytto_kg = Mineraalilann_typpi_kg.x + Erotus_kg)
colnames(Kasvitilojen_mineraalilannoite)[3:4]<-c("Mineraalilann_typpi_kg_alkup","Pros")

#tarkistus: alkuperäisen sekä korotetun/alennetun käytön erotuksen oltava sama kuin Lannalla_katettava

sum(Elaintilojen_mineraalilannoite$Mineraalilann_typpi_kg_alkup)-sum(Elaintilojen_mineraalilannoite$Alennettu_mineraalilann_kaytto_kg) 

sum(Kasvitilojen_mineraalilannoite$Korotettu_mineraalilann_kaytto_kg)-sum(Kasvitilojen_mineraalilannoite$Mineraalilann_typpi_kg_alkup)

#Yhdistetään
colnames(Kasvitilojen_mineraalilannoite)[7]<-"Tasokorjattu_mineraalilannoitteen_typpi_kg"
colnames(Elaintilojen_mineraalilannoite)[7]<-"Tasokorjattu_mineraalilannoitteen_typpi_kg"

Mineraalilannoite<-rbind(Kasvitilojen_mineraalilannoite, Elaintilojen_mineraalilannoite)

rm.all.but(c("Mineraalilannoite","lohkot_kaikki"))

#Aggregoidaan lannan typpi tätä vastaavalle aggregointitasolle. 

lantatyppi<-lohkot_kaikki %>% group_by(Tuotantosuunta, KASVIKOODI_lohkodata_reclass) %>% summarise(Lannan_typpi_kg = sum(typpi_lannasta_kg))

#Yhdistetään mineraalilannoitteeseen. Lanta on jaettu sen generoiville tuotantosuunnille, joten tyä

Liitos<-inner_join(lantatyppi,
Mineraalilannoite, by=c("Tuotantosuunta","KASVIKOODI_lohkodata_reclass"))


write.xlsx(Liitos, file="Ennakot.xlsx")
