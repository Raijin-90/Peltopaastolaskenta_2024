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

#MINERAALILANNOITUS: ERILLISESTÄ SKRIPTISTÄ. 

#Mineraalilannoite: ei koske luomulohkoja. Mineraalilannoituksen skriptissä tehdään  jaottelu luomuun ja normaaliin.
#Lisäksi skripti laskee mineraalilannoitteiden käytön typpikuorman kg per lohko, käyttäen kasvikohtaisia kertoimia jotka lasketaan erikseen 
#eläin- ja kasvitiloille. Saadaan siis kaksi erillistä kerrointa. 

source(here("Skriptit/Ravinnelaskenta/mineraaliLannoitusRavinnelaskut.R"))

rm.all.but(c("Lohkoittainen_min_lann_typpi_elaintilat","Lohkoittainen_min_lann_typpi_kasvitilat","lohkot_luomuviljelyssä","tavanomaisen_viljelyn_lohkot"))

sum(Lohkoittainen_min_lann_typpi_elaintilat$Maannossumma)+sum(Lohkoittainen_min_lann_typpi_kasvitilat$Maannossumma)+sum(lohkot_luomuviljelyssä$Maannossumma)
(sum(Lohkoittainen_min_lann_typpi_kasvitilat$Mineraalilannoitteen_typpi)+sum(Lohkoittainen_min_lann_typpi_elaintilat$Mineraalilannoitteen_typpi))/1000

#LANNAN RAVINTEET

#Lannan ravinteet. Tätä käytetään sekä luomu- että "normaaleilla" lohkoilla
#Ravinnekilot laskettu excelissä, otetaan sisään. Näissä arvoissa on mukana N20:n hävikki, mutta ei vielä NO:n vastaavaa. 

Lannan_ravinnelaskenta <- read_excel("Data/Ravinnelaskennan_aineisto/Lannan_ravinnelaskenta.xlsx", sheet = "Jako viljelyaloille", range = "A1:I27") 

Lantatyppi_ei_hävikkiä<-sum(Lannan_ravinnelaskenta$`Lannan typpi kg`)

Lannan_ravinnelaskenta<-Lannan_ravinnelaskenta %>% select(Tuotantosuunta,Lannan_typpi_N2O_levityshavikki_huomioitu_kg)

Lantasumma_N2Ohaviolla<-sum(Lannan_ravinnelaskenta$Lannan_typpi_N2O_levityshavikki_huomioitu_kg)

#Jaetaan ravinnekilot tuotantosuunnittain viljelyalalle, tuloksena joka tuotantosuunnalle oma kasvista riippumaton kerroin kg typpeä/ha. 
Kaikki_lohkot<-rbind(lohkot_luomuviljelyssä, tavanomaisen_viljelyn_lohkot)
sum(Kaikki_lohkot$Maannossumma)

#Grassland-tyypeissä on kasveja, joille ei tule mineraalilannoitusta (kerroin nolla). Koska niitä ei lannoiteta, ei laiteta niille myöskään lantaa. 
#Jos lannan levitysmäärä kesannoille ym.Muu peltoala-kasvityypeille joilla myöskään mineraalilannoitusta ei ole (kerroin = 0) halutaan nollata, 
#se suodatus on tehtävä ylläoleviin Kaikki_lohkot aloihin. Lannan ravinnetonnit loppusummana eivät muutu, koska pohjaavat eläinten määrään eikä pinta-alaan. 
#Koskee oheisia kasvikoodeja, jotka eritelty Kasvilista_lannoitus.xlsx tiedostoon tunnisteella "Ei lannoiteta" = 1, ETTL "muu peltoala". 

Ei_lantaa <-c(6050,
6051,
6220,
6300,
6600,
6710,
6720,
9101,
9102,
9403,
9404,
9405,
9412,
9413,
9422,
9423,
9424,
9620,
9700,
9801,
9802,
9803,
9804,
9805,
9806,
9807,
9808,
9810,
9811,
9812,
9820,
9830)

#Muutos 22/8/2025:
#Näiltä lohkoilta tuleva luonnonhuutouma lasketaan erillisistä kertoimista jotka kuvaavat valuma-alueita ilman ihmisvaikutusta (Mattson et al 2003). 
#Hylätylle peltomaalle spesifiä tällaista kerrointa ei ole.
#Allokoitavaa totaalia typen ja fosforin osalta vähennetään tämän verran. 

Luonnonhuuhtouma<-Kaikki_lohkot %>% filter((KASVIKOODI_lohkodata_reclass %in% Ei_lantaa)) 

luonnHuuht_N <- (140/100) #140 kg/km2 typpeä -> kg/ha muunto

luonnHuuht_P <- (5.4/100) #5.4 kg/km2 typpeä -> kg/ha muunto

LH_summat_alat<-Luonnonhuuhtouma %>%  mutate(Luonnonhuuhtouman_typpi = Maannossumma*luonnHuuht_N,
                            Luonnonhuuhtouman_fosfori = Maannossumma*luonnHuuht_P ) %>% group_by(Tuotantosuunta) %>% summarise(Luonnonhuuhtouman_P = sum(Luonnonhuuhtouman_fosfori),
                                                                                                  Luonnonhuuhtouman_typpi = sum(Luonnonhuuhtouman_typpi),
                                                                                                  Hehtaarit=sum(Maannossumma)) 

#Jatketaan laskentaa

#Poissuljetaan ne, joille lantaa ei laiteta
Kaikki_lohkot<-Kaikki_lohkot %>% filter(!(KASVIKOODI_lohkodata_reclass %in% Ei_lantaa)) 

alat<-Kaikki_lohkot %>% group_by(Tuotantosuunta) %>% summarise(viljelyala=sum(Maannossumma)) 

Lannan_ravinnelaskenta <-inner_join(Lannan_ravinnelaskenta, alat, by="Tuotantosuunta")

sum(Lannan_ravinnelaskenta$Lannan_typpi_N2O_levityshavikki_huomioitu_kg)
sum(Lannan_ravinnelaskenta$viljelyala)

#Typpikerroin: kg typpea/ha kokonaisalaa kullekin tuotantosuunnalle.                                    
Lantakertoimet<-Lannan_ravinnelaskenta %>% mutate(Lannan_typpi_kg_ha = Lannan_typpi_N2O_levityshavikki_huomioitu_kg/viljelyala) %>% select(Tuotantosuunta, Lannan_typpi_kg_ha) 
rm(Lannan_ravinnelaskenta)

#Normilanta-järjestelmä huomioi varastoinnin typenhävikin vaan ei levityksessä aiheutuvaa.
#Dokumentaation mukaan 1.2% kokonaistypestä häviää typpioksidin (NO) ilmapäästöinä. 
#Pudotetaan kutakin kerrointa tämän verran. 

Lantakertoimet<-Lantakertoimet %>% mutate(Lannan_typpi_kg_ha = Lannan_typpi_kg_ha*0.988)

#Lisäksi N2O:na häviää levitetystä lannasta n. 1,19 kilotonnia N2O, joka vastaa n.  723 tn typpeä. 
#Perustuu CRF 3C "N input from manure applied to soils" arvoon. 
#Tämä otetaan huomioon excelissä. N2O kilotonneista on laskettu
#typen osuus siinä. Jaettavaa lannan totaalia vähennetään tämä määrä.   

#Liitetään lohkoihin, sekä tavallisiin että luomu. 

Lohkoittainen_min_lann_typpi_elaintilat<-Lohkoittainen_min_lann_typpi_elaintilat %>% filter(!(KASVIKOODI_lohkodata_reclass %in% Ei_lantaa))
Elaintilalohkot_ravinnekertoimet_tavallinen<-inner_join(Lohkoittainen_min_lann_typpi_elaintilat,Lantakertoimet, by="Tuotantosuunta")
rm(Lohkoittainen_min_lann_typpi_elaintilat)

Lohkoittainen_min_lann_typpi_kasvitilat<-Lohkoittainen_min_lann_typpi_kasvitilat %>% filter(!(KASVIKOODI_lohkodata_reclass %in% Ei_lantaa))
Kasvitilalohkot_ravinnekertoimet_tavallinen<-inner_join(Lohkoittainen_min_lann_typpi_kasvitilat,Lantakertoimet, by="Tuotantosuunta")
rm(Lohkoittainen_min_lann_typpi_kasvitilat)

lohkot_luomuviljelyssä <-lohkot_luomuviljelyssä %>% filter(!(KASVIKOODI_lohkodata_reclass %in% Ei_lantaa)) 
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

sum(Luomulohkot_ravinnekertoimet$typpi_lannasta_kg)+sum(Tavallinen_viljely_ravinteet$typpi_lannasta_kg) #76 282 552 kg on toivottu lantatypen totaali JOS HÄVIKKIÄ EI HUOMIOIDA.

Lantasumma_NO_N2O_havikit_poistettu<-sum(sum(Luomulohkot_ravinnekertoimet$typpi_lannasta_kg)+sum(Tavallinen_viljely_ravinteet$typpi_lannasta_kg))
Havikki_yhteensa<-Lantatyppi_ei_hävikkiä-Lantasumma_NO_N2O_havikit_poistettu

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

sum(LH_summat_alat$Hehtaarit)+sum(alat$viljelyala)

rm.all.but(c("lohkot_kaikki", "LH_summat_alat", "alat","luonnonhuuhtouma"))

#TYPEN TARPEEN KATTAMINEN LANNALLA KOTIELÄINTILOILLA

#Mineraalilannoitteiden tarjonnan ja käytön erotus, joka katetaan lannalla 
#Typen tarjonta (1 589 01000 kg eli 158901 tn) on laskettu Yaran tuotantotiedoista (YLVA) Exceliin.
#Sisältää NPK-lannoitteen sekä spesifin typpilannoitteen (pieni virta)
#https://sykeintra.sharepoint.com/sites/msteams_8cb833_427324/Shared%20Documents/ENVIMATfood/K%C3%A4sikirjoitus_maatalouden_ravinnep%C3%A4%C3%A4st%C3%B6t/Typpi/Typpilannoitus_lannan_typpi.xlsx?web=1
#Mineraalilannoitteiden typen käyttö on laskettu Karin kertoimien pohjalta nyt R:ssä käsiteltävään dataan.  
#Erotus n. 13 000 tn

Lannalla_katettava<-(sum(lohkot_kaikki$Mineraalilannoitteen_typpi)- 158901000) #KILOJA!
Lannalla_katettava/1000 #tonnit

x<-lohkot_kaikki %>% group_by(Tuotantosuunta, KASVIKOODI_lohkodata_reclass, KASVINIMI_reclass) %>% summarise(Lannan_typpi_kg= sum(typpi_lannasta_kg), 
                                                                                                          Miner_lannoitteen_typpi_kg =sum(Mineraalilannoitteen_typpi))
sum(x$Miner_lannoitteen_typpi_kg)
sum(x$Lannan_typpi_kg)

#Tämän operaation tekemiseksi tilatasolla on erillinen skripti. 
#Tässä allokointi tehdään suoraan tuotantosuuntatasolle. 


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

rm.all.but(c("Mineraalilannoite","lohkot_kaikki","LH_summat_alat","alat"))

#Aggregoidaan lannan typpi tätä vastaavalle aggregointitasolle. 

lantatyppi<-lohkot_kaikki %>% group_by(Tuotantosuunta, KASVIKOODI_lohkodata_reclass) %>% summarise(Lannan_typpi_kg = sum(typpi_lannasta_kg))

#Yhdistetään mineraalilannoitteeseen. Lanta on jaettu sen generoiville tuotantosuunnille

Liitos<-inner_join(lantatyppi,
Mineraalilannoite, by=c("Tuotantosuunta","KASVIKOODI_lohkodata_reclass"))


Tulokset<-createWorkbook()
addWorksheet(Tulokset, "Typpitulokset")
addWorksheet(Tulokset,"Luonnonhuuhtouma_alat_summat")
addWorksheet(Tulokset,"Lannoitettu_ala")
writeData(Tulokset, "Typpitulokset", Liitos)
writeData(Tulokset, "Luonnonhuuhtouma_alat_summat", LH_summat_alat)
writeData(Tulokset, "Lannoitettu_ala", alat)
saveWorkbook(Tulokset, file=here("Output/Ravinnedata/Typpi_tuotantosuunnittain_tulokset_Muu_peltoala_muutettuna.xlsx"), overwrite = T)

#Bonari: samasta viljelyaladatasta (lohkot_kaikki) saadaan satomäärät ja suhteutus ravinteiden käyttö per sato
#Tämä ei onnistu kuin niille kasveille, joille ylipäänsä on satotieto. 

library(readxl)
Satokertoimet <- read_excel("Data/Satokertoimet.xlsx")
colnames(Satokertoimet)[1]<-"KASVIKOODI_lohkodata_reclass"
Satokertoimet$KASVIKOODI_lohkodata_reclass<-as.double(Satokertoimet$KASVIKOODI_lohkodata_reclass)

Lohkot_sato<-left_join(lohkot_kaikki, Satokertoimet, by="KASVIKOODI_lohkodata_reclass")

Lohkot_sato_aggre<-Lohkot_sato %>% 
  filter(!is.na(Hehtaarisato_tonnia_ha)) %>% 
  mutate(Satotonnit = Maannossumma*Hehtaarisato_tonnia_ha) %>%
  group_by(KASVINIMI_reclass,KASVIKOODI_lohkodata_reclass) %>% summarise(Satotonnit = sum(Satotonnit))

Satotonnitaulukko<-createWorkbook()
addWorksheet(Satotonnitaulukko,"Sadot")
writeData(Satotonnitaulukko, "Sadot", Lohkot_sato_aggre)
saveWorkbook(Satotonnitaulukko, file=here("Output/Ravinnedata/Sadot.xlsx"), overwrite = T)
