library(here);library(tidyverse)

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

#Mineraalilannoite: ei koske luomulohkoja. Mineraalilannoituksen skriptissä tehdään  jaottelu luomuun ja normaaliin.
#Lisäksi skripti laskee mineraalilannoitteiden käytön typpikuorman per lohko, käyttäen kasvikohtaisia kertoimia jotka lasketaan erikseen 
#eläin- ja kasvitiloille. 

source(here("Skriptit/Ravinnelaskenta/mineraaliLannoitusRavinnelaskut.R"))

rm.all.but(c("Lohkoittainen_min_lann_typpi_elaintilat","Lohkoittainen_min_lann_typpi_kasvitilat","lohkot_luomuviljelyssä","tavanomaisen_viljelyn_lohkot"))

sum(Lohkoittainen_min_lann_typpi_elaintilat$Maannossumma)+sum(Lohkoittainen_min_lann_typpi_kasvitilat$Maannossumma)+sum(lohkot_luomuviljelyssä$Maannossumma)
(sum(Lohkoittainen_min_lann_typpi_kasvitilat$Mineraalilannoitteen_typpi)+sum(Lohkoittainen_min_lann_typpi_elaintilat$Mineraalilannoitteen_typpi))/1000

#Lannan ravinteet. Tätä käytetään sekä luomu- että "normaaleilla" lohkoilla

Lannan_ravinnelaskenta <- read_excel("Data/Ravinnelaskennan_aineisto/Lannan_ravinnelaskenta.xlsx", sheet = "Jako viljelyaloille", range = "A1:F27") 
Lannan_ravinnelaskenta<-Lannan_ravinnelaskenta %>% select(Tuotantosuunta, `Lannan typpi kg`)

#Jaetaan ravinnekilot viljelyalalle, tuloksena joka tuotantosuunnalle oma kasvista riippumaton kerroin. 
Kaikki_lohkot<-rbind(lohkot_luomuviljelyssä, tavanomaisen_viljelyn_lohkot)
sum(Kaikki_lohkot$Maannossumma)

alat<-Kaikki_lohkot %>% group_by(Tuotantosuunta) %>% summarise(viljelyala=sum(Maannossumma))

Lannan_ravinnelaskenta<-inner_join(Lannan_ravinnelaskenta, alat, by="Tuotantosuunta")

sum(Lannan_ravinnelaskenta$`Lannan typpi kg`)
sum(Lannan_ravinnelaskenta$viljelyala)

#Typpikerroin: kg typpea/ha kokonaisalaa kullekin tuotantosuunnalle.                                    
Lantakertoimet<-Lannan_ravinnelaskenta %>% mutate(Lannan_typpi_kg_ha = `Lannan typpi kg`/viljelyala) %>% select(Tuotantosuunta, Lannan_typpi_kg_ha) 
rm(Lannan_ravinnelaskenta)



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

Tavallinen_viljely_ravinteet<-rbind(Elaintilalohkot_ravinnekertoimet_tavallinen, Kasvitilalohkot_ravinnekertoimet_tavallinen)

rm(Elaintilalohkot_ravinnekertoimet_tavallinen, Kasvitilalohkot_ravinnekertoimet_tavallinen)

sum(Luomulohkot_ravinnekertoimet$typpi_lannasta_kg)+sum(Tavallinen_viljely_ravinteet$typpi_lannasta_kg)


#Luomulohkot-aliaineistoon ei tule ollenkaan mineraalilannoitteita. Niille tarvitaan kuitenkin dummymuuttujat datan kokoamista varten

Luomulohkot_ravinnekertoimet<-Luomulohkot_ravinnekertoimet %>% mutate(typpikerroin_kg_ha = 0,
                                        typpikerroin_org_maa_kg_ha=0,
                                        Mineraalilannoitteen_typpi = 0,                                 
                                        Mineraalilannoitteen_typpi_eloper_maa = 0
                                      )
lohkot_kaikki<-rbind(Tavallinen_viljely_ravinteet, Luomulohkot_ravinnekertoimet)


sum(lohkot_kaikki$Maannossumma)



