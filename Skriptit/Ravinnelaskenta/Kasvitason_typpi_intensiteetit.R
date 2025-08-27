#Lasketaan tarkimman kasvitason mukainen satotasoon suhteutettu ravinne-intensiteetti. 
#Skripti on lähes 1:1 sama kuin Tilatason_typpi_intensiteetit_hajonnat.R, eroten lähinnä aggregointitasojen suhteen sekä satotonnien käytössä. 

library(tidyverse);library(here);library(openxlsx)

#' Source specific lines in an R file
#'
#' @param file character string with the path to the file to source.
#' @param lines numeric vector of lines to source in \code{file}.

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

#Skripti on lähes 1:1 sama kuin Tilatason_typpi_intensiteetit_hajonnat.R, eroten lähinnä aggregointitasojen suhteen sekä satotonnien käytössä.

source_lines(here("Skriptit/Ravinnelaskenta/Tilatason_typpi_intensiteetit_hajonnat.R"), 1:30)

#Eläintilojen mineraalilannoitteiden käytön tasokorjaus alaspäin, ja käänteisesti kasvitiloille, tehdään samalla tavoin kuin tilatyyppiskriptissäkin. 
#ELÄINTILAT: 

a<-lohkot_kaikki %>% filter(Tuotantosuunta %in% Animalfarms) %>% group_by(Tuotantosuunta, KASVIKOODI_lohkodata_reclass, KASVINIMI_reclass) %>% summarise(
  Mineraalilannoitteen_typpi = sum(Mineraalilannoitteen_typpi),
  typpi_lannasta_kg = sum(typpi_lannasta_kg)
)

#Mineraalilannoitteen N käytön jakauma kunkin tuotantosuunnan  sisällä 
a<-a %>% group_by(Tuotantosuunta) %>% mutate(Pros=Mineraalilannoitteen_typpi/sum(Mineraalilannoitteen_typpi))

#Kunkin tuotantosuunnan mineraalilannoite-typen käyttöä lasketaan yhteensä tämän verran
Jaettava_erotus<-Jaettava_erotus %>% select(Tuotantosuunta, osuus_erotuksesta)
#Allokoidaan kasveille kunkin tuotantosuunnan sisällä mineraalilannoitteen typen jakaumalla

a<-inner_join(a, Jaettava_erotus)
a<-a %>% mutate(Tasokorjattu_mineraalilannoitteen_typpi = Mineraalilannoitteen_typpi-(Pros*osuus_erotuksesta))


#KASVITILAT: KOROTUS
#Vastaavasti kasvitiloilla korotetaan erotuksen verran mineraalilannoitteiden käyttöä 

source_lines(here("Skriptit/Ravinnelaskenta/Ravinneallokointi_lohkoittain_lanta_lannoite_yhdistäminen.R"),230:235)


b<-lohkot_kaikki %>% filter(!(Tuotantosuunta %in% Animalfarms)) %>% group_by(Tuotantosuunta, KASVIKOODI_lohkodata_reclass, KASVINIMI_reclass) %>% summarise(
  Mineraalilannoitteen_typpi = sum(Mineraalilannoitteen_typpi),
  typpi_lannasta_kg = sum(typpi_lannasta_kg)
)

b<-b %>% group_by(Tuotantosuunta) %>% mutate(Pros=Mineraalilannoitteen_typpi/sum(Mineraalilannoitteen_typpi))


Jaettava_erotus<-Jaettava_erotus %>% select(Tuotantosuunta, osuus_erotuksesta)

b<-inner_join(b, Jaettava_erotus)
b<-b %>% mutate(Tasokorjattu_mineraalilannoitteen_typpi = Mineraalilannoitteen_typpi+(Pros*osuus_erotuksesta))

#Nämä voidaan nyt yhdistää riveittäin takaisin. 

c<-rbind(a,b)


#Summien tarkistus
sum(c$Mineraalilannoitteen_typpi)
sum(lohkot_kaikki$Mineraalilannoitteen_typpi)

sum(c$typpi_lannasta_kg)
sum(lohkot_kaikki$typpi_lannasta_kg)

sum(c$Tasokorjattu_mineraalilannoitteen_typpi)

sum(c$Tasokorjattu_mineraalilannoitteen_typpi)+sum(c$typpi_lannasta_kg)

rm(a,b,alat,Jaettava_erotus)



#Muunto intensiteeteiksi (ravinnemassat per hehtaari)
#Tässä alassa ei ole mukana lannoittamatonta grassland-alaa. 

gc()

lannoitettuAla<-lohkot_kaikki %>% group_by(Tuotantosuunta, KASVIKOODI_lohkodata_reclass, KASVINIMI_reclass) %>% summarise(Lannoitettu_viljelyala = sum(Maannossumma))    

Alat_ravinteet_kasveittain<-inner_join(c, lannoitettuAla, by =c("Tuotantosuunta", "KASVIKOODI_lohkodata_reclass", "KASVINIMI_reclass"))

#Alojen tarkistus
sum(Alat_ravinteet_kasveittain$Lannoitettu_viljelyala)+sum(LH_summat_alat$Hehtaarit)

#ETOL-koodit

library(readxl)
Muuntoavain_tuotantosuunnat_tuotteet_ETOL <- read_excel("Data/Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx")
colnames(Muuntoavain_tuotantosuunnat_tuotteet_ETOL)[1]<-"Tuotantosuunta"

rm.all.but(c("Alat_ravinteet_kasveittain","Muuntoavain_tuotantosuunnat_tuotteet_ETOL","LH_summat_alat"))

Alat_ravinteet_kasveittain<-Alat_ravinteet_kasveittain %>%   mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                                                             Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                                                                             Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                                                                             Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                                                                             Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                                                                             Tuotantosuunta == "Tattari ja kinoa"  ~ "Tattari_kinoa",
                                                                                             Tuotantosuunta == "Vihannekset ja juurekset"  ~ "Vihannekset_juurekset",
                                                                                             Tuotantosuunta == "Viljat pl. ohra"   ~ "Viljat_pl_ohra",
                                                                                             .default = Tuotantosuunta))

Alat_ravinteet_kasveittain<-inner_join(Alat_ravinteet_kasveittain, Muuntoavain_tuotantosuunnat_tuotteet_ETOL, by="Tuotantosuunta")

#Alojen tarkistus
sum(Alat_ravinteet_kasveittain$Lannoitettu_viljelyala)+sum(LH_summat_alat$Hehtaarit)

#Summataan lanta- ja lannoitetyppi
Alat_ravinteet_kasveittain<-Alat_ravinteet_kasveittain %>% mutate(Typpi_yhteensa = Tasokorjattu_mineraalilannoitteen_typpi+typpi_lannasta_kg)

#Jaetaan lannoittamattoman alan luonnonhuuhtoumatyppi kasveille
#Kunkin tilatyypin lannoittamattoman alan luonnonhuuhtoumatyppi saadaan LH_summat_alat framesta. Jaetaan typen käytön suhteessa tilatyypin tiloille, vähennetään typpitotaalista. 

sum(Alat_ravinteet_kasveittain$Lannoitettu_viljelyala)+sum(LH_summat_alat$Hehtaarit)

LH_summat_alat<-LH_summat_alat %>% select(1,3,4)
colnames(LH_summat_alat)[colnames(LH_summat_alat) == "Hehtaarit"]<-"Lannoittamaton_ala_ha"

LH_summat_alat<-LH_summat_alat %>%   mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                                       Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                                                       Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                                                       Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                                                       Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                                                       Tuotantosuunta == "Tattari ja kinoa"  ~ "Tattari_kinoa",
                                                                       Tuotantosuunta == "Vihannekset ja juurekset"  ~ "Vihannekset_juurekset",
                                                                       Tuotantosuunta == "Viljat pl. ohra"   ~ "Viljat_pl_ohra",
                                                                       .default = Tuotantosuunta))

Alat_ravinteet_kasveittain<-inner_join(Alat_ravinteet_kasveittain, LH_summat_alat, by=c("Tuotantosuunta")) %>% select(-Lannoittamaton_ala_ha)

#Luonnonhuuhtouman typpi-muuttuja on tilatyypin yhteenlaskettu lannoittamattoman alan luonnonhuuhtouma-luku. 
#Se jaetaan Typpi yhteensä-jakaumalla tilojen kesken

Alat_ravinteet_kasveittain<-Alat_ravinteet_kasveittain %>% group_by(Tuotantosuunta) %>% mutate(LH_typpi_tilalle = Luonnonhuuhtouman_typpi*(Typpi_yhteensa/sum(Typpi_yhteensa)))
#Ja poistetaan loppusummasta
Alat_ravinteet_kasveittain<-Alat_ravinteet_kasveittain %>% mutate(Typpi_yhteensa_ei_luonnonhuuhtoumaa = Typpi_yhteensa-LH_typpi_tilalle)


#Aggregoidaan turhat muuttujat pois. Tavoitteena kasvin N-intensiteetti yli tuotantosuuntien

Alat_ravinteet_aggre<- Alat_ravinteet_kasveittain %>% group_by(KASVIKOODI_lohkodata_reclass, KASVINIMI_reclass) %>% summarise(Typpi_yhteensa_kg_ei_luonnonhuuhtoumaa = sum(Typpi_yhteensa_ei_luonnonhuuhtoumaa), 
                                                                                                              Lannoitettu_viljelyala = sum(Lannoitettu_viljelyala))
Alat_ravinteet_aggre<-Alat_ravinteet_aggre %>% mutate(kg_N_ha = Typpi_yhteensa_kg_ei_luonnonhuuhtoumaa/Lannoitettu_viljelyala) 

sum(LH_summat_alat$Lannoittamaton_ala_ha)+sum(Alat_ravinteet_aggre$Lannoitettu_viljelyala)

Typpi<-createWorkbook()
addWorksheet(Typpi, "Kasveittainen_typpi_int")
writeData(Typpi, "Kasveittainen_typpi_int", Alat_ravinteet_aggre)
saveWorkbook(Typpi, file=here("Output/Ravinnedata/Emissiotulokset/Typen_intensiteetit_kasveille.xlsx"), overwrite = T)

gc()
rm(list=ls())

