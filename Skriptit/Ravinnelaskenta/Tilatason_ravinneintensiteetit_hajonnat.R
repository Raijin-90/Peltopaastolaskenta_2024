#Typen inputin allokointi lohkoista tilatasolle, tn/ha - intensiteettien laskenta
#Sekä hajonnat. 

library(tidyverse);library(here);library(openxlsx)

#' Source specific lines in an R file
#'
#' @param file character string with the path to the file to source.
#' @param lines numeric vector of lines to source in \code{file}.

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

#Lohkoittainen allokointi noudattaa aikaisempaa kaavaa. 
#Lasketaan erotus typen tarjonnasta ja käytöstä. Eläintilojen mineraalilannoitekäyttöä vähennetään erotuksen verran (korvautuminen lannalla). Kasvitiloilla saman verran korotusta mineraali-
#Lannoitteiden käyttöön. 

#Tiloittaisten intensiteettien tuottamiseksi ja hajontojen laskentaan  tämä allokointi tehdään nyt tilatasolle, ei aggregoida suoraan tuotantosuuntiin.  
#Käytetään aiempaa skriptiä pohjana: 

source_lines(here("Skriptit/Ravinnelaskenta/Ravinneallokointi_lohkoittain_lanta_lannoite_yhdistäminen.R"),1:210)

#ELÄINTILAT: 
#Eläintilojen mineraalilannoitekäyttöä pudotetaan erotuksen verran, mutta jaetaan tiloittain
#Jaettava erotus sisältää eläintilojen mineraalilannoitteen lannoitetyppeen tehtävän massamääräisen pudotuksen. 
#Se jaetaan nyt tilakohtaisesti

Animalfarms<-c("Hevostilat","Lammas- ja vuohitilat","Maitotilat", "Munatilat","Muut nautakarjatilat","Siipikarjatilat","Sikatilat","Turkistilat")

a<-lohkot_kaikki %>% filter(Tuotantosuunta %in% Animalfarms) %>% group_by(Tuotantosuunta, MAATILA_TUNNUS) %>% summarise(
  Mineraalilannoitteen_typpi = sum(Mineraalilannoitteen_typpi),
  typpi_lannasta_kg = sum(typpi_lannasta_kg)
)

#Mineraalilannoitteen N käytön jakauma kunkin tuotantosuunnan tilojen sisällä 
a<-a %>% group_by(Tuotantosuunta) %>% mutate(Pros=Mineraalilannoitteen_typpi/sum(Mineraalilannoitteen_typpi))

#Kunkin tuotantosuunnan mineraalilannoite-typen käyttöä lasketaan yhteensä tämän verran
Jaettava_erotus<-Jaettava_erotus %>% select(Tuotantosuunta, osuus_erotuksesta)
#Allokoidaan tiloille kunkin tuotantosuunnan sisällä mineraalilannoitteen typen jakaumalla

a<-inner_join(a, Jaettava_erotus)
a<-a %>% mutate(Tasokorjattu_mineraalilannoitteen_typpi = Mineraalilannoitteen_typpi-(Pros*osuus_erotuksesta))

#KASVITILAT: KOROTUS
#Vastaavasti kasvitiloilla korotetaan erotuksen verran mineraalilannoitteiden käyttöä 

source_lines(here("Skriptit/Ravinnelaskenta/Ravinneallokointi_lohkoittain_lanta_lannoite_yhdistäminen.R"),230:235)


b<-lohkot_kaikki %>% filter(!(Tuotantosuunta %in% Animalfarms)) %>% group_by(Tuotantosuunta, MAATILA_TUNNUS) %>% summarise(
  Mineraalilannoitteen_typpi = sum(Mineraalilannoitteen_typpi),
  typpi_lannasta_kg = sum(typpi_lannasta_kg)
)

b<-b %>% group_by(Tuotantosuunta) %>% mutate(Pros=Mineraalilannoitteen_typpi/sum(Mineraalilannoitteen_typpi))


Jaettava_erotus<-Jaettava_erotus %>% select(Tuotantosuunta, osuus_erotuksesta)

b<-inner_join(b, Jaettava_erotus)
b<-b %>% mutate(Tasokorjattu_mineraalilannoitteen_typpi = Mineraalilannoitteen_typpi+(Pros*osuus_erotuksesta))

#Nämä voidaan nyt yhdistää riveittäin. 

c<-rbind(a,b)

#Summien tarkistus
sum(c$Mineraalilannoitteen_typpi)
sum(lohkot_kaikki$Mineraalilannoitteen_typpi)

sum(c$typpi_lannasta_kg)
sum(lohkot_kaikki$typpi_lannasta_kg)

sum(c$Tasokorjattu_mineraalilannoitteen_typpi)

rm(a,b,alat,Jaettava_erotus,x)
#Muunto intensiteeteiksi (ravinnemassat per hehtaari)
#Tässä alassa ei ole mukana lannoittamatonta grassland-alaa. 
gc()

lannoitettuAla<-lohkot_kaikki %>% group_by(Tuotantosuunta, MAATILA_TUNNUS) %>% summarise(Lannoitettu_viljelyala = sum(Maannossumma))    

Alat_ravinteet_tiloittain<-inner_join(c, lannoitettuAla, by =c("Tuotantosuunta","MAATILA_TUNNUS"))

#ETOL-koodit

library(readxl)
Muuntoavain_tuotantosuunnat_tuotteet_ETOL <- read_excel("Data/Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx")
colnames(Muuntoavain_tuotantosuunnat_tuotteet_ETOL)[1]<-"Tuotantosuunta"

rm.all.but(c("Alat_ravinteet_tiloittain","Muuntoavain_tuotantosuunnat_tuotteet_ETOL","Luonnonhuuhtouma"))

Alat_ravinteet_tiloittain<-Alat_ravinteet_tiloittain %>%   mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                                  Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                                                  Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                                                  Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                                                  Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                                                  Tuotantosuunta == "Tattari ja kinoa"  ~ "Tattari_kinoa",
                                                                  Tuotantosuunta == "Vihannekset ja juurekset"  ~ "Vihannekset_juurekset",
                                                                  Tuotantosuunta == "Viljat pl. ohra"   ~ "Viljat_pl_ohra",
                                                                  .default = Tuotantosuunta))

sum(Alat_ravinteet_tiloittain$Lannoitettu_viljelyala)+sum(Luonnonhuuhtouma$Maannossumma)


Alat_ravinteet_tiloittain<-inner_join(Alat_ravinteet_tiloittain, Muuntoavain_tuotantosuunnat_tuotteet_ETOL, by="Tuotantosuunta")

Alat_ravinteet_tiloittain<-Alat_ravinteet_tiloittain %>% group_by(ETOL,ETOL_koodi,MAATILA_TUNNUS) %>% summarise(Mineraalilannoitteen_typpi_tasokorjattu=sum(Tasokorjattu_mineraalilannoitteen_typpi),
                                                                                           Lannan_typpi = sum(typpi_lannasta_kg),
                                                                                           Lannoitettu_viljelyala=sum(Lannoitettu_viljelyala))

sum(Alat_ravinteet_tiloittain$Lannoitettu_viljelyala)+sum(Luonnonhuuhtouma$Maannossumma)

#Seuraavaksi lasketaan yksittäisten tilojen intensiteetit, sitten keskiarvotetaan ne etol-luokittain ja lasketaan hajonnat. 
