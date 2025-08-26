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

#Nämä voidaan nyt yhdistää riveittäin takaisin. 

c<-rbind(a,b)

#Summien tarkistus
sum(c$Mineraalilannoitteen_typpi)
sum(lohkot_kaikki$Mineraalilannoitteen_typpi)

sum(c$typpi_lannasta_kg)
sum(lohkot_kaikki$typpi_lannasta_kg)

sum(c$Tasokorjattu_mineraalilannoitteen_typpi)

sum(c$Tasokorjattu_mineraalilannoitteen_typpi)+sum(c$typpi_lannasta_kg)

rm(a,b,alat,Jaettava_erotus,x)

#Muunto intensiteeteiksi (ravinnemassat per hehtaari)
#Tässä alassa ei ole mukana lannoittamatonta grassland-alaa. 

gc()

lannoitettuAla<-lohkot_kaikki %>% group_by(Tuotantosuunta, MAATILA_TUNNUS) %>% summarise(Lannoitettu_viljelyala = sum(Maannossumma))    

Alat_ravinteet_tiloittain<-inner_join(c, lannoitettuAla, by =c("Tuotantosuunta","MAATILA_TUNNUS"))

#Alojen tarkistus
sum(Alat_ravinteet_tiloittain$Lannoitettu_viljelyala)+sum(LH_summat_alat$Hehtaarit)

#ETOL-koodit

library(readxl)
Muuntoavain_tuotantosuunnat_tuotteet_ETOL <- read_excel("Data/Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx")
colnames(Muuntoavain_tuotantosuunnat_tuotteet_ETOL)[1]<-"Tuotantosuunta"

rm.all.but(c("Alat_ravinteet_tiloittain","Muuntoavain_tuotantosuunnat_tuotteet_ETOL","LH_summat_alat"))

Alat_ravinteet_tiloittain<-Alat_ravinteet_tiloittain %>%   mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                                  Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                                                  Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                                                  Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                                                  Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                                                  Tuotantosuunta == "Tattari ja kinoa"  ~ "Tattari_kinoa",
                                                                  Tuotantosuunta == "Vihannekset ja juurekset"  ~ "Vihannekset_juurekset",
                                                                  Tuotantosuunta == "Viljat pl. ohra"   ~ "Viljat_pl_ohra",
                                                                  .default = Tuotantosuunta))

Alat_ravinteet_tiloittain<-inner_join(Alat_ravinteet_tiloittain, Muuntoavain_tuotantosuunnat_tuotteet_ETOL, by="Tuotantosuunta")

#Alojen tarkistus
sum(Alat_ravinteet_tiloittain$Lannoitettu_viljelyala)+sum(LH_summat_alat$Hehtaarit)

#Summataan lanta- ja lannoitetyppi
Alat_ravinteet_tiloittain<-Alat_ravinteet_tiloittain %>% mutate(Typpi_yhteensa = Tasokorjattu_mineraalilannoitteen_typpi+typpi_lannasta_kg)

#Jaetaan lannoittamattoman alan luonnonhuuhtoumatyppi tilatyypeille. Tätä jakoa ei voi tehdä suoraan tilatunnuksesta, 
#koska tilajoukko ei ole sama Alat_ravinteet_tiloittain-joukossa, josta on poistettu lannoittamattomien kasvien ala, ja LH_summat_alat framessa, jossa taas ei ole muuta kuin lannoittamatonta alaa.  

#Kunkin tilatyypin lannoittamattoman alan luonnonhuuhtoumatyppi saadaan LH_summat_alat framesta. Jaetaan typen käytön suhteessa tilatyypin tiloille, vähennetään typpitotaalista. 

sum(Alat_ravinteet_tiloittain$Lannoitettu_viljelyala)+sum(LH_summat_alat$Hehtaarit)

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

Alat_ravinteet_tiloittain<-inner_join(Alat_ravinteet_tiloittain, LH_summat_alat, by=c("Tuotantosuunta")) %>% select(-Lannoittamaton_ala_ha)

#Luonnonhuuhtouman typpi-muuttuja on tilatyypin yhteenlaskettu lannoittamattoman alan luonnonhuuhtouma-luku. 
#Se jaetaan Typpi yhteensä-jakaumalla tilojen kesken

Alat_ravinteet_tiloittain<-Alat_ravinteet_tiloittain %>% group_by(Tuotantosuunta) %>% mutate(LH_typpi_tilalle = Luonnonhuuhtouman_typpi*(Typpi_yhteensa/sum(Typpi_yhteensa)))

Alat_ravinteet_tiloittain<-Alat_ravinteet_tiloittain %>% mutate(Typpi_yhteensa_ei_luonnonhuuhtoumaa = Typpi_yhteensa-LH_typpi_tilalle)


#Aggregoidaan turhat muuttujat pois

Alat_ravinteet_aggre<- Alat_ravinteet_tiloittain %>% group_by(MAATILA_TUNNUS, ETOL_koodi, ETOL) %>% summarise(Typpi_yhteensa_kg_ei_luonnonhuuhtoumaa = sum(Typpi_yhteensa_ei_luonnonhuuhtoumaa), 
                                                                                                              Lannoitettu_viljelyala = sum(Lannoitettu_viljelyala))
Alat_ravinteet_aggre<-Alat_ravinteet_aggre %>% mutate(kg_N_ha = Typpi_yhteensa_kg_ei_luonnonhuuhtoumaa/Lannoitettu_viljelyala) 


#Keskiarvotus 

Keskiarvot_etol<-Alat_ravinteet_aggre %>% group_by(ETOL, ETOL_koodi) %>% summarise(Keskimaarainen_intensiteetti_kg_N_ha = mean(kg_N_ha),
                                                                       Intensiteetin_keskihajonta = sd(kg_N_ha),
                                                                       Typpikilot_ei_luonnonhuuhtoumaa = sum(Typpi_yhteensa_kg_ei_luonnonhuuhtoumaa)
                                                                       )  

Lannoitettu_ala_etol<- Alat_ravinteet_tiloittain %>% group_by(ETOL, ETOL_koodi) %>% summarise(Lannoitettu_viljelyala=sum(Lannoitettu_viljelyala))
  
Keskiarvot_etol<-inner_join(Keskiarvot_etol, Lannoitettu_ala_etol, by=c("ETOL","ETOL_koodi"))  


#Aggregoitu keskiarvo kaikista kasvi- ja kaikista eläintiloista


Animalfarms<-c("Hevostilat","Lammas- ja vuohitilat","Maitotilat", "Munatilat","Muut nautakarjatilat","Siipikarjatilat","Sikatilat","Turkistilat")

Alat_ravinteet_aggre<-Alat_ravinteet_aggre %>% mutate(Farmtype = case_when(ETOL %in% Animalfarms ~ "Animal",
                                                          .default = "Crop"))

Keskiarvot_pääluokka<-Alat_ravinteet_aggre %>% group_by(Farmtype) %>% summarise(Keskimaarainen_intensiteetti_kg_N_ha = mean(kg_N_ha),
                                                                                             Intensiteetin_keskihajonta = sd(kg_N_ha))

Typpi<-createWorkbook()
addWorksheet(Typpi, "Keskiarvot")
writeData(Typpi, "Keskiarvot", Keskiarvot_etol)
addWorksheet(Typpi, "Keskiarvot_pääluokittain")
writeData(Typpi, "Keskiarvot_pääluokittain", Keskiarvot_pääluokka)
saveWorkbook(Typpi, file=here("Output/Ravinnedata/Emissiotulokset/Typen_intensiteetit_tilatyypeille_luonnonhuuht_poistettuna.xlsx"), overwrite = T)
write.xlsx(LH_summat_alat, file=here("Output/Ravinnedata/Emissiotulokset/Typen_luonnonhuuhtouma_lannoittamattomalta_alalta.xlsx"), overwrite = T)


gc()
rm(list=ls())
