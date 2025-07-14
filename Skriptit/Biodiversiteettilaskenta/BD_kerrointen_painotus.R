#Biodiversiteettikerrointen pinta-alapainotus.
#Esim. syys- ja kevätvehnä ovat eri bd-kertoimilla, mutta kuuluvat kumpikin ettl:ään vehnä.
#Painotetaan näiden bd-kertoimet kokonaisviljelyalalla, jotta saadaan 1 Vehnän molemmat huomioiva kerroin. Tuotemielessä vehnävariantit ovat jo lasketut mukaan Vehnä-tuotteen alle


#Aineistojen sisäänotto

library(varhandle);library(tidyverse);library(here)

#Bd-luokat
library(readxl)
Biodiversiteettiluokat_kasveille <- read_excel("Data/Biodiversiteettiluokat_kasveille.xlsx")

#Tuotejako

library(readxl)
ETTL <- read_excel("Data/Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx", 
                                                        sheet = "Kasvit_ETTL_BD")


#' Source specific lines in an R file
#'
#' @param file character string with the path to the file to source.
#' @param lines numeric vector of lines to source in \code{file}.

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

#kokonaisviljelyalan luonti

source_lines(here("Skriptit/Uudet skriptit/GTK_datan_aggregointi.R"),1:260)

Kokonaisala<-GTKdata %>% group_by(KASVIKOODI_lohkodata_reclass, KASVINIMI_reclass) %>% summarise(Kokonaisviljelyala = sum(Maannossumma))


#BD-kertoimien ja kokonaisalan yhdistäminen

colnames(Biodiversiteettiluokat_kasveille)[1]<-"KASVIKOODI_lohkodata_reclass"

Aineisto<-left_join(Kokonaisala, Biodiversiteettiluokat_kasveille, by="KASVIKOODI_lohkodata_reclass")

#BD-kerrointen numeroarvojen liitto

Biodiversiteettikertoimet <- read_excel("Data/Biodiversiteettikertoimet.xlsx")

Aineisto<-left_join(Aineisto, Biodiversiteettikertoimet, by="Soveltuva_biodiv_kerroin")


#ETTL tuotenimet

colnames(ETTL)[3]<-"KASVIKOODI_lohkodata_reclass"
Aineisto<-left_join(Aineisto, ETTL, by=c("KASVIKOODI_lohkodata_reclass"))


#Toistuvia muuttujia pois

Aineisto<-Aineisto %>% select(-Kasvi, -Biodiversiteettiluokitus_original, -Diversiteettikerroin, -`Ruokaviraston nimi`, -Soveltuva_biodiv_kerroin.x)
colnames(Aineisto)[colnames(Aineisto)=="Soveltuva_biodiv_kerroin.y"]<-"Soveltuva_biodiv_kerroin"

#Viljelyalapainotettu diversiteettikerroin 

#Viljelyala x div-kerroin per kasvi

Aineisto<-Aineisto %>% mutate(div_x_pinta_ala = Skaalattu_kokonaisdiversiteetti * Kokonaisviljelyala)

#Viljelyalasummat ettl_luokittain

Viljelyalasummat <- Aineisto %>% group_by(ETTL, `ETTL Nimike`) %>% summarise(ETTL_kokonaisalaviljelyala = sum(Kokonaisviljelyala))


Kerroinsummat <-Aineisto %>% group_by(ETTL, `ETTL Nimike`) %>% summarise(div_x_pinta_ala_tulosumma = sum(div_x_pinta_ala))

#Kasvien määrä kutakin ettl-luokkaa. 
#Jos 1, niin alkuperäistä kerrointa voi käyttää suoraan. Ei tarvitse painotusta. 

Aineisto<-Aineisto %>% mutate(laskuri =1)
Lajimaarat<- Aineisto %>% group_by(ETTL, `ETTL Nimike`) %>% summarise(kasvien_lkm_ettl_luokassa = sum(laskuri)) 


#Painotettu kerroin lasketaan jakamalla esim. ETTL:n ruis diversiteetti x pinta-ala - tulosumma sen kokonaisviljelyalalla

painotetutKertoimet<-left_join(Viljelyalasummat, Kerroinsummat, by=c("ETTL", "ETTL Nimike"))

painotetutKertoimet<-painotetutKertoimet %>% mutate(Viljelyalapainotettu_diversiteettikerroin = div_x_pinta_ala_tulosumma/ETTL_kokonaisalaviljelyala)

painotetutKertoimet<-left_join(painotetutKertoimet, Lajimaarat, by=c("ETTL", "ETTL Nimike"))

painotetutBDkertoimet<-createWorkbook()
addWorksheet(painotetutBDkertoimet,"painotetutKertoimet")
writeData(painotetutBDkertoimet,"painotetutKertoimet", painotetutKertoimet)
saveWorkbook(painotetutBDkertoimet,here("Data/Kokonaisviljelyalalla_painotetut_BD_kertoimet.xlsx"), overwrite = T)

print("BD-kertoimet painotettua")

rm.all.but("painotetutKertoimet")
