library(here);library(tidyverse);library(varhandle)

#Eläinmäärät tuotantosuunnittain, etol-luokituksen mukaisesti

#Pohjataan ravinnelaskennan ajantasaiseen eläimet tiloittain-dataan, johon liitetään tuotantosuunnat ja aggregoidaan

#' Source specific lines in an R file
#'
#' @param file character string with the path to the file to source.
#' @param lines numeric vector of lines to source in \code{file}.

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

library(readxl)
Elainmaarat <- read_excel("Data/Ravinnelaskennan_aineisto/Lannan_ravinnelaskenta.xlsx", 
                          sheet = "Eläimet_tiloittain_cleaned")

library(readxl)
TS_ETOL <- read_excel("Data/Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx")
colnames(TS_ETOL)[colnames(TS_ETOL) =="Tuotantosuunta_original"]<-"Tuotantosuunta"

library(readxl)
Tilat <- read_excel("Data/Korjatut_tuotantosuunnat_20032024.xlsx")


Tilat<-Tilat %>% select(Tilatunnus, Tuotantosuunta_muutos)%>% mutate(Tuotantosuunta_muutos = case_when(Tuotantosuunta_muutos == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                  Tuotantosuunta_muutos == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                  Tuotantosuunta_muutos == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                  Tuotantosuunta_muutos == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                  Tuotantosuunta_muutos == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                  Tuotantosuunta_muutos == "Tattari ja kinoa" ~ "Tattari_kinoa",
                                  Tuotantosuunta_muutos == "Vihannekset ja juurekset" ~ "Vihannekset_juurekset",
                                  Tuotantosuunta_muutos == "Viljat pl. ohra" ~ "Viljat_pl_ohra",
                                  .default = Tuotantosuunta_muutos))

colnames(Tilat)[colnames(Tilat)=="Tuotantosuunta_muutos"]<-"Tuotantosuunta"

#Liitetään ETOL-koodit ja tarkistetaan liitoksen onnistuminen

Tilat<-left_join(Tilat, TS_ETOL, by="Tuotantosuunta")


#' Filter for missing ETOL values and check for errors
#' If NA values remain, stop execution and raise an error message
#' @param Tilat a tibble containing the TILAT dataset
#' @return a tibble with only non-NA values of ETOL

a<-Tilat %>% filter(is.na(ETOL))

if (nrow(a) > 0){
  stop("Epätäydellinen tuotantosuuntien liitos, tarkista avain")
} else
  print("Tuotantosuuntaliitos onnistui")

rm(a, TS_ETOL)

#Kiinnitetään Tilatunnuksen mukaan tuotantosuunta eläinmääriin
#Tarkistetaan tyhjien olemassaolo kuten yllä

Data<-left_join(Elainmaarat, Tilat, by="Tilatunnus")

a<-Data %>% filter(is.na(ETOL))

if (nrow(a) > 0){
  stop("Epätäydellinen tuotantosuuntien liitos, tarkista avain")
} else
  print("Liitos eläinmääriin onnistui")

rm.all.but("Data")

#Aggregoidaan ja yksinkertaistetaan eläinten tyypittelyä rehuviljojen allokointia varten

Data<-Data %>% group_by(ETOL_koodi,ETOL) %>% summarise_if(is.numeric, sum, na.rm = F) 

Data<-Data %>% pivot_longer(cols= 3:length(Data), values_to = "Elainta", names_to = "Elaintyyppi")



Naudat<-c("Lypsylehmät_yli_24_kk",
"Emolehmät_yli_24_kk",
"Sonnivasikat_alle_6_kk",
"Sonnit_6_24_kk",
"Sonnit_yli_24_kk",
"Härkävasikat_alle_6_kk",
"Härät_6_24_kk",
"Härät_yli_24_kk",
"Lehmävasikat_alle_6_kk",
"Emolehmät_6_24_kk",
"Emolehmähiehot_6_24_kk",
"Emolehmähiehot_yli_24_kk_48_kk",
"Lypsylehmät_6_24_kk",
"Muut_naudat_yli_24_kk",
"Muut_naudat_6_24_kk")

Siat<-c("Lihasiat_yli_3_kk_alle_8_kk",
"Nuoret_siitossiat_yli_3_kk_alle_8_kk",
"Porsaat_3_kk_ja_alle",
"Emakot_väh_8_kk",
"Karjut_väh_8_kk")

Kanat<-c("Kanat",	
"Siitoskukot"	,
"Broileriemot",
"Broilerit",
"Kananpoikaset")	

Muut<-c("Hevoset_yhteismäärä",	
        "Uuhet_yli_12_kk",	
        "Karitsat_väh_3_kk_12kk",	
        "Pässit_yli_12_kk",	
        "Kutut_poikineet_yli_12_kk",	
        "Kilit_väh._3_kk_12kk",	
        "Pukit_yli_12_kk",
        "Muu_siipikarja",
        "Kalkkunat",
        "Minkit",
        "Ketut",
        "Supit")


Data<-Data %>% mutate(Aggregoitu_elainluokka=case_when(Elaintyyppi %in% Naudat ~ "Naudat",
                                                 Elaintyyppi %in% Siat ~ "Siat",
                                                 Elaintyyppi %in% Kanat ~ "Kanat",
                                                 Elaintyyppi %in% Muut ~ "Muut_elaimet",
                                                 ))




