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


#Koska käytetään suoraan tilojen tuotantosuuntalistaa eikä ajeta ensin peltoala-aggregointeja, täytyy turkistilat eritellä tässä. 
#Tämä vaihe suoritetaan normaalisti osana peltoalan aggregointia. 


Turkistilakoodit_vektori<-c("004010140",
                            "004012968",
                            "004025601",
                            "004029641",
                            "004046617",
                            "004069350",
                            "004085619",
                            "005080372",
                            "005108765",
                            "005202331",
                            "052008063",
                            "145085425",
                            "208051862",
                            "217033355",
                            "232395731",
                            "233043914",
                            "233059977",
                            "233202750",
                            "236075364",
                            "256003814",
                            "265046234",
                            "280002725",
                            "280004846",
                            "280004947",
                            "280007169",
                            "280036572",
                            "280041929",
                            "280054356",
                            "280055164",
                            "280089116",
                            "280093964",
                            "280098210",
                            "280099220",
                            "280112354",
                            "280113162",
                            "280313327",
                            "301132860",
                            "403035505",
                            "403067534",
                            "408060307",
                            "408238543",
                            "475014656",
                            "475132773",
                            "475136009",
                            "475195421",
                            "475200471",
                            "475221184",
                            "499039940",
                            "499127947",
                            "499145125",
                            "499219893",
                            "499282642",
                            "499333768",
                            "541127230",
                            "545013189",
                            "545089779",
                            "545210829",
                            "545219216",
                            "545240030",
                            "545248922",
                            "545278123",
                            "545291055",
                            "545464039",
                            "545526077",
                            "545563665",
                            "559004835",
                            "559027164",
                            "599041987",
                            "599046738",
                            "599047849",
                            "599075737",
                            "617018616",
                            "846058450",
                            "924040386",
                            "924100812",
                            "926056269",
                            "944001976",
                            "944023804",
                            "971011224",
                            "981032536",
                            "990004531")

Tilat$Tuotantosuunta[Tilat$Tilatunnus %in% Turkistilakoodit_vektori]<-"Turkistilat"




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

Data<-Data %>% group_by(ETOL_koodi,ETOL, Aggregoitu_elainluokka) %>% summarise(Elainta_yht = sum(Elainta)) 

Data<-Data %>% group_by(Aggregoitu_elainluokka) %>% mutate(Elaintyyppia_yht = sum(Elainta_yht))


#Tässä tilakohtaisessa analyysissä ei ole mukana turkistilojen minkkejä, supeja ja kettuja, koska lantalaskennassa niiden tilastoista poimittu kokonaismäärä allokoitiin suoraan ja kokonaan turkistarhat-toimialalle. 
#Turkiseläinten tiloittainen määrä ei ole tiedossa. Sillä ei kuitenkaan ole tässä vaiheessa väliä, millä turkistilalla on minkäkin monta eläintä. Tärkeintä on saada tuotantosuunnittainen eläinmäärä stemmaamaan 
#Lisätään tarvittavat määrät näitä eläimiä kategoriaan Turkistarhat, eläintyyppiin muut eläimet. 

Minkit<-1900000	
Ketut<-2492000	
Supit<-148000







Data<-Data %>% mutate(Elaintyypin_jakauma_tuotantos = Elainta_yht/Elaintyyppia_yht) %>% arrange(Aggregoitu_elainluokka) 
Tulos<-createWorkbook()
Tulos %>% addWorksheet("Elaimet_tuotantosuunnittain")
Tulos %>% writeData("Elaimet_tuotantosuunnittain", Data)
saveWorkbook(Tulos, file=here("Output/Elainmaaralaskenta/Elainmaarat_tuotantosuunnittain.xlsx"), overwrite = T)

             