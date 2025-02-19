library(tidyverse);library(here);library(usefun)

#Viljelyalojen laskenta
source(here("Skriptit/Biodiversiteettilaskenta/GTK_ala_biodiversiteettiluokittain_ettl.R"))


#Taiton testaus
#16 tibbleä samaan listaan

Data_list<-list(
  tibble(Grassland_korotettu_mineraalimaa_raivio_moniv),
  tibble(Grassland_korotettu_mineraalimaa_raivio_yksiv),
  tibble(Cropland_korotettu_mineraalimaa_raivio_moniv),
  tibble(Grassland_korotettu_elop_moniv),
  tibble(Cropland_korotettu_elop_moniv),
  tibble(Grassland_korotettu_elop_yksiv),
  tibble(Cropland_korotettu_mineraalimaa_moniv),
  tibble(Cropland_korotettu_mineraalimaa_yksiv),
  tibble(Cropland_korotettu_elop_raivio_yksiv),
  tibble(Cropland_korotettu_mineraalimaa_raivio_yksiv),
  tibble(Grassland_korotettu_mineraalimaa_moniv),
  tibble(Cropland_korotettu_elop_raivio_moniv),
  tibble(Grassland_korotettu_mineraalimaa_yksiv),
  tibble(Grassland_korotettu_elop_raivio_moniv),
  tibble(Grassland_korotettu_elop_raivio_yksiv),
  tibble(Cropland_korotettu_elop_yksiv)
)



#nimetään listaelementit lyhyemmin: ei tarvise jättää cropland-grassland-perennial-annual - jakoa kenttiin, se tiedetään jo nimestä 
names(Data_list)<-c("GrassPerMinRaiv",
                    "GrassAnnMinRaiv",
                    "CropPerMinRaiv",
                    "GrassPerOrg",
                    "CropPerOrg",
                    "GrassAnnOrg",
                    "CropPerMin",
                    "CropAnnMin",
                    "CropAnnOrgRaiv",
                    "CropAnnMinRaiv",
                    "GrassPerMin",
                    "CropPerOrgRaiv",
                    "GrassAnnMin",
                    "GrassPerOrgRaiv",
                    "GrassAnnOrgRaiv",
                    "CropAnnOrg")  
                    
                    
rm.all.but("Data_list")


#Eriytetään biodiversiteettilaskennan taulu pinta-alataulusta. Pinta-alataulu tarvitsee tässä vaiheessa vähemmän kenttiä. 
#BD-listan formaattivaatimuksia ei vielä ole päätetty
Data_list_BD<-Data_list


#Aggregoidaan BD-luokat ja muut tarpeettomat kategoriat pois
#Listan nimi kertoo jo kategoriat. Toimialakoodit taitetaan sarakkeille, nimikkeet kannattaa poistaa jotta dimensioita saa vähemmäjsi 

Aggregointifunktio<-function(x){
 x %>% group_by(ETOL_koodi,ETTL) %>% summarise_at(.vars = names(.)[8] , sum) 
}

Data_list<-map(Data_list,Aggregointifunktio)  

rm(Aggregointifunktio)

#Puuttuvien sarakkeiden lisäys

#Luodaan funktio, joka muuttaa x:n kolumnin nr. 8 nimeksi "Hehtaaria" ja kääntää pitkän muodon. 

#' Custom Function
#'
#' This function renames the columns of a dataframe and then pivots it wider.
#' @param x A dataframe with columns "ETOL_koodi" and "Hehtaaria".
#' @return A wide dataframe with renamed columns and a pivot table.
#' @export

  customFunction <- function(x) {
    rename(x, "Hehtaaria" = 3) %>%  pivot_wider(
      names_from = "ETOL_koodi",
      values_from = "Hehtaaria",
      values_fill = 0
    ) 
    
  }

#Map käy läpi data_listin jokaisen alalistan, ja toteuttaa siellä yllä määritellyn kustomfunktion
Data_list<-map(Data_list,\(x) customFunction(x)) 


#Määritellään puuttuvat etol-koodit, jotka pitää lisätä dataan. 

#Täysi listavektori kaikista etol-koodeista alkutuotannossa, järjestettynä oikein.
#Tämän perusteella katsotaan mitä datasta puuttuu ja mitä ei. Se on myös sorttivektori columnien järjestämiseen.  
#Sorttausfunktio perustuu tähän vektoriin, siksi mukaan myös muut sarakkeet

Kaikki_etolkoodit<-c(
  "ETTL",
  "ETTL Nimike",
  "0111a",
  "0111b",
  "0111c",
  "0111d",
  "0111e",
  "0111f",
  "0111g",
  "0111h",
  "0111i",
  "0111k",
  "0111l",
  "01131",
  "01132",
  "01133",
  "01134",
  "01191",
  "0124",
  "0125",
  "0130a",
  "0130b",
  "0141",
  "0142",
  "0143",
  "0145",
  "0146",
  "01471",
  "01472",
  "01491",
  "01492",
  "01499")

#Ne etol-koodit jotka datassa on. x  

Datan_etolit<-colnames(Data_list[[1]][3:length(Data_list[[1]])])

#Datan koodien ja täyden etol listan erotus. Nämä eivät vaihtele kategorioittain. 
Lisattavat<-outersect(Datan_etolit, Kaikki_etolkoodit)

addColumns<-function(x){
  cbind(
    x,
    `0111i` = 0,
    `01132` = 0,
    `01191` = 0,
    `0130a` = 0,
    `0130b` = 0,
    `01492` = 0) }

Data_list<-map(Data_list,\(x) addColumns(x))

#Sortataan täydennetyn sarakeluettelon perusteella 


Data_list<-map(Data_list, ~ relocate(.x, any_of(Kaikki_etolkoodit)))


#Tuoteosan täydennys. ####
#Pulmallinen vaihe: croplandista puuttuu tuotesuunnassa kaikki Muu peltoala, perennoista annualit, ja toisinpäin. Jokaisesta puuttuu eri tuotteet
#Tuotekoodien täytössä pitää tehdä erillinen tarkistus joka listaelementin puuttuvista koodeista, ja erillinen liitos (nollarivi) kullekin niistä. 

#Kaikki, tuotteet alkutuotannosta, täysi vektori. Kaikki "Eläviin lehmiin 014110" asti. Vain sellaiset, joiden osalta peltolohkot ovat relevantti asia (kasvi-RACit), ei eläimiä eikä jalosteita. 
#Lista tuotteista 
Taydennettavat_tuotteet<-c("011111",
"011112a",
"011112b",
"011120",
"011131a",
"011131b",
"011132a",
"011132b",
"011133a",
"011133b",
"011141",
"011142",
"011149",
"011150",
"011161",
"011162",
"011169",
"011171",
"011172",
"011173",
"011174",
"011175a",
"011175b",
"011179",
"011181",
"011182",
"011183",
"011191",
"011192",
"011193",
"011194",
"011195",
"011199a",
"011199b",
"011199c",
"011210",
"011311",
"011312a",
"011312b",
"011312c",
"011312d",
"011312e",
"011313a",
"011313b",
"011314a",
"011314b",
"011315",
"011316",
"011317",
"011319a",
"011319b",
"011319c",
"011319d",
"011319e",
"011319f",
"011321",
"011329",
"011331",
"011332a",
"011332b",
"011333",
"011334",
"011339a",
"011339b",
"011341a",
"011341b",
"011342",
"011343",
"011344",
"011349a",
"011349b",
"011349c",
"011349d",
"011349e",
"011349f",
"011351a",
"011351b",
"011351c",
"011351d",
"011351e",
"011351f",
"011352",
"011353",
"011359",
"011360",
"011371",
"011372",
"011380a",
"011380b",
"011390",
"011410",
"011611",
"011612",
"011619",
"01191",
"011921",
"011922",
"011931",
"011939",
"011l",
"011m",
"012111",
"012112",
"012211",
"012212",
"012213",
"012214",
"012219",
"012311",
"012312",
"012313",
"012314",
"012319",
"012410",
"012421",
"012422",
"012423",
"012424",
"012425",
"012426",
"012427",
"012429",
"012511",
"012512",
"012513",
"012519a",
"012519b",
"012519c",
"012519d",
"012519e",
"012519f",
"012519g",
"012519h",
"012519i",
"012519j",
"012519k",
"012520",
"012531",
"012532",
"012533",
"012534",
"012535",
"012539",
"012590",
"012611",
"012612",
"012620",
"012690",
"012711",
"012712",
"012713",
"012714",
"012811",
"012812",
"012813",
"012814",
"012815",
"012816",
"012817",
"012818",
"012819",
"012820",
"012830",
"01291",
"012920",
"012930",
"013010",
"Muu")

#Tätä pitää verrata jokaisen listaelementin tuotteisiin, katsoa mitä puuttuu, ja täydentää rivit. Jokaisesta puuttuu eri asiat. 
Listat<-seq_along(1:length(Data_list)) #looppivektori, numerot 1-16. Vastaa listojen lukumäärää datassa


#Tallennuspaikka puuttuville koodeille
#Tyhjä lista

Puuttuvat<-list()

#Puuttuvien määrittely

for (i in seq_along(Listat)) {
  
x<-outersect(Data_list[[i]][[1]], Taydennettavat_tuotteet)
Puuttuvat[[i]]<-x

}

#Näihin puuttuvien ETTLien listoihin on lisättävä samat tuotantosuuntasarakkeet kuin data_listissä. Niille tulee nolla-arvo. 
#Näitäkin tauluja syntyy 16 erilaista. Voidaan loopata tutulla Listat-vektorilla

Puuttuvat_taydennys<-list()

for (i in seq_along(Listat)) {
  
Puuttuvat_taydennys[[i]]<-data.frame(ETTL = Puuttuvat[[i]],
                               `0111a`=0,
                               `0111b`=0,
                               `0111c`=0,
                               `0111d`=0,
                               `0111e`=0,
                               `0111f`=0,
                               `0111g`=0,
                               `0111h`=0,
                               `0111i`=0,
                               `0111k`=0,
                               `0111l`=0,
                               `01131`=0,
                               `01132`=0,
                               `01133`=0,
                               `01134`=0,
                               `01191`=0,
                               `0124`=0,
                               `0125`=0,
                               `0130a`=0,
                               `0130b`=0,
                               `0141`=0,
                               `0142`=0,
                               `0143`=0,
                               `0145`=0,
                               `0146`=0,
                               `01471`=0,
                               `01472`=0,
                               `01491`=0,
                               `01492`=0,
                               `01499` = 0)
}
rm(Puuttuvat)


#Tämä lista voidaan nyt yhdistää rivisuunnassa (rbind) kiinni Data_listiin, 1. lista DataList 1:een ja niin edelleen. 

#Sarakenimien oikaisu####
#Oikaisuvektori 
Oikaisu<-c("ETTL",
"0111a",
"0111b",
"0111c",
"0111d",
"0111e",
"0111f",
"0111g",
"0111h",
"0111i",
"0111k",
"0111l",
"01131",
"01132",
"01133",
"01134",
"01191",
"0124",
"0125",
"0130a",
"0130b",
"0141",
"0142",
"0143",
"0145",
"0146",
"01471",
"01472",
"01491",
"01492",
"01499")


Puuttuvat_taydennys<-map(Puuttuvat_taydennys, set_names, Oikaisu)

Combined_list<-list()

for (i in Listat) {

Combined_list[[i]]<-rbind(Data_list[[i]], Puuttuvat_taydennys [[i]])

}


names(Combined_list)<-c("GrassPerMinRaiv",
                    "GrassAnnMinRaiv",
                    "CropPerMinRaiv",
                    "GrassPerOrg",
                    "CropPerOrg",
                    "GrassAnnOrg",
                    "CropPerMin",
                    "CropAnnMin",
                    "CropAnnOrgRaiv",
                    "CropAnnMinRaiv",
                    "GrassPerMin",
                    "CropPerOrgRaiv",
                    "GrassAnnMin",
                    "GrassPerOrgRaiv",
                    "GrassAnnOrgRaiv",
                    "CropAnnOrg")  


#SORTTAUS TUOTERIVIEN MUKAAN ####

#Sortataan täydennetty lista Taydennettavat_tuotteet-tuotevektorin mukaan, jossa jokainen alkutuotantoon kuuluva kasvi-RAC (ei jalosteita eikä eläimiä). Väli "durumvehnä" -> "istutustuotteet...jne".   011111->01301



#' Sort a data frame or matrix by the values in a given column
#'
#' @param x A data frame or matrix to sort.
#' @param ETTL The name of the column containing the values to sort by.
#' @param Taydennettavat_tuotteet A vector of unique values to use as levels for the factorization of `ETTL`.
#' @return A sorted version of `x` based on the values in `ETTL`.
#' @export

Rivisorttaus<-function(x){
  
  
x %>% arrange(factor(ETTL, levels=Taydennettavat_tuotteet))
  
}

Combined_list<-map(Combined_list, Rivisorttaus)


rm.all.but(c("Combined_list","Data_list","Data_list_BD"))







              