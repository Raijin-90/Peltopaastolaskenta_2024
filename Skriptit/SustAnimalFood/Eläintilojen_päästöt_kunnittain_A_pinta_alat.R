#Emissiot ja maalajijakauma eri eläintalouden tuotantosuunnille, halutaan kunnittain (lopulta ely-tasolla). 
#Kari K pyytänyt 1.4
#Vaatii aggregoinnin muokkausta jotta kuntataso saadaan huomioitua

library(stringr);library(usefun);library(varhandle);library(here);library(tidyverse)

#' Source specific lines in an R file
#'
#' @param file character string with the path to the file to source.
#' @param lines numeric vector of lines to source in \code{file}.

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

#GTK-datan virheenkorjaus- ja esikäsittely, ei vielä aggregointia

source_lines(here("Skriptit/Uudet skriptit/GTK_datan_aggregointi.R"), 1:263)

rm.all.but("GTKdata")

#Kuntanumero: peruslohkotunnuksen 3 ensimmäistä merkkiä

GTKdata$Kuntakoodi<- substr(GTKdata$PLTUNNUS, start=1, stop=3)


GTKdata_raivaamattomat<- filter(GTKdata, is.na(Raivattu))
GTK_raiviot<-filter(GTKdata, !(is.na(GTKdata$Raivattu))) #2000 jälkeen raivatut lohkot irti


#Pinta-alojen aggregointi ####
#Aggregoidaan tuote- ja tuotantosuuntatasolla, kunta huomioiden.  
#Toteutetaan raivatulle ja raivaamattomalle maalle. 

GTK_aggregointi_mineral <-
  aggregate(
    list(GTKdata_raivaamattomat$Mineraalia),
    by = list(
      GTKdata_raivaamattomat$Tuotantosuunta,
      GTKdata_raivaamattomat$Kuntakoodi, 
      GTKdata_raivaamattomat$KASVIKOODI_lohkodata_reclass,
      GTKdata_raivaamattomat$KASVINIMI_reclass
    ),
    sum
  )
colnames(GTK_aggregointi_mineral) <-
  c("Tuotantosuunta","Kuntakoodi","Kasvikoodi", "Kasvinimi", "Mineraalimaata")

GTK_aggregointi_elop <-
  aggregate(
    list(GTKdata_raivaamattomat$Eloperaista),
    by = list(
      GTKdata_raivaamattomat$Tuotantosuunta,
      GTKdata_raivaamattomat$Kuntakoodi, 
      GTKdata_raivaamattomat$KASVIKOODI_lohkodata_reclass,
      GTKdata_raivaamattomat$KASVINIMI_reclass
    ),
    sum
  )
colnames(GTK_aggregointi_elop) <-
  c("Tuotantosuunta","Kuntakoodi","Kasvikoodi", "Kasvinimi",
    "EloperäistäMaata")


GTK_aggregointi_mineral_raiviot <-
  aggregate(
    list(GTK_raiviot$Mineraalia),
    by = list(
      GTK_raiviot$Tuotantosuunta,
      GTK_raiviot$Kuntakoodi, 
      GTK_raiviot$KASVIKOODI_lohkodata_reclass,
      GTK_raiviot$KASVINIMI_reclass
    ),
    sum
  )
colnames(GTK_aggregointi_mineral_raiviot) <-
  c("Tuotantosuunta","Kuntakoodi","Kasvikoodi", "Kasvinimi", "Mineraalimaata")


GTK_aggregointi_elop_raiviot <-
  aggregate(
    list(GTK_raiviot$Eloperaista),
    by = list(
      GTK_raiviot$Tuotantosuunta,
      GTK_raiviot$Kuntakoodi,
      GTK_raiviot$KASVIKOODI_lohkodata_reclass,
      GTK_raiviot$KASVINIMI_reclass
    ),
    sum
  )
colnames(GTK_aggregointi_elop_raiviot) <-
  c("Tuotantosuunta",
    "Kuntakoodi",
    "Kasvikoodi",
    "Kasvinimi",
    "EloperäistäMaata")

rm.all.but(c("GTK_aggregointi_elop","GTK_aggregointi_elop_raiviot","GTK_aggregointi_mineral","GTK_aggregointi_mineral_raiviot"))
gc()

#Kasvitietojen täydennys kategoriatiedoilla ####
#Aggregointiskriptin lopputotteisiin liitetään luokkatiedot eri kasveista

library(readxl)
Kasvikategoriat_avain <- read_excel(here("Data","Kasvikategoriat_avain.xlsx"), 
                                    col_types = c("text", "text", "text", 
                                                  "numeric", "skip"))
GTK_aggregointi_mineral <-
  merge(GTK_aggregointi_mineral, Kasvikategoriat_avain, by = "Kasvikoodi")
GTK_aggregointi_elop <-
  merge(GTK_aggregointi_elop, Kasvikategoriat_avain, by = "Kasvikoodi")
GTK_aggregointi_mineral_raiviot <-
  merge(GTK_aggregointi_mineral_raiviot,
        Kasvikategoriat_avain,
        by = "Kasvikoodi")
GTK_aggregointi_elop_raiviot <-
  merge(GTK_aggregointi_elop_raiviot,
        Kasvikategoriat_avain,
        by = "Kasvikoodi")
rm(Kasvikategoriat_avain)
gc()

#Data matriisimuotoiseksi, NA:n muutos nolliksi. 
GTK_mineraali <-
  spread(GTK_aggregointi_mineral,
         Tuotantosuunta,
         Mineraalimaata) #Matriisiksi
GTK_mineraali[7:length(GTK_mineraali)] <-
  replace(GTK_mineraali[7:length(GTK_mineraali)], is.na(GTK_mineraali[7:length(GTK_mineraali)]), 0) #jos nolla, on nyt NA. Vaihdetaan takaisin nolliksi
rm(GTK_aggregointi_mineral)

GTK_eloperainen <-
  spread(GTK_aggregointi_elop,
         Tuotantosuunta,
         EloperäistäMaata) #matriisiksi
GTK_eloperainen[7:length(GTK_eloperainen)] <-
  replace(GTK_eloperainen[7:length(GTK_eloperainen)], is.na(GTK_eloperainen[7:length(GTK_eloperainen)]), 0) #jos nolla, on nyt NA. Vaihdetaan takaisin nolliksi
rm(GTK_aggregointi_elop)

GTK_mineraali_raivio<-
  spread(GTK_aggregointi_mineral_raiviot, 
         Tuotantosuunta,
         Mineraalimaata)
GTK_mineraali_raivio[7:length(GTK_mineraali_raivio)] <-
  replace(GTK_mineraali_raivio[7:length(GTK_mineraali_raivio)], is.na(GTK_mineraali_raivio[7:length(GTK_mineraali_raivio)]), 0) #jos nolla, on nyt NA. Vaihdetaan takaisin nolliksi
rm(GTK_aggregointi_mineral_raiviot)

GTK_eloperainen_raivio<-
  spread(GTK_aggregointi_elop_raiviot, 
         Tuotantosuunta,
         EloperäistäMaata)
GTK_eloperainen_raivio[7:length(GTK_eloperainen_raivio)] <-
  replace(GTK_eloperainen_raivio[7:length(GTK_eloperainen_raivio)], is.na(GTK_eloperainen_raivio[7:length(GTK_eloperainen_raivio)]), 0) #jos nolla, on nyt NA. Vaihdetaan takaisin nolliksi
rm(GTK_aggregointi_elop_raiviot)

#Jako Croplandiin ja Grasslandiin

Cropland_korotettu_mineraalimaa<-filter(GTK_mineraali,
                                        GTK_mineraali$`Cropland/grassland` == "Cropland")

Grassland_korotettu_mineraalimaa<-filter(GTK_mineraali,
                                         GTK_mineraali$`Cropland/grassland` == "Grassland")

Cropland_korotettu_elop<-filter(GTK_eloperainen,
                                GTK_eloperainen$`Cropland/grassland` == "Cropland")

Grassland_korotettu_elop<-filter(GTK_eloperainen,
                                 GTK_eloperainen$`Cropland/grassland` == "Grassland")


Cropland_korotettu_mineraalimaa_raivio<-filter(GTK_mineraali_raivio,
                                               GTK_mineraali_raivio$`Cropland/grassland` == "Cropland")

Grassland_korotettu_mineraalimaa_raivio<-filter(GTK_mineraali_raivio,
                                                GTK_mineraali_raivio$`Cropland/grassland` == "Grassland")

Cropland_korotettu_elop_raivio<-filter(GTK_eloperainen_raivio,
                                       GTK_eloperainen_raivio$`Cropland/grassland` == "Cropland")

Grassland_korotettu_elop_raivio<-filter(GTK_eloperainen_raivio,
                                        GTK_eloperainen_raivio$`Cropland/grassland` == "Grassland")

rm(GTK_mineraali, GTK_eloperainen, GTK_mineraali_raivio, GTK_eloperainen_raivio)


#Alat talteen 

Grassland_mineraalimaa_alat <- Grassland_korotettu_mineraalimaa
Grassland_elop_alat <- Grassland_korotettu_elop
Cropland_mineraalimaa_alat <- Cropland_korotettu_mineraalimaa
Cropland_elop_alat <- Cropland_korotettu_elop


Grassland_mineraalimaa_alat_raivio<-Grassland_korotettu_mineraalimaa_raivio
Grassland_elop_alat_raivio<-Grassland_korotettu_elop_raivio
Cropland_mineraalimaa_alat_raivio<-Cropland_korotettu_mineraalimaa_raivio
Cropland_elop_alat_raivio<-Cropland_korotettu_elop_raivio


sum(colSums(Cropland_mineraalimaa_alat[7:length(Cropland_mineraalimaa_alat)]))+
  sum(colSums(Cropland_mineraalimaa_alat_raivio[7:length(Cropland_mineraalimaa_alat_raivio)]))+
  sum(colSums(Cropland_elop_alat[7:length(Cropland_elop_alat)]))+
  sum(colSums(Cropland_elop_alat_raivio[7:length(Cropland_elop_alat_raivio)]))+
sum(colSums(Grassland_mineraalimaa_alat[7:length(Grassland_mineraalimaa_alat)]))+
  sum(colSums(Grassland_mineraalimaa_alat_raivio[7:length(Grassland_mineraalimaa_alat_raivio)]))+
  sum(colSums(Grassland_elop_alat[7:length(Grassland_elop_alat)]))+
  sum(colSums(Grassland_elop_alat_raivio[7:length(Grassland_elop_alat_raivio)]))
