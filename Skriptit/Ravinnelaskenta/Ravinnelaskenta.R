#Ravinnepäästöjen laskentaskripti 
#09042025 HV

#Perustuu laajempaan peltolohkoaineistoon, johon on liitetty tieto peltojen P-luvuista. 
#Lohkoille, joille P-lukutietoa ei ole, on laskettu viljavuusdatan pohjalta mediaani-P kullekin kasvi- ja tuotantosuunta - kombinaatiolle, ja annettu se arvo lohkolle. 
#Yhdistelmissä joita viljavuusdata ei tunnista, on käytetty tuotantosuunnan croplandien tai grasslandien mediaania, sen mukaan millainen kasvi puuttui.  

library(stringr);library(usefun);library(varhandle);library(here);library(tidyverse);library(gt);library(gtsummary)

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

#Imputoidun P-lukudatan sisäänotto
library(readr)
taydennettyLohkoaineisto <- read_delim("Data/Ravinnelaskennan_aineisto/Lohkoaineisto_imputoidut_Pluvut.csv", 
                                              delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                                              trim_ws = TRUE)

#Maalaji määrittyy GTK-datan järjestelmän mukaisesti: turve orgaanista, muu mineraalia 
#Helppotajuistetaan aineistoa poistamalla tässä turhia muuttujia..
lohkodataTrimmed<-taydennettyLohkoaineisto %>% select( 
       MAATILA_TUNNUS,
       PLTUNNUS,
       KLTUNNUS,
       ALA_HA,
       PL_ALA_HA,
       KOK_PELTOALA,
       KESKIARVO_,
       KASVIKOODI_lohkodata_reclass,
       KASVINIMI_reclass,
       Tuotantosuunta,
       Maannossumma,
       Eloperaista, 
       Mineraalia
       )
rm(taydennettyLohkoaineisto)
gc()

#Linkataan kasvit ja tuotantosuunnat ETOL- ja ETTL-luokkiin

library(readxl)
Kasvit <- read_excel("Data/Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx", sheet="Kasvit_ETTL_koodeittain" )
colnames(Kasvit)[colnames(Kasvit) == "Ruokaviraston koodi"]<-"KASVIKOODI_lohkodata_reclass"
Kasvit<-Kasvit %>% select(ETTL, `ETTL Nimike`, KASVIKOODI_lohkodata_reclass)

Tuotsuunnat<-read_excel("Data/Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx", sheet="Tuotantosuunnat ryhmittäin" )
colnames(Tuotsuunnat)[colnames(Tuotsuunnat) == "Tuotantosuunta_original"]<-"Tuotantosuunta"

lohkodataTrimmed<-inner_join(lohkodataTrimmed, Kasvit, by="KASVIKOODI_lohkodata_reclass")
lohkodataTrimmed<-inner_join(lohkodataTrimmed, Tuotsuunnat, by="Tuotantosuunta")

#rimmatun datan tallennus
#write.csv2(lohkodataTrimmed, file=here("Data/Ravinnelaskennan_aineisto/Kavennettu_lohkodata_imputoituP.csv"))

#Trimmattu versio tallennetaan nimellä "Kavennettu_lohkodata_imputoituP.csv", jotta saadaan tiedoston kokoa alemmas. 

#LASKENTA

#Määrittele jaettava totaalimassa ravinnetta (P, N)

#tonnia tai kiloa 
totalPMass<- 2543*1000 #kiloa. Lähde: SYKERA 22|2019, Puustinen et al. taulukko 41. 
#totalNmass <- INSERT VALUE

#Kerrotaan joka lohkon P-luvulla sen pinta-alaa

lohkodataTrimmed$Pluku_ala_tulo <- lohkodataTrimmed$KESKIARVO_*lohkodataTrimmed$Maannossumma

#Summataan tulot
prodsum<-sum(lohkodataTrimmed$Pluku_ala_tulo)

#Jaetaan totalPMass tulojen summalla. Saadaan kerroin, mallia kg P/P-luku/ha. 
#Koska kerrointa ei lasketa pelkästään hehtaareja kohti vaan hehtaareita JA P-LUKUA, kts rivit 72 ja 75, sillä ei saa kertoa pelkkiä hehtaareita -> tulisi vajausta, kun P-luku jää puuttumaan. 
#Kerrotaan P-luvun ja alan tuloa sen sijaan. 

Pcoeff<-totalPMass/prodsum
#Ncoeff<-totalNMass/Prodsum

lohkodataTrimmed$Pmass<-Pcoeff*lohkodataTrimmed$Pluku_ala_tulo
#lohkodataTrimmed$Nmass<-Ncoeff*lohkodataTrimmed$Pluku_ala_tulo

t<-sum(lohkodataTrimmed$Pmass) == totalPMass
stopifnot(t == TRUE)
rm.all.but("lohkodataTrimmed")

#Tulosten aggregointi
#Tarkin taso kasvien osalta. ETOL-ETTL aggregointi onnistuu molemmista

a<-lohkodataTrimmed %>% group_by(ETOL, ETOL_koodi) %>% summarise(P_load_kg = sum(Pmass))
b<-lohkodataTrimmed %>% group_by(KASVIKOODI_lohkodata_reclass, KASVINIMI_reclass) %>% summarise(P_load_kg = sum(Pmass))
c<-lohkodataTrimmed %>% group_by(ETOL, ETOL_koodi,KASVIKOODI_lohkodata_reclass, KASVINIMI_reclass) %>% summarise(P_load_kg = sum(Pmass))
d<-lohkodataTrimmed %>% group_by(ETOL, ETOL_koodi,ETTL, `ETTL Nimike`) %>% summarise(P_load_kg = sum(Pmass))

Output<-createWorkbook()

addWorksheet(Output, "ETOL")
writeData(Output, "ETOL", a)
addWorksheet(Output, "Kasveittain")
writeData(Output, "Kasveittain", b)
addWorksheet(Output, "ETOL&Kasvi")
writeData(Output, "ETOL&Kasvi", c)
addWorksheet(Output, "ETOL&ETTL")
writeData(Output, "ETOL&ETTL", d)

saveWorkbook(Output, here("Output/Ravinnedata/Emissiotulokset/Fosforin_jako_peltolohkoille.xlsx"), overwrite = T)



