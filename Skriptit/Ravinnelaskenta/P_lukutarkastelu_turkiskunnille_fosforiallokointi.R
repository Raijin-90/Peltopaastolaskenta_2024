library(tidyverse);library(openxlsx);library(here);library(varhandle);library(stringr)

#' Source specific lines in an R file
#'
#' @param file character string with the path to the file to source.
#' @param lines numeric vector of lines to source in \code{file}.

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

#Imputoiduilla P-luvuilla täydennetty peltolohkoaineisto

library(readr)
Kavennettu_lohkodata_imputoituP <- read_delim("Data/Ravinnelaskennan_aineisto/Kavennettu_lohkodata_imputoituP.csv", 
                                              delim = ";", escape_double = FALSE, col_types = cols(...1 = col_skip()), 
                                              locale = locale(decimal_mark = ","), 
                                              trim_ws = TRUE)
library(readxl)
Turkistarhaus_kunnittain_2015 <- read_excel("Data/Ravinnelaskennan_aineisto/Turkistarhaus_kunnittain_2015.xlsx", 
                                            sheet = "2017")
Turkistarhaus_kunnittain_2015<-Turkistarhaus_kunnittain_2015 %>% slice(-77:-79)

#Peltolohkokoodeista 3 ekaa nroa eli kunnan koodi irti 

Kavennettu_lohkodata_imputoituP$Kuntakoodi<-substr(Kavennettu_lohkodata_imputoituP$PLTUNNUS, start = 1, stop = 3)

#Irrotetaan liikevaihtodatasta turkistarhausta sisältävien kuntien koodit. Tässä tapauksessa vain keskittymät.  

Turkiskunnat <- Turkistarhaus_kunnittain_2015 %>%  filter(!(is.na(Keskittymä)))
Turkiskunnat<-unique(Turkistarhaus_kunnittain_2015$Kuntakoodi)

Turkiskuntien_pellot<- Kavennettu_lohkodata_imputoituP %>% filter(Kuntakoodi %in% Turkiskunnat)

#Sitten kaikkien muiden paitsi turkiskuntien pellot

Muiden_kuntien_pellot<- Kavennettu_lohkodata_imputoituP %>% filter(!(Kuntakoodi %in% Turkiskunnat))

rm.all.but(c("Muiden_kuntien_pellot","Turkiskuntien_pellot","Turkistarhaus_kunnittain_2015"))

#Turkiskuntien P-luvut viljelykasveittain

turkisk_aggre<-Turkiskuntien_pellot %>% group_by(KASVIKOODI_lohkodata_reclass, KASVINIMI_reclass) %>% summarise(Keskimaar_Pluku_turkiskunnat = mean(KESKIARVO_))

#Muiden kuntien vastaava aggregointi

muutk_aggre<- Muiden_kuntien_pellot %>% group_by(KASVIKOODI_lohkodata_reclass, KASVINIMI_reclass) %>% summarise(Keskimaar_Pluku_muut_kunnat = mean(KESKIARVO_))


#Liitto

liitos<-left_join(muutk_aggre, turkisk_aggre, by=c("KASVIKOODI_lohkodata_reclass", "KASVINIMI_reclass"))

write.xlsx(liitos, file=here("Output/Ravinnedata/P_lukuvertailu_turkiskunnat_muut.xlsx"))

##############################################################################################
#Osa 2: Turkistilojen P-ylimäärä 

#päästöjen laskenta vakioarvoista
#Vakioarvoista laskettu, lohkokohtainen fosforikuorma

source(here("Skriptit/Ravinnelaskenta/Ravinnelaskenta.R"))

rm.all.but("lohkodataTrimmed")

#Lohkojen & emissioiden erottelu kunnittain kuntakoodilla
lohkodataTrimmed$Kuntakoodi<-substr(lohkodataTrimmed$PLTUNNUS, start = 1, stop = 3)

library(readxl)
Turkistarhaus_kunnittain_2015 <- read_excel("Data/Ravinnelaskennan_aineisto/Turkistarhaus_kunnittain_2015.xlsx", 
                                            sheet = "2017")
Turkistarhaus_kunnittain_2015<-Turkistarhaus_kunnittain_2015 %>% slice(-77:-79)


Turkiskunnat <- Turkistarhaus_kunnittain_2015 %>%  filter(!(is.na(Keskittymä)))
Turkiskunnat<-unique(Turkistarhaus_kunnittain_2015$Kuntakoodi)


Turkiskuntien_lohkot<-lohkodataTrimmed %>% filter(Kuntakoodi %in% Turkiskunnat)

sum(Turkiskuntien_lohkot$Pmass)

#Skriptin 1 osan lopputuotteesta P_lukuvertailu_turkiskunnat_muut.xlsx tiedetään, mitkä turkiskuntien kasvit ovat keskimäärin korkeammalla p-luvulla 
#kuin muissa kunnissa, ja kuinka paljon P-luvun erotus keskimäärin on. Erotus voidaan kohdistaa kasvikoodilla P-lukuihin ja korottaa niitä ko. verran. 

P_lukuvertailu_turkiskunnat_muut <- read_excel("Output/Ravinnedata/P_lukuvertailu_turkiskunnat_muut.xlsx")

#Eritellään ne kasvit joissa pluku turkiskunnissa on keskimäärin suurempi kuin muualla

P_lukuvertailu_turkiskunnat_muut<-P_lukuvertailu_turkiskunnat_muut %>% mutate(Koholla = case_when(Keskimaar_Pluku_muut_kunnat < Keskimaar_Pluku_turkiskunnat ~ TRUE))

#Näille kasveille lasketaan erotus keskimäär.  P-luvusta, turkiskunnat miinus muut

P_lukuvertailu_turkiskunnat_muut<-P_lukuvertailu_turkiskunnat_muut %>% mutate(Erotus =case_when(Koholla == T ~ Keskimaar_Pluku_turkiskunnat-Keskimaar_Pluku_muut_kunnat))

P_luvun_korotus<- P_lukuvertailu_turkiskunnat_muut %>% select(KASVIKOODI_lohkodata_reclass,KASVINIMI_reclass,Erotus) %>% filter(!is.na(Erotus))

rm.all.but(c("P_luvun_korotus","Turkiskuntien_lohkot","lohkodataTrimmed"))

#Liitetään erotus kasvikoodilla turkiskuntien lohkoihin. Korotetaan vakio-P lukua erotuksen verran. 

P_luvun_korotus<-P_luvun_korotus %>% select(KASVIKOODI_lohkodata_reclass, Erotus)
Turkiskuntien_lohkot<-left_join(Turkiskuntien_lohkot,P_luvun_korotus, by="KASVIKOODI_lohkodata_reclass")

#Korotetaan näiden lohkojen vakio-P lukua erotuksen verran. Jos kasville ei tule korotusta (Pluku ei koholla turkiskunnissa), korotusosa on 0

Turkiskuntien_lohkot$Erotus[is.na(Turkiskuntien_lohkot$Erotus)]<-0
Turkiskuntien_lohkot<-Turkiskuntien_lohkot %>% mutate(Korotettu_Pluku = KESKIARVO_+Erotus)

#Irrotetaan muiden kuntien lohkot, harmonisoidaan muuttujat. Muilla kunnilla "Korotettu Pluku" on sama, kuin alkuperäinen 

Muiden_kuntien_lohkot<-lohkodataTrimmed %>% filter(!(Kuntakoodi %in% Turkiskuntien_lohkot$Kuntakoodi))
Muiden_kuntien_lohkot$Korotettu_Pluku <- Muiden_kuntien_lohkot$KESKIARVO_
Muiden_kuntien_lohkot$Erotus <- 0

#Nämä yhdistetään takaisin kokonaiseksi lohkodataksi tehdyin muutoksin. 

lohkodataTrimmed_turkistilojen_muutokset<-rbind(Turkiskuntien_lohkot, Muiden_kuntien_lohkot)

rm.all.but("lohkodataTrimmed_turkistilojen_muutokset")


#Fosforin allokointi tehdään muilta osin samalla tavalla kuin skriptissä Ravinnelaskenta.R

#Allokointi: 

#Määrittele jaettava totaalimassa ravinnetta (P, N)

#tonnia tai kiloa 
totalPMass<- 2543*1000 #kiloa. Lähde: SYKERA 22|2019, Puustinen et al. taulukko 41. 
#totalNmass <- INSERT VALUE

#Kerrotaan joka lohkon P-luvulla sen pinta-alaa

lohkodataTrimmed_turkistilojen_muutokset$Pluku_ala_tulo <- lohkodataTrimmed_turkistilojen_muutokset$Korotettu_Pluku*lohkodataTrimmed_turkistilojen_muutokset$Maannossumma

#Summataan tulot
prodsum<-sum(lohkodataTrimmed_turkistilojen_muutokset$Pluku_ala_tulo)

#Jaetaan totalPMass tulojen summalla. Saadaan kerroin, mallia kg P/P-luku/ha. 
#Koska kerrointa ei lasketa pelkästään hehtaareja kohti vaan hehtaareita JA P-LUKUA, kts rivit 72 ja 75, sillä ei saa kertoa pelkkiä hehtaareita -> tulisi vajausta, kun P-luku jää puuttumaan. 
#Kerrotaan P-luvun ja alan tuloa sen sijaan. 

Pcoeff<-totalPMass/prodsum
#Ncoeff<-totalNMass/Prodsum

lohkodataTrimmed_turkistilojen_muutokset$Pmass<-Pcoeff*lohkodataTrimmed_turkistilojen_muutokset$Pluku_ala_tulo
#lohkodataTrimmed_turkistilojen_muutokset$Nmass<-Ncoeff*lohkodataTrimmed_turkistilojen_muutokset$Pluku_ala_tulo

t<-sum(lohkodataTrimmed_turkistilojen_muutokset$Pmass) == totalPMass
stopifnot(t == TRUE)
rm.all.but("lohkodataTrimmed_turkistilojen_muutokset")

#Tulosten aggregointi
#Tarkin taso kasvien osalta. ETOL-ETTL aggregointi onnistuu molemmista

a<-lohkodataTrimmed_turkistilojen_muutokset %>% group_by(ETOL, ETOL_koodi) %>% summarise(P_load_kg = sum(Pmass))
b<-lohkodataTrimmed_turkistilojen_muutokset %>% group_by(KASVIKOODI_lohkodata_reclass, KASVINIMI_reclass) %>% summarise(P_load_kg = sum(Pmass))
c<-lohkodataTrimmed_turkistilojen_muutokset %>% group_by(ETOL, ETOL_koodi,KASVIKOODI_lohkodata_reclass, KASVINIMI_reclass) %>% summarise(P_load_kg = sum(Pmass))
d<-lohkodataTrimmed_turkistilojen_muutokset %>% group_by(ETOL, ETOL_koodi,ETTL, `ETTL Nimike`) %>% summarise(P_load_kg = sum(Pmass))

Output<-createWorkbook()

addWorksheet(Output, "ETOL")
writeData(Output, "ETOL", a)
addWorksheet(Output, "Kasveittain")
writeData(Output, "Kasveittain", b)
addWorksheet(Output, "ETOL&Kasvi")
writeData(Output, "ETOL&Kasvi", c)
addWorksheet(Output, "ETOL&ETTL")
writeData(Output, "ETOL&ETTL", d)

saveWorkbook(Output, here("Output/Ravinnedata/Emissiotulokset/Fosforin_jako_peltolohkoille_turkistilojen_ylimaara.xlsx"), overwrite = T)




