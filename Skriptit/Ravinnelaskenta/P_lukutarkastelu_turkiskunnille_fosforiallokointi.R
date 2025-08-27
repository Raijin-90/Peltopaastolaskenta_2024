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

source(here("Skriptit/Ravinnelaskenta/Ravinnelaskenta_P_VANHA.R"))

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

#Osa 3: 
#Fosforin allokointi tehdään muilta osin samalla tavalla kuin skriptissä Ravinnelaskenta.R

#Allokointi: 

#Määrittele jaettava totaalimassa ravinnetta (P, N)

#tonnia tai kiloa 
totalPMass<- 2543*1000 #kiloa. Lähde: SYKERA 22|2019, Puustinen et al. taulukko 41. 

#Muutos 26/08/25: Huomioidaan typpilaskennan tapaan lannoittamattoman alan emissio 

#Grassland-tyypeissä on kasveja, joille ei tule mineraalilannoitusta (kerroin nolla). Koska niitä ei lannoiteta, ei laiteta niille myöskään lantaa. 
#Jos lannan levitysmäärä kesannoille ym.Muu peltoala-kasvityypeille joilla myöskään mineraalilannoitusta ei ole (kerroin = 0) halutaan nollata, 
#se suodatus on tehtävä ylläoleviin lohkodataTrimmed_turkistilojen_muutokset aloihin. Lannan ravinnetonnit loppusummana eivät muutu, koska pohjaavat eläinten määrään eikä pinta-alaan. 
#Koskee oheisia kasvikoodeja, jotka eritelty Kasvilista_lannoitus.xlsx tiedostoon tunnisteella "Ei lannoiteta" = 1, ETTL "muu peltoala". 

Ei_lantaa <-c(6050,
              6051,
              6220,
              6300,
              6600,
              6710,
              6720,
              9101,
              9102,
              9403,
              9404,
              9405,
              9412,
              9413,
              9422,
              9423,
              9424,
              9620,
              9700,
              9801,
              9802,
              9803,
              9804,
              9805,
              9806,
              9807,
              9808,
              9810,
              9811,
              9812,
              9820,
              9830)

#Muutos 22/8/2025:
#Näiltä lohkoilta tuleva luonnonhuutouma lasketaan erillisistä kertoimista jotka kuvaavat valuma-alueita ilman ihmisvaikutusta (Mattson et al 2003). 
#Hylätylle peltomaalle spesifiä tällaista kerrointa ei ole.
#Allokoitavaa totaalia typen ja fosforin osalta vähennetään tämän verran. 
#Lasketaan tässä, vähennetään tilatyypittäisessä skriptissä. 

Luonnonhuuhtouma<-lohkodataTrimmed_turkistilojen_muutokset %>% filter((KASVIKOODI_lohkodata_reclass %in% Ei_lantaa)) 

luonnHuuht_P <- (5.4/100) #5.4 kg/km2 typpeä -> kg/ha muunto

LH_summat_alat<-Luonnonhuuhtouma %>%  mutate(Luonnonhuuhtouman_fosfori = Maannossumma*luonnHuuht_P ) %>% group_by(Tuotantosuunta) %>% summarise(Luonnonhuuhtouman_P = sum(Luonnonhuuhtouman_fosfori),
                                                                                                                                                Hehtaarit=sum(Maannossumma)) 
#Vähennetään allokoitavasta fosforitotaalista tämä osuus

totalPMass <- totalPMass - sum(LH_summat_alat$Luonnonhuuhtouman_P) 


#Jatketaan laskentaa

#Poissuljetaan ne, joille lantaa ei laiteta
lohkodataTrimmed_turkistilojen_muutokset<-lohkodataTrimmed_turkistilojen_muutokset %>% filter(!(KASVIKOODI_lohkodata_reclass %in% Ei_lantaa)) 


#Kerrotaan joka lohkon P-luvulla sen pinta-alaa

lohkodataTrimmed_turkistilojen_muutokset$Pluku_ala_tulo <- lohkodataTrimmed_turkistilojen_muutokset$Korotettu_Pluku*lohkodataTrimmed_turkistilojen_muutokset$Maannossumma

#Summataan tulot
prodsum<-sum(lohkodataTrimmed_turkistilojen_muutokset$Pluku_ala_tulo)

#Jaetaan totalPMass tulojen summalla. Saadaan kerroin, mallia kg P/P-luku/ha. 
#Koska kerrointa ei lasketa pelkästään hehtaareja kohti vaan hehtaareita JA P-LUKUA, kts rivit 72 ja 75, sillä ei saa kertoa pelkkiä hehtaareita -> tulisi vajausta, kun P-luku jää puuttumaan. 
#Kerrotaan P-luvun ja alan tuloa sen sijaan. 

Pcoeff<-totalPMass/prodsum


lohkodataTrimmed_turkistilojen_muutokset$Pmass<-Pcoeff*lohkodataTrimmed_turkistilojen_muutokset$Pluku_ala_tulo
#lohkodataTrimmed_turkistilojen_muutokset$Nmass<-Ncoeff*lohkodataTrimmed_turkistilojen_muutokset$Pluku_ala_tulo

t<-sum(lohkodataTrimmed_turkistilojen_muutokset$Pmass) == totalPMass
stopifnot(t == TRUE)
rm.all.but(c("lohkodataTrimmed_turkistilojen_muutokset","LH_summat_alat"))


#LASKETAAN TUOTANTOSUUNTAKOHTAISET INTENSITEETIT 
#Tulosten aggregointi: keskiarvot ja keskihajonnat. Ensin tilatason intensiteetit

Fosfori_aggre<-lohkodataTrimmed_turkistilojen_muutokset %>% group_by(ETOL, ETOL_koodi, MAATILA_TUNNUS) %>% summarise(kg_P_lannoitettu_ala = sum(Pmass),
                                                                                       Lannoitettu_ala = sum(Maannossumma),
                                                                                       kg_P_ha = kg_P_lannoitettu_ala/Lannoitettu_ala)  

Fosfori_aggre_ka<- Fosfori_aggre %>% group_by(ETOL, ETOL_koodi) %>% summarise(kg_P_lannoitettu_ala = sum(kg_P_lannoitettu_ala), 
                                                              Lannoitettu_ala = sum(Lannoitettu_ala),
                                                              kg_P_ha_keskimaar_intensiteetti = mean(kg_P_ha),
                                                              Intensiteetin_hajonta = sd(kg_P_ha))


#Luonnonhuuhtoumatiedot kytketään samaan tauluun

Muuntoavain_tuotantosuunnat_tuotteet_ETOL <- read_excel("Data/Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx")
colnames(Muuntoavain_tuotantosuunnat_tuotteet_ETOL)[colnames(Muuntoavain_tuotantosuunnat_tuotteet_ETOL)=="Tuotantosuunta_original"]<-"Tuotantosuunta"

LH_summat_alat<-inner_join(LH_summat_alat,Muuntoavain_tuotantosuunnat_tuotteet_ETOL,by=c("Tuotantosuunta"))

LH_Fosfori_aggre<-LH_summat_alat %>% group_by(ETOL_koodi, ETOL) %>% summarise(Luonnonhuuhtouman_P = sum(Luonnonhuuhtouman_P),
                                                            Lannoittamattomat_hehtaarit = sum(Hehtaarit)) 

Fosfori_tuotsuunnat<-inner_join(Fosfori_aggre_ka,LH_Fosfori_aggre, by=c("ETOL","ETOL_koodi"))

#Summien tarkistus
sum(Fosfori_tuotsuunnat$Lannoittamattomat_hehtaarit)+sum(Fosfori_tuotsuunnat$Lannoitettu_ala)
sum(Fosfori_tuotsuunnat$kg_HEP_lannoitettu_ala)+sum(Fosfori_tuotsuunnat$Luonnonhuuhtouman_P)

library(openxlsx)
TS_intensiteetit<-createWorkbook()
addWorksheet(TS_intensiteetit, "Fosfori_tuotsuunnat")
writeData(TS_intensiteetit, "Fosfori_tuotsuunnat", Fosfori_tuotsuunnat)
saveWorkbook(TS_intensiteetit, here("Output/Ravinnedata/Emissiotulokset/Fosforin_intensiteetit_tilatyypeille_luonnonhuuht_poistettuna.xlsx"),overwrite = T) 


#Aggregointi tarkimmalle kasvitasolle

kasvitAggre<-lohkodataTrimmed_turkistilojen_muutokset %>% group_by(KASVIKOODI_lohkodata_reclass,KASVINIMI_reclass) %>% summarise(kg_P_lannoitettu_ala = sum(Pmass), 
                                                                                         Lannoitettu_ala = sum(Maannossumma))
#Satokerrointen liitos,  liitetään niin että kaikki kasvit jäävät, oli niillä satokerroin tai ei

library(readxl)
Satokertoimet <- read_excel("Data/Satokertoimet.xlsx")
colnames(Satokertoimet)<-c("KASVIKOODI_lohkodata_reclass","KASVINIMI_reclass","Hehtaarisato_tonnia_ha")

kasvitAggre<-left_join(kasvitAggre, Satokertoimet, by=c("KASVIKOODI_lohkodata_reclass","KASVINIMI_reclass"))

kasvitAggre<-kasvitAggre %>% mutate(Sato_tn = Lannoitettu_ala*Hehtaarisato_tonnia_ha)

#lasketaan intensiteetti kg P/kg satoa
kasvitAggre<-kasvitAggre %>% mutate(Intensiteetti_kgP_kg = kg_P_lannoitettu_ala/(1000*Sato_tn)) 

Kasviaggregointi<-createWorkbook()
addWorksheet(Kasviaggregointi, "P_kasvit_intensiteetit")
writeData(Kasviaggregointi, "P_kasvit_intensiteetit", kasvitAggre)
saveWorkbook(Kasviaggregointi,file=here("Output/Ravinnedata/Emissiotulokset/Fosforin_intensiteetit_kasveille.xlsx"), overwrite = T)




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




