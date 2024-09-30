library(here)
#Sisäänluenta ja multavuusdatan yhdistäminen peltoaineistoon ####


library(readr)
Yhdistetty_peltodata_raivaukset_rehuvilja <- read_csv("Data/Yhdistetty_peltodata_raivaukset_rehuvilja.csv", 
                                                      col_types = cols(...1 = col_skip()))


#Otetaan lohkoittaiset viljavuusdatan multavuudet, liitetään 
library(readxl)
Lohkojen_multavuudet <- read_excel(here("Data","Lohkojen_multavuudet.xlsx"))
colnames(Lohkojen_multavuudet)<-c("PLTUNNUS","Multavuusluokka","Orgaanisen_aineen_pitoisuuspros")


Yhdistetty_peltodata_raivaukset_rehuvilja_multavuus<-merge(Yhdistetty_peltodata_raivaukset_rehuvilja, Lohkojen_multavuudet, by="PLTUNNUS", all=T)



rm(Lohkojen_multavuudet, Yhdistetty_peltodata_raivaukset_rehuvilja)

Kasvikategoriat_avain <- read_excel(here("Data","Kasvikategoriat_avain.xlsx"), 
                                    col_types = c("text", "text", "text", 
                                                  "numeric", "skip"))

#Virheiden korjaaminen ####

library(dplyr)


Viljavuusdata <-
  Yhdistetty_peltodata_raivaukset_rehuvilja_multavuus %>% filter(!(is.na(KESKIARVO_))) %>% select(PLTUNNUS, KLTUNNUS,MAATILA_TUNNUS,8,57, 30:36, 38:40, 92, 89, 94:95)


o<-unique(Viljavuusdata$PLTUNNUS)
multavuuspuuttuu<-filter(Viljavuusdata, is.na(Viljavuusdata$Multavuusluokka))
y<-unique(multavuuspuuttuu$PLTUNNUS)

attach(Viljavuusdata)
Viljavuusdata$Savet_ha <- SAVET_PR * KASVI_ALA_HA
Viljavuusdata$Hiesut_ha <- HIESUT_PR * KASVI_ALA_HA
Viljavuusdata$Karkeat_ha <- KARKEAT_PR * KASVI_ALA_HA
Viljavuusdata$Lieju_ha <- LIEJU_MUTA_PR * KASVI_ALA_HA
Viljavuusdata$Multa_ha <- MULTAMAA_PR * KASVI_ALA_HA
Viljavuusdata$Turve_ha <- TURVEMAAT_PR * KASVI_ALA_HA
detach(Viljavuusdata)

Viljavuusdata$Maalajisumma<-Viljavuusdata$Savet_ha+Viljavuusdata$Hiesut_ha+Viljavuusdata$Karkeat_ha+Viljavuusdata$Lieju_ha+Viljavuusdata$Turve_ha+Viljavuusdata$Multa_ha


Viljavuusdata$SAVET_PR <-NULL
Viljavuusdata$HIESUT_PR<-NULL 
Viljavuusdata$KARKEAT_PR <-NULL
Viljavuusdata$LIEJU_MUTA_PR <-NULL
Viljavuusdata$MULTAMAA_PR <-NULL
Viljavuusdata$TURVEMAAT_PR <-NULL



#Rehuviljojen ja elintarvikeviljojen erittelyn korjaus: 
#14002 = eläintilojen kaura, 14001 kasvitilojen Sama muille.
#Kaikki eläimettömät tilatyypit = kasvitiloja, jotka tuottavat elintarvikeviljaa. 
#Rehuohra tuotteena on oma koodinsa, rehuksi tuottajasta riippumatta

Eläintilat<-c("Sikatilat",
              "Maitotilat",
              "Lammas- ja vuohitilat",
              "Siipikarjatilat",
              "Muut nautakarjatilat",      
              "Hevostilat",
              "Munatilat")

#Muutos 15.1: poiskytketään erittely. Tämä merkittävä vasta elinkaaristen vaikutusten vaiheessa. Tällöin anna yksilölliset koodit, esim 14001 eläintilat ja 14002 kasvitilat, ja vastaavasti muille. 
Viljavuusdata$KASVITUNNU_reclass[Viljavuusdata$Tuotantosuunta %in% Eläintilat & Viljavuusdata$KASVITUNNU ==1400]<-1400
Viljavuusdata$KASVITUNNU_reclass[Viljavuusdata$Tuotantosuunta %in% Eläintilat & Viljavuusdata$KASVITUNNU ==1120]<-1120
Viljavuusdata$KASVITUNNU_reclass[Viljavuusdata$Tuotantosuunta %in% Eläintilat & Viljavuusdata$KASVITUNNU ==1110]<-1110
Viljavuusdata$KASVITUNNU_reclass[Viljavuusdata$Tuotantosuunta %in% Eläintilat & Viljavuusdata$KASVITUNNU ==1230]<-1230
Viljavuusdata$KASVITUNNU_reclass[Viljavuusdata$Tuotantosuunta %in% Eläintilat & Viljavuusdata$KASVITUNNU ==1220]<-1220
Viljavuusdata$KASVITUNNU_reclass[Viljavuusdata$Tuotantosuunta %in% Eläintilat & Viljavuusdata$KASVITUNNU ==1410]<-1410
Viljavuusdata$KASVITUNNU_reclass[Viljavuusdata$Tuotantosuunta %in% Eläintilat & Viljavuusdata$KASVITUNNU ==1330]<-1330 #syysohra


#Elintarviketilat - elintarvikeviljaa
Kasvitilat<-Viljavuusdata$Tuotantosuunta[!(Viljavuusdata$Tuotantosuunta %in% Eläintilat)]
Kasvitilat<-unique(Kasvitilat)


Viljavuusdata$KASVITUNNU_reclass[Viljavuusdata$Tuotantosuunta %in% Kasvitilat & Viljavuusdata$KASVITUNNU ==1400]<-1400
Viljavuusdata$KASVITUNNU_reclass[Viljavuusdata$Tuotantosuunta %in% Kasvitilat & Viljavuusdata$KASVITUNNU ==1120]<-1120
Viljavuusdata$KASVITUNNU_reclass[Viljavuusdata$Tuotantosuunta %in% Kasvitilat & Viljavuusdata$KASVITUNNU ==1110]<-1110
Viljavuusdata$KASVITUNNU_reclass[Viljavuusdata$Tuotantosuunta %in% Kasvitilat & Viljavuusdata$KASVITUNNU ==1230]<-1230
Viljavuusdata$KASVITUNNU_reclass[Viljavuusdata$Tuotantosuunta %in% Kasvitilat & Viljavuusdata$KASVITUNNU ==1220]<-1220
Viljavuusdata$KASVITUNNU_reclass[Viljavuusdata$Tuotantosuunta %in% Kasvitilat & Viljavuusdata$KASVITUNNU ==1410]<-1410
Viljavuusdata$KASVITUNNU_reclass[Viljavuusdata$Tuotantosuunta %in% Kasvitilat & Viljavuusdata$KASVITUNNU ==1330]<-1330 #syysohra


#Annetaan uudelleenluokitetuille rehu/elintarvikeviljoille nimet


Viljavuusdata$KASVINIMI_reclass<-Viljavuusdata$KASVINIMI


#Viljavuusdata$KASVINIMI_reclass[Viljavuusdata$KASVITUNNU_reclass == 14001 ]<-c("Kaura kasvitiloilta")
#Viljavuusdata$KASVINIMI_reclass[Viljavuusdata$KASVITUNNU_reclass == 14002 ]<-c("Kaura eläintiloilta")

#Viljavuusdata$KASVINIMI_reclass[Viljavuusdata$KASVITUNNU_reclass == 11201 ]<-c("Kevätvehnä kasvitiloilta")
#Viljavuusdata$KASVINIMI_reclass[Viljavuusdata$KASVITUNNU_reclass == 11202 ]<-c("Kevätvehnä eläintiloilta")

#Viljavuusdata$KASVINIMI_reclass[Viljavuusdata$KASVITUNNU_reclass == 11101 ]<-c("Syysvehnä kasvitiloilta")
#Viljavuusdata$KASVINIMI_reclass[Viljavuusdata$KASVITUNNU_reclass == 11102 ]<-c("Syysvehnä eläintiloilta")

#Viljavuusdata$KASVINIMI_reclass[Viljavuusdata$KASVITUNNU_reclass == 12301 ]<-c("Syysruis kasvitiloilta")
#Viljavuusdata$KASVINIMI_reclass[Viljavuusdata$KASVITUNNU_reclass == 12302 ]<-c("Syysruis eläintiloilta")


#Viljavuusdata$KASVINIMI_reclass[Viljavuusdata$KASVITUNNU_reclass == 12201 ]<-c("Kevätruis kasvitiloilta")
#Viljavuusdata$KASVINIMI_reclass[Viljavuusdata$KASVITUNNU_reclass == 12202 ]<-c("Kevätruis eläintiloilta")

#Viljavuusdata$KASVINIMI_reclass[Viljavuusdata$KASVITUNNU_reclass == 14101 ]<-c("Syyskaura kasvitiloilta")
#Viljavuusdata$KASVINIMI_reclass[Viljavuusdata$KASVITUNNU_reclass == 14102 ]<-c("Syyskaura eläintiloilta")

#Viljavuusdata$KASVINIMI_reclass[Viljavuusdata$KASVITUNNU_reclass == 13301 ]<-c("Syysohra kasvitiloilta")
#Viljavuusdata$KASVINIMI_reclass[Viljavuusdata$KASVITUNNU_reclass == 13302 ]<-c("Syysohra eläintiloilta")



#Korjataan muutama tyhjäksi jäänyt, mutta tunnettu, kasvikoodin nimi
z<-filter(Viljavuusdata, Viljavuusdata$KASVINIMI_reclass == "#N/A")
unique(z$KASVITUNNU_reclass)

Viljavuusdata$KASVINIMI_reclass[Viljavuusdata$KASVINIMI_reclass == "#N/A" & Viljavuusdata$KASVITUNNU_reclass==1410]<-c("Syyskaura")  
Viljavuusdata$KASVINIMI_reclass[Viljavuusdata$KASVINIMI_reclass == "#N/A" & Viljavuusdata$KASVITUNNU_reclass==2450]<-"Linssi" 
Viljavuusdata$KASVINIMI_reclass[Viljavuusdata$KASVINIMI_reclass == "#N/A" & Viljavuusdata$KASVITUNNU_reclass==4010]<-"Tupakka"  
Viljavuusdata$KASVINIMI_reclass[Viljavuusdata$KASVINIMI_reclass == "#N/A" & Viljavuusdata$KASVITUNNU_reclass==5133]<-"Lamopinaatti"
Viljavuusdata$KASVINIMI_reclass[Viljavuusdata$KASVINIMI_reclass == "#N/A" & Viljavuusdata$KASVITUNNU_reclass==5175]<-"Lehtimangoldi"
Viljavuusdata$KASVINIMI_reclass[Viljavuusdata$KASVINIMI_reclass == "#N/A" & Viljavuusdata$KASVITUNNU_reclass==5176]<-"Latva-artisokka"  
Viljavuusdata$KASVINIMI_reclass[Viljavuusdata$KASVINIMI_reclass == "#N/A" & Viljavuusdata$KASVITUNNU_reclass==1310]<-"Rehuohra"  
Viljavuusdata$KASVINIMI_reclass[Viljavuusdata$KASVINIMI_reclass == "#N/A" & Viljavuusdata$KASVITUNNU_reclass==4210]<-"Kevätrapsi"  


#2017 on vuosi, ei kasvikoodi. Annetaan sille lohkodatan mukainen kasvikoodi ja kasvinimi 
Viljavuusdata$KASVINIMI_reclass[Viljavuusdata$KASVITUNNU_reclass == 2017 & Viljavuusdata$KASVIKOODI == 1310] <- "Rehuohra"  
Viljavuusdata$KASVINIMI_reclass[Viljavuusdata$KASVITUNNU_reclass == 2017 & Viljavuusdata$KASVIKOODI == 4210]<- "Kevätrapsi"

Viljavuusdata$KASVITUNNU_reclass[Viljavuusdata$KASVITUNNU_reclass == 2017 & Viljavuusdata$KASVIKOODI == 1310] <- 1310
Viljavuusdata$KASVITUNNU_reclass[Viljavuusdata$KASVITUNNU == 2017 & Viljavuusdata$KASVIKOODI == 4210] <- 4210


#40 lohkoa joille lieju-multa-turve on NA. Ei voida tietää niiden suhdetta, poistetaan
Viljavuusdata<-filter(Viljavuusdata, !(is.na(Viljavuusdata$Maalajisumma)))

#Lisäys 26/9/2023: Muutetaan Ruokaviraston turkistila-aineistosta löytyville tilakoodeille tuotantosuunnaksi Turkistilat. 

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


Viljavuusdata$Tuotantosuunta[Viljavuusdata$MAATILA_TUNNUS %in% Turkistilakoodit_vektori]<-"Turkistilat"


Viljavuus_aggregointi_multavuus<-Viljavuusdata


#Lisäys 11/23: Muutetaan raivatun pellon käsittelyä. Poistetaan ne muiden joukosta, käsitellään erillään. 
#Niiden päästö lasketaan erikseen raivauskertoimella, ei summaamalla. 
#Frame "Viljavuusdata" jää ilman muutoksia. Viljavuus_aggregointi_multavuus sisältää vain raivaamattomat.  

Viljavuus_aggregointi_multavuus<-filter(Viljavuus_aggregointi_multavuus, is.na(Raivattu))


#Multavuusluokkien jako eloper./mineraalimaahan ####
#Määritellään mikä multavuus on eloperäistä ja mikä ei
Viljavuus_aggregointi_multavuus$Elop_Mineral <- NA
Viljavuus_aggregointi_multavuus$Elop_Mineral[Viljavuus_aggregointi_multavuus$Multavuusluokka == "Ei multavuustietoa"] <-
  "Mineraali"
Viljavuus_aggregointi_multavuus$Elop_Mineral[Viljavuus_aggregointi_multavuus$Multavuusluokka == "vm"] <-
  "Mineraali"
Viljavuus_aggregointi_multavuus$Elop_Mineral[Viljavuus_aggregointi_multavuus$Multavuusluokka == "m"] <-
  "Mineraali"
Viljavuus_aggregointi_multavuus$Elop_Mineral[Viljavuus_aggregointi_multavuus$Multavuusluokka == "rm"] <-
  "Mineraali"
Viljavuus_aggregointi_multavuus$Elop_Mineral[Viljavuus_aggregointi_multavuus$Multavuusluokka == "erm"] <-
  "Mineraali"
Viljavuus_aggregointi_multavuus$Elop_Mineral[Viljavuus_aggregointi_multavuus$Multavuusluokka == "mm"] <-
  "Eloperäinen"
Viljavuus_aggregointi_multavuus$Elop_Mineral[Viljavuus_aggregointi_multavuus$Multavuusluokka == "eloperäiset maat"] <-
  "Eloperäinen"


#Raivioille
#Ainoastaan raivatut, ei muita. 

Viljavuus_raivatut_aggregointi_multavuus<-filter(Viljavuusdata, !(is.na(Viljavuusdata$Raivattu))) #Raivatut jäävät ainoastaan tähän tiedostoon

Viljavuus_raivatut_aggregointi_multavuus$Elop_Mineral <- NA
Viljavuus_raivatut_aggregointi_multavuus$Elop_Mineral[Viljavuus_raivatut_aggregointi_multavuus$Multavuusluokka == "Ei multavuustietoa"] <-
  "Mineraali"
Viljavuus_raivatut_aggregointi_multavuus$Elop_Mineral[Viljavuus_raivatut_aggregointi_multavuus$Multavuusluokka == "vm"] <-
  "Mineraali"
Viljavuus_raivatut_aggregointi_multavuus$Elop_Mineral[Viljavuus_raivatut_aggregointi_multavuus$Multavuusluokka == "m"] <-
  "Mineraali"
Viljavuus_raivatut_aggregointi_multavuus$Elop_Mineral[Viljavuus_raivatut_aggregointi_multavuus$Multavuusluokka == "rm"] <-
  "Mineraali"
Viljavuus_raivatut_aggregointi_multavuus$Elop_Mineral[Viljavuus_raivatut_aggregointi_multavuus$Multavuusluokka == "erm"] <-
  "Mineraali"
Viljavuus_raivatut_aggregointi_multavuus$Elop_Mineral[Viljavuus_raivatut_aggregointi_multavuus$Multavuusluokka == "mm"] <-
  "Eloperäinen"
Viljavuus_raivatut_aggregointi_multavuus$Elop_Mineral[Viljavuus_raivatut_aggregointi_multavuus$Multavuusluokka == "eloperäiset maat"] <-
  "Eloperäinen"

#aggregointi ####
#Aggregointi uudelleen muodostetun elop_mineral muuttujan mukaisesti
#Näissä mukana AINOASTAAN RAIVAAMATTOMAT PELLOT

Aggre_simple<-aggregate(Viljavuus_aggregointi_multavuus$KASVI_ALA_HA ,   by = list(
  Viljavuus_aggregointi_multavuus$Tuotantosuunta,
  Viljavuus_aggregointi_multavuus$KASVITUNNU_reclass,
  Viljavuus_aggregointi_multavuus$KASVINIMI_reclass,
  Viljavuus_aggregointi_multavuus$Elop_Mineral),sum)


colnames(Aggre_simple)<-c("Tuotantosuunta","Kasvikoodi","Kasvinimi","Maalaji multavuuden perusteella","ala ha")


Aggre_simple_raiviot <-aggregate(Viljavuus_raivatut_aggregointi_multavuus$KASVI_ALA_HA,   by = list(
  Viljavuus_raivatut_aggregointi_multavuus$Tuotantosuunta,
  Viljavuus_raivatut_aggregointi_multavuus$KASVITUNNU_reclass,
  Viljavuus_raivatut_aggregointi_multavuus$KASVINIMI_reclass,
  Viljavuus_raivatut_aggregointi_multavuus$Elop_Mineral),sum)

colnames(Aggre_simple_raiviot)<-c("Tuotantosuunta","Kasvikoodi","Kasvinimi","Maalaji multavuuden perusteella","ala ha")

#tallennus exceliin ####
library(openxlsx)
Multavuusaggregointi_viljavuus<-createWorkbook()
addWorksheet(Multavuusaggregointi_viljavuus, "KaikkiPellot")
writeData(Multavuusaggregointi_viljavuus, "KaikkiPellot",Aggre_simple)
addWorksheet(Multavuusaggregointi_viljavuus, "Raivatut")
writeData(Multavuusaggregointi_viljavuus, "Raivatut",Aggre_simple_raiviot)
saveWorkbook(Multavuusaggregointi_viljavuus, file=here("Output/AreaAggregates","Viljavuusdata_aggregointi_multavuudesta.xlsx"),overwrite = T)
print("Aggregaatit tulostettu Exceliin")
rm(list=ls())
gc()

