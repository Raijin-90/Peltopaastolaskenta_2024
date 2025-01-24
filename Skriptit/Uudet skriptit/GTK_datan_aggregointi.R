library(here);library(openxlsx);library(readr)

#Sisäänluenta ja multavuusdatan yhdistäminen peltoaineistoon ####

library(readr)
Yhdistetty_peltodata_raivaukset_rehuvilja <- read_csv("Data/Yhdistetty_peltodata_raivaukset_rehuvilja.csv", 
                                                      col_types = cols(...1 = col_skip()))


y<-unique(Yhdistetty_peltodata_raivaukset_rehuvilja$MAATILA_TUNNUS)



library(readxl)
Kasvikategoriat_avain <- read_excel(here("Data","Kasvikategoriat_avain.xlsx"), 
                                    col_types = c("text", "text", "text", 
                                                  "numeric", "skip"))

#Aineiston valmistelu ja virheiden korjaaminen ####
library(dplyr)
GTKdata <-
  unique(
    select(
      Yhdistetty_peltodata_raivaukset_rehuvilja,
      PLTUNNUS,
      KLTUNNUS,
      MAATILA_TUNNUS,
      8,
      93,
      9:11,
      57,
      93,
      60:75,
      46:47,
      89 )
  )

   

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

GTKdata$Tuotantosuunta[GTKdata$MAATILA_TUNNUS %in% Turkistilakoodit_vektori]<-"Turkistilat"



#Muutos 01/24: Poiskytkentä tälle erittelylle. 
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
              "Munatilat",
              "Turkistilat")

GTKdata$KASVIKOODI_lohkodata_reclass[GTKdata$Tuotantosuunta %in% Eläintilat &
                                           GTKdata$KASVIKOODI == 1400] <- 1400
GTKdata$KASVIKOODI_lohkodata_reclass[GTKdata$Tuotantosuunta %in% Eläintilat &
                                           GTKdata$KASVIKOODI == 1120] <- 1120
GTKdata$KASVIKOODI_lohkodata_reclass[GTKdata$Tuotantosuunta %in% Eläintilat &
                                           GTKdata$KASVIKOODI == 1110] <- 1110
GTKdata$KASVIKOODI_lohkodata_reclass[GTKdata$Tuotantosuunta %in% Eläintilat &
                                           GTKdata$KASVIKOODI == 1230] <- 1230
GTKdata$KASVIKOODI_lohkodata_reclass[GTKdata$Tuotantosuunta %in% Eläintilat &
                                           GTKdata$KASVIKOODI == 1220] <- 1220
GTKdata$KASVIKOODI_lohkodata_reclass[GTKdata$Tuotantosuunta %in% Eläintilat &
                                           GTKdata$KASVIKOODI == 1410] <- 1410
GTKdata$KASVIKOODI_lohkodata_reclass[GTKdata$Tuotantosuunta %in% Eläintilat &
                                           GTKdata$KASVIKOODI == 1330] <- 1330 #syysohra


#Elintarviketilat - elintarvikeviljaa
Kasvitilat <-
  GTKdata$Tuotantosuunta[!(GTKdata$Tuotantosuunta %in% Eläintilat)]
Kasvitilat <- unique(Kasvitilat)


GTKdata$KASVIKOODI_lohkodata_reclass[GTKdata$Tuotantosuunta %in% Kasvitilat &
                                           GTKdata$KASVIKOODI == 1400] <- 1400
GTKdata$KASVIKOODI_lohkodata_reclass[GTKdata$Tuotantosuunta %in% Kasvitilat &
                                           GTKdata$KASVIKOODI == 1120] <- 1120
GTKdata$KASVIKOODI_lohkodata_reclass[GTKdata$Tuotantosuunta %in% Kasvitilat &
                                           GTKdata$KASVIKOODI == 1110] <- 1110
GTKdata$KASVIKOODI_lohkodata_reclass[GTKdata$Tuotantosuunta %in% Kasvitilat &
                                           GTKdata$KASVIKOODI == 1230] <- 1230
GTKdata$KASVIKOODI_lohkodata_reclass[GTKdata$Tuotantosuunta %in% Kasvitilat &
                                           GTKdata$KASVIKOODI == 1220] <- 1220
GTKdata$KASVIKOODI_lohkodata_reclass[GTKdata$Tuotantosuunta %in% Kasvitilat &
                                           GTKdata$KASVIKOODI == 1410] <- 1410
GTKdata$KASVIKOODI_lohkodata_reclass[GTKdata$Tuotantosuunta %in% Kasvitilat &
                                           GTKdata$KASVIKOODI == 1330] <- 1330 #syysohra


#Annetaan nimi uudelleenluokitetuille kasvikoodeille
GTKdata$KASVINIMI_reclass<-GTKdata$KasviNimi_1

GTKdata$KASVINIMI_reclass[GTKdata$KASVIKOODI_lohkodata_reclass  == 14001 ]<-c("Kaura")
GTKdata$KASVINIMI_reclass[GTKdata$KASVIKOODI_lohkodata_reclass == 14002 ]<-c("Kaura")

GTKdata$KASVINIMI_reclass[GTKdata$KASVIKOODI_lohkodata_reclass == 11201 ]<-c("Kevätvehnä")
GTKdata$KASVINIMI_reclass[GTKdata$KASVIKOODI_lohkodata_reclass == 11202 ]<-c("Kevätvehnä")

GTKdata$KASVINIMI_reclass[GTKdata$KASVIKOODI_lohkodata_reclass == 11101 ]<-c("Syysvehnä")
GTKdata$KASVINIMI_reclass[GTKdata$KASVIKOODI_lohkodata_reclass == 11102 ]<-c("Syysvehnä")

GTKdata$KASVINIMI_reclass[GTKdata$KASVIKOODI_lohkodata_reclass == 12301 ]<-c("Syysruis")
GTKdata$KASVINIMI_reclass[GTKdata$KASVIKOODI_lohkodata_reclass == 12302 ]<-c("Syysruis")


GTKdata$KASVINIMI_reclass[GTKdata$KASVIKOODI_lohkodata_reclass == 12201 ]<-c("Kevätruis")
GTKdata$KASVINIMI_reclass[GTKdata$KASVIKOODI_lohkodata_reclass == 12202 ]<-c("Kevätruis")

GTKdata$KASVINIMI_reclass[GTKdata$KASVIKOODI_lohkodata_reclass == 14101 ]<-c("Syyskaura")
GTKdata$KASVINIMI_reclass[GTKdata$KASVIKOODI_lohkodata_reclass == 14102 ]<-c("Syyskaura")

GTKdata$KASVINIMI_reclass[GTKdata$KASVIKOODI_lohkodata_reclass == 13301] <-
  c("Syysohra")
GTKdata$KASVINIMI_reclass[GTKdata$KASVIKOODI_lohkodata_reclass == 13302] <-
  c("Syysohra")


#maannossumman laskenta

GTKdata$Maannossumma<-(rowSums(GTKdata[10:25]/10000))

#Suodatetaan pois ne joilla ei ole maannostietoa

GTKdata<-filter(GTKdata, !(is.na(GTKdata$Maannossumma)))
#Uniikkeja lohkokoodeja
x<-unique(GTKdata$PLTUNNUS)


maannospuuttuu<-filter(GTKdata, (is.na(GTKdata$Maannossumma)))
z<-unique(maannospuuttuu$PLTUNNUS) #uniikkeja lohkokoodeja joilla ei maannosta


rm(Yhdistetty_peltodata_raivaukset_rehuvilja)

#Eloperäisen ja mineraalimaan määrittely  ####

#Viljavuusdatasta poiketen tästä aineistosta ei ole olemassa multavuustietoa. Eloperäiseksi määrittyy turvemaa eli histosolit. Muu on mineraalimaata
attach(GTKdata)
GTKdata$Eloperaista <-
  (
  OHUT_TURVEKERROS__TVO__RT + PAKSU_TURVEKERROS__TVP__RT
  ) / 10000 #m2 to ha


GTKdata$Mineraalia <-(SEKALAJITTEINEN_MAALAJI__PÄÄLAJITETTA_EI_SELVITETTY__SY__RT+
KARKEARAKEINEN_MAALAJI__PÄÄLAJITETTA_EI_SELVITETTY__KY__RT+ 
VESI__VE_+                                                   
SOISTUMA__TVS__RT+                                          
KALLIOMAA__KA__RT+                                         
KALLIOPALJASTUMA__KAPA__RT+                                
SAVI__SA__RT     +                                          
HIENOJAKOINEN_MAALAJI__PÄÄLAJITETTA_EI_SELVITETTY__HY__RT+   
RAKKA__RAKA__RT+                                     
KIVIÄ__KI__RT+                                            
LIEJU__LJ__RT+                                             
KARTOITTAMATON__0_+                                         
TÄYTEMAA__TA_+                                             
LIEJUINEN_HIENORAKEINEN_MAALAJI_RT)/10000
detach(GTKdata)


#Lasketaan maalajisuhde kullekin lohkolle:


GTKdata<-GTKdata %>% mutate(Mineraaliprosentti = (Mineraalia/Maannossumma)*100,
                                             Elop_prosentti = (Eloperaista/Maannossumma)*100)


z<-unique(GTKdata$PLTUNNUS)







#Pinta-alojen aggregointi ####
#Aggregoidaan tuote- ja tuotantosuuntatasolla koko datamassasta

GTK_aggregointi_kaikki_maa<-
  aggregate(
    list(GTKdata$Maannossumma),
    by = list(
      GTKdata$Tuotantosuunta,
      GTKdata$KASVIKOODI_lohkodata_reclass,
      GTKdata$KASVINIMI_reclass
    ),
    sum
  )
colnames(GTK_aggregointi_kaikki_maa) <-
  c("Tuotantosuunta", "Kasvikoodi", "Kasvinimi", "Peltoala_ha")


GTK_aggregointi_mineral <-
  aggregate(
    list(GTKdata$Mineraalia),
    by = list(
      GTKdata$Tuotantosuunta,
      GTKdata$KASVIKOODI_lohkodata_reclass,
      GTKdata$KASVINIMI_reclass
    ),
    sum
  )
colnames(GTK_aggregointi_mineral) <-
  c("Tuotantosuunta", "Kasvikoodi", "Kasvinimi", "Mineraalimaata")

GTK_aggregointi_elop <-
  aggregate(
    list(GTKdata$Eloperaista),
    by = list(
      GTKdata$Tuotantosuunta,
      GTKdata$KASVIKOODI_lohkodata_reclass,
      GTKdata$KASVINIMI_reclass
    ),
    sum
  )
colnames(GTK_aggregointi_elop) <-
  c("Tuotantosuunta",
    "Kasvikoodi",
    "Kasvinimi",
    "EloperäistäMaata")


simplesummary<-GTKdata %>% group_by(Tuotantosuunta) %>% summarise(Mineraalia = sum(Mineraalia), Eloperaista = sum(Eloperaista))
simplesummary$hehtaaria<-simplesummary$Mineraalia+simplesummary$Eloperaista
write.xlsx(simplesummary, file=here("Alat_gtk.xlsx"))



#Näistä tehdään oma tuloste. Mitään ei vielä suodateta pois. 

GTK_alat<-createWorkbook()

addWorksheet(GTK_alat,"Mineraaliala")
writeData(GTK_alat,"Mineraaliala", GTK_aggregointi_mineral)
addWorksheet(GTK_alat, "Eloperäinen ala")
writeData(GTK_alat,"Eloperäinen ala", GTK_aggregointi_elop)
addWorksheet(GTK_alat,"Kaikki ala")
writeData(GTK_alat,"Kaikki ala",GTK_aggregointi_kaikki_maa)

saveWorkbook(GTK_alat, file=here("Output/AreaAggregates/Tasokorjaamaton_kokonaisala_gtk.xlsx"), overwrite = T)


#Lisäys 7/11/23.
#Muutetaan raivauksen päästön laskentatapaa.
#Sitä ei lasketa summaamalla viljelyn ja raivauksen päästöä raivatun lohkon tapauksessa. 
#Vaan niillä raivauskerroin sisältää jo raivaustoiminnasta ja viljelystä aiheutuvan päästön, joka lasketaan vain 1 kerran.  
#Otetaan raivatut lohkot irti GTKdatasta. Niiden päästö lasketaan erikseen.  

GTKdata_raivaamattomat<- filter(GTKdata, is.na(Raivattu))

#Pinta-alojen aggregointi ####
#Aggregoidaan tuote- ja tuotantosuuntatasolla

GTK_aggregointi_mineral <-
  aggregate(
    list(GTKdata_raivaamattomat$Mineraalia),
    by = list(
      GTKdata_raivaamattomat$Tuotantosuunta,
      GTKdata_raivaamattomat$KASVIKOODI_lohkodata_reclass,
      GTKdata_raivaamattomat$KASVINIMI_reclass
    ),
    sum
  )
colnames(GTK_aggregointi_mineral) <-
  c("Tuotantosuunta", "Kasvikoodi", "Kasvinimi", "Mineraalimaata")

GTK_aggregointi_elop <-
  aggregate(
    list(GTKdata_raivaamattomat$Eloperaista),
    by = list(
      GTKdata_raivaamattomat$Tuotantosuunta,
      GTKdata_raivaamattomat$KASVIKOODI_lohkodata_reclass,
      GTKdata_raivaamattomat$KASVINIMI_reclass
    ),
    sum
  )
colnames(GTK_aggregointi_elop) <-
  c("Tuotantosuunta",
    "Kasvikoodi",
    "Kasvinimi",
    "EloperäistäMaata")


#RAIVIOT #### 


GTK_raiviot<-filter(GTKdata, !(is.na(GTKdata$Raivattu))) #2000 jälkeen raivatut lohkot irti


#Pinta-alojen aggregointi ####
#Aggregoidaan tuote- ja tuotantosuuntatasolla

GTK_aggregointi_mineral_raiviot <-
  aggregate(
    list(GTK_raiviot$Mineraalia),
    by = list(
      GTK_raiviot$Tuotantosuunta,
      GTK_raiviot$KASVIKOODI_lohkodata_reclass,
      GTK_raiviot$KASVINIMI_reclass
    ),
    sum
  )
colnames(GTK_aggregointi_mineral_raiviot) <-
  c("Tuotantosuunta", "Kasvikoodi", "Kasvinimi", "Mineraalimaata")


GTK_aggregointi_elop_raiviot <-
  aggregate(
    list(GTK_raiviot$Eloperaista),
    by = list(
      GTK_raiviot$Tuotantosuunta,
      GTK_raiviot$KASVIKOODI_lohkodata_reclass,
      GTK_raiviot$KASVINIMI_reclass
    ),
    sum
  )
colnames(GTK_aggregointi_elop_raiviot) <-
  c("Tuotantosuunta",
    "Kasvikoodi",
    "Kasvinimi",
    "EloperäistäMaata")


print("GTK-datan aggregointi suoritettu.")

gc()
