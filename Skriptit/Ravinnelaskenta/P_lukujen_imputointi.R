#Puuttuvien P-lukujen inputointi viljavuusdatan perusteella niille lohkoille joilta sitä tietoa ei ole. 
#HV 08042025


#P-LUKUKESKIARVOJEN LASKENTA ####

#Keskiarvo ja/tai mediaani viljelykasvi- tuotantosuuntaerotuksella niille lohkoille joilta puuttuu. 

library(stringr);library(usefun);library(varhandle);library(here);library(tidyverse)

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

#Viljavuusdatan esivalmistelu

source_lines(here("Skriptit/Uudet skriptit/Viljavuus_esikasittely_aggregointi.R"), 1:240)

rm.all.but("Viljavuus_aggregointi_multavuus")

Viljavuus_aggregointi_multavuus<-Viljavuus_aggregointi_multavuus %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                                                 Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                                                                 Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                                                                 Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                                                                 Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                                                                 Tuotantosuunta == "Tattari ja kinoa" ~ "Tattari_kinoa",
                                                                                 Tuotantosuunta == "Vihannekset ja juurekset" ~ "Vihannekset_juurekset",
                                                                                 Tuotantosuunta == "Viljat pl. ohra" ~ "Viljat_pl_ohra",
                                                                                 .default = Tuotantosuunta))
library(readxl)
Kasvikategoriat_avain <- read_excel("Data/Kasvikategoriat_avain.xlsx")
colnames(Kasvikategoriat_avain)[4]<-"KASVITUNNU_reclass"

Viljavuus_aggregointi_multavuus<-left_join(Viljavuus_aggregointi_multavuus, Kasvikategoriat_avain, by="KASVITUNNU_reclass" )

#P-lukujen keskiarvotus ja mediaani kasveille, tuotantosuunnille.

P_ka_median_tarkka<-Viljavuus_aggregointi_multavuus %>% group_by(Tuotantosuunta, KASVITUNNU_reclass,KASVINIMI_reclass) %>% summarise(P_luku_keskiarvo = mean(KESKIARVO_),P_luku_mediaani = median(KESKIARVO_)) 
#Käytetään puuttuvien P-lukujen täydennykseen kasvi-tuotantosuunta yhdistelmän pluku-mediaanilla tai keskiarvolla
                                                                                                        
P_ka_ts_kat<-Viljavuus_aggregointi_multavuus %>% group_by(Tuotantosuunta, `Cropland/grassland`) %>% summarise(P_luku_keskiarvo = mean(KESKIARVO_),P_luku_mediaani = median(KESKIARVO_))
#Käytetään niille, joille kasvi-tuotantosuunta komboa viljavuusdatasta ei löydy, mutta kattavammasta löytyy. Esim. maitotilojen croplandin keskiarvo-P

rm.all.but(c("P_ka_median_tarkka","P_ka_ts_kat","Kasvikategoriat_avain"))

##################################################################################################

#ESIVAIHEET

# Täyden lohkodatan sisäänotto #### 
# virheenkorjaukset, turkistilojen täydennys. Samat asiat, kuin gtk-datalle tehtäisiin GTK_datan_aggregointi.R-skriptissä. 
#Sitä skriptiä ei voida sourceta suoraan dimensioerojen takia, koska nyt tarvitaan käyttöön myös viljavuusdatan tietosisältöä (P-luvut) (dimensioherkkä skripti). 
#Lisäys 26/9/2023: Muutetaan Ruokaviraston turkistila-aineistosta löytyville tilakoodeille tuotantosuunnaksi Turkistilat. 



library(readr)
Yhdistetty_peltodata_raivaukset_rehuvilja <- read_csv("Data/Yhdistetty_peltodata_raivaukset_rehuvilja.csv", 
                                                      col_types = cols(...1 = col_skip()))


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

Yhdistetty_peltodata_raivaukset_rehuvilja$Tuotantosuunta[Yhdistetty_peltodata_raivaukset_rehuvilja$MAATILA_TUNNUS %in% Turkistilakoodit_vektori]<-"Turkistilat"

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

Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$Tuotantosuunta %in% Eläintilat &
                                       Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI == 1400] <- 1400
Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$Tuotantosuunta %in% Eläintilat &
                                       Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI == 1120] <- 1120
Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$Tuotantosuunta %in% Eläintilat &
                                       Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI == 1110] <- 1110
Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$Tuotantosuunta %in% Eläintilat &
                                       Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI == 1230] <- 1230
Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$Tuotantosuunta %in% Eläintilat &
                                       Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI == 1220] <- 1220
Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$Tuotantosuunta %in% Eläintilat &
                                       Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI == 1410] <- 1410
Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$Tuotantosuunta %in% Eläintilat &
                                       Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI == 1330] <- 1330 #syysohra


#Elintarviketilat - elintarvikeviljaa
Kasvitilat <-
  Yhdistetty_peltodata_raivaukset_rehuvilja$Tuotantosuunta[!(Yhdistetty_peltodata_raivaukset_rehuvilja$Tuotantosuunta %in% Eläintilat)]
Kasvitilat <- unique(Kasvitilat)


Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$Tuotantosuunta %in% Kasvitilat &
                                       Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI == 1400] <- 1400
Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$Tuotantosuunta %in% Kasvitilat &
                                       Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI == 1120] <- 1120
Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$Tuotantosuunta %in% Kasvitilat &
                                       Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI == 1110] <- 1110
Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$Tuotantosuunta %in% Kasvitilat &
                                       Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI == 1230] <- 1230
Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$Tuotantosuunta %in% Kasvitilat &
                                       Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI == 1220] <- 1220
Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$Tuotantosuunta %in% Kasvitilat &
                                       Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI == 1410] <- 1410
Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$Tuotantosuunta %in% Kasvitilat &
                                       Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI == 1330] <- 1330 #syysohra


#Annetaan nimi uudelleenluokitetuille kasvikoodeille
Yhdistetty_peltodata_raivaukset_rehuvilja$KASVINIMI_reclass<-Yhdistetty_peltodata_raivaukset_rehuvilja$KasviNimi_1

Yhdistetty_peltodata_raivaukset_rehuvilja$KASVINIMI_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass  == 14001 ]<-c("Kaura")
Yhdistetty_peltodata_raivaukset_rehuvilja$KASVINIMI_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass == 14002 ]<-c("Kaura")

Yhdistetty_peltodata_raivaukset_rehuvilja$KASVINIMI_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass == 11201 ]<-c("Kevätvehnä")
Yhdistetty_peltodata_raivaukset_rehuvilja$KASVINIMI_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass == 11202 ]<-c("Kevätvehnä")

Yhdistetty_peltodata_raivaukset_rehuvilja$KASVINIMI_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass == 11101 ]<-c("Syysvehnä")
Yhdistetty_peltodata_raivaukset_rehuvilja$KASVINIMI_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass == 11102 ]<-c("Syysvehnä")

Yhdistetty_peltodata_raivaukset_rehuvilja$KASVINIMI_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass == 12301 ]<-c("Syysruis")
Yhdistetty_peltodata_raivaukset_rehuvilja$KASVINIMI_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass == 12302 ]<-c("Syysruis")


Yhdistetty_peltodata_raivaukset_rehuvilja$KASVINIMI_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass == 12201 ]<-c("Kevätruis")
Yhdistetty_peltodata_raivaukset_rehuvilja$KASVINIMI_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass == 12202 ]<-c("Kevätruis")

Yhdistetty_peltodata_raivaukset_rehuvilja$KASVINIMI_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass == 14101 ]<-c("Syyskaura")
Yhdistetty_peltodata_raivaukset_rehuvilja$KASVINIMI_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass == 14102 ]<-c("Syyskaura")

Yhdistetty_peltodata_raivaukset_rehuvilja$KASVINIMI_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass == 13301] <-
  c("Syysohra")
Yhdistetty_peltodata_raivaukset_rehuvilja$KASVINIMI_reclass[Yhdistetty_peltodata_raivaukset_rehuvilja$KASVIKOODI_lohkodata_reclass == 13302] <-
  c("Syysohra")

#maannossumman laskenta gtk-maalajeista

Yhdistetty_peltodata_raivaukset_rehuvilja$Maannossumma<-(rowSums(Yhdistetty_peltodata_raivaukset_rehuvilja[60:75]/10000))

#Suodatetaan pois ne joilla ei ole maannostietoa

Yhdistetty_peltodata_raivaukset_rehuvilja<-filter(Yhdistetty_peltodata_raivaukset_rehuvilja, !(is.na(Yhdistetty_peltodata_raivaukset_rehuvilja$Maannossumma)))

#Eloperäisen ja mineraalimaan määrittely  ####

#Viljavuusdatasta poiketen tästä aineistosta ei ole olemassa multavuustietoa. Eloperäiseksi määrittyy turvemaa eli histosolit. Muu on mineraalimaata
attach(Yhdistetty_peltodata_raivaukset_rehuvilja)
Yhdistetty_peltodata_raivaukset_rehuvilja$Eloperaista <-
  (
    OHUT_TURVEKERROS__TVO__RT + PAKSU_TURVEKERROS__TVP__RT
  ) / 10000 #m2 to ha

Yhdistetty_peltodata_raivaukset_rehuvilja$Mineraalia <-(SEKALAJITTEINEN_MAALAJI__PÄÄLAJITETTA_EI_SELVITETTY__SY__RT+
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
detach(Yhdistetty_peltodata_raivaukset_rehuvilja)

#Lasketaan maalajisuhde kullekin lohkolle:


Yhdistetty_peltodata_raivaukset_rehuvilja<-Yhdistetty_peltodata_raivaukset_rehuvilja %>% mutate(Mineraaliprosentti = (Mineraalia/Maannossumma)*100,
                            Elop_prosentti = (Eloperaista/Maannossumma)*100)
########################################################################################################################

#VARSINAINEN LASKENTA
#P-LUVUTTOMIEN LOHKOJEN TUNNISTUS 

Yhdistetty_peltodata_raivaukset_rehuvilja<-Yhdistetty_peltodata_raivaukset_rehuvilja %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                                                                       Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                                                                                       Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                                                                                       Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                                                                                       Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                                                                                       Tuotantosuunta == "Tattari ja kinoa" ~ "Tattari_kinoa",
                                                                                                       Tuotantosuunta == "Vihannekset ja juurekset" ~ "Vihannekset_juurekset",
                                                                                                       Tuotantosuunta == "Viljat pl. ohra" ~ "Viljat_pl_ohra",
                                                                                                       .default = Tuotantosuunta))
#Kasvikategorioiden liitto
Kasvikategoriat_avain$`Yksi/monivuotinen`<-NULL
Kasvikategoriat_avain$Tuoteryhmä<-NULL
Kasvikategoriat_avain$Kasvi<-NULL
colnames(Kasvikategoriat_avain)[2]<- "KASVIKOODI_lohkodata_reclass"

Yhdistetty_peltodata_raivaukset_rehuvilja<-inner_join(Yhdistetty_peltodata_raivaukset_rehuvilja, Kasvikategoriat_avain, by="KASVIKOODI_lohkodata_reclass")


rm.all.but(c("Yhdistetty_peltodata_raivaukset_rehuvilja", "P_ka_median_tarkka","P_ka_ts_kat","Kasvikategoriat_avain"))


#P-lukudatassa maalajiprofiili tai P-luku EI VAIHTELE KASVULOHKOITTAIN, ne ovat samat yhden peruslohkon kaikilla kasvulohkoilla. 
#kasvit eroavat toisistaan kasvulohkoittain eli p-lukurivien välillä, kuten myös pinta-alat, mutta p-luku ei vaihtele.
#Kaikki p-lukumittaukset eri kasvulohkoilta löytyvät saman polygonin päältä. 

#VERROKKIDATA
#Selvitetään Kaikki peruslohkot, joilta on p-luvun arvo. Ei NA-arvoja
#Arvot eivät vaihtele kasvulohkojen kesken. Jos yhdeltä saman peruslohkon kasvulohkolta löytyy mittaus, se annetaan arvoksi muillekin sen peruslohkon alalohkoille. 

Pluku<-Yhdistetty_peltodata_raivaukset_rehuvilja %>% filter(!is.na(KESKIARVO_)) %>% select(PLTUNNUS, KESKIARVO_) #Pluku ei saa olla NA

Tunnusluvut_tiedetyistä_pluvuista<-Pluku %>% summarise(Keskimaarainen_tunnettu = mean(KESKIARVO_),
                    Hajonta = sd(KESKIARVO_),
                    Minimi = min(KESKIARVO_),
                    Maksimi =max(KESKIARVO_))





#Pari tapausta, joissa sama peruslohko saakin useamman p-luvun arvon. 
#Tarkastettu paikkatiedosta: kohdentuvat eri kasvulohkoille, eri sijainnit. 
#Voi olla sijaintivirhe paikkatiedossa. Tai toisaalta Juhan mukaan datassa joitain harvoja pisteitä, joissa kasvulohkokohtaiset sijainnit p-lukupisteelle. Nämä siivottu suurimmaksi osaksi pois, nämä pari jäämistöä siitä 
#Tässä keskiarvotetaan: tarvitaan 1 peruslohkolle 1 pluku


Pluku$KESKIARVO_[Pluku$PLTUNNUS =="0450104248"] <- mean(Pluku$KESKIARVO_[Pluku$PLTUNNUS =="0450104248"])
Pluku$KESKIARVO_[Pluku$PLTUNNUS =="2321286586"] <-mean(Pluku$KESKIARVO_[Pluku$PLTUNNUS =="2321286586"])

Pluku<-unique(Pluku) #lohkokoodikohtainen, uniikki P-luvun arvo. Nyt samasta lohkokoodista on tuplarivejä.


#P-LUKUJEN MÄÄRITTELYT

#Suodatetaan peltoaineisto kahteen osaan. 
# a) Sellaiset rivit otetaan erilleen, joilta P-lukutieto puuttuu kokonaan. Erotus tehdään ottamalla irti lohkotunnuksella ne, joita ei löydy yllä luodusta Pluku-listasta 

Ei_Plukua<-Yhdistetty_peltodata_raivaukset_rehuvilja %>% filter(!(PLTUNNUS %in% Pluku$PLTUNNUS)) #Sellaisia, joiden lohkokoodi puuttuu kokonaan pluku-datasta

#Näiden arvoksi annetaan kasvin ja tuotantosuunnan mukainen mediaani Pluvusta.

colnames(P_ka_median_tarkka)<-c("Tuotantosuunta","KASVIKOODI_lohkodata_reclass","KASVINIMI_reclass","P_luku_keskiarvo","P_luku_mediaani")
P_ka_median_tarkka$KASVINIMI_reclass<-NULL

Ei_Plukua_taydennys<-left_join(Ei_Plukua, P_ka_median_tarkka, by=c("Tuotantosuunta","KASVIKOODI_lohkodata_reclass"))

#Liitoksessa jää yhä muutamia NA-arvoja, johtuen siitä ettei viljavuusdata sisällä kaikkia tuotantosuunta-kasvikoodi yhdistelmiä joita laajassa lohkodatassa on. 

Puuttuu<-Ei_Plukua_taydennys %>% filter(is.na(P_luku_mediaani))

#Mitä nämä yhdistelmät ovat?
puuttuvienKoodit<-Puuttuu %>% select(Tuotantosuunta,KASVIKOODI_lohkodata_reclass)
puuttuvienKoodit<-unique(puuttuvienKoodit)
rm(puuttuvienKoodit)

#Tällaisissa tapauksissa tehdään lisätäydennys: P-luku määrätään tuotantosuunnan croplandin tai grasslandin mediaanin mukaan
#Viljelykierron takia yhden vuoden viljelykasvi ei suuresti vaikuta P-lukuun, mutta tuotantosuunta vaikuttaa johtuen lannoituskäytännöistä. 

Puuttuu<-inner_join(Puuttuu, P_ka_ts_kat, by=c("Tuotantosuunta","Cropland/grassland"))  
#nämä tulevat frameen päätteellä P_luku_mediaani.y. x. kenttä (alkuperäisestä liitoksesta, joka jäi tyhkäksi) on nyt turha. 
#Näiden nimet pitää palauttaa samaan asuun, kuin Ei_Plukua_taydennys - framessa, jonne nämä palautetaan kohta. 

Puuttuu$P_luku_keskiarvo.x<-NULL
Puuttuu$P_luku_mediaani.x<-NULL

colnames(Puuttuu)[101:102]<-c("P_luku_keskiarvo","P_luku_mediaani")

#Puuttuu-framen sisältö määriteltiin ottamalla irti ne Ei_Plukua_täydennyksen rivit, joiden kohdalla P-luvun mediaaniksi tuli na. 
#Näitä rivejä ei muutettu muuten, paitsi antamalla niille P-luvun KA ja mediaani muualta. 
#Nyt samat rivit on täydennetty frameksi Puuttuu. '
#Alkuperäiset rivit voidaan nyt poistaa Ei_Plukua_taydennyksesta, ja liittää tilalle frame "Puuttuu" riveittäin. --> Samat rivit kuin originaalissa, mutta NA-luvut täydennettynä 

nrow(Ei_Plukua_taydennys %>% filter(!(is.na(P_luku_mediaani))))+
nrow(Ei_Plukua_taydennys %>% filter((is.na(P_luku_mediaani))))

Ei_Plukua_taydennys<-Ei_Plukua_taydennys %>% filter(!(is.na(P_luku_mediaani)))

Ei_Plukua_taydennys<- rbind(Ei_Plukua_taydennys, Puuttuu)

rm(Ei_Plukua,Puuttuu)


#B) sellaiset rivit (lohkot) irti, joille löytyy ainakin jokin P-luvun arvo. 
#Osalla kasvulohkoista näiden kohdalla arvo on NA, vaikka peruslohkolle olisikin P-arvo: annetaan peruslohkon mukainen, sama arvo niille kaikille
Pluku_loytyy<-Yhdistetty_peltodata_raivaukset_rehuvilja %>% filter((PLTUNNUS %in% Pluku$PLTUNNUS)) #Sellaisia, joiden lohkokoodi löytyy pluku-datasta

#Sellaiset lohkot ylläolevasta, joille pitää määrittää peruslohkon mukainen p-lukuarvo (koska paikkatiedossa pisteet ovat peruslohkoittain, ei kasvulohkoittain)

filter(Pluku, duplicated(PLTUNNUS))

TyhjaP<-Pluku_loytyy %>% filter(is.na(KESKIARVO_))

TyhjaP<-left_join(TyhjaP, Pluku, by="PLTUNNUS")

#Uusi KESKIARVO_.y-muuttuja sisältää täydennetyt arvot. 

TyhjaP$KESKIARVO_.x<-NULL
colnames(TyhjaP)[colnames(TyhjaP)=="KESKIARVO_.y"]<-"KESKIARVO_"

#Nämä rivit voidaan nyt poistaa Pluku_loytyy framesta (taydentamattomat versiot), ja lisätä yllä luodut täydennetyt rivit niiden paikalle. 

Pluku_loytyy<-Pluku_loytyy %>% filter(!is.na(KESKIARVO_)) #poistetaan tyhjaP-framen rivien korjaamattomat versiot

Pluku_loytyy<-rbind(Pluku_loytyy, TyhjaP) #Laitetaan samat rivit takaisin korjattuna 

#Tarkistetaan että korjatut framet vastaavat toisiaan rivimäärissä ja pinta-aloissa
sum(Ei_Plukua_taydennys$Maannossumma)+sum(Pluku_loytyy$Maannossumma) == sum(Yhdistetty_peltodata_raivaukset_rehuvilja$Maannossumma)
sum(nrow(Ei_Plukua_taydennys))+sum(nrow(Pluku_loytyy)) == nrow(Yhdistetty_peltodata_raivaukset_rehuvilja)

rm.all.but(c("Yhdistetty_peltodata_raivaukset_rehuvilja","Pluku_loytyy","Ei_Plukua_taydennys"))


###############################################################################################################

#VIIMEISTELY

#VALINTA: Kumpi laitetaan Ei_plukua_täydennys framen KESKIARVO_ - muuttujaksi, mediaani vai KA?

#Alustavasti mediaani:

Ei_Plukua_taydennys$KESKIARVO_ <-Ei_Plukua_taydennys$P_luku_mediaani 

#Poistetaan mediaani- ja keskiarvo P-kentät jotta dimensiot saadaan samaksi

Ei_Plukua_taydennys$P_luku_keskiarvo<-NULL

Ei_Plukua_taydennys$P_luku_mediaani<-NULL

#Yhdistetään datat takaisin. 

Imputoitu_aineisto<-rbind(Pluku_loytyy, Ei_Plukua_taydennys)

sum(Imputoitu_aineisto$Maannossumma)

Imputoitu_aineisto %>% filter(is.na(KESKIARVO_))

######################################################

write.csv2(Imputoitu_aineisto, file=here("Data/Lohkoaineisto_imputoidut_Pluvut.csv"))


