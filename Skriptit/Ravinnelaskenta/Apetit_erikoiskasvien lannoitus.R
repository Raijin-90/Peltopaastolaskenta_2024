library(tidyverse);library(varhandle);library(stringr)

#Erikoiskasvien lannoitusaineisto:
#Kasveittainen lannoitus per ha, huomioiden eri lannoitetyypit

#Yhdistellään lohkon tunniste, kasvatettu kasvi ja lohkolla käytetty lannoite

#Datataulut sisään

library(readxl)
lannoitusrivi <- read_excel("Data/Ravinnelaskennan_aineisto/Apetit_erikoiskasvien_lannoitus.xlsx", 
                                              sheet = "Kasvulohkon lannoitusrivi")

lannoitusotsikko<-read_excel("Data/Ravinnelaskennan_aineisto/Apetit_erikoiskasvien_lannoitus.xlsx", 
                          sheet = "Kasvulohkon lannoitusotsikko")
colnames(lannoitusotsikko)[3]<-"Lannoitusotsikkonumero"

Kasvulohko_otsikko <- read_excel("Data/Ravinnelaskennan_aineisto/Apetit_erikoiskasvien_lannoitus.xlsx", 
                            sheet = "Kasvulohko otsikko")
Kasvulohko_otsikko<-Kasvulohko_otsikko %>% select(1:19)


#Lannoitusotsikkonumero(kasvulohko)-kentällä yhdistetään kasvulohkon lannoitusotsikko ja lannoitusrivi.  
#Saadaan tieto, mitä lannoitetta lohkolla on käytetty, sekä mihin peruslohkoon se kuuluu

a<-merge(lannoitusotsikko, lannoitusrivi, by="Lannoitusotsikkonumero")

a<-a %>% select(
  -Luoja.y,
  -Luoja.x,
             -Luontipäivä.x,
             -Luontipäivä.y,
             -Muuttaja.x,
             -Muuttaja.y,
             -Muutospäivä.x,
             -Muutospäivä.y)

#Tähän tieto, mikä sopimuskasvi lohkolla on. Tieto löytyy "Kasvulohko otsikko" lehdeltä, jossa on kasvulohkonumero. Se vastaa lannoitusotsikkonumeroa.  

Kasvulohko_otsikko$Lannoitusotsikkonumero<-Kasvulohko_otsikko$Kasvulohkonumero

b<-  merge(a, Kasvulohko_otsikko, by="Lannoitusotsikkonumero")
Data<-b

rm.all.but("Data")

#Lannoitteiden osalta tarvitaan N-P-K koostumus. Se on aina ilmoitettu mallilla "(1-2-3)". 
#Kaikissa nimikkeissä sitä ei kerrota, esim. "Soluboori". 
#Poimitaan irti ne joissa sulkeita on, ja sulkeiden välissä mitä tahansa 

Lannoitteet<-unique(Data$Lannoiteteksti)

Pitoisuustiedot<-str_subset(Lannoitteet, pattern = "\\([^\\)]+\\)")

#Uniikit permutaatiot pitoisuustiedoista lannoitteissa irroteltu. Tällä saadaan irti ne Datan rivit, joille voi laskea N-P-K määrät. 

#Otetaan datasta tämän vektorin perusteella irti ne rivit, joille voidaan määritellä NPK

Data_pitoisuudet<-Data %>% filter(Lannoiteteksti %in% Pitoisuustiedot)

colnames(Data_pitoisuudet)[colnames(Data_pitoisuudet) == "...6"]<-"Sopimuskasvi_nimi"


#LUOMUJEN POISTO: kommentoi pois jos haluat jättää

#On tapauksia, joissa luomu-status löytyykin "lajike"-kentästä eikä sopimuskasvin alta. Mm. luomupakastehernettä on laitettu pakasteherneen koodille. 
#Jos vertaa suoraan exceliä ja tätä outputia, tässä on vähemmän tavaraa siksi, että mukana on vain ne rivit joille saa N-P-K prosentit. 
#Tämä poistaa myös osan luomupakasteherneistä. 

luomu<-filter(Data_pitoisuudet, (str_detect(Data_pitoisuudet$Lajike, "luomu")))

#Ja nämä luomuksi todetut, pitoisuudelliset rivit voi nyt poistaa kasvulohkotunnisteella datan joukosta 

Data_pitoisuudet<-Data_pitoisuudet %>% filter(!Lannoitusotsikkonumero %in% luomu$Lannoitusotsikkonumero) 

Samoin poistetaan luomupakasteherneet

Data_pitoisuudet<-Data_pitoisuudet %>% filter(!str_detect(Sopimuskasvi_nimi ,"LUOMU|luomu"))

rm(luomu)


#Näistä tarvitaan numeroarvot N-P-K prosenteille lannoitetekstin osana
#Sulkeet ja kaikki tavara niiden sisältä: 

Pitoisuuskoodit<- str_extract(Data_pitoisuudet$Lannoiteteksti,"\\(.*?\\)")
Pitoisuuskoodit<-str_sub(Pitoisuuskoodit, 2, -2)

#Keskimmäinen luku. Haetaan viivojen "-" edessä ja takana olevat mitkä tahansa merkit.  
P<-str_extract(Pitoisuuskoodit,"\\-.*?\\-")

#Viivan vasen puoli, N-luku.  
N<-str_extract(Pitoisuuskoodit,".*?\\-")
#Viivan oikea puoli, K-luku. 
K<-str_sub(Pitoisuuskoodit,-2)

#Tämä nappaa myö viivat mukaan. Otetaan ne pois

N<-str_replace_all(N, "([-])", "")
P<-str_replace_all(P, "([-])", "")
K<-str_replace_all(K, "([-])", "")

#Nämä ovat N, P ja K-prosentteja. Pitää saada numeroiksi jotta voidaan kertoa. Liitetään dataan sillä tavoin. 
#Joissakin harvoissa näistä on desimaalierotin, ja siellä se on pilkku. Ärrä haluaa pisteen. Vaihdetaan se. 

N<-str_replace_all(N, "([,])", ".")
P<-str_replace_all(P, "([,])", ".")
K<-str_replace_all(K, "([,])", ".")

Data_pitoisuudet$Npros<-as.numeric(N)

Data_pitoisuudet$Ppros<-as.numeric(P)

Data_pitoisuudet$Kpros<-as.numeric(K)

#Tiedetään lannoitteen annosmäärä hehtaarilla. Ja näistä prosenttiosuuksista saadaan, kuinka iso osa siitä määrästä on kutakin ravinnetta.
#Pros. luku pitää muistaa jakaa sadalla ensin. 

Data_pitoisuudet<-Data_pitoisuudet %>% mutate(N_kg_ha = (Npros/100)*`Lannoitemäärä per hehtaari`,
                            P_kg_ha = (Ppros/100)*`Lannoitemäärä per hehtaari`,
                            K_kg_ha = (Kpros/100)*`Lannoitemäärä per hehtaari`)


#Vihdoin ja viimein summarisoidaan se, montako kg/ha kutakin kolmea ravinnetta laitetaan eri kasveille keskimäärin.
#Ja näille hajonnat. 


Apetit_lannoiteravinteet_ka<-Data_pitoisuudet %>% group_by(Sopimuskasvi, Sopimuskasvi_nimi) %>% summarise(N_kg_ha_keskiarvo = mean(N_kg_ha),
                                                                                                          N_kg_ha_sd = sd(N_kg_ha),
                                                                                                          N_kg_ha_median = median(N_kg_ha),
                                                                                                          N_kg_ha_min = min(N_kg_ha),
                                                                                                          N_kg_ha_max = max(N_kg_ha),
                                                                                                          P_kg_ha_keskiarvo = mean(P_kg_ha),
                                                                                                          P_kg_ha_sd=sd(P_kg_ha),
                                                                                                          P_kg_ha_median = median(P_kg_ha),
                                                                                                          P_kg_ha_min = min(P_kg_ha),
                                                                                                          P_kg_ha_max = max(P_kg_ha),
                                                                                                          K_kg_ha_keskiarvo = mean(K_kg_ha),
                                                                                                          K_kg_ha_sd = sd(K_kg_ha),
                                                                                                          K_kg_ha_median = median(K_kg_ha),
                                                                                                          K_kg_ha_min = min(K_kg_ha),
                                                                                                          K_kg_ha_max = max(K_kg_ha))
library(openxlsx);library(here)
Aggregointi<-createWorkbook()
addWorksheet(Aggregointi, "Apetit_lannoiteravinteet_kg_ha")
writeData(Aggregointi, "Apetit_lannoiteravinteet_kg_ha", Apetit_lannoiteravinteet_ka)
saveWorkbook(Aggregointi ,here("Output/Ravinnedata/Apetit_lannoitus_ravinteet_kaikki.xlsx"), overwrite = T)
