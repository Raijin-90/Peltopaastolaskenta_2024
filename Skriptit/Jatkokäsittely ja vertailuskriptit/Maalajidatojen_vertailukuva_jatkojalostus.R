library(here)
library(tidyverse)

#Funktio jolla toisen skriptin saa sourcettua vain annettujen rivien osalta

source_lines <- function(file, lines) {
  source(textConnection(readLines(file)[lines]))
}

# Datojen nouto ja yhdistäminen ####

#Noudetaan GTK-aineisto
source_lines(here("Skriptit/Uudet Skriptit/GTK_datan_aggregointi.R"),1:259)

#Maalajien vertailua varten gtk-datan kasvulohkot täytyy yhdistää kuvaamaan kutakin peruslohkoa yhteensä. 
#Näin siksi, ettei viljavuusaineisto tunnista kasvulohkoja samalla tarkkuudella. Se ei erota kasvulohkopolygoneja toisistaan nimeltä, vaan ainoastaan sisällöltä (1 peruslohko = monta riviä, jos monta kasvulohkoa).
#KLTUNNUS muuttuja ei vaihtele, sillä kaikki plukumittaukset ja muu siihen aineistoon kuuluva saa saman koordinaatin. 

GTKdata<-arrange(GTKdata, PLTUNNUS) 

#Data sisältää jokaisen peruslohkon sisältämät, kaikki kasvulohkot (A,B,C...) omina polygoneinaan. Niillä kaikilla on oma, erillinen pinta-ala. 

#Aggregoidaan muuttujat Maannossumma (kasvulohkon yhteispinta-ala), sekä mineraali- että turveala siltä lohkolta PLTUNNUS muuttujan mukaisesti. Saadaan peruslohkon koko ala, jolla mukana kaikki sen kasvulohkot. 

GTK_alat<-GTKdata %>% 
  group_by(PLTUNNUS) %>% 
  summarise(Mineraalimaata = sum(Mineraalia),Eloperaista = sum(Eloperaista)) %>%
  arrange(PLTUNNUS)
GTK_alat$Yhteensa <- GTK_alat$Mineraalimaata+GTK_alat$Eloperaista
GTK_alat$Eloperaisen_osuus<- (GTK_alat$Eloperaista/GTK_alat$Yhteensa)*100 

filter(GTK_alat, duplicated(PLTUNNUS))

#Noudetaan viljavuusaineisto

source_lines(here("Skriptit/Viljavuus_esikasittely_aggregointi.R"),1:235)
rm(list=c("Kasvikategoriat_avain", "Viljavuus_aggregointi_multavuus", "Yhdistetty_peltodata_raivaukset_rehuvilja_multavuus","z"))


Viljavuus_alat<-Viljavuusdata %>% select(PLTUNNUS, Multavuusluokka, KASVI_ALA_HA) 
Viljavuus_alat$laskuri<-1
Viljavuus_alat<-arrange(Viljavuus_alat, PLTUNNUS)

#Tarkistetaan, montako eri multavuutta data ehdottaa per lohko
Multavuuksia_lohkolle<-Viljavuus_alat %>% group_by(PLTUNNUS, Multavuusluokka) %>% summarise(Maalajeja_ehdotettu_lohkolla = sum(laskuri))
Multavuuksia_lohkolle<-arrange(Multavuuksia_lohkolle, PLTUNNUS)
#Tavoite on että kullakin lohkolla on vain yksi sitä kuvaava multavuus joka määrittää maalajin. Ei tulisi olla toistuvia lohkokoodeja, kun tarkastellaan multavuuksien frekvenssejä lohkoittain. 

Multavuuksia_lohkolle %>% filter(duplicated(PLTUNNUS))

#Nolla toistuvaa lohkokoodia. Voidaan jatkaa. 

#VIljavuus_alat framessa on sama lohko useita kertoja, jos koostuu eri kasvulohkoista. Niillä on kaikilla oma pinta-ala, KASVI_ALA_HA.  Jos yhdestä, lohko on mukana kerran.
#Summaamalla kaikki samaan Peruslohkon tunnukseen kuuluvat KASVI_ALA_HA pinta-alat (kasvulohkojen alat), saadaan niistä muodostettua peruslohkon yhteenlaskettu pinta ala

filter(Viljavuus_alat, duplicated(PLTUNNUS))


Viljavuus_alat_summaus<-Viljavuus_alat %>% 
  group_by(PLTUNNUS, Multavuusluokka) %>% 
  summarise(Peruslohkon_ala_viljavuus = sum(KASVI_ALA_HA))

filter(Viljavuus_alat_summaus, duplicated(PLTUNNUS))

#Multavuusluokkien jako eloper./mineraalimaahan
#Määritellään mikä multavuus on eloperäistä ja mikä ei



Viljavuus_alat_summaus$Elop_Mineral <- NA
Viljavuus_alat_summaus$Elop_Mineral[Viljavuus_alat_summaus$Multavuusluokka == "Ei multavuustietoa"] <-
  "Mineraali"
Viljavuus_alat_summaus$Elop_Mineral[Viljavuus_alat_summaus$Multavuusluokka == "vm"] <-
  "Mineraali"
Viljavuus_alat_summaus$Elop_Mineral[Viljavuus_alat_summaus$Multavuusluokka == "m"] <-
  "Mineraali"
Viljavuus_alat_summaus$Elop_Mineral[Viljavuus_alat_summaus$Multavuusluokka == "rm"] <-
  "Mineraali"
Viljavuus_alat_summaus$Elop_Mineral[Viljavuus_alat_summaus$Multavuusluokka == "erm"] <-
  "Mineraali"
Viljavuus_alat_summaus$Elop_Mineral[Viljavuus_alat_summaus$Multavuusluokka == "mm"] <-
  "Eloperäinen"
Viljavuus_alat_summaus$Elop_Mineral[Viljavuus_alat_summaus$Multavuusluokka == "eloperäiset maat"] <-
  "Eloperäinen"


Viljavuus_alat_summaus<-filter(Viljavuus_alat_summaus, !is.na(Multavuusluokka))
Viljavuus_alat_summaus<-filter(Viljavuus_alat_summaus, !(Multavuusluokka == "Ei multavuustietoa"))
#Vertailuaineiston luominen
#Yhdistetään yllä luodut peruslohkotason aineistot Viljavuus_alat_summaus ja GTK_alat:
#Otetaan vektori, jossa mukana lohkokoodit jotka esiintyvät kummassakin datassa

Yhteiset<- intersect(GTK_alat$PLTUNNUS, Viljavuus_alat_summaus$PLTUNNUS)

#Kummastakin datasta otetaan irti nämä samat lohkot. GTK data on suurempi, otetaan siitä irti samat lohkot kuin viljavuudessa on

GTK_verrokit<-filter(GTK_alat, GTK_alat$PLTUNNUS %in% Viljavuus_alat_summaus$PLTUNNUS)

library(usefun)
outersect(GTK_verrokit$PLTUNNUS, Viljavuus_alat_summaus$PLTUNNUS) #Samat lohkot kummastakin mukana.Outersect näyttäisi eroavat arvot kahden vektorin välillä, ts. anti-intersect.  


#Lasketaan GTK-verrokkeihin eloperäisen maan osuusluokitus desiileittäin 

GTK_verrokit$Eloperaisen_osuus<-round(GTK_verrokit$Eloperaisen_osuus, 0)

GTK_verrokit<-GTK_verrokit %>%
  mutate(Turveprosentti_luokitus = case_when(Eloperaisen_osuus >= 0 & Eloperaisen_osuus <= 10 ~ "0-10",
                                             Eloperaisen_osuus  >=  11 & Eloperaisen_osuus <= 20 ~ ">10-20",
                                             Eloperaisen_osuus  >=  21 & Eloperaisen_osuus <= 30 ~ ">20-30",
                                             Eloperaisen_osuus  >=  31 & Eloperaisen_osuus <= 40 ~ ">30-40",
                                             Eloperaisen_osuus  >=  41 & Eloperaisen_osuus <= 50 ~ ">40-50",
                                             Eloperaisen_osuus  >=  51 & Eloperaisen_osuus <= 60 ~ ">50-60",
                                             Eloperaisen_osuus  >=  61 & Eloperaisen_osuus <= 70 ~ ">60-70",
                                             Eloperaisen_osuus  >=  71 & Eloperaisen_osuus <= 80 ~ ">70-80",
                                             Eloperaisen_osuus  >=  81 & Eloperaisen_osuus <= 90 ~ ">80-90",
                                             Eloperaisen_osuus  >=  91 & Eloperaisen_osuus <= 100 ~ ">90-100"))

filter(GTK_verrokit, duplicated(PLTUNNUS))



#Sitten yhdistetään nämä kaksi aineistoa, GTK_verrokit ja Viljavuus_alat_summaus

Yhdistetty_aineisto<-inner_join(Viljavuus_alat_summaus, GTK_verrokit, by="PLTUNNUS")
colnames(Yhdistetty_aineisto) <-
  c(
    "PLTUNNUS",
    "Multavuusluokka_viljavuus",
    "Peruslohkon_ala_viljavuus",
    "Elop_Mineral",
    "Mineraalimaata_gtk",
    "Eloperaista_gtk",
    "Peruslohkon_ala_yhteensa_gtk",
    "Eloperaisen_osuus_gtk",
    "Turveprosentti_luokitus_gtk"
  )


rm(GTK_alat, GTK_verrokit, GTKdata, Multavuuksia_lohkolle, Viljavuus_alat, Viljavuus_alat_summaus, Viljavuusdata)
gc()
 



#Montako prosenttia viljavuusdatan alasta menee mihinkin gtk-desiiliin? Ei eritellä mineraalia ja orgaanista tässä. 
  
#Totaalimäärä kutakin viljavuusdatan kategoriaa, elop tai mineral.   


Jakauma <-Yhdistetty_aineisto %>% 
  group_by(Turveprosentti_luokitus_gtk) %>% 
  summarise(ha =sum(Peruslohkon_ala_viljavuus)) 

Jakauma$Osuus<-(Jakauma$ha/sum(Jakauma$ha))*100

Jakauma <- Jakauma %>% #Määritellään palkkien järjestys tekemällä luokkafaktori, jossa levels määrää piirtojärjestyksen
  arrange(Turveprosentti_luokitus_gtk) %>%
  mutate(Turveprosentti_luokitus_gtk = fct_relevel(
    Turveprosentti_luokitus_gtk,
      "0-10",
      ">10-20",
      ">20-30",
      ">30-40",
      ">40-50",
      ">50-60",
      ">60-70",
      ">70-80",
      ">80-90",
      ">90-100"
    )
  )

Jakauma %>% 
  ggplot(aes(x = Turveprosentti_luokitus_gtk, y =Osuus, fill = Turveprosentti_luokitus_gtk)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(limits=c(0,100),breaks = seq(0,100,by=10))+
  scale_fill_viridis(discrete = T)+
  xlab("Classification of parcels by % of org. soils")+
  ylab("%") + 
  labs(fill="% of organic soils")+  
  geom_text(aes(label = paste(round(Osuus,0),"%"), vjust = -1, hjust=0.5))





#Pelkkä turve jaettuna desiileille
#Montako prosenttia viljavuusdatan alasta menee mihinkin gtk-desiiliin? Ei eritellä mineraalia ja orgaanista tässä. 

#Totaalimäärä kutakin viljavuusdatan kategoriaa, elop tai mineral.   


Jakauma <-Yhdistetty_aineisto %>% 
  group_by(Turveprosentti_luokitus_gtk, Elop_Mineral) %>% 
  summarise(ha =sum(Peruslohkon_ala_viljavuus)) %>% filter(Elop_Mineral == "Eloperäinen")

Jakauma$Osuus<-(Jakauma$ha/sum(Jakauma$ha))*100

Jakauma <- Jakauma %>% #Määritellään palkkien järjestys tekemällä luokkafaktori, jossa levels määrää piirtojärjestyksen
  arrange(Turveprosentti_luokitus_gtk) %>%
  mutate(Turveprosentti_luokitus_gtk = fct_relevel(
    Turveprosentti_luokitus_gtk,
    "0-10",
    ">10-20",
    ">20-30",
    ">30-40",
    ">40-50",
    ">50-60",
    ">60-70",
    ">70-80",
    ">80-90",
    ">90-100"
  )
  )

Jakauma %>% 
  ggplot(aes(x = Turveprosentti_luokitus_gtk, y =Osuus, fill = Turveprosentti_luokitus_gtk)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(limits=c(0,100),breaks = seq(0,100,by=10))+
  scale_fill_viridis(discrete = T)+
  xlab("Classification of organic parcels by % of org. soils, geospatial data")+
  ylab("%") + 
  labs(fill="% of organic soils")+  
  geom_text(aes(label = paste(round(Osuus,0),"%"), vjust = -1, hjust=0.5))

