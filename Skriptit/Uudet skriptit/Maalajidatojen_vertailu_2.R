library(here)
library(tidyverse)

#Funktio jolla toisen skriptin saa sourcettua vain annettujen rivien osalta

source_lines <- function(file, lines) {
  source(textConnection(readLines(file)[lines]))
}

# Datojen nouto ja yhdistäminen ####

#Noudetaan GTK-aineisto
source_lines(here("Skriptit/Uudet skriptit/GTK_datan_aggregointi.R"),1:260)

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
GTK_alat$Mineraalin_osuus<-(GTK_alat$Mineraalimaata/GTK_alat$Yhteensa)*100



filter(GTK_alat, duplicated(PLTUNNUS)) #Tarkoitus on, että 1 pltunnus on mukana vain kerran

#Noudetaan viljavuusaineisto

source_lines(here("Skriptit/Uudet skriptit/Viljavuus_esikasittely_aggregointi.R"),1:238)
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
#Käänteisenä myös mineraalimaan osuus 

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


GTK_verrokit<-GTK_verrokit %>%
  mutate(Mineraaliprosentti_luokitus = case_when(Mineraalin_osuus >= 0 & Mineraalin_osuus <= 10 ~ "0-10",
                                             Mineraalin_osuus  >=  11 & Mineraalin_osuus <= 20 ~ ">10-20",
                                             Mineraalin_osuus  >=  21 & Mineraalin_osuus <= 30 ~ ">20-30",
                                             Mineraalin_osuus  >=  31 & Mineraalin_osuus <= 40 ~ ">30-40",
                                             Mineraalin_osuus  >=  41 & Mineraalin_osuus <= 50 ~ ">40-50",
                                             Mineraalin_osuus  >=  51 & Mineraalin_osuus <= 60 ~ ">50-60",
                                             Mineraalin_osuus  >=  61 & Mineraalin_osuus <= 70 ~ ">60-70",
                                             Mineraalin_osuus  >=  71 & Mineraalin_osuus <= 80 ~ ">70-80",
                                             Mineraalin_osuus  >=  81 & Mineraalin_osuus <= 90 ~ ">80-90",
                                             Mineraalin_osuus  >=  91 & Mineraalin_osuus <= 100 ~ ">90-100"))




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
    "Mineraalin_osuus_gtk",
    "Turveprosentti_luokitus_gtk",
    "Mineraaliprosentti_luokitus_gtk"
  )


rm(GTK_alat, GTK_verrokit, GTKdata, Multavuuksia_lohkolle, Viljavuus_alat, Viljavuus_alat_summaus, Viljavuusdata)
gc()
 

#Plotti 1 ####
#Montako tuhatta hehtaaria viljavuusdatan maalajiluokkia menee mihinkin GTK:n luokkaan? 

#Yhdistetty_aineisto$Peruslohkon_ala_viljavuus<-
  #Yhdistetty_aineisto$Peruslohkon_ala_viljavuus/1000 #ha -> kha



 Plotti1 <- Yhdistetty_aineisto %>% 
  filter(!is.na(Elop_Mineral)) %>% 
  ggplot(aes(x = Turveprosentti_luokitus_gtk, y =Peruslohkon_ala_viljavuus , fill = Elop_Mineral)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#d8b365","#5ab4ac"))+
  facet_grid(cols=vars(Elop_Mineral)) + 
  xlab("Categorization of parcels by % of org. soils in geospatial data")+
  ylab("kHa") +
  theme(legend.position="none")


#Plotti 2: Kuten edellä, mutta muutetaan prosenteiksi

#Totaalimäärä kutakin viljavuusdatan kategoriaa, elop tai mineral.   

Jakauma<-Yhdistetty_aineisto %>% group_by(Turveprosentti_luokitus_gtk, Elop_Mineral) %>% summarise(kHa=sum(Peruslohkon_ala_viljavuus))

#Totaali kutakin gtk-datan luokkaa. 

Totals<- Jakauma %>% group_by(Turveprosentti_luokitus_gtk) %>% summarise(kHa_total = sum (kHa))

#Yhdistetään Jakauma ja Totals, muunnetaan prosenteiksi.

Prosenttikuva<-inner_join(Jakauma, Totals, by="Turveprosentti_luokitus_gtk")

Prosenttikuva<-Prosenttikuva %>% mutate(Percentage = (kHa/kHa_total)*100)

Prosenttikuva$Elop_Mineral[Prosenttikuva$Elop_Mineral=="Mineraali"]<-"Mineral soils"

Prosenttikuva$Elop_Mineral[Prosenttikuva$Elop_Mineral=="Eloperäinen"]<-"Organic soils"

Plotti2<-Prosenttikuva %>% ggplot(aes(x = Turveprosentti_luokitus_gtk, y =Percentage, fill = Elop_Mineral)) +  theme_bw()+
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#d8b365","#5ab4ac"))+
  facet_grid(cols=vars(Elop_Mineral)) + 
  xlab("Classification of parcels by % of org. soils, geospatial data")+
  ylab("%") + scale_y_continuous(breaks = seq(0,100,by=10))+  labs(fill="Soil class in soil sampling data")+  geom_text(aes(label = paste(round(Percentage,0),"%"), vjust = 1.5, hjust=0.5))


#Plotti 3: sama, mutta peilikuvana.  

library(viridis)

Plotti3 <- Prosenttikuva %>%  ggplot(aes(x = Elop_Mineral, y =Percentage, fill = Turveprosentti_luokitus_gtk)) +  theme_bw()+
  geom_bar(stat = "identity") +
  scale_fill_viridis(discrete = T, alpha = 0.55) +
  facet_grid(cols=vars(Turveprosentti_luokitus_gtk)) + 
  xlab("Classification of parcels to organic and mineral soils, soil fertility data")+
  ylab("%") + 
  scale_y_continuous(breaks = seq(0,110,by=10))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+labs(fill="% of organic soils, geospatial data")+geom_text(aes(label = paste(round(Percentage,0),"%"), vjust = 0, hjust=0.5))


library(gridExtra)

Yhdistetty_kuva<-grid.arrange(Plotti2, Plotti3)


#Tunnuslukutaulukointia
Jakaumataulu<-Prosenttikuva %>% group_by(Elop_Mineral) %>% summarise(summa=sum(kHa))
Jakaumataulu$Pros<-round((Jakaumataulu$summa/sum(Jakaumataulu$summa))*100)


library(gt)
z<-Prosenttikuva
z$kHa_total<-NULL
z<-ungroup(z)
gt(z) %>%
  fmt_number(columns=3:4, decimals = 1)



#####################

#Lohkokohtainen erittely

Yhdistetty_aineisto$laskuri<-1

Yhdistetty_aineisto$Peruslohkon_ala_viljavuus

a<-Yhdistetty_aineisto %>% group_by(Elop_Mineral,Turveprosentti_luokitus_gtk ) %>% summarise(Maalajia_viljavuuden_mujaan=sum(Peruslohkon_ala_viljavuus),
                                                                                             lohkolukum=sum(laskuri))


b<-filter(a, Elop_Mineral=="Eloperäinen")
b$HehtaariPros<-(b$Maalajia_viljavuuden_mujaan/sum(b$Maalajia_viljavuuden_mujaan))*100

sum(b$Maalajia_viljavuuden_mujaan)
b$HehtaariPros<-round(b$HehtaariPros, 0)
b$Maalajia_viljavuuden_mujaan<-round(b$Maalajia_viljavuuden_mujaan)

b$Elop_Mineral<-"Organic"
b<- b %>% group_by(Elop_Mineral)
b$Elop_Mineral<-NULL

gt(b, groupname_col = NULL) %>%
  cols_label(Turveprosentti_luokitus_gtk = "% of organic soils, geospatial data",
             Maalajia_viljavuuden_mujaan = "ha",
             lohkolukum = "Parcel count",
             HehtaariPros = "%") %>%
  cols_align(align = "left", columns = 1) %>%
  cols_align(align="center", columns=2:4) %>%
  fmt_number(sep_mark = " ",
             decimals = 0,
             columns = 2:4) %>%
  tab_header("Organic parcels in soil fertility data") %>%
  cols_move(columns = 4, after = 2)


#Yli puolet turvetta gtk:n mukaan sisältävien lohkojen ha määrä ja pros määrä
#Nämä kaikki viljavuudessa eloperäistä maata

sum(b$Maalajia_viljavuuden_mujaan[6:10]) 

sum(b$HehtaariPros[6:10]) 






c<-filter(a, Elop_Mineral=="Mineraali")
c$HehtaariPros<-(c$Maalajia_viljavuuden_mujaan/sum(c$Maalajia_viljavuuden_mujaan))*100

c$Elop_Mineral<-"Mineral"
c<- c %>% group_by(Elop_Mineral)
c$Elop_Mineral<-NULL

gt(c, groupname_col = NULL) %>%
  cols_label(Turveprosentti_luokitus_gtk = "% of organic soils, geospatial data",
             Maalajia_viljavuuden_mujaan = "ha",
             lohkolukum = "Parcel count",
             HehtaariPros = "%") %>%
  cols_align(align = "left", columns = 1) %>%
  cols_align(align="center", columns=2:4) %>%
  fmt_number(sep_mark = " ",
             decimals = 0,
             columns = 2) %>%
  fmt_number(decimals = 1,
             columns = 4) %>%
  fmt_number(sep_mark = " ",
             columns = 3,
             decimals = 0) %>%
  tab_header("Mineral parcels in soil fertility data") %>%
  cols_move(columns = 4, after = 2)

#Yli puolet turvetta gtk:n mukaan sisältävien lohkojen ha määrä ja pros määrä
#Nämä kaikki viljavuudessa mineraalimaata

sum(c$Maalajia_viljavuuden_mujaan[6:10]) 

sum(c$HehtaariPros[6:10]) 


#Yhdistetään mineraali ja organic-taulut, b ja c


Orgtab<-b
colnames(Orgtab)<-c("Turveprosentti_luokitus_gtk","Eloperaista_ha","lohkolukum_elop","HehtaariPros_elop")


Mintab<-c
colnames(Mintab)<-c("Turveprosentti_luokitus_gtk","Mineraalia_ha","lohkolukum_min","HehtaariPros_min")


Min_Org<-inner_join(Mintab, Orgtab, by=c("Turveprosentti_luokitus_gtk"))

#Montako hehtaaria on mineraalia ja orgaanista, viljavuusdatan mukaan?

sum(Min_Org$Mineraalia_ha)
sum(Min_Org$Eloperaista_ha)

#Editointi kuvaan 22/08/24. Yksinkertaistetaan, käytetään lohkojen lukumääriä eikä hehtaareita. 

Min_Org<-Min_Org %>% select(1,3,6) %>% mutate(lohkoja_yht=lohkolukum_min+lohkolukum_elop)
Min_Org<-Min_Org %>% mutate(Mineraalipros = 100*(lohkolukum_min/lohkoja_yht),
                   Turvepros = 100*(lohkolukum_elop/lohkoja_yht))
Min_Org<-Min_Org %>% select(1,2,5,3,6,4)

write.xlsx(Min_Org, file = here("Output/Yksinkertaistettu_intensiteettil/Maalajivertailun_luvut.xlsx"))
library(gt)
gt(Min_Org, groupname_col = NULL) %>%
  cols_label(Turveprosentti_luokitus_gtk = "% of organic soils, geospatial data",
             lohkolukum_min = "Mineral parcels, count",
             Mineraalipros = "%",
             lohkolukum_elop="Organic parcels count",
             Turvepros = "%",
             lohkoja_yht = "Total parcel count") %>%
  grand_summary_rows(columns = c(2,4,6),
               fns= Total ~ sum(.),
               fmt = ~ fmt_number(., decimals = 0, sep_mark =" ")) %>%
  fmt_number(sep_mark = " ",
             decimals = 0,
             columns = c(2,4,6)) %>%
  fmt_number(decimals = 1,
             columns = c(3,5)) %>%
gtsave(filename="Aineistojen_maalajivertailu.docx", path = here("Output/Yksinkertaistettu_intensiteettilaskenta"))






# Minitaulukko ilman lohkojen erittelyä viljavuuden maalajiluokille



d <-
  Yhdistetty_aineisto %>% group_by(Turveprosentti_luokitus_gtk) %>% summarise(
    Maalajia_viljavuuden_mujaan = sum(Peruslohkon_ala_viljavuus),
    lohkolukum =
      sum(laskuri)
  )
d$HehtaariPros<-(d$Maalajia_viljavuuden_mujaan/sum(d$Maalajia_viljavuuden_mujaan))*100



Pienia_lohkoja<-Yhdistetty_aineisto %>% filter(Peruslohkon_ala_yhteensa_gtk >= 0 & Peruslohkon_ala_yhteensa_gtk <= 0.5)
Medium_lohkoja <- Yhdistetty_aineisto %>% filter(Peruslohkon_ala_yhteensa_gtk > 0.5 & Peruslohkon_ala_yhteensa_gtk <= 5 )
Isoja_lohkoja <-Yhdistetty_aineisto %>% filter(Peruslohkon_ala_yhteensa_gtk > 0.5 & Peruslohkon_ala_yhteensa_gtk > 5 )

rm(list=(setdiff(ls(),c("Isoja_lohkoja", "Medium_lohkoja","Pienia_lohkoja","Yhdistetty_aineisto") )))
