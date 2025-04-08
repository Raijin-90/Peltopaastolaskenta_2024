library(stringr);library(usefun);library(varhandle);library(here);library(tidyverse)

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

#Aggregointi

source(here("Skriptit/SustAnimalFood/Eläintilojen_päästöt_kunnittain_A_pinta_alat.R"))

#Määritellään päästökertoimet

source_lines(here("Skriptit/Uudet skriptit/CO2 intensiteetit.R"), 6:68)


#PÄÄSTÖJEN LASKENTA. CO2. . ####
#Kerrotaan lasketut pinta-alat päästökertoimilla. Oikea kerroin määritettävä kohtaan { x * TÄHÄN PÄÄSTÖKERROIN} 
#Mineraalimaa. Näissä mukana ainoastaan raivaamattomat. 
Cropland_korotettu_mineraalimaa[7:length(Cropland_korotettu_mineraalimaa)] <-
  apply(Cropland_korotettu_mineraalimaa[7:length(Cropland_korotettu_mineraalimaa)], 2, function(x) {
    x * viljely_CO2_cropland_mineral
  })

Grassland_korotettu_mineraalimaa[7:length(Grassland_korotettu_mineraalimaa)]<-
  apply(Grassland_korotettu_mineraalimaa[7:length(Grassland_korotettu_mineraalimaa)], 2, function(x) {
    x * viljely_CO2_grassland_mineral
  })

#Mineraalimaa, raivio

Cropland_korotettu_mineraalimaa_raivio[7:length(Cropland_korotettu_mineraalimaa_raivio)] <-
  apply(Cropland_korotettu_mineraalimaa_raivio[7:length(Cropland_korotettu_mineraalimaa_raivio)], 2, function(x) {
    x * raivaus_CO2_cropland_mineral
  })

Grassland_korotettu_mineraalimaa_raivio[7:length(Grassland_korotettu_mineraalimaa_raivio)]<-
  apply(Grassland_korotettu_mineraalimaa_raivio[7:length(Grassland_korotettu_mineraalimaa_raivio)], 2, function(x) {
    x * raivaus_CO2_grassland_mineral
  })

#Eloperainen maa 

#Erillinen kerroin tämän kategorian annual cropsille, ja grassille (monivuotiset, sadolliset nurmikasvit). Ts. Croplandin sisällä olevat, sadolliset monivuotisnurmet kuten rehunurmet.  
#Otetaan kumpikin erilleen, lasketaan päästö, ja liitetään takaisin yhteen. 

a<-filter(Cropland_korotettu_elop, Cropland_korotettu_elop$`Yksi/monivuotinen` == "Monivuotinen")

a[7:length(a)] <-
  apply(a[7:length(a)], 2, function(x) {
    x * viljely_CO2_cropland_elop_grass
  })

b<- filter(Cropland_korotettu_elop, Cropland_korotettu_elop$`Yksi/monivuotinen` == "Yksivuotinen")

b[7:length(b)] <-
  apply(b[7:length(b)], 2, function(x) {
    x * viljely_CO2_cropland_elop_annual_crops
  })

#Yhdistetään takaisin
Cropland_korotettu_elop<-rbind(a,b)


Grassland_korotettu_elop[7:length(Grassland_korotettu_elop)]<-
  apply(Grassland_korotettu_elop[7:length(Grassland_korotettu_elop)], 2, function(x) {
    x * viljely_CO2_grassland_elop
  })


#Eloperäinen maa, raivio 

Cropland_korotettu_elop_raivio[7:length(Cropland_korotettu_elop_raivio)] <-
  apply(Cropland_korotettu_elop_raivio[7:length(Cropland_korotettu_elop_raivio)], 2, function(x) {
    x * raivaus_CO2_cropland_elop
    
  })
Grassland_korotettu_elop_raivio[7:length(Grassland_korotettu_elop_raivio)]<-
  apply(Grassland_korotettu_elop_raivio[7:length(Grassland_korotettu_elop_raivio)], 2, function(x) {
    x * raivaus_CO2_grassland_elop
  })



#RAIVAUKSEN JA VILJELYN PÄÄSTÖJEN SUMMAAMINEN TUOTANTOSUUNNALLE/TUOTTEELLE ####
#Yhdistetään viljelypäästöjen frameihin raivauksesta aiheutuva päästö. 
#Frameen tulee 2 kpl kutakin tuotantosuuntaa, x ja y, joista toinen on raivauksen päästö ja toinen viljelyn. Nämä summataan. 

Grassland_korotettu_elop_raivio<-
  select(Grassland_korotettu_elop_raivio,
        1:6,
         7:length(Grassland_korotettu_elop_raivio))
Cropland_korotettu_elop_raivio <-
  select(Cropland_korotettu_elop_raivio,
        1:6,
         7:length(Cropland_korotettu_elop_raivio))
Grassland_korotettu_mineraalimaa_raivio<-
  select(Grassland_korotettu_mineraalimaa_raivio,
        1:6,
         7:length(Grassland_korotettu_mineraalimaa_raivio))
Cropland_korotettu_mineraalimaa_raivio <-
  select(Cropland_korotettu_mineraalimaa_raivio,
        1:6,
         7:length(Cropland_korotettu_mineraalimaa_raivio))

#Merge eloperäiset
Grassland_elop<-  merge(
  Grassland_korotettu_elop,
  Grassland_korotettu_elop_raivio,
  by = c("Kasvikoodi","Kuntakoodi","Kasvinimi","Cropland/grassland","Yksi/monivuotinen","Tuoteryhmä"),
  all = T
)
Grassland_elop[is.na(Grassland_elop)]<-0
sum(colSums(Grassland_elop[7:31]))+
  sum(colSums(Grassland_elop[32:length(Grassland_elop)]))

Cropland_elop <-
  merge(
    Cropland_korotettu_elop,
    Cropland_korotettu_elop_raivio,
    by = c("Kasvikoodi","Kuntakoodi","Kasvinimi","Cropland/grassland","Yksi/monivuotinen","Tuoteryhmä"),
    all = T
  )
Cropland_elop[is.na(Cropland_elop)]<-0
sum(colSums(Cropland_elop[7:31]))+
  sum(colSums(Cropland_elop[32:length(Cropland_elop)]))

#merge mineraali
Grassland_mineral<-  merge(
  Grassland_korotettu_mineraalimaa,
  Grassland_korotettu_mineraalimaa_raivio,
  by = c("Kasvikoodi","Kuntakoodi","Kasvinimi","Cropland/grassland","Yksi/monivuotinen","Tuoteryhmä"),
  all = T
)
Grassland_mineral[is.na(Grassland_mineral)]<-0

sum(colSums(Grassland_mineral[7:31]))+
  sum(colSums(Grassland_mineral[32:length(Grassland_mineral)]))

Cropland_mineral <-
  merge(
    Cropland_korotettu_mineraalimaa,
    Cropland_korotettu_mineraalimaa_raivio,
    by = c("Kasvikoodi","Kuntakoodi","Kasvinimi","Cropland/grassland","Yksi/monivuotinen","Tuoteryhmä"),
    all = T
  )
Cropland_mineral[is.na(Cropland_mineral)]<-0
sum(colSums(Cropland_mineral[7:31]))+
  sum(colSums(Cropland_mineral[32:length(Cropland_mineral)]))

#Tärkeät framet ovat Cropland/Grassland elop ja -mineral. 

#Summataan: x on viljelyn ja y raivauksen päästö, luotu uusi muuttuja näiden summa
#Mutate across ei nyt onnistu koska jokaiselle tuotantosuunnalle tehdään eri operaatio. 

source_lines(here("Skriptit/Uudet skriptit/CO2 intensiteetit.R"), 305:414) 

#Poistetaan x- ja y-muuttujat, jätetään vain summa. 

Cropland_mineral[7:58]<-NULL
Cropland_elop[7:58]<-NULL
Grassland_mineral[7:58]<-NULL
Grassland_elop[7:58]<-NULL #dimensiot muutettu huomioimaan turkistilojen mukaanotto


sum(Cropland_mineral[7:length(Cropland_mineral)])+sum(Cropland_elop[7:length(Cropland_elop)])

#Kokonaispäästöjen laskenta ####
#Yhdistetään päästöt eloperäiseltä ja mineraalimaalta. Raivaus on laskettu jo mukaan. 

Cropland_yhdistetyt_paastot <-
  merge(
    Cropland_mineral,
    Cropland_elop,
    by = c(
      "Kasvikoodi",
      "Kuntakoodi", 
      "Kasvinimi",
      "Cropland/grassland",
      "Yksi/monivuotinen",
      "Tuoteryhmä"
    ),
    all = T
  )

Cropland_yhdistetyt_paastot[is.na(Cropland_yhdistetyt_paastot)]<-0
#Check sums
sum(colSums(Cropland_yhdistetyt_paastot[7:length(Cropland_yhdistetyt_paastot)])) == sum(Cropland_mineral[7:length(Cropland_mineral)])+sum(Cropland_elop[7:length(Cropland_elop)])

#Sato- ja europohjaiset intensiteetit tarvitsevat croplandin päästöt, mutta ei grasslandia

#Mineraali ja eloperäisen croplandin päästöjen summa. Vanhan skriptin uudellenkäyttöä.
source_lines(here("Skriptit/Uudet skriptit/CO2 intensiteetit.R"), 450:476) 

Cropland_yhdistetyt_paastot<- Cropland_yhdistetyt_paastot %>% select(1:6,59:length(Cropland_yhdistetyt_paastot))

#Grasslandin käsittely vastaavalla tavalla:

#Mineraali ja eloperäisen grasslandin päästöjen summa

Grassland_yhdistetyt_paastot<-
  merge(
    Grassland_mineral,
    Grassland_elop,
    by = c(
      "Kasvikoodi",
      "Kuntakoodi", 
      "Kasvinimi",
      "Cropland/grassland",
      "Yksi/monivuotinen",
      "Tuoteryhmä"
    ),
    all = T
  )
Grassland_yhdistetyt_paastot[is.na(Grassland_yhdistetyt_paastot)]<-0
sum(colSums(Grassland_yhdistetyt_paastot[7:length(Grassland_yhdistetyt_paastot)])) == sum(Grassland_mineral[7:length(Grassland_mineral)])+sum(Grassland_elop[7:length(Grassland_elop)])

#Mineraalin ja eloperäisen summa grasslandille
source_lines(here("Skriptit/Uudet skriptit/CO2 intensiteetit.R"), 548:573) 

Grassland_yhdistetyt_paastot<- Grassland_yhdistetyt_paastot %>% select(1:6,59:length(Grassland_yhdistetyt_paastot))

rm(
  Cropland_korotettu_elop,
  Cropland_korotettu_elop_raivio,
  Grassland_korotettu_elop,
  Grassland_korotettu_elop_raivio,
  Cropland_korotettu_mineraalimaa,
  Cropland_korotettu_mineraalimaa_raivio,
  Grassland_korotettu_mineraalimaa,
  Grassland_korotettu_mineraalimaa_raivio
)
rm(Cropland_elop,
   Cropland_mineral,
   Grassland_elop,
   Grassland_mineral 
)


#Lisäys  08/23: päästöt voidaan myös aggregoida tuotantosuuntatasolle, jotta näkee miten päästökakku jakautuu niiden kesken.
#tUOTANTOSUUNTARYHMÄT, MUUTTAMINEN PITKÄÄN MUOTOON
#Tämän voi tehdä etol-yhteensopivasti tai originaalilla, jolle oma erillinen avain. Alla, kommentoitu pois. 

library(readxl)
Tuotantosuuntaryhmat <-
  read_excel(
    here("Data","Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx"),
    sheet = "Tuotantosuunnat ryhmittäin",
    col_types = c("text",
                  "text",
                  "skip"
    )
  )
colnames(Tuotantosuuntaryhmat) <-
  c("Tuotantosuunta", "Tuotantosuuntaryhmä")



Cropland_yhdistetyt_paastot <- Cropland_yhdistetyt_paastot %>% 
  pivot_longer(cols = 7:length(Cropland_yhdistetyt_paastot), names_to = "Tuotantosuunta", values_to = "Paastotonnit_CO2")

library(usefun)
outersect(Cropland_yhdistetyt_paastot$Tuotantosuunta, Tuotantosuuntaryhmat$Tuotantosuunta)

Cropland_yhdistetyt_paastot <-
  merge(
    Cropland_yhdistetyt_paastot,
    Tuotantosuuntaryhmat,
    by = "Tuotantosuunta",
    all = T
  )


Grassland_yhdistetyt_paastot <- Grassland_yhdistetyt_paastot %>% 
  pivot_longer(cols = 7:length(Grassland_yhdistetyt_paastot), names_to = "Tuotantosuunta", values_to = "Paastotonnit_CO2")


outersect(Grassland_yhdistetyt_paastot$Tuotantosuunta, Tuotantosuuntaryhmat$Tuotantosuunta)


Grassland_yhdistetyt_paastot <-
  merge(
    Grassland_yhdistetyt_paastot,
    Tuotantosuuntaryhmat,
    by = "Tuotantosuunta",
    all = T
  )



Cropland_yhdistetyt_paastot<-Cropland_yhdistetyt_paastot %>% group_by(Tuotantosuuntaryhmä, Kuntakoodi, Kasvikoodi, Kasvinimi,Tuoteryhmä,`Cropland/grassland`,`Yksi/monivuotinen`) %>% summarise(Paastotonnit_CO2 = sum(Paastotonnit_CO2))

Grassland_yhdistetyt_paastot<-Grassland_yhdistetyt_paastot %>% group_by(Tuotantosuuntaryhmä, Kuntakoodi, Kasvikoodi, Kasvinimi,Tuoteryhmä,`Cropland/grassland`,`Yksi/monivuotinen`) %>% summarise(Paastotonnit_CO2 = sum(Paastotonnit_CO2))


#Cropland- ja grassland-päästöjen summaus
#Summataan nämä kokonaispäästöiksi. Mukaan on laskettu cropland ja grassland, sekä raivattu että raivaamaton, molemmat maalajit 

CO2_kokonaispäästöt<- merge(Cropland_yhdistetyt_paastot, Grassland_yhdistetyt_paastot, by=c("Tuotantosuuntaryhmä", "Kuntakoodi", "Kasvikoodi", "Kasvinimi","Tuoteryhmä","Cropland/grassland","Yksi/monivuotinen"), all=T)
CO2_kokonaispäästöt[is.na(CO2_kokonaispäästöt)]<-0
CO2_kokonaispäästöt$CO2_yhteensa<-CO2_kokonaispäästöt$Paastotonnit_CO2.x+CO2_kokonaispäästöt$Paastotonnit_CO2.y
CO2_kokonaispäästöt<-CO2_kokonaispäästöt %>% select(1:7,10)


#Pinta-alojen yhdistäminen ensin toisiinsa, sitten päästöjen linkkaus näihin. 


rm.all.but(
  c(
    "CO2_kokonaispäästöt",
    "Cropland_elop_alat",
    "Cropland_elop_alat_raivio",
    "Grassland_elop_alat",
    "Grassland_elop_alat_raivio",
    "Cropland_mineraalimaa_alat",
    "Cropland_mineraalimaa_alat_raivio",
    "Grassland_mineraalimaa_alat",
    "Grassland_mineraalimaa_alat_raivio"
  )
)

#Alat yhteen

Cropland_elop_alat <- Cropland_elop_alat %>%  pivot_longer(
  cols = 7:length(Cropland_elop_alat),
  names_to = "Tuotantosuunta",
  values_to = "Cropland_elop_ha"
)

Cropland_elop_alat_raivio <- Cropland_elop_alat_raivio %>%  pivot_longer(
  cols = 7:length(Cropland_elop_alat_raivio),
  names_to = "Tuotantosuunta",
  values_to = "Cropland_elop_ha_raiv"
)

Elop_Crop<-merge(
  Cropland_elop_alat,
  Cropland_elop_alat_raivio,
  by = c(
    "Kasvikoodi",
    "Kuntakoodi",
    "Kasvinimi",
    "Cropland/grassland",
    "Yksi/monivuotinen",
    "Tuoteryhmä",
    "Tuotantosuunta"
  ),
  all = T
)
Elop_Crop[is.na(Elop_Crop)]<-0

rm(Cropland_elop_alat,Cropland_elop_alat_raivio)


Cropland_mineraalimaa_alat <- Cropland_mineraalimaa_alat %>%  pivot_longer(
  cols = 7:length(Cropland_mineraalimaa_alat),
  names_to = "Tuotantosuunta",
  values_to = "Cropland_mineraalimaa_ha"
)

Cropland_mineraalimaa_alat_raivio <- Cropland_mineraalimaa_alat_raivio %>%  pivot_longer(
  cols = 7:length(Cropland_mineraalimaa_alat_raivio),
  names_to = "Tuotantosuunta",
  values_to = "Cropland_mineraalimaa_ha_raiv"
)

Min_crop<-merge(
  Cropland_mineraalimaa_alat,
  Cropland_mineraalimaa_alat_raivio,
  by = c(
    "Kasvikoodi",
    "Kuntakoodi",
    "Kasvinimi",
    "Cropland/grassland",
    "Yksi/monivuotinen",
    "Tuoteryhmä",
    "Tuotantosuunta"
  ),
  all = T
)
Min_crop[is.na(Min_crop)]<-0

rm(Cropland_mineraalimaa_alat_raivio,Cropland_mineraalimaa_alat)


Grassland_elop_alat <- Grassland_elop_alat %>%  pivot_longer(
  cols = 7:length(Grassland_elop_alat),
  names_to = "Tuotantosuunta",
  values_to = "Grassland_elop_ha"
)

Grassland_elop_alat_raivio <- Grassland_elop_alat_raivio %>%  pivot_longer(
  cols = 7:length(Grassland_elop_alat_raivio),
  names_to = "Tuotantosuunta",
  values_to = "Grassland_elop_ha_raiv"
)

Elop_grass<-merge(
  Grassland_elop_alat,
  Grassland_elop_alat_raivio,
  by = c(
    "Kasvikoodi",
    "Kuntakoodi",
    "Kasvinimi",
    "Cropland/grassland",
    "Yksi/monivuotinen",
    "Tuoteryhmä",
    "Tuotantosuunta"
  ),
  all = T
)
Elop_grass[is.na(Elop_grass)]<-0

rm(Grassland_elop_alat, Grassland_elop_alat_raivio)




Grassland_mineraalimaa_alat <- Grassland_mineraalimaa_alat %>%  pivot_longer(
  cols = 7:length(Grassland_mineraalimaa_alat),
  names_to = "Tuotantosuunta",
  values_to = "Grassland_mineraalimaa_ha"
)

Grassland_mineraalimaa_alat_raivio <- Grassland_mineraalimaa_alat_raivio %>%  pivot_longer(
  cols = 7:length(Grassland_mineraalimaa_alat_raivio),
  names_to = "Tuotantosuunta",
  values_to = "Grassland_mineraalimaa_ha_raiv"
)

Min_grass<-merge(
  Grassland_mineraalimaa_alat,
  Grassland_mineraalimaa_alat_raivio,
  by = c(
    "Kasvikoodi",
    "Kuntakoodi",
    "Kasvinimi",
    "Cropland/grassland",
    "Yksi/monivuotinen",
    "Tuoteryhmä",
    "Tuotantosuunta"
  ),
  all = T
)
Min_grass[is.na(Min_grass)]<-0
rm(Grassland_mineraalimaa_alat, Grassland_mineraalimaa_alat_raivio)

#Lopullinen yhdistäminen

Merged_cropland<-merge(Elop_Crop,
Min_crop, by=c("Kasvikoodi", "Kuntakoodi", "Kasvinimi", "Cropland/grassland", "Yksi/monivuotinen", "Tuoteryhmä", "Tuotantosuunta"), all=T)

Merged_grassland<-merge(Elop_grass,
                        Min_grass, by=c("Kasvikoodi", "Kuntakoodi", "Kasvinimi", "Cropland/grassland", "Yksi/monivuotinen", "Tuoteryhmä", "Tuotantosuunta"), all=T)

Final_merge <- merge(
  Merged_cropland,
  Merged_grassland,
  by = c(
    "Kasvikoodi",
    "Kuntakoodi",
    "Kasvinimi",
    "Cropland/grassland",
    "Yksi/monivuotinen",
    "Tuoteryhmä",
    "Tuotantosuunta"
  ),
  all = T
)

Final_merge[is.na(Final_merge)]<-0

Final_merge$Yhteenlaskettu_ala<-rowSums(Final_merge[8:length(Final_merge)])

#Alat tuotantosuuntaryhmiksi

Final_merge<-Final_merge %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                  Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                  Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                  Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                  Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                  Tuotantosuunta == "Tattari ja kinoa" ~ "Tattari_kinoa",
                                  Tuotantosuunta == "Vihannekset ja juurekset" ~ "Vihannekset_juurekset",
                                  Tuotantosuunta == "Viljat pl. ohra" ~ "Viljat_pl_ohra",
                                  .default = Tuotantosuunta))
library(readxl)
Tuotantosuuntaryhmat <-
  read_excel(
    here("Data","Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx"),
    sheet = "Tuotantosuunnat ryhmittäin",
    col_types = c("text",
                  "text",
                  "skip"
    )
  )
colnames(Tuotantosuuntaryhmat) <-
  c("Tuotantosuunta", "Tuotantosuuntaryhmä")


Final_merge<-merge(Final_merge, Tuotantosuuntaryhmat, by="Tuotantosuunta")

rm(Tuotantosuuntaryhmat)


Final_merge<-Final_merge %>% group_by(Tuotantosuuntaryhmä,Kasvikoodi,Kasvinimi,Kuntakoodi,`Cropland/grassland`,`Yksi/monivuotinen`,Tuoteryhmä) %>% summarise_at(
  c("Cropland_elop_ha",
  "Cropland_elop_ha_raiv",
  "Cropland_mineraalimaa_ha",
  "Cropland_mineraalimaa_ha_raiv",
  "Grassland_elop_ha",
  "Grassland_elop_ha_raiv",
  "Grassland_mineraalimaa_ha",
  "Grassland_mineraalimaa_ha_raiv",
  "Yhteenlaskettu_ala"), sum
)

rm.all.but(c("Final_merge", "CO2_kokonaispäästöt"))

#Päästöt kiinni

Tulos <- merge(
  CO2_kokonaispäästöt,
  Final_merge,
  by = c(
    "Tuotantosuuntaryhmä",
    "Kasvikoodi",
    "Kasvinimi",
    "Kuntakoodi",
    "Cropland/grassland",
    "Yksi/monivuotinen",
    "Tuoteryhmä"
  ),
  all = T
)

Tulos<-Tulos %>% mutate(Eloperäistä = Cropland_elop_ha+Cropland_elop_ha_raiv+Grassland_elop_ha+Grassland_elop_ha_raiv,
                 Mineraalia = Cropland_mineraalimaa_ha+Cropland_mineraalimaa_ha_raiv+Grassland_mineraalimaa_ha+Grassland_mineraalimaa_ha_raiv)
Tulos<-Tulos %>% select(1:8, 17:19)
Tulos %>% select(
  Kuntakoodi,
  Tuotantosuuntaryhmä,
  Kasvikoodi,
  Kasvinimi,
  `Cropland/grassland`,
  `Yksi/monivuotinen`,
  Tuoteryhmä,
  CO2_yhteensa,
  Yhteenlaskettu_ala,
  Eloperäistä,
  Mineraalia
)
Output<-createWorkbook()
addWorksheet(Output,"CO2_alat_kunnat_maaperäpäästöt")
writeData(Output,"CO2_alat_kunnat_maaperäpäästöt", Tulos)
saveWorkbook(Output, file=here("Output/SustAnimalFood/Maaperapaasto_kunnat.xlsx"))
