#Muutoin samat kuin alkuperäiset tasokorjaus-nimiset skriptit, mutta viljelyala jätetään ennalleen, sitä ei verrata inventaariin eikä korotella.
# DATA SISÄÄN ####
library(readxl)
library(dplyr)
library(tidyr)
library(here)
library(usefun)
#Muutos 09/23: laskennan dimensioita muutettu siten, että turkistarhaus (matriisin sarakedimensio 22 27:stä) tulee huomioiduksi.


#Ennen tätä aja esikäsittelyskripti:
source(here("Skriptit", "Viljavuus_esikasittely_aggregointi.R"))
#Se luo tarvittavat korjaukset ja aggregaatit, jotka ladataan seuraavana sisään.

Viljavuus_Kaikki <-
  read_excel(
    here(
      "Output/AreaAggregates",
      "Viljavuusdata_aggregointi_multavuudesta.xlsx"
    ),
    sheet = "KaikkiPellot"
  ) #LAdataan esikäsittelyskriptissä luotu aggregaatti sisään
#Tässä välilehdessä on 11/2023 tehtyjen muutosten myötä mukana VAIN RAIVAAMATTOMAT PELLOT. välilehtinimeä ei vaihdeta sen laajojen sidonnaisuuksien vuoksi

#Ota kategorioihin mukaan vain kasvikoodi, älä nimeä. Numerosarja riittää yhdistämisiin.
Kasvikategoriat_avain <-
  read_excel(
    here("Data", "Kasvikategoriat_avain.xlsx"),
    col_types = c("text", "text", "text",
                  "numeric", "skip")
  )

Viljavuus_Kaikki <-
  merge(Viljavuus_Kaikki, Kasvikategoriat_avain, by = "Kasvikoodi")
rm(Kasvikategoriat_avain)


#MINERAALIMAAN EROTTAMINEN AINEISTOSTA ####

Viljavuus_Kaikki_min <-
  filter(Viljavuus_Kaikki,
         Viljavuus_Kaikki$`Maalaji multavuuden perusteella` == "Mineraali")
Viljavuus_Kaikki_min <-
  spread(Viljavuus_Kaikki_min, Tuotantosuunta, `ala ha`) #matriisiksi

Viljavuus_Kaikki_min[7:length(Viljavuus_Kaikki_min)] <-
  replace(Viljavuus_Kaikki_min[7:length(Viljavuus_Kaikki_min)], is.na(Viljavuus_Kaikki_min[7:length(Viljavuus_Kaikki_min)]), 0) #jos nolla, on nyt NA. Vaihdetaan takaisin nolliksi

Viljavuus_Kaikki_elop <-
  filter(
    Viljavuus_Kaikki,
    Viljavuus_Kaikki$`Maalaji multavuuden perusteella` == "Eloperäinen"
  )
Viljavuus_Kaikki_elop <-
  spread(Viljavuus_Kaikki_elop, Tuotantosuunta, `ala ha`) #matriisiksi

Viljavuus_Kaikki_elop[7:length(Viljavuus_Kaikki_elop)] <-
  replace(Viljavuus_Kaikki_elop[7:length(Viljavuus_Kaikki_elop)], is.na(Viljavuus_Kaikki_elop[7:length(Viljavuus_Kaikki_elop)]), 0) #jos nolla, on nyt NA. Vaihdetaan takaisin nolliksi


Cropland_korotettu_mineraalimaa <- filter(Viljavuus_Kaikki_min,
                                          Viljavuus_Kaikki_min$`Cropland/grassland` == "Cropland")


Grassland_korotettu_mineraalimaa <- filter(Viljavuus_Kaikki_min,
                                           Viljavuus_Kaikki_min$`Cropland/grassland` == "Grassland")

Cropland_korotettu_elop <- filter(Viljavuus_Kaikki_elop,
                                  Viljavuus_Kaikki_elop$`Cropland/grassland` == "Cropland")

Grassland_korotettu_elop <- filter(Viljavuus_Kaikki_elop,
                                   Viljavuus_Kaikki_elop$`Cropland/grassland` == "Grassland")

#Multavuusmuuttujia ei enää tässä vaiheessa tarvita koska datat on jo jaetstu eloperäisiin ja mineraaleihin.
#Jos ne jättää, pitää jatkossa muistaa että tuotantosuunnat alkavat indeksistä 7 eikä 6.

Cropland_korotettu_elop$`Maalaji multavuuden perusteella` <- NULL
Grassland_korotettu_elop$`Maalaji multavuuden perusteella` <- NULL
Cropland_korotettu_mineraalimaa$`Maalaji multavuuden perusteella` <-
  NULL
Grassland_korotettu_mineraalimaa$`Maalaji multavuuden perusteella` <-
  NULL

#NaN arvot nollille

Cropland_korotettu_elop[6:length(Cropland_korotettu_elop)] <-
  replace(Cropland_korotettu_elop[6:length(Cropland_korotettu_elop)], is.na(Cropland_korotettu_elop[6:length(Cropland_korotettu_elop)]), 0) #jos nolla, on nyt NA. Vaihdetaan takaisin nolliksi


Grassland_korotettu_elop[6:length(Grassland_korotettu_elop)] <-
  replace(Grassland_korotettu_elop[6:length(Grassland_korotettu_elop)],
          is.na(Grassland_korotettu_elop[6:length(Grassland_korotettu_elop)]),
          0) #jos nolla, on nyt NA. Vaihdetaan takaisin nolliksi

Cropland_korotettu_mineraalimaa[6:length(Cropland_korotettu_mineraalimaa)] <-
  replace(Cropland_korotettu_mineraalimaa[6:length(Cropland_korotettu_mineraalimaa)],
          is.na(Cropland_korotettu_mineraalimaa[6:length(Cropland_korotettu_mineraalimaa)]),
          0) #jos nolla, on nyt NA. Vaihdetaan takaisin nolliksi

Grassland_korotettu_mineraalimaa[6:length(Grassland_korotettu_mineraalimaa)] <-
  replace(Grassland_korotettu_mineraalimaa[6:length(Grassland_korotettu_mineraalimaa)],
          is.na(Grassland_korotettu_mineraalimaa[6:length(Grassland_korotettu_mineraalimaa)]),
          0) #jos nolla, on nyt NA. Vaihdetaan takaisin nolliksi



rm(Viljavuus_Kaikki,
   Viljavuus_Kaikki_elop,
   Viljavuus_Kaikki_min)


#Pinta-alojen tallennus. Muista yllä syntyvistä frameista syntyy päästötaulut.

#Tallennetaan tasokorjausskriptistä syntyvät pinta-alat. Näitä käytetään t päästöä/hehtaari kerrointen laskentaan.



Grassland_mineraalimaa_alat <- Grassland_korotettu_mineraalimaa
Grassland_elop_alat <- Grassland_korotettu_elop
Cropland_mineraalimaa_alat <- Cropland_korotettu_mineraalimaa
Cropland_elop_alat <- Cropland_korotettu_elop

#Tuotantosuuntanimien korjaus
Nimet <- c(
  "Kasvikoodi",
  "Kasvinimi",
  "Cropland/grassland",
  "Yksi/monivuotinen",
  "Tuoteryhmä",
  "Energiakasvit",
  "Hedelmät",
  "Hevostilat",
  "Hunajatuotanto",
  "Lammas_ja_vuohitilat",
  "Maitotilat",
  "Mallasohra",
  "Marjat",
  "Maustekasvit",
  "Munatilat",
  "Muut_nautakarjatilat",
  "Nurmet_laitumet_hakamaat",
  "Palkokasvit_pl_tarhaherne",
  "Peruna",
  "Rypsi_rapsi",
  "Siipikarjatilat",
  "Sikatilat",
  "Sokerijuurikas",
  "Tarhaherne",
  "Tattari_kinoa",
  "Turkistilat",
  "Vihannekset_juurekset",
  "Viljat_pl_ohra",
  "Yrtit",
  "Öljyhamppu",
  "Öljypellava"
)


colnames(Grassland_mineraalimaa_alat) <- Nimet

colnames(Cropland_mineraalimaa_alat) <- Nimet

colnames(Cropland_elop_alat) <- Nimet

colnames(Grassland_elop_alat) <- Nimet



print("Tasokorjataan raiviot")

source(here("Skriptit/Uudet skriptit", "Viljavuus_tasokorjaus_ohitus_raiviot.R")) #Dimensiot muutettu huomioimaan turkistilat 27.9.23 HV



sum(sum(colSums(Grassland_mineraalimaa_alat[6:length(Grassland_mineraalimaa_alat)])),
sum(colSums(Cropland_mineraalimaa_alat[6:length(Cropland_mineraalimaa_alat)])),
sum(colSums(Grassland_elop_alat[6:length(Grassland_elop_alat)])),
sum(colSums(Cropland_elop_alat[6:length(Cropland_elop_alat)])),
sum(colSums(Grassland_mineraalimaa_alat_raivio[6:length(Grassland_mineraalimaa_alat_raivio)])),
sum(colSums(Cropland_mineraalimaa_alat_raivio[6:length(Cropland_mineraalimaa_alat_raivio)])),
sum(colSums(Grassland_elop_alat_raivio[6:length(Grassland_elop_alat_raivio)])),
sum(colSums(Cropland_elop_alat_raivio[6:length(Cropland_elop_alat_raivio)])))



print("Viljavuuspinta-alojen aggregointi suoritettu")

print("Lasketaan satotonnit eloperäiseltä ja mineraalimailta niille kasveille, joille on hehtaarisadot") #Muutetaan raivauksen laskutapa vastaamaan nykyistä: raivatut lohkot mukana vain omassa framessaan, niiltäkin tarvitaan sadot. 

source(here("Skriptit/Uudet skriptit", "Satotonnien_laskenta_luokat_yhteen_viljav.R")) #Dimensiot muutettu huomioimaan turkistilat 27.9.23 HV

print("Satotonnit laskettu")

print("Lasketaan peltoalan euromääräinen arvo")

source(here("Skriptit/Uudet skriptit","Peltoalan_euroarvo_luokat_yhteen_viljav.R")) #Raivauksen käsittelytapa muutettu: lasketaan myös raivaus-framesta, joka yhdistetään muihin lohkoihin. 

print("Euromääräiset arvot peltoalalle laskettu")


print("Aja seuraavaksi päästölaskenta")

