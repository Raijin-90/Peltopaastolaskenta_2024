# DATA SISÄÄN ####
library(readxl)
library(dplyr)
library(tidyr)
Viljavuus_Kaikki <-
  read_excel(
    "D:/Paastolaskenta_peltodata/Viljavuusdata_aggregointi_multavuudesta.xlsx",
    sheet = "KaikkiPellot"
  ) #LAdataan esikäsittelyskriptissä luotu aggregaatti sisään


library(readxl)
Tasokorjaamaton_kokonaisala_gtk <- read_excel("Output/AreaAggregates/Tasokorjaamaton_kokonaisala_gtk.xlsx")





library(readxl)
library(readxl)
#Ota kategorioihin mukaan vain kasvikoodi, älä nimeä. Numerosarja riittää yhdistämisiin.
Kasvikategoriat_avain <- read_excel("Kasvikategoriat_avain.xlsx",
                                    col_types = c("text", "text", "text",
                                                  "numeric", "skip"))

Viljavuus_Kaikki <-
  merge(Viljavuus_Kaikki, Kasvikategoriat_avain, by = "Kasvikoodi")
rm(Kasvikategoriat_avain)

#MINERAALIMAAN EROTTAMINEN AINEISTOSTA ####

Viljavuus_Kaikki_min <-
  filter(Viljavuus_Kaikki,
         Viljavuus_Kaikki$`Maalaji multavuuden perusteella` == "Mineraali")

Perunakoodit <- c(3110,
                  3120,
                  3160,
                  3130,
                  3190
                  )
library(dplyr)
Perunat_mineraalimaa <-
  filter(Viljavuus_Kaikki_min,
         Viljavuus_Kaikki_min$Kasvikoodi %in% Perunakoodit)


Perunat_mineraalimaa <-
  spread(Perunat_mineraalimaa, Tuotantosuunta, `ala ha`) #matriisiksi

Perunat_mineraalimaa[7:length(Perunat_mineraalimaa)] <-
  replace(Perunat_mineraalimaa[7:length(Perunat_mineraalimaa)], is.na(Perunat_mineraalimaa[7:length(Perunat_mineraalimaa)]), 0) #jos nolla, on nyt NA. Vaihdetaan takaisin nolliksi



#Envimatissa öljyhamppu ja pellava summattu. Yhdistetään ne. 

Perunat_mineraalimaa$Oljyhamppu_ja_oljypellava <-
  Perunat_mineraalimaa$Öljyhamppu + Perunat_mineraalimaa$Öljypellava




#Jakaumaosuuksiksi. Miten kunkin tuotantosuunnan totaali pottumäärä jakautuu eri alatuotteille?

Perunat_mineraalimaa[7:length(Perunat_mineraalimaa)]<-apply(Perunat_mineraalimaa[7:length(Perunat_mineraalimaa)], 2, function(x) {
  x / sum(x)
})





#ELOPERÄISEN MAAN EROTTAMINEN AINEISTOSTA ####

Viljavuus_Kaikki_elop <-
  filter(
    Viljavuus_Kaikki,
    Viljavuus_Kaikki$`Maalaji multavuuden perusteella` == "Eloperäinen"
  )

Perunat_turvemaa <-
  filter(Viljavuus_Kaikki_elop,
         Viljavuus_Kaikki_elop$Kasvikoodi %in% Perunakoodit)

Perunat_turvemaa <-
  spread(Perunat_turvemaa, Tuotantosuunta, `ala ha`) #matriisiksi

Perunat_turvemaa[7:length(Perunat_turvemaa)] <-
  replace(Perunat_turvemaa[7:length(Perunat_turvemaa)], is.na(Perunat_turvemaa[7:length(Perunat_turvemaa)]), 0) #jos nolla, on nyt NA. Vaihdetaan takaisin nolliksi

#Vain osa tuotantosuuntia kasvattaa perunaa turpeella. Putoavat aineistosta. 





#Jakaumaosuuksiksi. Miten kunkin tuotantosuunnan totaali pottumäärä jakautuu eri alatuotteille?

Perunat_turvemaa[7:length(Perunat_turvemaa)]<-apply(Perunat_turvemaa[7:length(Perunat_turvemaa)], 2, function(x) {
  x / sum(x)
})