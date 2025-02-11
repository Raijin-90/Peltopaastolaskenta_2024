

library(varhandle);library(tidyverse);library(openxlsx)


#Pinta-ala-aggregaatti gtk-aineistosta, tarkkuustasona ETTL, ETOL, biodiversiteettiluokka. 


source(here("Skriptit/Uudet skriptit/GTK_datan_tasokorjaus_ohitus.R"))


#Viljelyalan tarkistus

rm.all.but(c("Cropland_korotettu_elop",
             "Cropland_korotettu_elop_raivio",
             "Cropland_korotettu_mineraalimaa",
             "Cropland_korotettu_mineraalimaa_raivio",
             "Grassland_korotettu_elop",
             "Grassland_korotettu_elop_raivio",
             "Grassland_korotettu_mineraalimaa",
             "Grassland_korotettu_mineraalimaa_raivio"))
             
a<-sum(sum(colSums(Cropland_korotettu_elop[6:length(Cropland_korotettu_elop)])),
sum(colSums(Cropland_korotettu_elop_raivio[6:length(Cropland_korotettu_elop_raivio)])),
sum(colSums(Grassland_korotettu_elop[6:length(Grassland_korotettu_elop)])),
sum(colSums(Grassland_korotettu_elop_raivio[6:length(Grassland_korotettu_elop_raivio)])))


b<-sum(sum(colSums(Cropland_korotettu_mineraalimaa[6:length(Cropland_korotettu_mineraalimaa)])),
sum(colSums(Cropland_korotettu_mineraalimaa_raivio[6:length(Cropland_korotettu_mineraalimaa_raivio)])),
sum(colSums(Grassland_korotettu_mineraalimaa[6:length(Grassland_korotettu_mineraalimaa)])),
sum(colSums(Grassland_korotettu_mineraalimaa_raivio[6:length(Grassland_korotettu_mineraalimaa_raivio)])))

#Pinta-alatarkistus kun tiedät, mikä sen pitää olla. Jos ei ole, niin jossain vaiheessa dataa putoaa (epätarkka joini) 
if(round(sum(a,b),0) != 2329482) {
 stop("VÄÄRÄ PINTA-ALA, TARKISTA AGGREGOINTI")}


#Liitetään biodiversiteetin kerroin

library(readxl)
Biodiversiteettiluokat_kasveille <- read_excel("Data/Biodiversiteettiluokat_kasveille.xlsx", 
                                               col_types = c("numeric", "text", "skip", 
                                                             "text"))
Biodiversiteettiluokat_kasveille$Kasvi<-NULL


Cropland_korotettu_elop<-inner_join(Cropland_korotettu_elop, Biodiversiteettiluokat_kasveille, by="Kasvikoodi")
Cropland_korotettu_elop_raivio<-inner_join(Cropland_korotettu_elop_raivio, Biodiversiteettiluokat_kasveille, by="Kasvikoodi")

Cropland_korotettu_mineraalimaa<-inner_join(Cropland_korotettu_mineraalimaa, Biodiversiteettiluokat_kasveille, by="Kasvikoodi")
Cropland_korotettu_mineraalimaa_raivio<-inner_join(Cropland_korotettu_mineraalimaa_raivio, Biodiversiteettiluokat_kasveille, by="Kasvikoodi")

Grassland_korotettu_mineraalimaa<-inner_join(Grassland_korotettu_mineraalimaa, Biodiversiteettiluokat_kasveille, by="Kasvikoodi")
Grassland_korotettu_mineraalimaa_raivio<-inner_join(Grassland_korotettu_mineraalimaa_raivio, Biodiversiteettiluokat_kasveille, by="Kasvikoodi")

Grassland_korotettu_elop<-inner_join(Grassland_korotettu_elop, Biodiversiteettiluokat_kasveille, by="Kasvikoodi")
Grassland_korotettu_elop_raivio<-inner_join(Grassland_korotettu_elop_raivio, Biodiversiteettiluokat_kasveille, by="Kasvikoodi")


Cropland_korotettu_elop<-pivot_longer(Cropland_korotettu_elop, cols = 6:31, names_to = "Tuotantosuunta", values_to = "Eloperaista")
Cropland_korotettu_elop_raivio<-pivot_longer(Cropland_korotettu_elop_raivio, cols = 6:31, names_to = "Tuotantosuunta", values_to = "Eloperaista")
Cropland_korotettu_mineraalimaa<-pivot_longer(Cropland_korotettu_mineraalimaa, cols = 6:31, names_to = "Tuotantosuunta", values_to = "Mineraalia")
Cropland_korotettu_mineraalimaa_raivio<-pivot_longer(Cropland_korotettu_mineraalimaa_raivio, cols = 6:31, names_to = "Tuotantosuunta", values_to = "Mineraalia")

Grassland_korotettu_elop<-pivot_longer(Grassland_korotettu_elop, cols = 6:31, names_to = "Tuotantosuunta", values_to = "Eloperaista" )
Grassland_korotettu_elop_raivio<-pivot_longer(Grassland_korotettu_elop_raivio, cols = 6:31, names_to = "Tuotantosuunta",values_to = "Eloperaista")
Grassland_korotettu_mineraalimaa<-pivot_longer(Grassland_korotettu_mineraalimaa, cols = 6:31, names_to = "Tuotantosuunta",values_to = "Mineraalia")
Grassland_korotettu_mineraalimaa_raivio<-pivot_longer(Grassland_korotettu_mineraalimaa_raivio, cols = 6:31, names_to = "Tuotantosuunta",values_to = "Mineraalia")


a<-sum(Cropland_korotettu_elop$Eloperaista)+sum(Cropland_korotettu_elop_raivio$Eloperaista)+sum(Cropland_korotettu_mineraalimaa$Mineraalia)+sum(Cropland_korotettu_mineraalimaa_raivio$Mineraalia)
b<-sum(Grassland_korotettu_elop$Eloperaista)+sum(Grassland_korotettu_elop_raivio$Eloperaista)+sum(Grassland_korotettu_mineraalimaa$Mineraalia)+sum(Grassland_korotettu_mineraalimaa_raivio$Mineraalia)

#Pinta-alatarkistus kun tiedät, mikä sen pitää olla. Jos ei ole, niin jossain vaiheessa dataa putoaa (epätarkka joini) 
if(round(sum(a,b),0) != 2329482) {
  stop("VÄÄRÄ PINTA-ALA, TARKISTA AGGREGOINTI")}

#Tuotantosuuntanimien oikomiset ####
Cropland_korotettu_elop<- Cropland_korotettu_elop %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                                                        Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                                                                        Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                                                                        Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                                                                        Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                                                                        Tuotantosuunta == "Tattari ja kinoa" ~ "Tattari_kinoa",
                                                                                        Tuotantosuunta == "Vihannekset ja juurekset" ~ "Vihannekset_juurekset",
                                                                                        Tuotantosuunta == "Viljat pl. ohra" ~ "Viljat_pl_ohra",
                                                                                        .default = Tuotantosuunta))

  Cropland_korotettu_elop_raivio<-Cropland_korotettu_elop_raivio %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                                                                     Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                                                                                     Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                                                                                     Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                                                                                     Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                                                                                     Tuotantosuunta == "Tattari ja kinoa" ~ "Tattari_kinoa",
                                                                                                     Tuotantosuunta == "Vihannekset ja juurekset" ~ "Vihannekset_juurekset",
                                                                                                     Tuotantosuunta == "Viljat pl. ohra" ~ "Viljat_pl_ohra",
                                                                                                     .default = Tuotantosuunta))

  Cropland_korotettu_mineraalimaa<-Cropland_korotettu_mineraalimaa %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                                                                       Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                                                                                       Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                                                                                       Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                                                                                       Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                                                                                       Tuotantosuunta == "Tattari ja kinoa" ~ "Tattari_kinoa",
                                                                                                       Tuotantosuunta == "Vihannekset ja juurekset" ~ "Vihannekset_juurekset",
                                                                                                       Tuotantosuunta == "Viljat pl. ohra" ~ "Viljat_pl_ohra",
                                                                                                       .default = Tuotantosuunta))

Cropland_korotettu_mineraalimaa_raivio<-Cropland_korotettu_mineraalimaa_raivio %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                                                                                     Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                                                                                                     Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                                                                                                     Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                                                                                                     Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                                                                                                     Tuotantosuunta == "Tattari ja kinoa" ~ "Tattari_kinoa",
                                                                                                                     Tuotantosuunta == "Vihannekset ja juurekset" ~ "Vihannekset_juurekset",
                                                                                                                     Tuotantosuunta == "Viljat pl. ohra" ~ "Viljat_pl_ohra",
                                                                                                                     .default = Tuotantosuunta))

Grassland_korotettu_elop<-Grassland_korotettu_elop %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                                                         Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                                                                         Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                                                                         Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                                                                         Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                                                                         Tuotantosuunta == "Tattari ja kinoa" ~ "Tattari_kinoa",
                                                                                         Tuotantosuunta == "Vihannekset ja juurekset" ~ "Vihannekset_juurekset",
                                                                                         Tuotantosuunta == "Viljat pl. ohra" ~ "Viljat_pl_ohra",
                                                                                         .default = Tuotantosuunta))

Grassland_korotettu_elop_raivio<-Grassland_korotettu_elop_raivio %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                                                                       Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                                                                                       Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                                                                                       Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                                                                                       Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                                                                                       Tuotantosuunta == "Tattari ja kinoa" ~ "Tattari_kinoa",
                                                                                                       Tuotantosuunta == "Vihannekset ja juurekset" ~ "Vihannekset_juurekset",
                                                                                                       Tuotantosuunta == "Viljat pl. ohra" ~ "Viljat_pl_ohra",
                                                                                                       .default = Tuotantosuunta))
Grassland_korotettu_mineraalimaa<- Grassland_korotettu_mineraalimaa %>%  mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                                                                           Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                                                                                           Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                                                                                           Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                                                                                           Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                                                                                           Tuotantosuunta == "Tattari ja kinoa" ~ "Tattari_kinoa",
                                                                                                           Tuotantosuunta == "Vihannekset ja juurekset" ~ "Vihannekset_juurekset",
                                                                                                           Tuotantosuunta == "Viljat pl. ohra" ~ "Viljat_pl_ohra",
                                                                                                           .default = Tuotantosuunta))

Grassland_korotettu_mineraalimaa_raivio <- Grassland_korotettu_mineraalimaa_raivio %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                                                                                         Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                                                                                                         Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                                                                                                         Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                                                                                                         Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                                                                                                         Tuotantosuunta == "Tattari ja kinoa" ~ "Tattari_kinoa",
                                                                                                                         Tuotantosuunta == "Vihannekset ja juurekset" ~ "Vihannekset_juurekset",
                                                                                                                         Tuotantosuunta == "Viljat pl. ohra" ~ "Viljat_pl_ohra",
                                                                                                                         .default = Tuotantosuunta))



#ETOL ja ETTL-koodit kiinni ####

ETOL<-read_excel("Data/Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx", 
                 sheet = "Tuotantosuunnat ryhmittäin")
colnames(ETOL)[1]<-"Tuotantosuunta"

ETTL<-read_excel("Data/Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx", 
                 sheet = "Kasvit_ETTL_koodeittain")  
colnames(ETTL)[3]<-"Kasvikoodi"
#ETOL
Cropland_korotettu_elop<-inner_join(Cropland_korotettu_elop, ETOL, by = "Tuotantosuunta") 
Cropland_korotettu_elop_raivio<-inner_join(Cropland_korotettu_elop_raivio, ETOL, by = "Tuotantosuunta")
Cropland_korotettu_mineraalimaa<-inner_join(Cropland_korotettu_mineraalimaa, ETOL, by = "Tuotantosuunta")
Cropland_korotettu_mineraalimaa_raivio<-inner_join(Cropland_korotettu_mineraalimaa_raivio, ETOL, by = "Tuotantosuunta")

Grassland_korotettu_elop<-inner_join(Grassland_korotettu_elop, ETOL, by = "Tuotantosuunta") 
Grassland_korotettu_elop_raivio<-inner_join(Grassland_korotettu_elop_raivio, ETOL, by = "Tuotantosuunta")
Grassland_korotettu_mineraalimaa<-inner_join(Grassland_korotettu_mineraalimaa, ETOL, by = "Tuotantosuunta")
Grassland_korotettu_mineraalimaa_raivio<-inner_join(Grassland_korotettu_mineraalimaa_raivio, ETOL, by = "Tuotantosuunta")

#ETTL
Cropland_korotettu_elop<-inner_join(Cropland_korotettu_elop, ETTL, by = "Kasvikoodi") 
Cropland_korotettu_elop_raivio<-inner_join(Cropland_korotettu_elop_raivio, ETTL, by = "Kasvikoodi")
Cropland_korotettu_mineraalimaa<-inner_join(Cropland_korotettu_mineraalimaa, ETTL, by = "Kasvikoodi")
Cropland_korotettu_mineraalimaa_raivio<-inner_join(Cropland_korotettu_mineraalimaa_raivio, ETTL, by = "Kasvikoodi")

Grassland_korotettu_elop<-inner_join(Grassland_korotettu_elop, ETTL, by = "Kasvikoodi") 
Grassland_korotettu_elop_raivio<-inner_join(Grassland_korotettu_elop_raivio, ETTL, by = "Kasvikoodi")
Grassland_korotettu_mineraalimaa<-inner_join(Grassland_korotettu_mineraalimaa, ETTL, by = "Kasvikoodi")
Grassland_korotettu_mineraalimaa_raivio<-inner_join(Grassland_korotettu_mineraalimaa_raivio, ETTL, by = "Kasvikoodi")

#Aggregoidaan ettl ja etol luokkien ja biodiv. kertoimen mukaisesti 

Cropland_korotettu_elop<- Cropland_korotettu_elop %>% group_by(
  ETOL_koodi,
  ETOL,
  ETTL,
  `ETTL Nimike`,
  `Yksi/monivuotinen`,
  `Cropland/grassland`,
  Soveltuva_biodiv_kerroin
) %>% summarise_at("Eloperaista", sum)

  Cropland_korotettu_elop_raivio<-Cropland_korotettu_elop_raivio %>% group_by(
  ETOL_koodi,
  ETOL,
  ETTL,
  `ETTL Nimike`,
  `Yksi/monivuotinen`,
  `Cropland/grassland`,
  Soveltuva_biodiv_kerroin
) %>% summarise_at("Eloperaista", sum)

Cropland_korotettu_mineraalimaa<-Cropland_korotettu_mineraalimaa %>% group_by(
  ETOL_koodi,
  ETOL,
  ETTL,
  `ETTL Nimike`,
  `Yksi/monivuotinen`,
  `Cropland/grassland`,
  Soveltuva_biodiv_kerroin
) %>% summarise_at("Mineraalia", sum)

Cropland_korotettu_mineraalimaa_raivio<-Cropland_korotettu_mineraalimaa_raivio %>% group_by(
  ETOL_koodi,
  ETOL,
  ETTL,
  `ETTL Nimike`,
  `Yksi/monivuotinen`,
  `Cropland/grassland`,
  Soveltuva_biodiv_kerroin
) %>% summarise_at("Mineraalia", sum)

Grassland_korotettu_elop<-Grassland_korotettu_elop%>% group_by(
  ETOL_koodi,
  ETOL,
  ETTL,
  `ETTL Nimike`,
  `Yksi/monivuotinen`,
  `Cropland/grassland`,
  Soveltuva_biodiv_kerroin
) %>% summarise_at("Eloperaista", sum)

Grassland_korotettu_elop_raivio<-Grassland_korotettu_elop_raivio%>% group_by(
  ETOL_koodi,
  ETOL,
  ETTL,
  `ETTL Nimike`,
  `Yksi/monivuotinen`,
  `Cropland/grassland`,
  Soveltuva_biodiv_kerroin
) %>% summarise_at("Eloperaista", sum)

Grassland_korotettu_mineraalimaa<-Grassland_korotettu_mineraalimaa %>% group_by(
  ETOL_koodi,
  ETOL,
  ETTL,
  `ETTL Nimike`,
  `Yksi/monivuotinen`,
  `Cropland/grassland`,
  Soveltuva_biodiv_kerroin
) %>% summarise_at("Mineraalia", sum)

Grassland_korotettu_mineraalimaa_raivio<-Grassland_korotettu_mineraalimaa_raivio %>% group_by(
  ETOL_koodi,
  ETOL,
  ETTL,
  `ETTL Nimike`,
  `Yksi/monivuotinen`,
  `Cropland/grassland`,
  Soveltuva_biodiv_kerroin
) %>% summarise_at("Mineraalia", sum)


#Tarkista, että ala stemmaa. 

a<-sum(Cropland_korotettu_elop$Eloperaista,
Cropland_korotettu_elop_raivio$Eloperaista,
Cropland_korotettu_mineraalimaa$Mineraalia,
Cropland_korotettu_mineraalimaa_raivio$Mineraalia)

b<-sum(Grassland_korotettu_elop$Eloperaista,
Grassland_korotettu_elop_raivio$Eloperaista,
Grassland_korotettu_mineraalimaa$Mineraalia,
Grassland_korotettu_mineraalimaa_raivio$Mineraalia)

#Pinta-alatarkistus kun tiedät, mikä sen pitää olla. Jos ei ole, niin jossain vaiheessa dataa putoaa (epätarkka joini) 
if(round(sum(a,b),0) != 2329482) {
  stop("VÄÄRÄ PINTA-ALA, TARKISTA AGGREGOINTI")}


#Kiinnitetään BD-kerroin

library(readxl)
BD_kertoimet <- read_excel(here("Data/Biodiversiteettikertoimet.xlsx"))

rm.all.but(c("Cropland_korotettu_elop",
             "Cropland_korotettu_elop_raivio",
             "Cropland_korotettu_mineraalimaa",
             "Cropland_korotettu_mineraalimaa_raivio",
             "Grassland_korotettu_elop",
             "Grassland_korotettu_elop_raivio",
             "Grassland_korotettu_mineraalimaa",
             "Grassland_korotettu_mineraalimaa_raivio",
             "BD_kertoimet"))

Cropland_korotettu_elop<-Cropland_korotettu_elop %>% inner_join(BD_kertoimet, by="Soveltuva_biodiv_kerroin")
Cropland_korotettu_elop_raivio<-Cropland_korotettu_elop_raivio %>% inner_join(BD_kertoimet, by="Soveltuva_biodiv_kerroin")
Cropland_korotettu_mineraalimaa<-Cropland_korotettu_mineraalimaa %>% inner_join(BD_kertoimet, by="Soveltuva_biodiv_kerroin")
Cropland_korotettu_mineraalimaa_raivio<-Cropland_korotettu_mineraalimaa_raivio %>% inner_join(BD_kertoimet, by="Soveltuva_biodiv_kerroin")

Grassland_korotettu_elop<-Grassland_korotettu_elop %>% inner_join(BD_kertoimet, by="Soveltuva_biodiv_kerroin")
Grassland_korotettu_elop_raivio<-Grassland_korotettu_elop_raivio %>% inner_join(BD_kertoimet, by="Soveltuva_biodiv_kerroin")
Grassland_korotettu_mineraalimaa<-Grassland_korotettu_mineraalimaa%>% inner_join(BD_kertoimet, by="Soveltuva_biodiv_kerroin")
Grassland_korotettu_mineraalimaa_raivio<-Grassland_korotettu_mineraalimaa_raivio %>% inner_join(BD_kertoimet, by="Soveltuva_biodiv_kerroin")


#Tarkista, että ala stemmaa. 

a<-sum(Cropland_korotettu_elop$Eloperaista,
       Cropland_korotettu_elop_raivio$Eloperaista,
       Cropland_korotettu_mineraalimaa$Mineraalia,
       Cropland_korotettu_mineraalimaa_raivio$Mineraalia)

b<-sum(Grassland_korotettu_elop$Eloperaista,
       Grassland_korotettu_elop_raivio$Eloperaista,
       Grassland_korotettu_mineraalimaa$Mineraalia,
       Grassland_korotettu_mineraalimaa_raivio$Mineraalia)

#Pinta-alatarkistus kun tiedät, mikä sen pitää olla. Jos ei ole, niin jossain vaiheessa dataa putoaa (epätarkka joini) 
if(round(sum(a,b),0) != 2329482) {
  stop("VÄÄRÄ PINTA-ALA, TARKISTA AGGREGOINTI")}




#Ositetaan perennat ja yksivuotiset omiin..

Cropland_korotettu_elop_yksiv<-Cropland_korotettu_elop %>% filter(`Yksi/monivuotinen` == "Yksivuotinen")
Cropland_korotettu_elop_moniv<-Cropland_korotettu_elop %>% filter(`Yksi/monivuotinen` == "Monivuotinen")

Cropland_korotettu_elop_raivio_yksiv<-Cropland_korotettu_elop_raivio %>% filter(`Yksi/monivuotinen` == "Yksivuotinen")
Cropland_korotettu_elop_raivio_moniv<-Cropland_korotettu_elop_raivio %>% filter(`Yksi/monivuotinen` == "Monivuotinen")

Cropland_korotettu_mineraalimaa_yksiv<-Cropland_korotettu_mineraalimaa %>% filter(`Yksi/monivuotinen` == "Yksivuotinen")
Cropland_korotettu_mineraalimaa_moniv<-Cropland_korotettu_mineraalimaa %>% filter(`Yksi/monivuotinen` == "Monivuotinen")

Cropland_korotettu_mineraalimaa_raivio_yksiv<-Cropland_korotettu_mineraalimaa_raivio %>% filter(`Yksi/monivuotinen` == "Yksivuotinen")
Cropland_korotettu_mineraalimaa_raivio_moniv<-Cropland_korotettu_mineraalimaa_raivio %>% filter(`Yksi/monivuotinen` == "Monivuotinen")

Grassland_korotettu_elop_yksiv<-Grassland_korotettu_elop %>% filter(`Yksi/monivuotinen` == "Yksivuotinen")
Grassland_korotettu_elop_moniv<-Grassland_korotettu_elop %>% filter(`Yksi/monivuotinen` == "Monivuotinen")

Grassland_korotettu_elop_raivio_yksiv<-Grassland_korotettu_elop_raivio %>% filter(`Yksi/monivuotinen` == "Yksivuotinen")
Grassland_korotettu_elop_raivio_moniv<-Grassland_korotettu_elop_raivio %>% filter(`Yksi/monivuotinen` == "Monivuotinen")

Grassland_korotettu_mineraalimaa_yksiv<-Grassland_korotettu_mineraalimaa %>% filter(`Yksi/monivuotinen` == "Yksivuotinen")
Grassland_korotettu_mineraalimaa_moniv<-Grassland_korotettu_mineraalimaa %>% filter(`Yksi/monivuotinen` == "Monivuotinen")

Grassland_korotettu_mineraalimaa_raivio_yksiv<-Grassland_korotettu_mineraalimaa_raivio %>% filter(`Yksi/monivuotinen` == "Yksivuotinen")
Grassland_korotettu_mineraalimaa_raivio_moniv<-Grassland_korotettu_mineraalimaa_raivio %>% filter(`Yksi/monivuotinen` == "Monivuotinen")



rm.all.but(c(
  "Cropland_korotettu_elop_yksiv",
  "Cropland_korotettu_elop_moniv",
  "Cropland_korotettu_elop_raivio_yksiv",
  "Cropland_korotettu_elop_raivio_moniv",
  "Cropland_korotettu_mineraalimaa_yksiv",
  "Cropland_korotettu_mineraalimaa_moniv",
  "Cropland_korotettu_mineraalimaa_raivio_yksiv",
  "Cropland_korotettu_mineraalimaa_raivio_moniv",
  "Grassland_korotettu_elop_yksiv",
  "Grassland_korotettu_elop_moniv",
  "Grassland_korotettu_elop_raivio_yksiv",
  "Grassland_korotettu_elop_raivio_moniv",
  "Grassland_korotettu_mineraalimaa_yksiv",
  "Grassland_korotettu_mineraalimaa_moniv",
  "Grassland_korotettu_mineraalimaa_raivio_yksiv",
  "Grassland_korotettu_mineraalimaa_raivio_moniv"))

#Työkirja ja sheetit
library(openxlsx)

Alat<-createWorkbook()
addWorksheet(Alat, "Cropland_min_yksiv")
addWorksheet(Alat, "Cropland_min_moniv")
addWorksheet(Alat, "Cropland_min_raivio_yksiv")
addWorksheet(Alat, "Cropland_min_raivio_moniv")

addWorksheet(Alat, "Cropland_elop_maa_yksiv")
addWorksheet(Alat, "Cropland_elop_maa_moniv")
addWorksheet(Alat, "Cropland_elop_maa_raivio_yksiv")
addWorksheet(Alat, "Cropland_elop_maa_raivio_moniv")

addWorksheet(Alat, "Grassland_min_yksiv")
addWorksheet(Alat, "Grassland_min_moniv")
addWorksheet(Alat, "Grassland_min_raivio_yksiv")
addWorksheet(Alat, "Grassland_min_raivio_moniv")

addWorksheet(Alat, "Grassland_elop_maa_yksiv")
addWorksheet(Alat, "Grassland_elop_maa_moniv")
addWorksheet(Alat, "Grassland_elop_maa_raivio_yksiv")
addWorksheet(Alat, "Grassland_elop_maa_raivio_moniv")

#Kirjoitetaan

writeData(Alat, "Cropland_min_yksiv", Cropland_korotettu_mineraalimaa_yksiv )
writeData(Alat, "Cropland_min_moniv", Cropland_korotettu_mineraalimaa_moniv)
writeData(Alat, "Cropland_min_raivio_yksiv", Cropland_korotettu_mineraalimaa_raivio_yksiv)
writeData(Alat, "Cropland_min_raivio_moniv",Cropland_korotettu_mineraalimaa_raivio_moniv)

writeData(Alat, "Cropland_elop_maa_yksiv",Cropland_korotettu_elop_yksiv)
writeData(Alat, "Cropland_elop_maa_moniv",Cropland_korotettu_elop_moniv)
writeData(Alat, "Cropland_elop_maa_raivio_yksiv",Cropland_korotettu_elop_raivio_yksiv)
writeData(Alat, "Cropland_elop_maa_raivio_moniv",Cropland_korotettu_elop_raivio_moniv)

writeData(Alat, "Grassland_min_yksiv",Grassland_korotettu_mineraalimaa_yksiv)
writeData(Alat, "Grassland_min_moniv",Grassland_korotettu_mineraalimaa_moniv)
writeData(Alat, "Grassland_min_raivio_yksiv",Grassland_korotettu_mineraalimaa_raivio_yksiv)
writeData(Alat, "Grassland_min_raivio_moniv",Grassland_korotettu_mineraalimaa_raivio_moniv)

writeData(Alat, "Grassland_elop_maa_yksiv", Grassland_korotettu_elop_yksiv)
writeData(Alat, "Grassland_elop_maa_moniv",Grassland_korotettu_elop_moniv)
writeData(Alat, "Grassland_elop_maa_raivio_yksiv",Grassland_korotettu_elop_raivio_yksiv)
writeData(Alat, "Grassland_elop_maa_raivio_moniv",Grassland_korotettu_elop_raivio_moniv)



saveWorkbook(Alat, file=here("Output/AreaAggregates/GTK_viljelyalat_BDLuokka_07022025_korjatut_koodit.xlsx"),overwrite = T)
