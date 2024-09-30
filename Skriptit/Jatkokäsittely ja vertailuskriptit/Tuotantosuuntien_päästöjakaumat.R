#Turkistilojen mukaanotto huomioitu 28.9.2023/HV
library(here)
library(tidyverse)

#CO2: yhdistetään viljat ja nurmet samaksi tauluksi
library(readxl)
Paastojakauma_CO2_viljav_min <- read_excel(here("Output/EmissionResults/Paastojakauma_CO2_viljav.xlsx"), 
                                       sheet = "CropMin")
colnames(Paastojakauma_CO2_viljav_min)<-c("Production line","CO2 emission, t, mineral soils")

library(readxl)
Paastojakauma_CO2_viljav_org <- read_excel(here("Output/EmissionResults/Paastojakauma_CO2_viljav.xlsx"),
                                       sheet = "CropOrg")

colnames(Paastojakauma_CO2_viljav_org)<-c("Production line","CO2 emission, t, organic soils")

CO2_cropland<-merge(Paastojakauma_CO2_viljav_min, Paastojakauma_CO2_viljav_org, by="Production line", all=T)

rm(Paastojakauma_CO2_viljav_org, Paastojakauma_CO2_viljav_min)


library(readxl)
Paastojakauma_CO2_viljav_min_grass <- read_excel(here("Output/EmissionResults/Paastojakauma_CO2_viljav.xlsx"), 
                                           sheet = "GrassMin")
colnames(Paastojakauma_CO2_viljav_min_grass)<-c("Production line","CO2 emission, t, mineral soils, grasslands")

library(readxl)
Paastojakauma_CO2_viljav_org_grass <- read_excel(here("Output/EmissionResults/Paastojakauma_CO2_viljav.xlsx"), 
                                           sheet = "GrassOrg")
colnames(Paastojakauma_CO2_viljav_org_grass)<-c("Production line","CO2 emission, t, organic soils, grasslands")


CO2_grassland<-merge(Paastojakauma_CO2_viljav_min_grass, Paastojakauma_CO2_viljav_org_grass, by="Production line", all=T)


rm(Paastojakauma_CO2_viljav_min_grass,Paastojakauma_CO2_viljav_org_grass)


CO2_merged<-merge(CO2_cropland, CO2_grassland, by="Production line", all=T)


#Käännökset #### 

#ETOL
CO2_merged<-CO2_merged %>% mutate(`Production line` = case_when(`Production line` == "Hedelmien viljely" ~ "Fruits",
                                                      `Production line` == "Hevostilat" ~ "Horse farms",
                                                      `Production line` == "Hunajatuotanto, muu eläimen hoito" ~ "Honey production",
                                                      `Production line` == "Kauran, ohran, rukiin ja vehnän viljely" ~ "Oat, barley, rye and wheat",
                                                      `Production line` == "Kuminan ja muiden maustekasvien viljely" ~ "Cumin & other spice crops",
                                                      `Production line` == "Lammas- ja vuohitilat" ~ "Sheep & goat farms",
                                                      `Production line` == "Maitotilat" ~ "Dairy farms",
                                                      `Production line` == "Mallasohran viljely" ~ "Malt barley",
                                                      `Production line` == "Marjojen viljely" ~ "Berries",
                                                      `Production line` == "Muiden vihannesten ja juuresten viljely" ~ "Other vegetables & root vegetables",
                                                      `Production line` =="Munatilat" ~ "Egg-laying poultry",
                                                      `Production line` =="Muut nautakarjatilat" ~ "Other cattle farms",
                                                      `Production line` == "Peltoherneen ja härkäpavun viljely" ~ "Field pea & faba bean",
                                                      `Production line` == "Perunan viljely" ~ "Potato",
                                                      `Production line` == "Rehukasvien viljely" ~ "Fodder crops & pastures",
                                                      `Production line` == "Rypsin ja rapsin viljely" ~ "Rapeseed & Turnip rape",
                                                      `Production line` == "Siipikarjatilat" ~ "Poultry",
                                                      `Production line` == "Sikatilat" ~ "Pig farms",
                                                      `Production line` == "Sokerijuurikkaan viljely" ~ "Sugar beet",
                                                      `Production line` == "Tarhaherneen viljely" ~ "Garden pea",
                                                      `Production line` == "Tattarin ja kinoan viljely" ~ "Buckwheat & Quinoa",
                                                      `Production line` == "Turkistarhaus" ~ "Fur farms",
                                                      `Production line` == "Yrttien viljely avomaalla" ~ "Open-ground herbs",
                                                      `Production line` == "Öljyhampun ja -pellavan viljely" ~ "Oilseed hemp and -flax"))
                                                      
                                                      
                                                      
#Alkuperäiset tuotantosuuntaryhmät


#CO2_merged %>% mutate(`Production line` = case_when(`Production line` == "Nurmet_laitumet_hakamaat" ~ "Grasslands, pastures & fodder crops",
                                                      #`Production line` == "Hedelmat_marjat" ~ "Fruits & berries",
                                                      #`Production line` == "Hevostilat" ~ "Horse farms",
                                                      #`Production line` == "Hunajatuotanto" ~ "Honey production",
                                                      #`Production line` == "Lammas_ja_vuohitilat" ~ "Sheep & goat farms",
                                                      #`Production line` == "Maitotilat" ~ "Dairy farms",
                                                      #`Production line` == "Mallasohra" ~ "Malt barley",
                                                      #`Production line` == "Yrtit_mausteet_vihannekset" ~ "Spices, herbs, vegetables",
                                                      #`Production line` == "Munatilat" ~ "Egg farms",
                                                      #`Production line` == "Muut_nautakarjatilat" ~ "Other cattle farms",
                                                      #`Production line` == "Palkokasvit_pl_tarhaherne" ~ "Legumes, excl. garden pea",
                                                      #`Production line` == "Peruna" ~ "Potato",
                                                      #`Production line` == "Viljat_rehuohra" ~ "Cereals",
                                                      #`Production line` == "Öljykasvit" ~ "Oilseed crops",
                                                      #`Production line` == "Siipikarjatilat" ~ "Broiler poultry",
                                                      #`Production line` == "Sikatilat" ~ "Pig farms",
                                                      #`Production line` == "Sokerijuurikas" ~ "Sugar beet",
                                                      #`Production line` == "Tarhaherne" ~ "Garden pea",
                                                      #`Production line` == "Turkistilat" ~ "Fur farms"))
                                                 
                                                    

                                                
Data<-CO2_merged

Data$`t CO2, total`<-rowSums(Data[2:length(Data)]) 

Data<-arrange(Data, desc(`t CO2, total`))

Data$`%`<-(Data$`t CO2, total`/sum(Data$`t CO2, total`))*100


CO2_jakauma<-select(Data,1,6,7)





rm(CO2_cropland,CO2_grassland,CO2_merged,Data)

#################################
#N2O

library(readxl)
N2O_cropland <- read_excel(here("Output/EmissionResults/N2O_päästöjakauma_viljav.xlsx"), 
                                       sheet = "CropOrg")
colnames(N2O_cropland)[2]<-"kg_N2O_cropland"


N2O_grassland<- read_excel(here("Output/EmissionResults/N2O_päästöjakauma_viljav.xlsx"), 
                           sheet = "GrassOrg")
colnames(N2O_grassland)[2]<-"kg_N2O_grassland"

Data<-merge(N2O_cropland, N2O_grassland, by="Tuotantosuuntaryhmä",all=T)

Data$kg_N2O_yht<-rowSums(Data[2:3])

colnames(Data)<-c("Production line", "kg N2O, cropland", "kg N2O, grassland", "kg N2O, total")

Data$`%`<-(Data$`kg N2O, total`/sum(Data$`kg N2O, total`))*100




#ETOL
N2O_jakauma<-Data %>% mutate(`Production line` = case_when(`Production line` == "Hedelmien viljely" ~ "Fruits",
                                                                  `Production line` == "Hevostilat" ~ "Horse farms",
                                                                  `Production line` == "Hunajatuotanto, muu eläimen hoito" ~ "Honey production",
                                                                  `Production line` == "Kauran, ohran, rukiin ja vehnän viljely" ~ "Oat, barley, rye and wheat",
                                                                  `Production line` == "Kuminan ja muiden maustekasvien viljely" ~ "Cumin & other spice crops",
                                                                  `Production line` == "Lammas- ja vuohitilat" ~ "Sheep & goat farms",
                                                                  `Production line` == "Maitotilat" ~ "Dairy farms",
                                                                  `Production line` == "Mallasohran viljely" ~ "Malt barley",
                                                                  `Production line` == "Marjojen viljely" ~ "Berries",
                                                                  `Production line` == "Muiden vihannesten ja juuresten viljely" ~ "Other vegetables & root vegetables",
                                                                  `Production line` =="Munatilat" ~ "Egg-laying poultry",
                                                                  `Production line` =="Muut nautakarjatilat" ~ "Other cattle farms",
                                                                  `Production line` == "Peltoherneen ja härkäpavun viljely" ~ "Field pea & faba bean",
                                                                  `Production line` == "Perunan viljely" ~ "Potato",
                                                                  `Production line` == "Rehukasvien viljely" ~ "Fodder crops & pastures",
                                                                  `Production line` == "Rypsin ja rapsin viljely" ~ "Rapeseed & Turnip rape",
                                                                  `Production line` == "Siipikarjatilat" ~ "Poultry",
                                                                  `Production line` == "Sikatilat" ~ "Pig farms",
                                                                  `Production line` == "Sokerijuurikkaan viljely" ~ "Sugar beet",
                                                                  `Production line` == "Tarhaherneen viljely" ~ "Garden pea",
                                                                  `Production line` == "Tattarin ja kinoan viljely" ~ "Buckwheat & Quinoa",
                                                                  `Production line` == "Turkistarhaus" ~ "Fur farms",
                                                                  `Production line` == "Yrttien viljely avomaalla" ~ "Open-ground herbs",
                                                                  `Production line` == "Öljyhampun ja -pellavan viljely" ~ "Oilseed hemp and -flax"))


#Alkuperäiset tuotantosuuntaryhmät


#N2O_jakauma %>% mutate(`Production line` = case_when(`Production line` == "Nurmet_laitumet_hakamaat" ~ "Grasslands, pastures & fodder crops",
#`Production line` == "Hedelmat_marjat" ~ "Fruits & berries",
#`Production line` == "Hevostilat" ~ "Horse farms",
#`Production line` == "Hunajatuotanto" ~ "Honey production",
#`Production line` == "Lammas_ja_vuohitilat" ~ "Sheep & goat farms",
#`Production line` == "Maitotilat" ~ "Dairy farms",
#`Production line` == "Mallasohra" ~ "Malt barley",
#`Production line` == "Yrtit_mausteet_vihannekset" ~ "Spices, herbs, vegetables",
#`Production line` == "Munatilat" ~ "Egg farms",
#`Production line` == "Muut_nautakarjatilat" ~ "Other cattle farms",
#`Production line` == "Palkokasvit_pl_tarhaherne" ~ "Legumes, excl. garden pea",
#`Production line` == "Peruna" ~ "Potato",
#`Production line` == "Viljat_rehuohra" ~ "Cereals",
#`Production line` == "Öljykasvit" ~ "Oilseed crops",
#`Production line` == "Siipikarjatilat" ~ "Broiler poultry",
#`Production line` == "Sikatilat" ~ "Pig farms",
#`Production line` == "Sokerijuurikas" ~ "Sugar beet",
#`Production line` == "Tarhaherne" ~ "Garden pea",
#`Production line` == "Turkistilat" ~ "Fur farms"))





# ########################################################################
# #Vesi. Grasslandia ei kastella. 
# #Kommentoidaan kokonaisuutenaan pois, ei käytetä tässä vaiheessa.
# 
# library(readxl)
# Vesi_min <- read_excel("Skriptit/Results/Vesi/Paastojakauma_Vesi_viljav.xlsx", 
#                                         sheet = "CropMin")
# 
# colnames(Vesi_min)[2]<-"Water Use, m3, min"
# 
# library(readxl)
# Vesi_org <- read_excel("Skriptit/Results/Vesi/Paastojakauma_Vesi_viljav.xlsx", 
#                                         sheet = "CropOrg")
# 
# colnames(Vesi_org)[2]<-"Water Use, m3, org"
# 
# Data<-merge(Vesi_min, Vesi_org, by="Tuotantosuuntaryhmä",all=T)
# colnames(Data)<-c("Production line", "Water use, m3, mineral soils", "Water use, m3, organic soils")
# 
# 
# Data$`Water use, total, m3`<-rowSums(Data[2:3])
# 
# Data$`%`<-(Data$`Water use, total, m3`/sum(Data$`Water use, total, m3`))*100
# 
# 
# colnames(Data)[1]<-"Production line"
# 
# sarakesumma <-
#   data_frame(
#     "Total",
#     sum(Data$`Water use, m3, mineral soils`),
#     sum(Data$`Water use, m3, organic soils`),
#     sum(Data$`Water use, total, m3`),
#     sum(Data$`%`))
# colnames(sarakesumma)<-c("Production line","Water use, m3, mineral soils","Water use, m3, organic soils","Water use, total, m3","%")
# 
# Data<-rbind(Data, sarakesumma)
# 
# Vesi_jakauma<-Data
# 
# Vesi_jakauma<-select(Vesi_jakauma,1,4,5)
# 
# rm(Data, Vesi_min, Vesi_org,sarakesumma)
# 
# 
# Vesi_jakauma$`Production line`[Vesi_jakauma$`Production line` ==
#                                   "Hedelmät ja marjat"] <-
#   "Fruit & berry farms"
# Vesi_jakauma$`Production line`[Vesi_jakauma$`Production line` ==
#                                   "Lammas- ja vuohitilat"] <-
#   "Sheep & goat farms"
# Vesi_jakauma$`Production line`[Vesi_jakauma$`Production line` ==
#                                   "Muut nautakarjatilat"] <-
#   "Other cattle farms"
# Vesi_jakauma$`Production line`[Vesi_jakauma$`Production line` ==
#                                   "Peruna"] <-
#   "Potato farms"
# Vesi_jakauma$`Production line`[Vesi_jakauma$`Production line` ==
#                                   "Sokerijuurikas"] <-
#   "Sugar beet farms"
# Vesi_jakauma$`Production line`[Vesi_jakauma$`Production line` ==
#                                   "Öljykasvit"] <-
#   "Oilseed crop farms"
# Vesi_jakauma$`Production line`[Vesi_jakauma$`Production line` ==
#                                   "Hevostilat"] <-
#   "Horse farms"
# Vesi_jakauma$`Production line`[Vesi_jakauma$`Production line` ==
#                                   "Maitotilat"] <-
#   "Dairy farms"
# Vesi_jakauma$`Production line`[Vesi_jakauma$`Production line` ==
#                                   "Nurmet_laitumet_hakamaat"] <-
#   "Grasslands & pastures"
# Vesi_jakauma$`Production line`[Vesi_jakauma$`Production line` ==
#                                   "Siipikarjatilat"] <-
#   "Broiler poultry farms"
# Vesi_jakauma$`Production line`[Vesi_jakauma$`Production line` ==
#                                   "Viljat_tattari_kvinoa"] <-
#   "Cereal, buckwheat & quinoa farms"
# Vesi_jakauma$`Production line`[Vesi_jakauma$`Production line` ==
#                                   "Hunajatuotanto"] <-
#   "Honey production farms"
# Vesi_jakauma$`Production line`[Vesi_jakauma$`Production line` ==
#                                   "Munatilat"] <-
#   "Egg-laying poultry farms"
# Vesi_jakauma$`Production line`[Vesi_jakauma$`Production line` ==
#                                   "Palkokasvit"] <-
#   "Legume farms"
# Vesi_jakauma$`Production line`[Vesi_jakauma$`Production line` ==
#                                   "Sikatilat"] <-
#   "Pig farms"
# Vesi_jakauma$`Production line`[Vesi_jakauma$`Production line` ==
#                                   "Yrtit_mausteet ja vihannekset"] <-
#   "Herb, spice & vegetable farms"
# Vesi_jakauma$`Production line`[Vesi_jakauma$`Production line` ==
#                                    "Turkistilat"] <-
#   "Fur farms"

##############################################
#Kaikkien emissioiden päästötotaalit yhteen ja samaan tauluun. Kiinteä summarivi

Jakaumataulu<-merge(CO2_jakauma, N2O_jakauma, by="Production line",all=T)

Jakaumataulu<-Jakaumataulu %>% bind_rows(summarise(.,
                    across(where(is.numeric), sum),
                    across(where(is.character), ~"Total")))

#Samaan yksikköön, molemmat tonneiksi

Jakaumataulu$`kg N2O, total`<-
  Jakaumataulu$`kg N2O, total`/1000

#Jakaumataulu<-merge(Jakaumataulu, Vesi_jakauma, by="Production line", all=T)



#LISÄYS 7.2: otetaan mukaan päästöintensiteetti. Tätä varten tarvitaan pinta-alat ####
#Päästöissä lasketaan yhteen raivattu ja raivaamaton, eloperäinen ja mineraali. Paitsi N2O:ssa, jossa vain eloperäinen 
#Sama aggregointi tehdään näissäkin


#Otetaan toisesta skriptistä inventaarista noudetut tasokorjausalat jotta voi tarkistaa yhdistämiset
library(here)
source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

source_lines(here("Skriptit/GTK_datan_tasokorjaus.R"), 69:94)


#Viljelty cropland, maalajien yhdistäminen
library(readxl)
a <- read_excel("Output/AreaAggregates/Tasokorjattu_viljelyaladata_gtk.xlsx", sheet="Cropland_mineral")
b<-read_excel("Output/AreaAggregates/Tasokorjattu_viljelyaladata_gtk.xlsx", sheet="Cropland_elop")

a<-a %>% pivot_longer(cols = 6:length(a), names_to = "Tuotantosuunta", values_to = "Hehtaaria_min") %>% select(1,2,6,7) 
b<-b %>% pivot_longer(cols = 6:length(b), names_to = "Tuotantosuunta", values_to = "Hehtaaria_org") %>% select(1,2,6,7) 

Cropland<-merge(a,b, by=c("Kasvikoodi","Kasvinimi","Tuotantosuunta"), all=T)


sum(a$Hehtaaria_min)+
sum(b$Hehtaaria_org)

Cropland$Hehtaaria_yht<-Cropland$Hehtaaria_min+Cropland$Hehtaaria_org

sum(Cropland$Hehtaaria_yht)
Inventaarinala_elop_cropland+Inventaarinala_mineral_cropland

#Ala vastaa sitä, mikä tasokorjauksessa määritellään inventaarin perusteella ko. kategorian alaksi

rm(a,b)

#Raivatut croplandit, yhdistetään maalajit

a <- read_excel("Output/AreaAggregates/Tasokorjattu_viljelyaladata_gtk.xlsx", sheet="Cropland_mineral_raiv")
b<-read_excel("Output/AreaAggregates/Tasokorjattu_viljelyaladata_gtk.xlsx", sheet="Cropland_elop_raiv")


a<-a %>% pivot_longer(cols = 6:length(a), names_to = "Tuotantosuunta", values_to = "Hehtaaria_min") %>% select(1,2,6,7) 
b<-b %>% pivot_longer(cols = 6:length(b), names_to = "Tuotantosuunta", values_to = "Hehtaaria_org") %>% select(1,2,6,7) 

sum(a$Hehtaaria_min)
sum(b$Hehtaaria_org)

Cropland_raiv<-merge(a,b, by=c("Kasvikoodi","Kasvinimi","Tuotantosuunta"), all=T)


rm(a,b)

Cropland_raiv$Hehtaaria_yht<-Cropland_raiv$Hehtaaria_min+Cropland_raiv$Hehtaaria_org

sum(Cropland_raiv$Hehtaaria_yht) 
Inventaarinala_elop_cropland_raivio+Inventaarinala_mineral_cropland_raivio

#Yhdistetään raivattu ja raivaamaton ala croplandista

Cropland<-select(Cropland,1:3,6)

Cropland_raiv<-select(Cropland_raiv,1:3,6)

Cropland_yhdistetty_ala<- merge(Cropland, Cropland_raiv, by=c("Kasvikoodi","Kasvinimi","Tuotantosuunta"), all=T)
Cropland_yhdistetty_ala$Hehtaaria_yht.x[is.na(Cropland_yhdistetty_ala$Hehtaaria_yht.x)]<-0
Cropland_yhdistetty_ala$Hehtaaria_yht.y[is.na(Cropland_yhdistetty_ala$Hehtaaria_yht.y)]<-0

sum(Cropland_yhdistetty_ala$Hehtaaria_yht.x)
sum(Cropland$Hehtaaria_yht)

sum(Cropland_yhdistetty_ala$Hehtaaria_yht.y)
sum(Cropland_raiv$Hehtaaria_yht)

Cropland_yhdistetty_ala$Hehtaaria_yhteensa <-
    Cropland_yhdistetty_ala$Hehtaaria_yht.x + Cropland_yhdistetty_ala$Hehtaaria_yht.y

rm(Cropland, Cropland_raiv)

sum(Cropland_yhdistetty_ala$Hehtaaria_yhteensa) 
Inventaarinala_elop_cropland+Inventaarinala_elop_cropland_raivio+Inventaarinala_mineral_cropland+Inventaarinala_mineral_cropland_raivio

# Sama Yhdistäminen grasslandille


#Viljelty grassland, maalajien yhdistäminen
library(readxl)
a <- read_excel("Output/AreaAggregates/Tasokorjattu_viljelyaladata_gtk.xlsx", sheet="Grassland_mineral")
b<-read_excel("Output/AreaAggregates/Tasokorjattu_viljelyaladata_gtk.xlsx", sheet="Grassland_elop")

a<-a %>% pivot_longer(cols = 6:length(a), names_to = "Tuotantosuunta", values_to = "Hehtaaria_min") %>% select(1,2,6,7) 
b<-b %>% pivot_longer(cols = 6:length(b), names_to = "Tuotantosuunta", values_to = "Hehtaaria_org") %>% select(1,2,6,7) 

Grassland<-merge(a,b, by=c("Kasvikoodi","Kasvinimi","Tuotantosuunta"), all=T)


sum(a$Hehtaaria_min)+
  sum(b$Hehtaaria_org)

Grassland$Hehtaaria_yht<-Grassland$Hehtaaria_min+Grassland$Hehtaaria_org

sum(Grassland$Hehtaaria_yht)
Inventaarinala_elop_grassland+Inventaarinala_mineral_grassland

#Raivatut grasslandit, yhdistetään maalajit

a <- read_excel("Output/AreaAggregates/Tasokorjattu_viljelyaladata_gtk.xlsx", sheet="Grassland_mineral_raiv")
b<-read_excel("Output/AreaAggregates/Tasokorjattu_viljelyaladata_gtk.xlsx", sheet="Grassland_elop_raiv")


a<-a %>% pivot_longer(cols = 6:length(a), names_to = "Tuotantosuunta", values_to = "Hehtaaria_min") %>% select(1,2,6,7) 
b<-b %>% pivot_longer(cols = 6:length(b), names_to = "Tuotantosuunta", values_to = "Hehtaaria_org") %>% select(1,2,6,7) 

sum(a$Hehtaaria_min)
sum(b$Hehtaaria_org)

Grassland_raiv<-merge(a,b, by=c("Kasvikoodi","Kasvinimi","Tuotantosuunta"), all=T)


rm(a,b)

Grassland_raiv$Hehtaaria_yht<-Grassland_raiv$Hehtaaria_min+Grassland_raiv$Hehtaaria_org

sum(Grassland_raiv$Hehtaaria_yht) 
Inventaarinala_elop_grassland_raivio+Inventaarinala_mineral_grassland_raivio

#Yhdistetään raivattu ja raivaamaton ala grasslandista

Grassland<-select(Grassland,1:3,6)

Grassland_raiv<-select(Grassland_raiv,1:3,6)

Grassland_yhdistetty_ala<- merge(Grassland, Grassland_raiv, by=c("Kasvikoodi","Kasvinimi","Tuotantosuunta"), all=T)
Grassland_yhdistetty_ala$Hehtaaria_yht.x[is.na(Grassland_yhdistetty_ala$Hehtaaria_yht.x)]<-0
Grassland_yhdistetty_ala$Hehtaaria_yht.y[is.na(Grassland_yhdistetty_ala$Hehtaaria_yht.y)]<-0

sum(Grassland_yhdistetty_ala$Hehtaaria_yht.x)
sum(Grassland$Hehtaaria_yht)

sum(Grassland_yhdistetty_ala$Hehtaaria_yht.y)
sum(Grassland_raiv$Hehtaaria_yht)

Grassland_yhdistetty_ala$Hehtaaria_yhteensa <-
  Grassland_yhdistetty_ala$Hehtaaria_yht.x + Grassland_yhdistetty_ala$Hehtaaria_yht.y

rm(Grassland, Grassland_raiv)

sum(Grassland_yhdistetty_ala$Hehtaaria_yhteensa) 
Inventaarinala_elop_grassland+Inventaarinala_elop_grassland_raivio+Inventaarinala_mineral_grassland+Inventaarinala_mineral_grassland_raivio















library(gt)
library(RColorBrewer)

Kuva<-gt(Jakaumataulu) %>%
  sub_missing() %>%
  tab_style(
    style=cell_text(weight="bold"),
    locations=cells_column_labels()) %>%
  fmt_number(decimals=0, sep_mark = " ") %>%
  tab_style(
    style= cell_borders(sides="b", 
                        color="silver", 
                        style="solid",
                        weight=px(4)),
    locations = cells_column_labels())  %>%
  fmt_number(decimals =1, sep_mark = ".", columns = starts_with("%")) %>%
  cols_label(`t CO2, total` =html("tn CO<sub>2</sub>"),
             `kg N2O, total` = html("tn N<sub>2</sub>O"),
             starts_with("%") ~ "%",) %>%
  cols_align(align="left", columns="Production line") %>%
  cols_align(align="center", columns= 2:length(Jakaumataulu)) %>%
  data_color(columns=c(3,5,7),
             rows = 1:(nrow(Jakaumataulu)-1),    
             method="numeric",
             palette="OrRd",
             reverse=F) %>%
  tab_style(
    style=cell_text(weight="bold"),
    locations=cells_body(rows = nrow(Jakaumataulu))) %>%
  cols_hide(columns = c(`kg N2O, cropland`, `kg N2O, grassland`)) %>%
  tab_style(
    style= cell_borders(sides="b", 
                        color="silver", 
                        style="solid",
                        weight=px(4)),
    locations = cells_body(rows=nrow(Jakaumataulu)-1)) %>%
  tab_header("Distribution of GHG emissions (tn) between production lines")

gtsave(Kuva, filename = here("Output/Grafiikka/Paastojakauma.docx"))

  

