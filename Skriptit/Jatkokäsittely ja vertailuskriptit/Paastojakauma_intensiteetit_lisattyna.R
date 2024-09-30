library(tidyverse);library(here);library(gt)
#LISÄYS 7.2: otetaan mukaan päästöintensiteetti. Tätä varten tarvitaan pinta-alat ####
#Päästöissä lasketaan yhteen raivattu ja raivaamaton, eloperäinen ja mineraali. Paitsi N2O:ssa, jossa vain eloperäinen 
#Sama aggregointi tehdään näissäkin


#Otetaan toisesta skriptistä inventaarista noudetut tasokorjausalat jotta voi tarkistaa yhdistämiset
library(here)
source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

source_lines(here("Skriptit/GTK_datan_tasokorjaus.R"), 69:99)

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


#Alojen viimeinen yhdistäminen: cropland ja grassland samaksi, totaali-viljelyalaksi. 

Cropland_yhdistetty_ala<-Cropland_yhdistetty_ala %>% select(-4:-5)
Grassland_yhdistetty_ala<-Grassland_yhdistetty_ala %>% select(-4:-5)

Kokonaisala<-rbind(Cropland_yhdistetty_ala, Grassland_yhdistetty_ala) #Täytyy tehdä rbindin kautta koska croplandin ja grasslandin kasvit erit. Ei päällekkäistä. 

#Alojen vastaavuus: onnistuiko yhdistely ilman hävikkiä 
Inventaarinala_elop_grassland + Inventaarinala_elop_grassland_raivio + Inventaarinala_mineral_grassland +
  Inventaarinala_mineral_grassland_raivio +Inventaarinala_elop_cropland+Inventaarinala_elop_cropland_raivio+Inventaarinala_mineral_cropland+Inventaarinala_mineral_cropland_raivio

sum(Kokonaisala$Hehtaaria_yhteensa)

rm(Cropland_yhdistetty_ala, Grassland_yhdistetty_ala)

#Emissiototaalien laskenta
#Näissä on valmiiksi yhdistetty raivattu ja raivaamaton ala. 
#N2O syntyy vain eloperäiseltä maalta

#Turkistilojen mukaanotto huomioitu 28.9.2023/HV
library(here)
library(tidyverse)

#CO2: yhdistetään viljat ja nurmet samaksi tauluksi
library(readxl)
Paastojakauma_CO2_min <- read_excel(here("Output/EmissionResults/Paastojakauma_CO2_gtk.xlsx"), 
                                           sheet = "CropMin")
colnames(Paastojakauma_CO2_min)<-c("Production line","CO2 emission, t, mineral soils")

library(readxl)
Paastojakauma_CO2_org <- read_excel(here("Output/EmissionResults/Paastojakauma_CO2_gtk.xlsx"),
                                           sheet = "CropOrg")

colnames(Paastojakauma_CO2_org)<-c("Production line","CO2 emission, t, organic soils")

CO2_cropland<-merge(Paastojakauma_CO2_min, Paastojakauma_CO2_org, by="Production line", all=T)

rm(Paastojakauma_CO2_org, Paastojakauma_CO2_min)


library(readxl)
Paastojakauma_CO2_min_grass <- read_excel(here("Output/EmissionResults/Paastojakauma_CO2_gtk.xlsx"), 
                                                 sheet = "GrassMin")
colnames(Paastojakauma_CO2_min_grass)<-c("Production line","CO2 emission, t, mineral soils, grasslands")

library(readxl)
Paastojakauma_CO2_org_grass <- read_excel(here("Output/EmissionResults/Paastojakauma_CO2_gtk.xlsx"), 
                                                 sheet = "GrassOrg")
colnames(Paastojakauma_CO2_org_grass)<-c("Production line","CO2 emission, t, organic soils, grasslands")


CO2_grassland<-merge(Paastojakauma_CO2_min_grass, Paastojakauma_CO2_org_grass, by="Production line", all=T)


rm(Paastojakauma_CO2_min_grass,Paastojakauma_CO2_org_grass)


CO2_merged<-merge(CO2_cropland, CO2_grassland, by="Production line", all=T)

rm(CO2_cropland, CO2_grassland)


Data<-CO2_merged

Data$`t CO2, total`<-rowSums(Data[2:length(Data)]) 

Data<-arrange(Data, desc(`t CO2, total`))

Data$`%`<-(Data$`t CO2, total`/sum(Data$`t CO2, total`))*100


CO2_jakauma<-select(Data,1,6,7)


rm(CO2_cropland,CO2_grassland,CO2_merged,Data)


rm(Data)

#################################
#N2O

library(readxl)
N2O_cropland <- read_excel(here("Output/EmissionResults/N2O_päästöjakauma_gtk.xlsx"), 
                           sheet = "CropOrg")
colnames(N2O_cropland)[2]<-"kg_N2O_cropland"


N2O_grassland<- read_excel(here("Output/EmissionResults/N2O_päästöjakauma_gtk.xlsx"), 
                           sheet = "GrassOrg")
colnames(N2O_grassland)[2]<-"kg_N2O_grassland"

N2O_jakauma<-merge(N2O_cropland, N2O_grassland, by="Tuotantosuuntaryhmä",all=T)

N2O_jakauma$kg_N2O_yht<-rowSums(N2O_jakauma[2:3])

colnames(N2O_jakauma)<-c("Production line", "kg N2O, cropland", "kg N2O, grassland", "kg N2O, total")

N2O_jakauma$`%`<-(N2O_jakauma$`kg N2O, total`/sum(N2O_jakauma$`kg N2O, total`))*100


rm(N2O_cropland, N2O_grassland)


#Kaikkien emissioiden päästötotaalit yhteen ja samaan tauluun. Kiinteä summarivi

Jakaumataulu<-merge(CO2_jakauma, N2O_jakauma, by="Production line",all=T)



#Samaan yksikköön, molemmat tonneiksi

Jakaumataulu$`kg N2O, total`<-
  Jakaumataulu$`kg N2O, total`/1000

colnames(Jakaumataulu)[6]<-"tn N2O, total"

Jakaumataulu$`%.x`<-NULL

Jakaumataulu$`%.y`<-NULL
Jakaumataulu$`kg N2O, cropland`<-NULL
Jakaumataulu$`kg N2O, grassland`<-NULL
colnames(Jakaumataulu)[1]<-c("Production line")


#Tänne liitetään viljelyala. Tätä ennen täytyy aggregoida viljelyala tuotantosuunnittain. 
#Päästöt on laskettu ottamatta kasvilajia huomioon, ts. tuotantosuunnan totaalipäästönä. Samanlainen aggregointi myös aloille. 
#Tasokorotettujen viljelyalojen tuotantosuuntanimet pitää muuttaa yksisanaiseen muotoon ensin...

Kokonaisala<-Kokonaisala %>% mutate(Tuotantosuunta= case_when(Tuotantosuunta== "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                  Tuotantosuunta== "Muut nautakarjatilat" ~  "Muut_nautakarjatilat",
                                  Tuotantosuunta== "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                  Tuotantosuunta== "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                  Tuotantosuunta== "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                  Tuotantosuunta== "Tattari ja kinoa" ~ "Tattari_kinoa",
                                  Tuotantosuunta== "Vihannekset ja juurekset"~ "Vihannekset_juurekset",
                                  Tuotantosuunta== "Viljat pl. ohra" ~ "Viljat_pl_ohra",
                                 .default = Tuotantosuunta
                           
                       ))

Nimet<-c(
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
  "Rehuohra",
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
  "Öljypellava")


library(usefun)

outersect(Kokonaisala$Tuotantosuunta, Nimet) #Jos tyhjä, kaikki tuotantosuuntanimet vastaavat toisiaan
 




library(readxl)
Tuotantosuuntaryhmat <-
  read_excel(
    here("Data","Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx"),
    sheet = "Tuotantosuunnat ryhmittäin",
    col_types = c(
      "text",
      "text"))
colnames(Tuotantosuuntaryhmat)<-c("Tuotantosuunta","Tuotantosuuntaryhmä")

outersect(Kokonaisala$Tuotantosuunta,  Tuotantosuuntaryhmat$Tuotantosuunta)

Kokonaisala<-merge(Kokonaisala, Tuotantosuuntaryhmat, by=c("Tuotantosuunta"))



Kokonaisala<-Kokonaisala %>% group_by(Tuotantosuuntaryhmä) %>% summarise(Hehtaaria_tuotantosuunnalla = sum(Hehtaaria_yhteensa))
colnames(Kokonaisala)[1]<-"Production line"



outersect(Jakaumataulu$`Production line`, Kokonaisala$`Production line`)
Jakaumataulu<-merge(Jakaumataulu, Kokonaisala, by=("Production line"), all=T)


#Kiinteä summarivi
Jakaumataulu<-Jakaumataulu %>% bind_rows(summarise(.,
                                                   across(where(is.numeric), sum),
                                                   across(where(is.character), ~"Total")))


#ETOL nimien englanninnos
Jakaumataulu<-Jakaumataulu %>% mutate(`Production line` = case_when(`Production line` == "Hedelmien viljely" ~ "Fruits",
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
                                                           `Production line` == "Öljyhampun ja -pellavan viljely" ~ "Oilseed hemp and -flax",
                                                           `Production line` == "Total" ~ "Total"))

#Muunnetaan N2o-tonnit CO2-ekvivalenteiksi kertomalla GWP-kertoimella 265 (AR5-kerroin)


Jakaumataulu$`tn N2O, total`<- Jakaumataulu$`tn N2O, total`* 265

#Summataan Emissiot. Lopputulos on tonneja CO2-ekvivalenttina. 

Jakaumataulu$`tn CO2e, total`<-Jakaumataulu$`t CO2, total`+Jakaumataulu$`tn N2O, total`


Jakaumataulu<-Jakaumataulu %>% mutate(tn_CO2e_ha = `tn CO2e, total`/Hehtaaria_tuotantosuunnalla)
Jakaumataulu$tn_CO2e_ha<-round(Jakaumataulu$tn_CO2e_ha,1)
Jakaumataulu<-slice(Jakaumataulu, -25)

Jakaumataulu<-Jakaumataulu %>% select(1,5,4,6)



#Ryhmittelyavaimet
Animal_farms<-c("Horse farms",
                "Honey production",
                "Sheep & goat farms",
                "Dairy farms",
                "Egg-laying poultry",
                "Other cattle farms",
                "Poultry",
                "Pig farms",
                "Fur farms")

Animal_farms<-sort(Animal_farms)

Crop_cultivation<-outersect(Jakaumataulu$`Production line`, Animal_farms)
Crop_cultivation <- Crop_cultivation[-16]

Crop_cultivation<-sort(Crop_cultivation)

library(gt)
library(RColorBrewer)
#aakkostus
Jakaumataulu<-arrange(Jakaumataulu, `Production line`)
Jakaumataulu$Ryhmittely<-NA
Jakaumataulu$Ryhmittely[Jakaumataulu$`Production line` %in% Crop_cultivation]<-"Crop cultivation"
Jakaumataulu$Ryhmittely[Jakaumataulu$`Production line` %in% Animal_farms]<-"Animal_farms" #Piilotettava muuttuja jolla targetoidaan värjäys



Kuva<-gt(Jakaumataulu) %>%
  sub_missing() %>%
  fmt_number(columns = 2, decimals = 0, sep_mark = " ") %>%
  fmt_number(columns = 3, decimals = 0, sep_mark = " ") %>%
  tab_row_group(label = "Crop cultivation", rows = `Production line` %in% Crop_cultivation) %>%
  tab_row_group(label = "Animal farms", rows = `Production line` %in% Animal_farms ) %>%
  row_group_order(groups = c("Crop cultivation", "Animal farms")) %>%
  summary_rows(groups = matches("Crop cultivation"), columns =2:3 , fns = list(
    Mean = ~mean(., na.rm=T),
    St.Dev = ~sd(., na.rm=T)),
    fmt = ~fmt_number(., decimals=0, sep_mark =" ")) %>%
  summary_rows(groups = matches("Animal farms"), columns =2:3 , fns = list(
  Mean = ~mean(., na.rm=T),
  St.Dev = ~sd(., na.rm=T)),
  fmt = ~fmt_number(., decimals=0, sep_mark = " ")) %>%
  summary_rows(groups = matches("Crop cultivation"), columns =4 , fns = list(
    Mean = ~mean(., na.rm=T),
    St.Dev = ~sd(., na.rm=T)),
    fmt = ~fmt_number(., decimals=1, sep_mark =" ")) %>%
  summary_rows(groups = matches("Animal farms"), columns =4 , fns = list(
    Mean = ~mean(., na.rm=T),
    St.Dev = ~sd(., na.rm=T)),
    fmt = ~fmt_number(., decimals=1, sep_mark = " ")) %>%
  tab_style(
    style=cell_text(weight="bold"),
    locations=cells_column_labels()) %>%
  tab_style(
    style=cell_text(weight="bold"),
    locations=cells_summary()) %>% 
  tab_style(
  style=cell_text(weight="bold"),
  locations=cells_stub_summary()) %>% 
  cols_label(`tn_CO2e_ha` =html("tn CO<sub>2</sub>-eq ha<sup>-1</sup>")) %>%
  cols_label(`tn CO2e, total` =html("tn CO<sub>2</sub>-eq<sup>-1</sup>")) %>%
  cols_label(Hehtaaria_tuotantosuunnalla = html("Hectares")) %>%
  cols_hide(Ryhmittely) %>%
  data_color(columns=c(4),
             rows = Ryhmittely == "Crop cultivation",  
             method="bin",
             bins = 3,
             palette="Oranges",
             reverse=F) %>%
  data_color(columns=c(4),
             rows = Ryhmittely == "Animal_farms",  
             method="bin",
             bins=3,
             palette="Oranges",
             reverse=F) 
 
gtsave(Kuva, filename = here("Output/Grafiikka/Paastojakauma_intensiteetit_gtk.docx")) 
  
 
Jakaumataulu_gtk<-Jakaumataulu


# JAKAUMATAULUKOINTI VILJAVUUSDATASTA    
# Sama prosessi, mutta viljavuusdataa käyttäen

library(tidyverse);library(here);library(gt)
#LISÄYS 7.2: otetaan mukaan päästöintensiteetti. Tätä varten tarvitaan pinta-alat ####
#Päästöissä lasketaan yhteen raivattu ja raivaamaton, eloperäinen ja mineraali. Paitsi N2O:ssa, jossa vain eloperäinen 
#Sama aggregointi tehdään näissäkin

#Inventaarin alat joihin näitä verrataan ovat samat kuin 1. vaiheessa. 

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}




source_lines(here("Skriptit/GTK_datan_tasokorjaus.R"), 69:99)



#Viljelty cropland, maalajien yhdistäminen
library(readxl)
a <- read_excel("Output/AreaAggregates/Tasokorjattu_viljelyaladata_viljav.xlsx", sheet="Cropland_mineral")
b<-read_excel("Output/AreaAggregates/Tasokorjattu_viljelyaladata_viljav.xlsx", sheet="Cropland_elop")

a<-a %>% pivot_longer(cols = 6:length(a), names_to = "Tuotantosuunta", values_to = "Hehtaaria_min") %>% select(1,2,6,7) 
b<-b %>% pivot_longer(cols = 6:length(b), names_to = "Tuotantosuunta", values_to = "Hehtaaria_org") %>% select(1,2,6,7) 

Cropland<-merge(a,b, by=c("Kasvikoodi","Kasvinimi","Tuotantosuunta"), all=T)

Cropland$Hehtaaria_min[is.na(Cropland$Hehtaaria_min)]<-0
Cropland$Hehtaaria_org[is.na(Cropland$Hehtaaria_org)]<-0

sum(a$Hehtaaria_min)+
  sum(b$Hehtaaria_org)

Cropland$Hehtaaria_yht<-Cropland$Hehtaaria_min+Cropland$Hehtaaria_org

sum(Cropland$Hehtaaria_yht)
Inventaarinala_elop_cropland+Inventaarinala_mineral_cropland

#Ala vastaa sitä, mikä tasokorjauksessa määritellään inventaarin perusteella ko. kategorian alaksi

rm(a,b)

#Raivatut croplandit, yhdistetään maalajit

a <- read_excel("Output/AreaAggregates/Tasokorjattu_viljelyaladata_viljav.xlsx", sheet="Cropland_mineral_raiv")
b<-read_excel("Output/AreaAggregates/Tasokorjattu_viljelyaladata_viljav.xlsx", sheet="Cropland_elop_raiv")


a<-a %>% pivot_longer(cols = 6:length(a), names_to = "Tuotantosuunta", values_to = "Hehtaaria_min") %>% select(1,2,6,7) 
b<-b %>% pivot_longer(cols = 6:length(b), names_to = "Tuotantosuunta", values_to = "Hehtaaria_org") %>% select(1,2,6,7) 

sum(a$Hehtaaria_min)
sum(b$Hehtaaria_org)

Cropland_raiv<-merge(a,b, by=c("Kasvikoodi","Kasvinimi","Tuotantosuunta"), all=T)

rm(a,b)


Cropland_raiv$Hehtaaria_min[is.na(Cropland_raiv$Hehtaaria_min)]<-0
Cropland_raiv$Hehtaaria_org[is.na(Cropland_raiv$Hehtaaria_org)]<-0

Cropland_raiv$Hehtaaria_yht<-Cropland_raiv$Hehtaaria_min+Cropland_raiv$Hehtaaria_org

Inventaarinala_elop_cropland_raivio+Inventaarinala_mineral_cropland_raivio
sum(Cropland_raiv$Hehtaaria_yht)

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
a <- read_excel("Output/AreaAggregates/Tasokorjattu_viljelyaladata_viljav.xlsx", sheet="Grassland_mineral")
b<-read_excel("Output/AreaAggregates/Tasokorjattu_viljelyaladata_viljav.xlsx", sheet="Grassland_elop")

a<-a %>% pivot_longer(cols = 6:length(a), names_to = "Tuotantosuunta", values_to = "Hehtaaria_min") %>% select(1,2,6,7) 
b<-b %>% pivot_longer(cols = 6:length(b), names_to = "Tuotantosuunta", values_to = "Hehtaaria_org") %>% select(1,2,6,7) 

Grassland<-merge(a,b, by=c("Kasvikoodi","Kasvinimi","Tuotantosuunta"), all=T)


sum(a$Hehtaaria_min)+
  sum(b$Hehtaaria_org)

Grassland$Hehtaaria_min[is.na(Grassland$Hehtaaria_min)]<-0
Grassland$Hehtaaria_org[is.na(Grassland$Hehtaaria_org)]<-0


Grassland$Hehtaaria_yht<-Grassland$Hehtaaria_min+Grassland$Hehtaaria_org

sum(Grassland$Hehtaaria_yht)
Inventaarinala_elop_grassland+Inventaarinala_mineral_grassland

#Raivatut grasslandit, yhdistetään maalajit

a <- read_excel("Output/AreaAggregates/Tasokorjattu_viljelyaladata_viljav.xlsx", sheet="Grassland_mineral_raiv")
b<-read_excel("Output/AreaAggregates/Tasokorjattu_viljelyaladata_viljav.xlsx", sheet="Grassland_elop_raiv")


a<-a %>% pivot_longer(cols = 6:length(a), names_to = "Tuotantosuunta", values_to = "Hehtaaria_min") %>% select(1,2,6,7) 
b<-b %>% pivot_longer(cols = 6:length(b), names_to = "Tuotantosuunta", values_to = "Hehtaaria_org") %>% select(1,2,6,7) 

sum(a$Hehtaaria_min)
sum(b$Hehtaaria_org)

Grassland_raiv<-merge(a,b, by=c("Kasvikoodi","Kasvinimi","Tuotantosuunta"), all=T)

Grassland_raiv$Hehtaaria_min[is.na(Grassland_raiv$Hehtaaria_min)]<-0
Grassland_raiv$Hehtaaria_org[is.na(Grassland_raiv$Hehtaaria_org)]<-0


Grassland_raiv$Hehtaaria_yht<-Grassland_raiv$Hehtaaria_min+Grassland_raiv$Hehtaaria_org

sum(Grassland_raiv$Hehtaaria_yht) 
Inventaarinala_elop_grassland_raivio+Inventaarinala_mineral_grassland_raivio




rm(a,b)




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


#Alojen viimeinen yhdistäminen: cropland ja grassland samaksi, totaali-viljelyalaksi. 

Cropland_yhdistetty_ala<-Cropland_yhdistetty_ala %>% select(-4:-5)
Grassland_yhdistetty_ala<-Grassland_yhdistetty_ala %>% select(-4:-5)

Kokonaisala<-rbind(Cropland_yhdistetty_ala, Grassland_yhdistetty_ala) #Täytyy tehdä rbindin kautta koska croplandin ja grasslandin kasvit erit. Ei päällekkäistä. 

#Alojen vastaavuus: onnistuiko yhdistely ilman hävikkiä 
Inventaarinala_elop_grassland + Inventaarinala_elop_grassland_raivio + Inventaarinala_mineral_grassland +
  Inventaarinala_mineral_grassland_raivio +Inventaarinala_elop_cropland+Inventaarinala_elop_cropland_raivio+Inventaarinala_mineral_cropland+Inventaarinala_mineral_cropland_raivio

sum(Kokonaisala$Hehtaaria_yhteensa)

rm(Cropland_yhdistetty_ala, Grassland_yhdistetty_ala)

#Emissiototaalien laskenta
#Näissä on valmiiksi yhdistetty raivattu ja raivaamaton ala. 
#N2O syntyy vain eloperäiseltä maalta

#Turkistilojen mukaanotto huomioitu 28.9.2023/HV
library(here)
library(tidyverse)

#CO2: yhdistetään viljat ja nurmet samaksi tauluksi
library(readxl)
Paastojakauma_CO2_min <- read_excel(here("Output/EmissionResults/Paastojakauma_CO2_viljav.xlsx"), 
                                    sheet = "CropMin")
colnames(Paastojakauma_CO2_min)<-c("Production line","CO2 emission, t, mineral soils")

library(readxl)
Paastojakauma_CO2_org <- read_excel(here("Output/EmissionResults/Paastojakauma_CO2_viljav.xlsx"),
                                    sheet = "CropOrg")

colnames(Paastojakauma_CO2_org)<-c("Production line","CO2 emission, t, organic soils")

CO2_cropland<-merge(Paastojakauma_CO2_min, Paastojakauma_CO2_org, by="Production line", all=T)

rm(Paastojakauma_CO2_org, Paastojakauma_CO2_min)


library(readxl)
Paastojakauma_CO2_min_grass <- read_excel(here("Output/EmissionResults/Paastojakauma_CO2_viljav.xlsx"), 
                                          sheet = "GrassMin")
colnames(Paastojakauma_CO2_min_grass)<-c("Production line","CO2 emission, t, mineral soils, grasslands")

library(readxl)
Paastojakauma_CO2_org_grass <- read_excel(here("Output/EmissionResults/Paastojakauma_CO2_viljav.xlsx"), 
                                          sheet = "GrassOrg")
colnames(Paastojakauma_CO2_org_grass)<-c("Production line","CO2 emission, t, organic soils, grasslands")


CO2_grassland<-merge(Paastojakauma_CO2_min_grass, Paastojakauma_CO2_org_grass, by="Production line", all=T)


rm(Paastojakauma_CO2_min_grass,Paastojakauma_CO2_org_grass)


CO2_merged<-merge(CO2_cropland, CO2_grassland, by="Production line", all=T)

rm(CO2_cropland, CO2_grassland)


Data<-CO2_merged

Data$`t CO2, total`<-rowSums(Data[2:length(Data)]) 

Data<-arrange(Data, desc(`t CO2, total`))

Data$`%`<-(Data$`t CO2, total`/sum(Data$`t CO2, total`))*100


CO2_jakauma<-select(Data,1,6,7)


rm(CO2_cropland,CO2_grassland,CO2_merged,Data)


rm(Data)

#################################
#N2O

library(readxl)
N2O_cropland <- read_excel(here("Output/EmissionResults/N2O_päästöjakauma_viljav.xlsx"), 
                           sheet = "CropOrg")
colnames(N2O_cropland)[2]<-"kg_N2O_cropland"


N2O_grassland<- read_excel(here("Output/EmissionResults/N2O_päästöjakauma_viljav.xlsx"), 
                           sheet = "GrassOrg")
colnames(N2O_grassland)[2]<-"kg_N2O_grassland"

N2O_jakauma<-merge(N2O_cropland, N2O_grassland, by="Tuotantosuuntaryhmä",all=T)

N2O_jakauma$kg_N2O_yht<-rowSums(N2O_jakauma[2:3])

colnames(N2O_jakauma)<-c("Production line", "kg N2O, cropland", "kg N2O, grassland", "kg N2O, total")

N2O_jakauma$`%`<-(N2O_jakauma$`kg N2O, total`/sum(N2O_jakauma$`kg N2O, total`))*100


rm(N2O_cropland, N2O_grassland)


#Kaikkien emissioiden päästötotaalit yhteen ja samaan tauluun. Kiinteä summarivi

Jakaumataulu<-merge(CO2_jakauma, N2O_jakauma, by="Production line",all=T)



#Samaan yksikköön, molemmat tonneiksi

Jakaumataulu$`kg N2O, total`<-
  Jakaumataulu$`kg N2O, total`/1000

colnames(Jakaumataulu)[6]<-"tn N2O, total"

Jakaumataulu$`%.x`<-NULL

Jakaumataulu$`%.y`<-NULL
Jakaumataulu$`kg N2O, cropland`<-NULL
Jakaumataulu$`kg N2O, grassland`<-NULL
colnames(Jakaumataulu)[1]<-c("Production line")


#Tänne liitetään viljelyala. Tätä ennen täytyy aggregoida viljelyala tuotantosuunnittain. 
#Päästöt on laskettu ottamatta kasvilajia huomioon, ts. tuotantosuunnan totaalipäästönä. Samanlainen aggregointi myös aloille. 
#Tasokorotettujen viljelyalojen tuotantosuuntanimet pitää muuttaa yksisanaiseen muotoon ensin...

Kokonaisala<-Kokonaisala %>% mutate(Tuotantosuunta= case_when(Tuotantosuunta== "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                              Tuotantosuunta== "Muut nautakarjatilat" ~  "Muut_nautakarjatilat",
                                                              Tuotantosuunta== "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                                              Tuotantosuunta== "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                                              Tuotantosuunta== "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                                              Tuotantosuunta== "Tattari ja kinoa" ~ "Tattari_kinoa",
                                                              Tuotantosuunta== "Vihannekset ja juurekset"~ "Vihannekset_juurekset",
                                                              Tuotantosuunta== "Viljat pl. ohra" ~ "Viljat_pl_ohra",
                                                              .default = Tuotantosuunta
                                                              
))

Nimet<-c(
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
  "Rehuohra",
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
  "Öljypellava")


library(usefun)

outersect(Kokonaisala$Tuotantosuunta, Nimet) #Jos tyhjä, kaikki tuotantosuuntanimet vastaavat toisiaan





library(readxl)
Tuotantosuuntaryhmat <-
  read_excel(
    here("Data","Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx"),
    sheet = "Tuotantosuunnat ryhmittäin",
    col_types = c(
      "text",
      "text"))
colnames(Tuotantosuuntaryhmat)<-c("Tuotantosuunta","Tuotantosuuntaryhmä")

outersect(Kokonaisala$Tuotantosuunta,  Tuotantosuuntaryhmat$Tuotantosuunta)

Kokonaisala<-merge(Kokonaisala, Tuotantosuuntaryhmat, by=c("Tuotantosuunta"))



Kokonaisala<-Kokonaisala %>% group_by(Tuotantosuuntaryhmä) %>% summarise(Hehtaaria_tuotantosuunnalla = sum(Hehtaaria_yhteensa))
colnames(Kokonaisala)[1]<-"Production line"



outersect(Jakaumataulu$`Production line`, Kokonaisala$`Production line`)
Jakaumataulu<-merge(Jakaumataulu, Kokonaisala, by=("Production line"), all=T)


#Kiinteä summarivi
Jakaumataulu<-Jakaumataulu %>% bind_rows(summarise(.,
                                                   across(where(is.numeric), sum),
                                                   across(where(is.character), ~"Total")))


#ETOL nimien englanninnos
Jakaumataulu<-Jakaumataulu %>% mutate(`Production line` = case_when(`Production line` == "Hedelmien viljely" ~ "Fruits",
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
                                                                    `Production line` == "Öljyhampun ja -pellavan viljely" ~ "Oilseed hemp and -flax",
                                                                    `Production line` == "Total" ~ "Total"))

#Muunnetaan N2o-tonnit CO2-ekvivalenteiksi kertomalla GWP-kertoimella 265 (AR5-kerroin)


Jakaumataulu$`tn N2O, total`<- Jakaumataulu$`tn N2O, total`* 265

#Summataan Emissiot. Lopputulos on tonneja CO2-ekvivalenttina. 

Jakaumataulu$`tn CO2e, total`<-Jakaumataulu$`t CO2, total`+Jakaumataulu$`tn N2O, total`


Jakaumataulu<-Jakaumataulu %>% mutate(tn_CO2e_ha = `tn CO2e, total`/Hehtaaria_tuotantosuunnalla)
Jakaumataulu$tn_CO2e_ha<-round(Jakaumataulu$tn_CO2e_ha,1)
Jakaumataulu<-slice(Jakaumataulu, -25)

Jakaumataulu<-Jakaumataulu %>% select(1,5,4,6)



#Ryhmittelyavaimet
Animal_farms<-c("Horse farms",
                "Honey production",
                "Sheep & goat farms",
                "Dairy farms",
                "Egg-laying poultry",
                "Other cattle farms",
                "Poultry",
                "Pig farms",
                "Fur farms")

Animal_farms<-sort(Animal_farms)

Crop_cultivation<-outersect(Jakaumataulu$`Production line`, Animal_farms)
Crop_cultivation <- Crop_cultivation[-16]

Crop_cultivation<-sort(Crop_cultivation)

library(gt)
library(RColorBrewer)
#aakkostus
Jakaumataulu<-arrange(Jakaumataulu, `Production line`)
Jakaumataulu$Ryhmittely<-NA
Jakaumataulu$Ryhmittely[Jakaumataulu$`Production line` %in% Crop_cultivation]<-"Crop cultivation"
Jakaumataulu$Ryhmittely[Jakaumataulu$`Production line` %in% Animal_farms]<-"Animal_farms" #Piilotettava muuttuja jolla targetoidaan värjäys



Kuva<-gt(Jakaumataulu) %>%
  sub_missing() %>%
  fmt_number(columns = 2, decimals = 0, sep_mark = " ") %>%
  fmt_number(columns = 3, decimals = 0, sep_mark = " ") %>%
  tab_row_group(label = "Crop cultivation", rows = `Production line` %in% Crop_cultivation) %>%
  tab_row_group(label = "Animal farms", rows = `Production line` %in% Animal_farms ) %>%
  row_group_order(groups = c("Crop cultivation", "Animal farms")) %>%
  summary_rows(groups = matches("Crop cultivation"), columns =2:3 , fns = list(
    Mean = ~mean(., na.rm=T),
    St.Dev = ~sd(., na.rm=T)),
    fmt = ~fmt_number(., decimals=0, sep_mark =" ")) %>%
  summary_rows(groups = matches("Animal farms"), columns =2:3 , fns = list(
    Mean = ~mean(., na.rm=T),
    St.Dev = ~sd(., na.rm=T)),
    fmt = ~fmt_number(., decimals=0, sep_mark = " ")) %>%
  summary_rows(groups = matches("Crop cultivation"), columns =4 , fns = list(
    Mean = ~mean(., na.rm=T),
    St.Dev = ~sd(., na.rm=T)),
    fmt = ~fmt_number(., decimals=1, sep_mark =" ")) %>%
  summary_rows(groups = matches("Animal farms"), columns =4 , fns = list(
    Mean = ~mean(., na.rm=T),
    St.Dev = ~sd(., na.rm=T)),
    fmt = ~fmt_number(., decimals=1, sep_mark = " ")) %>%
  tab_style(
    style=cell_text(weight="bold"),
    locations=cells_column_labels()) %>%
  tab_style(
    style=cell_text(weight="bold"),
    locations=cells_summary()) %>% 
  tab_style(
    style=cell_text(weight="bold"),
    locations=cells_stub_summary()) %>% 
  cols_label(`tn_CO2e_ha` =html("tn CO<sub>2</sub>-eq ha<sup>-1</sup>")) %>%
  cols_label(`tn CO2e, total` =html("tn CO<sub>2</sub>-eq<sup>-1</sup>")) %>%
  cols_label(Hehtaaria_tuotantosuunnalla = html("Hectares")) %>%
  cols_hide(Ryhmittely) %>%
  data_color(columns=c(4),
             rows = Ryhmittely == "Crop cultivation",  
             method="bin",
             bins = 3,
             palette="Oranges",
             reverse=F) %>%
  data_color(columns=c(4),
             rows = Ryhmittely == "Animal_farms",  
             method="bin",
             bins=3,
             palette="Oranges",
             reverse=F) 

gtsave(Kuva, filename = here("Output/Grafiikka/Paastojakauma_intensiteetit_viljav.docx")) 

Jakaumataulu_viljav<-Jakaumataulu

#Nyt voidaan verrata eri aineistoista johdettuja jakaumia ja intensiteettejä vertaamalla kahta eri Jakaumataulu-framea. Muita ei tarvita.

rm(list=c("CO2_jakauma", "Jakaumataulu", "Kokonaisala", "Kuva", "N2O_jakauma", "Tuotantosuuntaryhmat"))

Jakaumataulut_merge<-inner_join(Jakaumataulu_gtk, Jakaumataulu_viljav, by="Production line")


z<-gt(Jakaumataulut_merge) %>%
  cols_align("left",1) %>%
  cols_align("center",2:length(Jakaumataulut_merge)) %>%
  fmt_number(columns = 2:3, decimals = 0, sep_mark = " ") %>%
  fmt_number(columns = 6:7, decimals = 0, sep_mark = " ") %>%
  tab_row_group(label = "Crop cultivation", rows = `Production line` %in% Crop_cultivation) %>%
  tab_row_group(label = "Animal farms", rows = `Production line` %in% Animal_farms ) %>%
  row_group_order(groups = c("Crop cultivation", "Animal farms")) %>%
  row_group_order(groups = c("Crop cultivation", "Animal farms")) %>%
  summary_rows(groups = matches("Crop cultivation"), columns =c(4,8) , fns = list(
    Mean = ~mean(., na.rm=T),
    St.Dev = ~sd(., na.rm=T)),
    fmt = ~fmt_number(., decimals=1, sep_mark =" ")) %>%
  summary_rows(groups = matches("Animal farms"), columns =c(4,8) , fns = list(
    Mean = ~mean(., na.rm=T),
    St.Dev = ~sd(., na.rm=T)),
    fmt = ~fmt_number(., decimals=1, sep_mark =" ")) %>%
  tab_spanner(label = "Geospatial data", columns = ends_with(".x")) %>%
  tab_spanner(label = "Soil fertility data", columns = ends_with(".y")) %>%
  tab_style(
    style=cell_text(weight="bold"),
    locations=cells_column_labels()) %>%
  tab_style(
    style=cell_text(weight="bold"),
    locations=cells_summary()) %>% 
  tab_style(
    style=cell_text(weight="bold"),
    locations=cells_stub_summary()) %>%
  cols_label(`tn CO2e, total.x` =html("tn CO<sub>2</sub>-eq<sup>-1</sup>"),
             Hehtaaria_tuotantosuunnalla.x	= "Hectares",
             tn_CO2e_ha.x =html("tn CO<sub>2</sub>-eq ha<sup>-1</sup>"), 
             `tn CO2e, total.y` = html("tn CO<sub>2</sub>-eq <sup>-1</sup>"),
             Hehtaaria_tuotantosuunnalla.y	= "Hectares",
             tn_CO2e_ha.y =html("tn CO<sub>2</sub>-eq ha<sup>-1</sup>")) %>%
  data_color(columns=c(4),
             rows = Ryhmittely.x == "Crop cultivation",  
             method="bin",
             bins = 3,
             palette="Oranges",
             reverse=F) %>%
  data_color(columns=c(4),
             rows = Ryhmittely.x == "Animal_farms",  
             method="bin",
             bins=3,
             palette="Oranges",
             reverse=F) %>%
  data_color(columns=c(8),
             rows = Ryhmittely.x == "Crop cultivation",  
             method="bin",
             bins = 3,
             palette="Oranges",
             reverse=F) %>%
  data_color(columns=c(8),
             rows = Ryhmittely.x == "Animal_farms",  
             method="bin",
             bins=3,
             palette="Oranges",
             reverse=F) %>%
  cols_hide(columns=c(Ryhmittely.x,
            Ryhmittely.y))


gtsave(z, filename = here("Output/Grafiikka/Paastojakauma_intensiteetit_rinnakkain.docx")) 



sum(Jakaumataulut_merge$Hehtaaria_tuotantosuunnalla.x)               
sum(Jakaumataulut_merge$Hehtaaria_tuotantosuunnalla.y)               

 
  
  
  
  
  

  
  
  

  
