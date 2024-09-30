library(gt)
library(readxl)
library(tidyverse)
library(usefun)

library(readxl)
CO2_tuotekertoimet_gtk <-
  read_excel(
    "Output/Yksinkertaistettu_intensiteettilaskenta/CO2_tuotekertoimet_gtk.xlsx",
    sheet = "Satokertoimet_vilja"
  )

library(readxl)
N2O_tuotekertoimet_gtk <-
  read_excel(
    "Output/Yksinkertaistettu_intensiteettilaskenta/N2O_tuotekertoimet_gtk.xlsx",
    sheet = "Satokertoimet_vilja"
  )

#Yhdistetään emissiot

Viljapäästöt <-
  merge(
    CO2_tuotekertoimet_gtk,
    N2O_tuotekertoimet_gtk,
    by = c("Kasvikoodi", "Kasvinimi", "Tuotantosuuntaryhmä")
  )

colnames(Viljapäästöt)<-c("Kasvikoodi",
                          "Kasvinimi",
                          "Tuotantosuuntaryhmä",
                          "Paastotonnit_CO2",
                          "Satotonnia",
                          "Paastokilot_N2O",
                          "Satotonnia.y")

rm(N2O_tuotekertoimet_gtk, CO2_tuotekertoimet_gtk)

Viljapäästöt$Satotonnia.y<-NULL

#Muunnetaan N2o-päästö ensin tonneiksi, ja siitä CO2eq:ksi

Viljapäästöt$Päästötonnit_N2O_muunto_CO2eq <-
  (Viljapäästöt$Paastokilot_N2O / 1000) * 265

Viljapäästöt$CO2eq_t_yht <-
  Viljapäästöt$Paastotonnit_CO2 + Viljapäästöt$Päästötonnit_N2O_muunto_CO2eq

Viljapäästöt <- Viljapäästöt %>% select(1:3, 5, 8)


Viljapäästöt$Kerroin<-Viljapäästöt$CO2eq_t_yht/Viljapäästöt$Satotonnia

#Karsittu tuotelista:
#Heivataan listasta kevätruis ja syysvehnä, mallas ja rehuohra mukaan

Karsitut_viljat<-c("Kaura", 
"Syysruis",  
"Kevätvehnä", 
"Mallasohra",
"Rehuohra")


Viljapäästöt<-Viljapäästöt %>% filter(Kasvinimi %in% Karsitut_viljat)


#Lisätään viljelyalat vertailutauluun
Alat_vilja <-
  read_excel(
    "Output/Yksinkertaistettu_intensiteettilaskenta/CO2_tuotekertoimet_gtk.xlsx",
    sheet = "Herkkyystarkastelu_alat" #On sama, kuin croplandin kokonaisala. Tästä suodatetaan myöhemmin herkkyystarkastelun tuotteey   
  )
sum(Alat_vilja$Hehtaaria_yhteensa_elop)+sum(Alat_vilja$Hehtaaria_yhteensa_min)


library(readxl)
Tuotantosuuntaryhmat <-
  read_excel(
    here("Data","Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx"),
    sheet = "Tuotantosuunnat ryhmittäin",
    col_types = c("text",
                  "text"
    )
  )
colnames(Tuotantosuuntaryhmat) <-
  c("Tuotantosuunta", "Tuotantosuuntaryhmä")


Alat_vilja<-merge(Alat_vilja, Tuotantosuuntaryhmat, by="Tuotantosuunta")
rm(Tuotantosuuntaryhmat)


Alat_vilja<-Alat_vilja %>% filter(Kasvinimi %in% Karsitut_viljat)

Alat_vilja<-Alat_vilja %>% mutate(Kokonaisviljelyala = Hehtaaria_yhteensa_min+Hehtaaria_yhteensa_elop) #Summataan eloperäinen ja mineraali. Näihin laskettu mukaan raivaamaton ja raivattu kumpikin
sum(Alat_vilja$Kokonaisviljelyala)

Alat_vilja<-Alat_vilja %>% group_by(Kasvikoodi,Kasvinimi, Tuotantosuuntaryhmä) %>% summarise(Kokonaisala=sum(Kokonaisviljelyala))
                      

Viljapäästöt<-merge(Viljapäästöt, Alat_vilja, by=c("Kasvikoodi","Kasvinimi","Tuotantosuuntaryhmä"))              




#ETOL nimien englanninnos
Viljapäästöt <-
  Viljapäästöt %>% mutate(
    Tuotantosuuntaryhmä = case_when(
      Tuotantosuuntaryhmä == "Hedelmien viljely" ~ "Fruits",
      Tuotantosuuntaryhmä == "Hevostilat" ~ "Horse farms",
      Tuotantosuuntaryhmä == "Hunajatuotanto, muu eläimen hoito" ~ "Honey production",
      Tuotantosuuntaryhmä == "Kauran, ohran, rukiin ja vehnän viljely" ~ "Oat, barley, rye and wheat",
      Tuotantosuuntaryhmä == "Kuminan ja muiden maustekasvien viljely" ~ "Cumin & other spice crops",
      Tuotantosuuntaryhmä == "Lammas- ja vuohitilat" ~ "Sheep & goat farms",
      Tuotantosuuntaryhmä == "Maitotilat" ~ "Dairy farms",
      Tuotantosuuntaryhmä == "Mallasohran viljely" ~ "Malt barley",
      Tuotantosuuntaryhmä == "Marjojen viljely" ~ "Berries",
      Tuotantosuuntaryhmä == "Muiden vihannesten ja juuresten viljely" ~ "Other vegetables & root vegetables",
      Tuotantosuuntaryhmä =="Munatilat" ~ "Egg-laying poultry",
      Tuotantosuuntaryhmä == "Muut nautakarjatilat" ~ "Other cattle farms",
      Tuotantosuuntaryhmä == "Peltoherneen ja härkäpavun viljely" ~ "Field pea & faba bean",
      Tuotantosuuntaryhmä == "Perunan viljely" ~ "Potato",
      Tuotantosuuntaryhmä == "Rehukasvien viljely" ~ "Fodder crops & pastures",
      Tuotantosuuntaryhmä == "Rypsin ja rapsin viljely" ~ "Rapeseed & Turnip rape",
      Tuotantosuuntaryhmä == "Siipikarjatilat" ~ "Poultry",
      Tuotantosuuntaryhmä == "Sikatilat" ~ "Pig farms",
      Tuotantosuuntaryhmä == "Sokerijuurikkaan viljely" ~ "Sugar beet",
      Tuotantosuuntaryhmä == "Tarhaherneen viljely" ~ "Garden pea",
      Tuotantosuuntaryhmä == "Tattarin ja kinoan viljely" ~ "Buckwheat & Quinoa",
      Tuotantosuuntaryhmä == "Turkistarhaus" ~ "Fur farms",
      Tuotantosuuntaryhmä == "Yrttien viljely avomaalla" ~ "Open-ground herbs",
      Tuotantosuuntaryhmä == "Öljyhampun ja -pellavan viljely" ~ "Oilseed hemp and -flax",
      Tuotantosuuntaryhmä == "Total" ~ "Total"
    )
  )

#Ryhmittelyavaimet
Animal_farms <- c(
  "Horse farms",
  "Honey production",
  "Sheep & goat farms",
  "Dairy farms",
  "Egg-laying poultry",
  "Other cattle farms",
  "Poultry",
  "Pig farms",
  "Fur farms"
)

Animal_farms <- sort(Animal_farms)

Crop_cultivation <-
  outersect(Viljapäästöt$Tuotantosuuntaryhmä, Animal_farms)
Crop_cultivation <- sort(Crop_cultivation)

Viljapäästöt$Ryhma[Viljapäästöt$Tuotantosuuntaryhmä %in% Animal_farms] <-
  "Animal farms"

Viljapäästöt$Ryhma[Viljapäästöt$Tuotantosuuntaryhmä %in% Crop_cultivation] <-
  "Crop cultivation"

#Millä tuotantosuunnilla rajuimmat viljapäästöt ylipäänsä?

library(usefun)
ranking<-Viljapäästöt %>% group_by(Tuotantosuuntaryhmä) %>% summarise(Paastosumma = sum(CO2eq_t_yht))
ranking<-ranking %>% arrange(desc(Paastosumma)) 
ranking$pros<-(ranking$Paastosumma/sum(ranking$Paastosumma))*100
ranking$pros<-round(ranking$pros, 1)


Tarkeimmat<-ranking[1:4,]
sum(Tarkeimmat$pros)


#Taulukoidaan 4 tärkeintä tuotantosuuntaa, vastaavat 90% tämän tuotejoukon CO2eq emissioista

Viljapäästöt<-Viljapäästöt %>% filter(Tuotantosuuntaryhmä %in% Tarkeimmat$Tuotantosuuntaryhmä)

Viljapäästöt<-Viljapäästöt %>% select(1:7)
Viljapäästöt<-Viljapäästöt %>% select(1:3,7,4:6)


a<-gt(Viljapäästöt) %>%
  tab_row_group(label = "Spring wheat", rows = Kasvinimi %in% "Kevätvehnä") %>%  
  tab_row_group(label = "Fall rye", rows = Kasvinimi %in% "Syysruis") %>%  
  tab_row_group(label = "Fodder barley", rows = Kasvinimi %in% "Rehuohra") %>%  
  tab_row_group(label = "Malt barley", rows = Kasvinimi %in% "Mallasohra") %>%
  tab_row_group(label = "Oat", rows = Kasvinimi %in% "Kaura")  %>%
  cols_hide(c("Kasvikoodi", "Kasvinimi","Satotonnia")) %>%
  fmt_number(columns = c(4), decimals = 0, sep_mark = " ") %>%
  fmt_number(columns = c(7), decimals = 1, sep_mark = " ") %>%
  fmt_number(columns = c(6), decimals = 0, sep_mark = " ")%>%
  cols_align(align="left",3) %>%
  cols_align(align="center",4:length(Viljapäästöt)) %>%
  tab_style(style=cell_borders(sides="bottom", color = "silver", weight=px(3)), 
            locations = cells_column_labels()) %>%
  tab_style(style=cell_text(style=c("italic")),
            locations = cells_row_groups()) %>%
  cols_label(Tuotantosuuntaryhmä = "Production line",
             Satotonnia = "tn production",
             CO2eq_t_yht = html("CO<sub>2</sub>-eq, tn"),
             Kerroin = html("tn CO<sub>2</sub>-eq tn <sup>-1</sup>"),
             Kokonaisala = "Cultivated area, hectares") %>%
  summary_rows(groups = everything(), columns = Kerroin, fns= list(Mean=~mean(.)),
               fmt = list(~ fmt_number(., decimals = 1)))

library(here)
gtsave(a, filename=here("Output/Yksinkertaistettu_intensiteettilaskenta/Viljavertailu_13.6.2024.docx"), overwrite=T)
  
  

Viljojen_keskimaar_kertoimet<-Viljapäästöt %>% group_by(Kasvikoodi,Kasvinimi) %>% summarise(Keskimaarainen_kerroin=mean(Kerroin))
write.xlsx(Viljojen_keskimaar_kertoimet,here("Output/Yksinkertaistettu_intensiteettilaskenta/Viljavertailu_keskiarvot_13.6.2024.xlsx"))


