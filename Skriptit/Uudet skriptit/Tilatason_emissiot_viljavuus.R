library(tidyverse);library(here);library(varhandle)
#Hajontojen laskeminen tuotantosuuntien päästöille. 
#Aggregointi, Viljavuusdata

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

#Suoritetaan GTK-datan aggregointiskriptistä alku, vaan ei itse aggregointia: siihen kohdistetaan muutoksia
source_lines(here("Skriptit/Uudet skriptit/Viljavuus_esikasittely_aggregointi.R"), 1:289)



#tuotantosuuntanimien korjaus

Viljavuus_aggregointi_multavuus<-Viljavuus_aggregointi_multavuus %>% mutate(Tuotantosuunta= case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                             Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                             Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~"Nurmet_laitumet_hakamaat",
                             Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~"Palkokasvit_pl_tarhaherne",
                             Tuotantosuunta == "Rypsi ja rapsi" ~"Rypsi_rapsi",
                             Tuotantosuunta == "Tattari ja kinoa" ~"Tattari_kinoa",
                             Tuotantosuunta == "Vihannekset ja juurekset" ~"Vihannekset_juurekset",
                             Tuotantosuunta == "Viljat pl. ohra" ~ "Viljat_pl_ohra",
                             .default = Tuotantosuunta))
  
  
  
Viljavuus_raivatut_aggregointi_multavuus<-Viljavuus_raivatut_aggregointi_multavuus %>% mutate(Tuotantosuunta= case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                                             Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                                                             Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~"Nurmet_laitumet_hakamaat",
                                                                             Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~"Palkokasvit_pl_tarhaherne",
                                                                             Tuotantosuunta == "Rypsi ja rapsi" ~"Rypsi_rapsi",
                                                                             Tuotantosuunta == "Tattari ja kinoa" ~"Tattari_kinoa",
                                                                             Tuotantosuunta == "Vihannekset ja juurekset" ~"Vihannekset_juurekset",
                                                                             Tuotantosuunta == "Viljat pl. ohra" ~ "Viljat_pl_ohra",
                                                                             .default = Tuotantosuunta))

#Tuotantosuuntaryhmien lisäys ####

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




Viljavuus_aggregointi_multavuus<-merge(Viljavuus_aggregointi_multavuus,Tuotantosuuntaryhmat, by="Tuotantosuunta")

Viljavuus_raivatut_aggregointi_multavuus<-merge(Viljavuus_raivatut_aggregointi_multavuus,Tuotantosuuntaryhmat, by="Tuotantosuunta")





#aggregointi ####
#Aggregointi uudelleen muodostetun elop_mineral muuttujan mukaisesti
#Näissä mukana AINOASTAAN RAIVAAMATTOMAT PELLOT

Aggre_simple<-aggregate(Viljavuus_aggregointi_multavuus$KASVI_ALA_HA ,   by = list(
  Viljavuus_aggregointi_multavuus$MAATILA_TUNNUS,
  Viljavuus_aggregointi_multavuus$Tuotantosuuntaryhmä,
  Viljavuus_aggregointi_multavuus$KASVITUNNU_reclass,
  Viljavuus_aggregointi_multavuus$KASVINIMI_reclass,
  Viljavuus_aggregointi_multavuus$Elop_Mineral),sum)


colnames(Aggre_simple)<-c("Tilatunnus","Tuotantosuuntaryhmä","Kasvikoodi","Kasvinimi","Maalaji multavuuden perusteella","ala ha")


Aggre_simple_raiviot <-aggregate(Viljavuus_raivatut_aggregointi_multavuus$KASVI_ALA_HA,   by = list(
  Viljavuus_raivatut_aggregointi_multavuus$MAATILA_TUNNUS,
  Viljavuus_raivatut_aggregointi_multavuus$Tuotantosuuntaryhmä,
  Viljavuus_raivatut_aggregointi_multavuus$KASVITUNNU_reclass,
  Viljavuus_raivatut_aggregointi_multavuus$KASVINIMI_reclass,
  Viljavuus_raivatut_aggregointi_multavuus$Elop_Mineral),sum)

colnames(Aggre_simple_raiviot)<-c("Tilatunnus","Tuotantosuuntaryhmä","Kasvikoodi","Kasvinimi","Maalaji multavuuden perusteella","ala ha")


#Kasvikategorioiden lisääminen

Kasvikategoriat_avain <-
  read_excel(
    here("Data", "Kasvikategoriat_avain.xlsx"),
    col_types = c("text", "text", "text",
                  "numeric", "skip")
  )

Aggre_simple<-merge(Aggre_simple, Kasvikategoriat_avain, by = "Kasvikoodi")

Aggre_simple_raiviot<-merge(Aggre_simple_raiviot, Kasvikategoriat_avain, by = "Kasvikoodi")

rm(Kasvikategoriat_avain)


#EMISSIOIDEN LASKENTA ####

#Määritellään päästökertoimet

source_lines(here("Skriptit/Uudet skriptit/CO2 intensiteetit.R"),7:65)


#Kerrotaan lasketut pinta-alat päästökertoimilla kategoriatietojen perusteella.
#Mineraalimaa cropland, viljely. 

Aggre_simple <-
  Aggre_simple %>% mutate(
    CO2_tn = case_when(
      `Maalaji multavuuden perusteella` == "Mineraali" &
        `Cropland/grassland` == "Cropland" ~ `ala ha` * viljely_CO2_cropland_mineral
    )
  )

#Mineraali, grassland, viljely. Lasketaan samaan tauluun.
#Edellä laskettu on muistettava jättää voimaan .default komennolla, 
Aggre_simple <-
  Aggre_simple %>% mutate(
    CO2_tn = case_when(
      `Maalaji multavuuden perusteella` == "Mineraali" &
        `Cropland/grassland` == "Grassland" ~ `ala ha` * viljely_CO2_grassland_mineral,
      .default = CO2_tn
    )
  )

#Mineraalimaa, raivio

Aggre_simple_raiviot <-
  Aggre_simple_raiviot %>% mutate(
    CO2_tn = case_when(
      `Maalaji multavuuden perusteella` == "Mineraali" &
        `Cropland/grassland` == "Cropland" ~ `ala ha` * raivaus_CO2_cropland_mineral
    )
  )

#Mineraalimaa, grassland. Aiemmin laskettu jätettävä voimaan.


Aggre_simple_raiviot <-
  Aggre_simple_raiviot %>% mutate(
    CO2_tn = case_when(
      `Maalaji multavuuden perusteella` == "Mineraali" &
        `Cropland/grassland` == "Grassland" ~ `ala ha` * raivaus_CO2_grassland_mineral,
      .default = CO2_tn
    )
  )



#Eloperainen maa 
#Cropland
#Erillinen kerroin tämän kategorian annual cropsille, ja grassille (monivuotiset, sadolliset nurmikasvit). Ts. Croplandin sisällä olevat, sadolliset monivuotisnurmet kuten rehunurmet.  

Aggre_simple <-
  Aggre_simple %>% mutate(
    CO2_tn = case_when(
      `Maalaji multavuuden perusteella` == "Eloperäinen" &
        `Cropland/grassland` == "Cropland" & `Yksi/monivuotinen` == "Yksivuotinen" ~ `ala ha` * viljely_CO2_cropland_elop_annual_crops,
      .default = CO2_tn
    )
  )

Aggre_simple <-
  Aggre_simple %>% mutate(
    CO2_tn = case_when(
      `Maalaji multavuuden perusteella` == "Eloperäinen" &
        `Cropland/grassland` == "Cropland" & `Yksi/monivuotinen` == "Monivuotinen" ~ `ala ha` * viljely_CO2_cropland_elop_grass,
      .default = CO2_tn
    )
  )

#Eloperäinen Grassland


Aggre_simple <-
  Aggre_simple %>% mutate(
    CO2_tn = case_when(
      `Maalaji multavuuden perusteella` == "Eloperäinen" &
        `Cropland/grassland` == "Grassland"  ~ `ala ha` * viljely_CO2_grassland_elop,
      .default = CO2_tn
    )
  )

#Raiviot
#Eloperäinen maa, raivio, grassland 

Aggre_simple_raiviot <- Aggre_simple_raiviot %>% mutate(
  CO2_tn = case_when(
    `Maalaji multavuuden perusteella` == "Eloperäinen" &
      `Cropland/grassland` == "Grassland"  ~ `ala ha` * raivaus_CO2_grassland_elop,
    .default = CO2_tn
  )
)


#cropland

Aggre_simple_raiviot <- Aggre_simple_raiviot %>% mutate(
  CO2_tn = case_when(
    `Maalaji multavuuden perusteella` == "Eloperäinen" &
      `Cropland/grassland` == "Cropland"  ~ `ala ha` * raivaus_CO2_cropland_elop,
    .default = CO2_tn
  )
)


#Tarkistetaan että kategoriat käyty kaikki läpi. Jääkö tyhjiä?
filter(Aggre_simple, is.na(CO2_tn))
filter(Aggre_simple_raiviot, is.na(CO2_tn))


sum(Aggre_simple$`ala ha`)+sum(Aggre_simple_raiviot$`ala ha`)

#SUmmataan tila ja tuotantosuunta huomioiden alat ja CO2

a<-Aggre_simple %>% group_by(Tilatunnus, Tuotantosuuntaryhmä) %>% summarise(Ala_yhteensa = sum(`ala ha`),
                                                                         CO2tn_yhteensa = sum(CO2_tn))

b<-Aggre_simple_raiviot %>% group_by(Tilatunnus, Tuotantosuuntaryhmä) %>% summarise(Ala_yhteensa_raiv = sum(`ala ha`),
                                                                                   CO2tn_yhteensa_raiv = sum(CO2_tn))

#Yhdistetään raivausdata ja viljelydata, summataa

c<-merge(a,b, by=c("Tilatunnus","Tuotantosuuntaryhmä"), all=T)
c[is.na(c)]<-0

c$Peltoala_yht<-c$Ala_yhteensa_raiv+c$Ala_yhteensa

c$CO2_summa <-c$CO2tn_yhteensa+c$CO2tn_yhteensa_raiv

sum(c$Peltoala_yht)


#Lasketaan intensiteetti t CO2/ha

c<-c %>% mutate(Intensiteetti_t_CO2_ha = CO2_summa/Peltoala_yht)

#Lasketaan intensiteettien hajonta tuotantosuuntaryhmän mukaisesti

Hajonnat<-c %>% group_by(Tuotantosuuntaryhmä) %>% summarise(Intensiteetin_hajonta = sd(Intensiteetti_t_CO2_ha))


#Lasketaan intensiteettien mediaanit

Mediaanit<-c %>% group_by(Tuotantosuuntaryhmä) %>% summarise(Intensiteetin_hajonta = median(Intensiteetti_t_CO2_ha))

library(openxlsx)
x<-createWorkbook()
addWorksheet(x,"Keskihajonnat")
addWorksheet(x,"Mediaanit")
writeData(x, "Keskihajonnat",Hajonnat)
writeData(x, "Mediaanit",Mediaanit)
saveWorkbook(x, file=here("Output/Yksinkertaistettu_intensiteettilaskenta/Tilatyyppien_intensiteetin_hajonta_viljav.xlsx"), overwrite = T)
