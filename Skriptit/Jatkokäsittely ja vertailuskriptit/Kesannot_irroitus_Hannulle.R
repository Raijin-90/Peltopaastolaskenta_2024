require(tidyverse)
require(openxlsx)
library(here)

#GTK-AINEISTO ####

source(here("Skriptit/GTK_datan_aggregointi.R")) #Scriptistä saadaan irti GTKData, jossa kaikki gtk-aineisto
Kat <- read.xlsx(here("Data/Kasvikategoriat_avain.xlsx"))

colnames(Kat)[colnames(Kat) == "Kasvikoodi"] <-
  "KASVIKOODI" #Yhdistetään tieto kasvikategorioista


rm(list = ls()[!(ls() %in% c('Kat', 'GTKdata'))])


df <-
  merge(GTKdata, Kat, by = "KASVIKOODI") #Aggregoitava data, originaali jää jos tulee ongelmia. Tämä kategoria-avain sisältää sekä disaggregoidut (rehu/ruokavilja) että ei-disaggregoidut koodit.

#Nimeämiskorjaus niille tuotantosuunnille, joilla raakadatassa on välilyöntejä

uudet_nimet <- c(
  "Viljat pl. ohra"  = "Viljat_pl_ohra",
  "Muut nautakarjatilat"  = "Muut_nautakarjatilat",
  "Vihannekset ja juurekset" = "Vihannekset_juurekset",
  "Palkokasvit pl. tarhaherne" = "Palkokasvit_pl_tarhaherne",
  "Rypsi ja rapsi" = "Rypsi_rapsi",
  "Lammas- ja vuohitilat" = "Lammas_ja_vuohitilat",
  "Nurmet, laitumet, hakamaat" = "Nurmet_laitumet_hakamaat",
  "Tattari ja kinoa" = "Tattari_kinoa",
  "Hunajatuotanto" = "Hunajantuotanto"
)


df$Tuotantosuunta <-
  str_replace_all(df$Tuotantosuunta, uudet_nimet)

#Muutos 16.11.2023.
#Lisätään tuotantosuuntaryhmäluokat aggregointiavaimeksi.

TS_Ryhmat <-
  read.xlsx(here("Data/Muuntoavain_tuotantosuunnat_tuotteet.xlsx"),
            sheet = "Tuotantosuunnat ryhmittäin")
colnames(TS_Ryhmat) <- c("Tuotantosuunta", "Tuotantosuuntaryhma")

filter(df,!(Tuotantosuunta %in% TS_Ryhmat$Tuotantosuunta))  #Tavoitteena tyhjä vektori eli samat nimet

df <- merge(df, TS_Ryhmat, by = "Tuotantosuunta")


#Toinen muutos: lisätään ENVIMAT-tuotteiden ja kasvikoodien vastinetieto

ETTL <-
  read_excel(
    here(
      "Data/Ruokavirasto-ENVIMATFOOD-muuntoavain_tuotteet_tuotantosuunnat.xlsx"
    ),
    sheet = "Ruokavirasto-ETTL tuotemuunto"
  )

ETTL <- select(ETTL, 1:3)
colnames(ETTL)[3] <- "KASVIKOODI"

df <- merge(df, ETTL, by = "KASVIKOODI")


## Kesantokoodien erittely

Kesannot<-createWorkbook() 
Kesannot %>% addWorksheet("Tarkka_data")

x<-df %>% filter(Tuoteryhmä == "Kesannot,laitumet,yms.") %>% group_by(Tuotantosuuntaryhma,ETTL,`ETTL Nimike`, Kasvi) %>% summarise(
  Mineraalimaata = sum(Mineraalia),
  Eloperaista_maata = sum(Eloperaista)
)

Kesannot %>% writeData("Tarkka_data", x)



Kesannot %>% addWorksheet("ETTL")


y<-df %>% filter(Tuoteryhmä == "Kesannot,laitumet,yms.") %>% group_by(ETTL,`ETTL Nimike`) %>% summarise(
  Mineraalimaata = sum(Mineraalia),
  Eloperaista_maata = sum(Eloperaista)
)

Kesannot %>% writeData("ETTL",y)



z<- df %>% filter(Tuoteryhmä == "Kesannot,laitumet,yms.") %>% group_by(KASVIKOODI, Kasvi) %>% summarise(
    Mineraalimaata = sum(Mineraalia),
    Eloperaista_maata = sum(Eloperaista)
  )

Kesannot %>% addWorksheet("Kasvikoodi_kasvi")

Kesannot %>% writeData("Kasvikoodi_kasvi",z)




o<- df %>% filter(Tuoteryhmä == "Kesannot,laitumet,yms.") %>% group_by(Tuotantosuuntaryhma) %>% summarise(
  Mineraalimaata = sum(Mineraalia),
  Eloperaista_maata = sum(Eloperaista)
)

Kesannot %>% addWorksheet("TSryhma")


Kesannot %>% writeData("TSryhma",o)

saveWorkbook(Kesannot, file="Kesantojen_erittely.xlsx")
