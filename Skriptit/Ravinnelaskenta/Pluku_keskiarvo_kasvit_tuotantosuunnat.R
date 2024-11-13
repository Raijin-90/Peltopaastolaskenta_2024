library(tidyverse);library(varhandle)

#Ajetaan viljavuusdatan esikäsittely siihen asti, että mukaan saadaan datan lohkot erottelematta raivattua ja raivausta. 

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

source_lines(here("Skriptit/Uudet skriptit/Viljavuus_esikasittely_aggregointi.R"), 1:240)

rm.all.but("Viljavuusdata")

#Liitetään tuotantosuuntaryhmä, yhdenmukainen envimatin luokituksen kanssa

library(readxl)
Muuntoavain_tuotantosuunnat_tuotteet_ETOL <- read_excel("Data/Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx", sheet="Tuotantosuunnat ryhmittäin")
colnames(Muuntoavain_tuotantosuunnat_tuotteet_ETOL)[1]<-"Tuotantosuunta" 
         

#Kirjoitusasut
Viljavuusdata<-Viljavuusdata %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                  Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                  Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                  Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                  Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                  Tuotantosuunta == "Tattari ja kinoa"  ~ "Tattari_kinoa",
                                  Tuotantosuunta == "Vihannekset ja juurekset"  ~ "Vihannekset_juurekset",
                                  Tuotantosuunta == "Viljat pl. ohra"   ~ "Viljat_pl_ohra",
                                  .default = Tuotantosuunta))

Viljavuusdata<-inner_join(Viljavuusdata, Muuntoavain_tuotantosuunnat_tuotteet_ETOL, by="Tuotantosuunta")

#Ryhmittelyperuste: kasvi- ja tuotantosuuntaryhmä 

Tuote_ts<-Viljavuusdata %>% group_by(ETOL, KASVITUNNU_reclass,KASVINIMI_reclass) %>% summarise(Keskimaar_Pluku = mean(KESKIARVO_))
ts<-Viljavuusdata %>% group_by(ETOL) %>% summarise(Keskimaar_Pluku = mean(KESKIARVO_))
tuote<-Viljavuusdata %>% group_by(KASVITUNNU_reclass, KASVINIMI_reclass) %>% summarise(Keskimaar_Pluku = mean(KESKIARVO_))

library(openxlsx)
Kooste<-createWorkbook()
addWorksheet(Kooste,"Tuote_tuotantos")
addWorksheet(Kooste,"Tuote")
addWorksheet(Kooste,"Tuotantosuunta")

writeData(Kooste,"Tuote_tuotantos",Tuote_ts)
writeData(Kooste,"Tuote",tuote)
writeData(Kooste,"Tuotantosuunta",ts)

saveWorkbook(wb=Kooste,file=here("Output/Ravinnedata/Pluku_tuote_tuotantos.xlsx"), overwrite = T)

