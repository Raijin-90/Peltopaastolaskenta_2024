# Kasvi- ja tuotantosuuntakohtainen lannoite-input Kari Koppelmäen lannoitekertoimista7
# HV 13032025

library(varhandle);library(readxl);library(here);library(tidyverse)

#Lannoitusaineisto sisään, kertoimet siistiin formaattiin

lannoitusdata <- read_excel(here("Data/Ravinnelaskennan_aineisto/Kasvilista_lannoitus.xlsx"))
lannoitus<-lannoitusdata %>% select(-`Sato tn/ha`,-`N min kg/ha`,-`N maks kg/ha`,-Kommentit,-Lähteet)

colnames(lannoitus)[colnames(lannoitus)=="Kasvi"]<-"Kasvinimi"
lannoitus$Kasvinimi<-NULL
lannoitus$Tuoteryhmä<-NULL
#Viljelyala-aineiston aggregointi

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

source_lines(here("Skriptit/Uudet skriptit/GTK_datan_aggregointi.R"),1:260)

rm.all.but(c("GTKdata","lannoitus"))


#Aggregoidaan alat kasvi-tuotantosuunta leveliin. 


aggreData <- GTKdata %>% group_by(Tuotantosuunta,
                                  KASVIKOODI_lohkodata_reclass,
                                  KASVINIMI_reclass) %>% summarise(across(c(Maannossumma, Mineraalia, Eloperaista), sum))
colnames(aggreData)[colnames(aggreData)=="KASVIKOODI_lohkodata_reclass"]<-"Kasvikoodi" 
colnames(aggreData)[colnames(aggreData)=="KASVINIMI_reclass"]<-"Kasvinimi" 
                                                                                                                     
#Liitetään luomun osuus viljelyalasta (laskettu erikseen). Tätä prosenttia viljelyalaa ei lannoiteta. 

library(readxl)
luomuosuudet_viljelykasveille <- read_excel(here("Output/Ravinnedata/Luomuosuudet_viljelykasveille_GTK.xlsx"))

#Luomuerittely tilakoodin mukaan johtaa hieman suurempaan luomualaan
#luomuosuudet_viljelykasveille <- read_excel(here("Output/Ravinnedata/Luomuosuudet_viljelykasveille_gtk_vertailtava_versio.xlsx"))



luomu<-luomuosuudet_viljelykasveille %>% select(Tuotantosuunta,Kasvikoodi,Kasvinimi,Luomuosuus_kaikki,Luomuosuus_mineraali,Luomuosuus_elop)

aggreData<-inner_join(aggreData, luomu, by=c("Tuotantosuunta","Kasvikoodi","Kasvinimi"))

sum(aggreData$Maannossumma)

rm.all.but(c("aggreData","lannoitus"))

#Lasketaan viljelyalasta tavanomaisen viljelyn osuus

aggreData<-aggreData %>% mutate(Luomuala_kaikki = Luomuosuus_kaikki*Maannossumma, 
                     Luomuala_elop = Luomuosuus_elop*Eloperaista,
                     Luomuala_mineral = Luomuosuus_mineraali*Mineraalia)

aggreData<-aggreData %>% mutate(Tavallinen_viljely_kaikki = Maannossumma-Luomuala_kaikki,
                     Tavallinen_viljely_elop = Eloperaista-Luomuala_elop,
                     Tavallinen_viljely_mineral = Mineraalia-Luomuala_mineral)

#Osuusprosentteja ei tarvita enää 
aggreData<-aggreData %>% select(c(-Luomuosuus_kaikki,-Luomuosuus_mineraali,-Luomuosuus_elop))


#Liitetään lannoituskerroin aineistoon



aggreData<-left_join(aggreData, lannoitus, by=c("Kasvikoodi"))


#Turvemailla käytetään vähemmän typpilannoitteita, koska orgaanisesta aineesta vapautuu mineraalimaita selvästi enemmän typpeä.
#Lannoitussuosituksissa typpilannoituksen määrä voi olla esim. 80 kg/ha mineraalimaille ja 60 kg/ha turvemaille. 
#Monen kasvin osalta tätä tietoa ei ole saatavilla ja useiden kasvin viljely on turvemailla ylipäätänsä marginaalista. 
#Nyt turvemaiden lannoitusmäärät on typen osalta laitettu näiden suositusten/lähteiden mukaan niiltä osin, kun tietoa on ollut. 
#Koska turvemaiden typpilannoitemäärät seuraavat enemmän tai vähemmän samaa kaavaa niin voisi olla perusteltua käyttää typpilannoitteen osalta vakiokerrointa (esim. 0.8 x N lannoitus mineraalimailla).
#Kari Koppelmäki 13/3/2025

#Tältä pohjalta säädetään turvemaiden typpilannoituksen kerrointa. Sen suuruudeksi asetetaan kautta linjan 80% mineraalimaiden lannoituksesta kullekin kasville



aggreData[is.na(aggreData)]<-0


aggreData<-aggreData %>% mutate(`N kg/ha organic soil UUSI` = 0.8* `N kg/ha mineral soil`)

#Lannoituksen ravinne-inputin laskenta (ravinnekerroin * perinteisen viljelyn  ala, EI LUOMUALAA)


basicOutput<-aggreData %>% mutate(N_kg_mineral = Tavallinen_viljely_mineral*`N kg/ha mineral soil`,
                     N_kg_organic = Tavallinen_viljely_elop *`N kg/ha organic soil UUSI`, 
                     P_kg_mineral = Tavallinen_viljely_mineral * `P kg /ha mineral soil`,
                     P_kg_organic = Tavallinen_viljely_elop *`P kg/ha orgainc soil`)


basicOutput<-basicOutput %>% mutate(P_yht_tn = (P_kg_mineral+P_kg_organic)/1000,
                       N_yht_tn = (N_kg_mineral+N_kg_organic)/1000)



sum(basicOutput$Luomuala_elop)+sum(basicOutput$Luomuala_mineral)+sum(basicOutput$Tavallinen_viljely_elop)+sum(basicOutput$Tavallinen_viljely_mineral)


sum(basicOutput$N_yht_tn)
sum(basicOutput$Luomuala_kaikki)
sum(basicOutput$Tavallinen_viljely_kaikki)

sum(basicOutput$Luomuala_kaikki)+sum(basicOutput$Tavallinen_viljely_kaikki)


#Lannoitustarve per viljelykasvi

A<-basicOutput %>% group_by(Kasvikoodi, Kasvinimi) %>% summarise(N_tarve_kg_min = sum(N_kg_mineral),  
                                                              N_tarve_kg_org = sum(N_kg_organic),
                                                              Mineraaliala = sum(Tavallinen_viljely_mineral),
                                                              Eloperaista = sum(Tavallinen_viljely_elop))  %>% mutate(N_tarve_yht = N_tarve_kg_min+N_tarve_kg_org)

                                                              
                                                              
                                                              
#LASKETAAN ERILLISET KERTOIMET KASVI- JA ELÄINTILOILLE KASVEITTAIN. NÄILLÄ ALLOKOIDAAN TYPPIKUORMA MINERAALILANNOITTEISTA LOHKOILLE.                                                              
#ERI ELÄINTILAT KÄYTTÄVÄT SIIS SAMAA KASVEITTAIN VAIHTELEVAA KERROINTA, JA ERI KASVITILAT SAMAA. 

#Per kasvi, kotieläintiloilla

Animalfarms<-c("Hevostilat","Lammas- ja vuohitilat","Maitotilat", "Munatilat","Muut nautakarjatilat","Siipikarjatilat","Sikatilat","Turkistilat")

B <- basicOutput %>% filter(Tuotantosuunta %in% Animalfarms) %>% group_by(Kasvikoodi, Kasvinimi) %>%   summarise(
  N_tarve_kg_min = sum(N_kg_mineral),
  N_tarve_kg_org = sum(N_kg_organic),
  Mineraaliala = sum(Tavallinen_viljely_mineral),
  Eloperaista = sum(Tavallinen_viljely_elop)) %>% mutate(N_tarve_yht = N_tarve_kg_min +
                                              N_tarve_kg_org)

#PER KASVI, KASVITILOILLA
#Huomioiden kasvitilat ja kasvi
  
C<-basicOutput %>% 
  filter(!(Tuotantosuunta %in% Animalfarms)) %>% 
  group_by(Kasvikoodi, Kasvinimi) %>% 
  summarise(
    N_tarve_kg_min = sum(N_kg_mineral),
    N_tarve_kg_org = sum(N_kg_organic),
    Mineraaliala = sum(Tavallinen_viljely_mineral),
    Eloperaista = sum(Tavallinen_viljely_elop)) %>% 
      mutate(N_tarve_yht = N_tarve_kg_min + N_tarve_kg_org)
                                                

#Tulostyökirja

output<-createWorkbook()
addWorksheet(output,"Ravinnemassat")
writeData(output,"Ravinnemassat",basicOutput)
saveWorkbook(output,file=here("Output/Ravinnedata/lannoitus_kasveille_Karin_kertoimet.xlsx"), overwrite = T)

#Tarkennetut tulokset

detailed_output<-createWorkbook()
addWorksheet(detailed_output,"Lannoitustarve_kasvit")
writeData(detailed_output,"Lannoitustarve_kasvit",A)

addWorksheet(detailed_output,"Lannoitustrv_kasvit_elaintilat")
writeData(detailed_output,"Lannoitustrv_kasvit_elaintilat",B)

addWorksheet(detailed_output,"Lannoitustrv_kasvit_kasvitilat")
writeData(detailed_output,"Lannoitustrv_kasvit_kasvitilat",C)

saveWorkbook(detailed_output,file=here("Output/Ravinnedata/Lannoitustarvetarkennus.xlsx"), overwrite = T)



