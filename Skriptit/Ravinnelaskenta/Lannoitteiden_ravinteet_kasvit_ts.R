# Kasvi- ja tuotantosuuntakohtainen lannoite-input Kari Koppelmäen lannoitekertoimista7
# HV 13032025

library(varhandle);library(readxl);library(here);library(tidyverse)

#Lannoitusaineisto sisään, kertoimet siistiin formaattiin

lannoitusdata <- read_excel(here("Data/Ravinnelaskennan_aineisto/Kasvilista_lannoitus.xlsx"))
lannoitus<-lannoitusdata %>% select(-`Sato tn/ha`,-`N min kg/ha`,-`N maks kg/ha`,-Kommentit,-Lähteet)

#Viljelyala-aineiston aggregointi

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

source_lines(here("Skriptit/Uudet skriptit/GTK_datan_aggregointi.R"),1:260)

rm.all.but(c("GTKdata","Lannoitus"))


#Aggregoidaan alat kasvi-tuotantosuunta leveliin. 


aggreData <- GTKdata %>% group_by(Tuotantosuunta,
                                  KASVIKOODI_lohkodata_reclass,
                                  KASVINIMI_reclass) %>% summarise(across(c(Maannossumma, Mineraalia, Eloperaista), sum))
colnames(aggreData)[colnames(aggreData)=="KASVIKOODI_lohkodata_reclass"]<-"Kasvikoodi" 
                                                                                                                     





#Liitetään lannoituskerroin aineistoon

aggreData<-left_join(aggreData, Lannoitus, by="Kasvikoodi")

#Tehdään muuttuja jonka pohjalta voi erotella lohkot, joista löytyy ainakin yksi lannoituskerroin. 
#Datassa on 4 eri lannoituskerrointa. 
#Jos kaikkien eri kerrointen summa lohkolla on NA, niin sellaiselle riville ei ole minkäänlaista lannoituskerrointa. Nämä poistetaan laskuista

aggreData<-aggreData %>% mutate(Suodatin = `N kg/ha mineral soil`+`N kg/ha organic soil`+`P kg/ha orgainc soil`+`P kg /ha mineral soil`)
aggreData<-aggreData %>% filter(!(is.na(Suodatin)))  


#Turvemailla käytetään vähemmän typpilannoitteita, koska orgaanisesta aineesta vapautuu mineraalimaita selvästi enemmän typpeä.
#Lannoitussuosituksissa typpilannoituksen määrä voi olla esim. 80 kg/ha mineraalimaille ja 60 kg/ha turvemaille. 
#Monen kasvin osalta tätä tietoa ei ole saatavilla ja useiden kasvin viljely on turvemailla ylipäätänsä marginaalista. 
#Nyt turvemaiden lannoitusmäärät on typen osalta laitettu näiden suositusten/lähteiden mukaan niiltä osin, kun tietoa on ollut. 
#Koska turvemaiden typpilannoitemäärät seuraavat enemmän tai vähemmän samaa kaavaa niin voisi olla perusteltua käyttää typpilannoitteen osalta vakiokerrointa (esim. 0.8 x N lannoitus mineraalimailla).
#Kari Koppelmäki 13/3/2025

#Tältä pohjalta säädetään turvemaiden typpilannoituksen kerrointa. Sen suuruudeksi asetetaan kautta linjan 80% mineraalimaiden lannoituksesta kullekin kasville

aggreData<-aggreData %>% mutate(`N kg/ha organic soil` = 0.8* `N kg/ha mineral soil`)

#Lannoituksen ravinne-inputin laskenta (ravinnekerroin * ala)
#Peruskertoimet (taulukkoarvoit)

basicOutput<-aggreData %>% mutate(N_kg_mineral = Mineraalia*`N kg/ha mineral soil`,
                     N_kg_organic = Eloperaista *`N kg/ha organic soil`, 
                     P_kg_mineral = Mineraalia * `P kg /ha mineral soil`,
                     P_kg_organic = Eloperaista *`P kg/ha orgainc soil`) %>% group_by(Tuotantosuunta, Kasvikoodi, KASVINIMI_reclass) %>% summarise(across(c(N_kg_mineral,N_kg_organic,P_kg_mineral,P_kg_organic)))

#Tulostyökirja

output<-createWorkbook()
addWorksheet(output,"Taulukkokertoimilla")
writeData(output,"Taulukkokertoimilla",basicOutput)
saveWorkbook(output,file=here("Output/Ravinnedata/lannoitus_kasveille_Karin_kertoimet.xlsx"))
