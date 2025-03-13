# Kasvi- ja tuotantosuuntakohtainen lannoite-input Kari Koppelmäen lannoitekertoimista7
# HV 13032025

library(varhandle);library(readxl);library(here);library(tidyverse)

#Lannoitusaineisto sisään, kertoimet siistiin formaattiin

Lannoitusdata <- read_excel(here("Data/Ravinnelaskennan_aineisto/Kasvilista_lannoitus.xlsx"))
Lannoitus<-Lannoitusdata %>% select(-`Sato tn/ha`,-`N min kg/ha`,-`N maks kg/ha`,-Kommentit,-Lähteet)

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
colnames(aggreData)[colnames(aggreData)=="Kasvikoodi_lohkodata_reclass"]<-"Kasvikoodi" 
                                                                                                                     





#Liitetään lannoituskerroin aineistoon

GTKdata<-left_join(GTKdata, Lannoitus, by="KASVIKOODI_lohkodata_reclass")

#Tehdään muuttuja jonka pohjalta voi erotella lohkot, joista löytyy ainakin yksi lannoituskerroin. 
#Datassa on 4 eri lannoituskerrointa. 
#Jos kaikkien eri kerrointen summa lohkolla on NA, niin sellaiselle riville ei ole minkäänlaista lannoituskerrointa. Nämä poistetaan laskuista

GTKdata<-GTKdata %>% mutate(Suodatin = `N kg/ha mineral soil`+`N kg/ha organic soil`+`P kg/ha orgainc soil`+`P kg /ha mineral soil`)
GTKdata<-GTKdata %>% filter(!(is.na(Suodatin)))  


