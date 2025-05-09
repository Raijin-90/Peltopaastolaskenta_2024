library(varhandle);library(readxl);library(here);library(tidyverse)

#' Source specific lines in an R file
#'
#' @param file character string with the path to the file to source.
#' @param lines numeric vector of lines to source in \code{file}.

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

#Kertoimet joilla lasketaan mineraalilannoitteiden käyttöä kasveille syntyvät skriptistä Lannoitteiden_ravinteet_kasvit_ts.R. Nämä ajetaan ensin

source_lines(here("Skriptit/Ravinnelaskenta/Lannoitteiden_ravinteet_kasvit_ts.R"), 1:148)
rm.all.but(c("A","B","C"))

#lannoitetypen tarve kasvilajeittain, sisältäen kaikki tuotantosuunnat. Luomualaa ei ole mukana, sitä ei lannoiteta kivennäisillä. 
Kasveittain<-A 
rm(A)

#Muuten kuten A, mutta sisältää ainoastaan eri eläintilatyypit
Kasveittain_elaintilat<-B
rm(B)

#Sama kuin yllä, mutta eläintilat poissuljettu
Kasveittain_kasvitilat<-C
rm(C)

Kasveittain<-Kasveittain %>% mutate(Viljelyala = Mineraaliala_ha+Elop_ala)
Kasveittain_elaintilat<-Kasveittain_elaintilat %>% mutate(Viljelyala = Mineraaliala+Eloperaista)
Kasveittain_kasvitilat<-Kasveittain_kasvitilat %>% mutate(Viljelyala = Mineraaliala+Eloperaista)

#Kertoimien laskenta. Erillinen kerroin viljelyalalle yhteensä, ja toinen kerroin orgaanisille. 

Kasveittain<-Kasveittain %>% mutate(typpikerroin_kg_ha = N_tarve_yht/Viljelyala,
                       typpikerroin_org_maa_kg_ha = N_tarve_kg_org/Eloperaista)

Kasveittain_elaintilat<-Kasveittain_elaintilat %>% mutate(typpikerroin_kg_ha = N_tarve_yht/Viljelyala,
                                    typpikerroin_org_maa_kg_ha = N_tarve_kg_org/Eloperaista)

Kasveittain_kasvitilat<-Kasveittain_kasvitilat %>% mutate(typpikerroin_kg_ha = N_tarve_yht/Viljelyala,
                                                          typpikerroin_org_maa_kg_ha = N_tarve_kg_org/Eloperaista)

#Eloperäisen maan kerroin voi olla 0/0 -> Nan. 

Kasveittain[is.na(Kasveittain)]<-0
Kasveittain_elaintilat[is.na(Kasveittain_elaintilat)]<-0
Kasveittain_kasvitilat[is.na(Kasveittain_kasvitilat)]<-0




#Tavanomaisen viljelyalan (luomu poisluettu) laskenta

source_lines(here("Skriptit/Ravinnelaskenta/Luomuosuudet_viljelyalasta_gtk.R"), 1:33)

#Tapahtuu liittämällä luomustatus lohkoihin 

lohkot<-left_join(GTKdata, Luomulohkot, by="yhdistettyLohkokoodi")

luomuviljelyssä <- lohkot %>% filter(LUOMUN_VAIHE == "4 Luomuviljelyssä")

sum(luomuviljelyssä$Maannossumma) 

#Jos lohko puuttuu luomualojen listasta, se ei saa arvoa. Se merkitään silloin tavanomaiseen viljelyyn. Jos arvoa ei anna, niin filter suodattaa automaattisesti NA-lohkot pois
lohkot$LUOMUN_VAIHE[is.na(lohkot$LUOMUN_VAIHE)]<-"Tavanomainen_viljely"

#Suodatetaan pois luomuviljelyn lohkot

tavanomaisen_viljelyn_lohkot<-lohkot %>% filter(LUOMUN_VAIHE != "4 Luomuviljelyssä")

#Tarkistetaan alat ja rivimäärä
sum(luomuviljelyssä$Maannossumma)+
sum(tavanomaisen_viljelyn_lohkot$Maannossumma)

nrow(luomuviljelyssä)+
  nrow(tavanomaisen_viljelyn_lohkot)

rm.all.but("tavanomaisen_viljelyn_lohkot")

