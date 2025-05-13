#Mineraalilannoitteiden käytön laskenta peltolohkodatasta,
#Kari Koppelmäen lannoitustarve-arvioihin perustuen. 
#HV 09052025

#Mineraalilannoitteista peräisin olevan typen massa lasketaan sekä aggregoidusti, että joka lohkolle erikseen. 

#Data ja paketit

library(varhandle);library(readxl);library(here);library(tidyverse)


#' Source specific lines in an R file
#'
#' @param file character string with the path to the file to source.
#' @param lines numeric vector of lines to source in \code{file}.

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

#KERROINTEN LASKENTA
#Kertoimet joilla lasketaan mineraalilannoitteiden käyttöä kasveille syntyvät skriptistä Lannoitteiden_ravinteet_kasvit_ts.R. Nämä ajetaan ensin

source_lines(here("Skriptit/Ravinnelaskenta/Lannoitteiden_ravinteet_kasvit_ts.R"), 1:148)
rm.all.but(c("A","B","C"))

#lannoitetypen tarve kasvilajeittain, sisältäen kaikki tuotantosuunnat. Luomualaa ei ole mukana, sitä ei lannoiteta kivennäisillä. 
Kertoimet_kasveittain<-A 
rm(A)

#Muuten kuten A, mutta sisältää ainoastaan eri eläintilatyypit
Kertoimet_kasveittain_elaintilat<-B
rm(B)

#Sama kuin yllä, mutta eläintilat poissuljettu
Kertoimet_kasveittain_kasvitilat<-C
rm(C)

Kertoimet_kasveittain<-Kertoimet_kasveittain %>% mutate(Viljelyala = Mineraaliala+Eloperaista)
Kertoimet_kasveittain_elaintilat<-Kertoimet_kasveittain_elaintilat %>% mutate(Viljelyala = Mineraaliala+Eloperaista)
Kertoimet_kasveittain_kasvitilat<-Kertoimet_kasveittain_kasvitilat %>% mutate(Viljelyala = Mineraaliala+Eloperaista)

#Kertoimien laskenta. Erillinen kerroin viljelyalalle yhteensä, ja toinen kerroin orgaanisille. 

Kertoimet_kasveittain<-Kertoimet_kasveittain %>% mutate(typpikerroin_kg_ha = N_tarve_yht/Viljelyala,
                       typpikerroin_org_maa_kg_ha = N_tarve_kg_org/Eloperaista)

Kertoimet_kasveittain_elaintilat<-Kertoimet_kasveittain_elaintilat %>% mutate(typpikerroin_kg_ha = N_tarve_yht/Viljelyala,
                                    typpikerroin_org_maa_kg_ha = N_tarve_kg_org/Eloperaista)

Kertoimet_kasveittain_kasvitilat<-Kertoimet_kasveittain_kasvitilat %>% mutate(typpikerroin_kg_ha = N_tarve_yht/Viljelyala,
                                                          typpikerroin_org_maa_kg_ha = N_tarve_kg_org/Eloperaista)

#Eloperäisen maan kerroin voi olla 0/0 -> Nan. 

Kertoimet_kasveittain[is.na(Kertoimet_kasveittain)]<-0
Kertoimet_kasveittain_elaintilat[is.na(Kertoimet_kasveittain_elaintilat)]<-0
Kertoimet_kasveittain_kasvitilat[is.na(Kertoimet_kasveittain_kasvitilat)]<-0

print("Mineraalilannoituskertoimet laskettu")

#VILJELYALAN LASKENTA
#Tavanomaisen viljelyalan (luomu poisluettu) laskenta.

source_lines(here("Skriptit/Ravinnelaskenta/Luomuosuudet_viljelyalasta_gtk.R"), 1:33)

#Tapahtuu liittämällä luomustatus lohkoihin 

kaikki_lohkot<-left_join(GTKdata, Luomulohkot, by="yhdistettyLohkokoodi")

lohkot_luomuviljelyssä <- kaikki_lohkot %>% filter(LUOMUN_VAIHE == "4 Luomuviljelyssä")

sum(lohkot_luomuviljelyssä$Maannossumma) 

#Jos lohko puuttuu luomualojen listasta, se ei saa arvoa. Se merkitään silloin tavanomaiseen viljelyyn. Jos arvoa ei anna, niin filter suodattaa automaattisesti NA-kaikki_lohkot pois
kaikki_lohkot$LUOMUN_VAIHE[is.na(kaikki_lohkot$LUOMUN_VAIHE)]<-"Tavanomainen_viljely"

#Suodatetaan pois luomuviljelyn kaikki_lohkot

tavanomaisen_viljelyn_lohkot<-kaikki_lohkot %>% filter(LUOMUN_VAIHE != "4 Luomuviljelyssä")

#Tarkistetaan alat ja rivimäärä
sum(lohkot_luomuviljelyssä$Maannossumma)+
sum(tavanomaisen_viljelyn_lohkot$Maannossumma)

nrow(lohkot_luomuviljelyssä)+
  nrow(tavanomaisen_viljelyn_lohkot)

sum(lohkot_luomuviljelyssä$Maannossumma)+
  sum(tavanomaisen_viljelyn_lohkot$Maannossumma)

print("Tavanomainen viljelyala laskettu")

rm.all.but(c("tavanomaisen_viljelyn_lohkot", "Kertoimet_kasveittain","Kertoimet_kasveittain_elaintilat","Kertoimet_kasveittain_kasvitilat"))


#PINTA-ALOJEN AGGREGOINTI

#Aggregoidut alat

#Eläintilojen nimet
Animalfarms<-c("Hevostilat","Lammas- ja vuohitilat","Maitotilat", "Munatilat","Muut nautakarjatilat","Siipikarjatilat","Sikatilat","Turkistilat")

#kaikki_lohkot Kertoimet_kasveittain, kaikki tilatyypit
aggregoidut_lohkot_kasveittain<-tavanomaisen_viljelyn_lohkot %>% group_by(KASVIKOODI_lohkodata_reclass,KASVINIMI_reclass) %>% summarise(Eloperaista = sum(Eloperaista),
                                                                                                        Mineraalia = sum(Mineraalia ),
                                                                                                        Ala_yht = sum(Maannossumma)) 
#eläintilojen kaikki_lohkot Kertoimet_kasveittain
aggregoidut_lohkot_kasveittain_elaintilat<-tavanomaisen_viljelyn_lohkot %>% 
  filter(Tuotantosuunta %in% Animalfarms) %>% 
  group_by(KASVIKOODI_lohkodata_reclass,KASVINIMI_reclass) %>% 
  summarise(Eloperaista = sum(Eloperaista), 
            Mineraalia = sum(Mineraalia),
            Ala_yht = sum(Maannossumma))
                                                                                                                                                    
#Kasvitilojen kaikki_lohkot

#eläintilojen kaikki_lohkot Kertoimet_kasveittain
aggregoidut_lohkot_kasveittain_kasvitilat<-tavanomaisen_viljelyn_lohkot %>% 
  filter(!(Tuotantosuunta %in% Animalfarms)) %>% 
  group_by(KASVIKOODI_lohkodata_reclass,KASVINIMI_reclass) %>% 
  summarise(Eloperaista = sum(Eloperaista), 
            Mineraalia = sum(Mineraalia),
            Ala_yht = sum(Maannossumma))

rm.all.but(
  c("aggregoidut_lohkot_kasveittain",
  "aggregoidut_lohkot_kasveittain_elaintilat",
  "aggregoidut_lohkot_kasveittain_kasvitilat",
  "Kertoimet_kasveittain_elaintilat",
  "Kertoimet_kasveittain_kasvitilat",
  "Kertoimet_kasveittain",
  "tavanomaisen_viljelyn_lohkot")
)

#Kerrointen liittäminen aggregaatteihin
colnames(aggregoidut_lohkot_kasveittain)[1:2]<-c("Kasvikoodi","Kasvinimi")
Kertoimet_kasveittain<-Kertoimet_kasveittain %>% select(Kasvikoodi,Kasvinimi,typpikerroin_kg_ha,typpikerroin_org_maa_kg_ha)
Typpitarve_kasveittain<-inner_join(Kertoimet_kasveittain,aggregoidut_lohkot_kasveittain, by = c("Kasvikoodi","Kasvinimi"))

colnames(aggregoidut_lohkot_kasveittain_elaintilat)[1:2]<-c("Kasvikoodi","Kasvinimi")
Kertoimet_kasveittain_elaintilat<-Kertoimet_kasveittain_elaintilat %>% select(Kasvikoodi,Kasvinimi,typpikerroin_kg_ha,typpikerroin_org_maa_kg_ha)
Typpitarve_kasveittain_elaintilat<-inner_join(Kertoimet_kasveittain_elaintilat,aggregoidut_lohkot_kasveittain_elaintilat, by = c("Kasvikoodi","Kasvinimi"))

colnames(aggregoidut_lohkot_kasveittain_kasvitilat)[1:2]<-c("Kasvikoodi","Kasvinimi")
Kertoimet_kasveittain_kasvitilat <- Kertoimet_kasveittain_kasvitilat  %>% select(Kasvikoodi,Kasvinimi,typpikerroin_kg_ha,typpikerroin_org_maa_kg_ha)
Typpitarve_kasveittain_kasvitilat<-inner_join(Kertoimet_kasveittain_kasvitilat,aggregoidut_lohkot_kasveittain_kasvitilat, by = c("Kasvikoodi","Kasvinimi"))

#ravinnekilojen laskenta aggregoituna kasvikohtaisesti : 

Typpitarve_kasveittain<-Typpitarve_kasveittain %>% mutate(kg_n_koko_ala = typpikerroin_kg_ha*Ala_yht,
                       kg_n_org = typpikerroin_org_maa_kg_ha*Eloperaista)


Typpitarve_kasveittain_elaintilat<-Typpitarve_kasveittain_elaintilat %>% mutate(kg_n_koko_ala = typpikerroin_kg_ha*Ala_yht,
                                    kg_n_org = typpikerroin_org_maa_kg_ha*Eloperaista)

Typpitarve_kasveittain_kasvitilat<-Typpitarve_kasveittain_kasvitilat %>% mutate(kg_n_koko_ala = typpikerroin_kg_ha*Ala_yht,
                                                         kg_n_org = typpikerroin_org_maa_kg_ha*Eloperaista)


rm.all.but(c("Typpitarve_kasveittain","Typpitarve_kasveittain_elaintilat","Typpitarve_kasveittain_kasvitilat","tavanomaisen_viljelyn_lohkot","Kertoimet_kasveittain","Kertoimet_kasveittain_kasvitilat","Kertoimet_kasveittain_elaintilat"))

#LOHKOKOHTAINEN TYPPITARVE
#Kerrointen sovittaminen yksittäisiin lohkoihin.

#Jaetaan kaikki_lohkot tuotantosuuntien perusteella. Kasvitila-kertoimet kasvitiloihin, eläintila-kertoimet eläintiloihin. 

Animalfarms<-c("Hevostilat","Lammas- ja vuohitilat","Maitotilat", "Munatilat","Muut nautakarjatilat","Siipikarjatilat","Sikatilat","Turkistilat")

kasvitilalohkot<- tavanomaisen_viljelyn_lohkot %>% filter(!(Tuotantosuunta %in% Animalfarms))

elaintilalohkot<- tavanomaisen_viljelyn_lohkot %>% filter((Tuotantosuunta %in% Animalfarms))
  
#Kertoimien liittäminen

colnames(Kertoimet_kasveittain_kasvitilat)[1:2]<-c("KASVIKOODI_lohkodata_reclass","KASVINIMI_reclass")
Kertoimet_kasveittain_kasvitilat <- Kertoimet_kasveittain_kasvitilat %>% select(-KASVINIMI_reclass)

colnames(Kertoimet_kasveittain_elaintilat)[1:2]<-c("KASVIKOODI_lohkodata_reclass","KASVINIMI_reclass")
Kertoimet_kasveittain_elaintilat <- Kertoimet_kasveittain_elaintilat %>% select(-KASVINIMI_reclass)

nrow(inner_join(kasvitilalohkot, Kertoimet_kasveittain_kasvitilat, by=c("KASVIKOODI_lohkodata_reclass"))) 
kasvitilalohkot<-left_join(kasvitilalohkot, Kertoimet_kasveittain_kasvitilat, by=c("KASVIKOODI_lohkodata_reclass"))
              
nrow(inner_join(elaintilalohkot, Kertoimet_kasveittain_elaintilat, by=c("KASVIKOODI_lohkodata_reclass"))) 
elaintilalohkot<-left_join(elaintilalohkot, Kertoimet_kasveittain_kasvitilat, by=c("KASVIKOODI_lohkodata_reclass")) 

rm.all.but(c("Typpitarve_kasveittain", "Typpitarve_kasveittain_elaintilat", "Typpitarve_kasveittain_kasvitilat", "kasvitilalohkot", "elaintilalohkot"))

#Lohkoittainen typpikilojen laskenta. Kerrotaan typpikertoimella maalajin hehtaarimäärää, joko kokonaisalaa tai eloperäistä. 


kasvitilalohkot$typpikerroin_kg_ha[is.na(kasvitilalohkot$typpikerroin_kg_ha)]<-0
kasvitilalohkot$typpikerroin_org_maa_kg_ha[is.na(kasvitilalohkot$typpikerroin_org_maa_kg_ha)]<-0

elaintilalohkot$typpikerroin_kg_ha[is.na(elaintilalohkot$typpikerroin_kg_ha)]<-0
elaintilalohkot$typpikerroin_org_maa_kg_ha[is.na(elaintilalohkot$typpikerroin_org_maa_kg_ha)]<-0


Lohkoittainen_min_lann_typpi_kasvitilat<-kasvitilalohkot %>% mutate(Mineraalilannoitteen_typpi = typpikerroin_kg_ha*Maannossumma,
                           Mineraalilannoitteen_typpi_eloper_maa = typpikerroin_org_maa_kg_ha*Eloperaista) 

Lohkoittainen_min_lann_typpi_elaintilat<-elaintilalohkot %>% mutate(Mineraalilannoitteen_typpi = typpikerroin_kg_ha*Maannossumma,
                                                                    Mineraalilannoitteen_typpi_eloper_maa = typpikerroin_org_maa_kg_ha*Eloperaista) 

#Vastaa suunnilleen karin kertoimista laskettua totaalia n. 171 000 tn. 
sum(Lohkoittainen_min_lann_typpi_elaintilat$Mineraalilannoitteen_typpi)/1000+sum(Lohkoittainen_min_lann_typpi_kasvitilat$Mineraalilannoitteen_typpi)/1000

#Käyttömäärien aggregointi tuotantosuunnille. 

Elaintila_typpi_aggre<-Lohkoittainen_min_lann_typpi_elaintilat %>% group_by(Tuotantosuunta) %>% summarise(Mineraalilannoitteiden_typpi_kg_kaikki_maa = sum(Mineraalilannoitteen_typpi),
                                                                                   Mineraalilannoitteiden_typpi_elop_maa = sum(Mineraalilannoitteen_typpi_eloper_maa)) 

Kasvitilat_tyypi_aggre<-Lohkoittainen_min_lann_typpi_kasvitilat %>% group_by(Tuotantosuunta) %>% summarise(Mineraalilannoitteiden_typpi_kg_kaikki_maa= sum(Mineraalilannoitteen_typpi),
                                                                                   Mineraalilannoitteiden_typpi_elop_maa = sum(Mineraalilannoitteen_typpi_eloper_maa)) 

                                                                                   
Mineraalilannoitteiden_typpi_aggre<-rbind(Elaintila_typpi_aggre, Kasvitilat_tyypi_aggre)

library(openxlsx)
write.xlsx(Mineraalilannoitteiden_typpi_aggre, file=here("Output/Ravinnedata/Emissiotulokset/Mineraalilannoitteiden_typpi_tuotantosuunnittain.xlsx"))
