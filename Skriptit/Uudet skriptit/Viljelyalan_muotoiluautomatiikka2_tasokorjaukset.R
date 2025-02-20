#Viljelyalojen tasokorjausskriptin automatisoitu versio / HV 20.2.2024

library(tidyverse);library(here)

#Aja viljelyalojen aggregointi- ja muotoiluskripti

source(here("Skriptit/Uudet skriptit/Viljelyalan muotoiluautomatiikka.R"))


Combined_list_corrected<-Combined_list

#Kullekin kategorialle on laskettu valmiiksi erotus, paljonko NIR:n totaaliala eroaa datan vastaavasta.
#Tämä kannattaa tuottaa ennakkoon. 
#Viljelyalaa korotetaan/lasketaan sen verran per kategoria. Lopputuloksena ala on sama kuin NIR:ssä. 

Jaettavaa <- read_excel("D:/Peltopaastolaskenta_2024/Tasokorotettavat_maarat.xlsx", 
                        sheet = "Data")

#Näiden arvojen rivijärjestys muutetaan samaksi kuin Data_listissä. Näin varmistetaan, että 1. rivin jaettava hehtaarimäärä vastaa sitä joka tarvitaan Data_listin 1. jäseneen. 

Jaettavaa<-Jaettavaa %>% arrange(Category, names(Data_list))

#Lasketaan inventaarin ja datan viljelyalan erotuksesta kullekin tuotantosuunnalle menevä osa 
#Alan jakauma tuotantosuunnittain datassa. Summataan alat tuotantosuunnittain, jaetaan datan pinta-alalla yhteensä --> jakauma %  

Jakaumafunktio <- function(x) {
  colSums(x[2:length(x)]) /
    sum(colSums(x[2:length(x)]))
}

t<-map(Combined_list_corrected, Jakaumafunktio)

#Nyt tiedetään viljelyalan jakauma sarakkeittain eli tuotantosuunnittain kullekin 16 listalle. 
#Sitten lasketaan, montako hehtaaria tämä tarkoittaa. Jaettavaa-taulun kukin rivi kerrotaan vastaavalla t - dataframen listalla. 




Jaettavaa$Tasokorjattava_hehtaarimäärä[Jaettavaa$Category == "GrassPerMinRaiv"]
