#Viljelyalojen tasokorjausskriptin automatisoitu versio / HV 20.2.2024

library(tidyverse);library(here)

#Aja viljelyalojen aggregointi- ja muotoiluskripti. 

source(here("Skriptit/Uudet skriptit/Viljelyalan muotoiluautomatiikka.R"))

#Jätetään korottamaton versio varmuudeksi talteen. 

Combined_list_corrected<-Combined_list

#Kullekin kategorialle on laskettu valmiiksi erotus, paljonko NIR:n totaaliala eroaa datan vastaavasta kategorioittain. 
#Tämä kannattaa tuottaa ennakkoon. 
#Viljelyalaa korotetaan/lasketaan erotuksen verran per kategoria. Lopputuloksena ala on sama kuin NIR:ssä. 

Jaettavaa <- read_excel("D:/Peltopaastolaskenta_2024/Tasokorotettavat_maarat.xlsx", 
                        sheet = "Data")

#Näiden arvojen rivijärjestys muutetaan samaksi kuin Data_listissä. Arvot Jaettavaa-taulussa laitetaan noudattamaan samaa nimeämistapaa.  Näin varmistetaan, että 1. rivin jaettava hehtaarimäärä vastaa sitä joka tarvitaan Data_listin 1. jäseneen. 

Jaettavaa<-Jaettavaa %>% arrange(factor(Category, levels=names(Data_list)))

#Lasketaan inventaarin ja datan viljelyalan erotuksesta kullekin tuotantosuunnalle menevä osa 
#Alan jakauma tuotantosuunnittain datassa. Summataan alat tuotantosuunnittain, jaetaan datan pinta-alalla yhteensä --> jakauma %  

Jakaumafunktio <- function(x) {
  colSums(x[2:length(x)]) /
    sum(colSums(x[2:length(x)]))
}

t<-map(Combined_list_corrected, Jakaumafunktio)

#Nyt tiedetään viljelyalan jakauma sarakkeittain eli tuotantosuunnittain kullekin 16 listalle. 
#Sitten lasketaan, montako hehtaaria tämä tarkoittaa. Jaettavaa-taulun kukin rivi kerrotaan vastaavalla t - dataframen listalla. 

#Lopputulos b-framessa kertoo joka kategoriasta, kuinka iso hehtaarimäärä sen kategorian sisällä pitää jakaa kunkin tuotantosuunnan tuotteille   

a<-as.list(Jaettavaa$Tasokorjattava_hehtaarimäärä) #Jaettavat kokonaismäärät hehtaareina irti. 

Listat<-seq_along(Combined_list_corrected) #Listojen numerot 1-16, joiden yli loopataan.

b<-list() #tyhjä lista lopputuloksille

for(i in Listat){
  
b[[i]]<-a[[i]]*t[[i]]    
  
}
