#Viljelyalojen tasokorjausskriptin automatisoitu versio / HV 20.2.2024

library(tidyverse);library(here)

#Aja viljelyalojen aggregointi- ja muotoiluskripti. 

source(here("Skriptit/Uudet skriptit/Viljelyalan_muotoiluautomatiikka.R"))

#ETTL koodi rivinimiksi muuttujan sijasta. Saadaan kaikki muuttujat numeerisiksi ja laskenta helpottuu. 

for (i in Listat){
  
  rownames(Combined_list[[i]])<-Combined_list[[i]][[1]]  
  Combined_list[[i]][[1]]<-NULL

}

listaNimet<-names(Combined_list) #listanimet talteen. 

#Kullekin kategorialle on laskettu valmiiksi erotus, paljonko NIR:n totaaliala eroaa datan vastaavasta kategorioittain. 
#Tämä kannattaa tuottaa ennakkoon. 
#Viljelyalaa korotetaan/lasketaan erotuksen verran per kategoria. Lopputuloksena ala on sama kuin NIR:ssä. 

Jaettavaa <- read_excel(here("Data/Tasokorotettavat_maarat.xlsx"), 
                        sheet = "Data")

#Näiden arvojen rivijärjestys muutetaan samaksi kuin Combined_listissä Arvot Jaettavaa-taulussa laitetaan noudattamaan samaa nimeämistapaa.  Näin varmistetaan, että 1. rivin jaettava hehtaarimäärä vastaa sitä joka tarvitaan Data_listin 1. jäseneen. 

Jaettavaa<-Jaettavaa %>% arrange(factor(Category, levels=names(Combined_list)))


#Lasketaan inventaarin ja datan viljelyalan erotuksena kullekin tuotantosuunnalle kuuluva korjausosa hehtaareita 
#Alan jakauma tuotantosuunnittain datassa. Summataan alat tuotantosuunnittain, jaetaan datan pinta-alalla yhteensä --> jakauma %  

Jakaumafunktio <- function(x) {
  colSums(x) /
    sum(colSums(x))
}

t<-map(Combined_list, Jakaumafunktio)
rm(Jakaumafunktio)

#Nyt tiedetään viljelyalan jakauma sarakkeittain eli tuotantosuunnittain, kullekin 16 kategorialistalle. 

#lasketaan jakauman (t) perusteella, montako hehtaaria Jaettavat-listasta kullekin  tuotantosuunnista pitää antaa lisää/vähentää. 
#Jaettavaa-taulun kukin rivi (jaettava määrä) kerrotaan vastaavalla t - dataframen listalla (eli alan jakaumalla tuotantosuunnittain). 

a<-as.list(Jaettavaa$Tasokorjattava_hehtaarimäärä) #Jaettavat kokonaismäärät hehtaareina irti. 

Listat<-seq_along(Combined_list) #Listojen numerot 1-16, joiden yli loopataan.

b<-list() #tyhjä lista lopputuloksille

for(i in Listat){
  
b[[i]]<-a[[i]]*t[[i]] #jaettava määrä * tuotantosuunnalle kuuluva % osa viljelyalaa    
  
}

rm(t)

#Tämä jaettava kokonaismäärä hehtaareita, eli yllä laskettu b. on nyt summattava tuotantosuuntien alkuperäisiin sarakesummiin, 

#Alkuperäiset sarakesummat

c<-function(x){
  
  x %>% colSums
  
}

#Lasketaan originaalit sarakesummat joka tuotantosuunnalle

oldColSums<-map(Combined_list, c)
#Ynnätään originaalit sarakesummat ja niille kuuluva osa tasokorjattavaa määrää. Saadaan uudet, tasokorjatut sarakesummat.

newColSums<-list()


for (i in Listat){
  
newColSums[[i]]<-b[[i]]+oldColSums[[i]]  
  
}
names(newColSums)<-listaNimet
rm(a, b)

#Seuraavaksi nämä on jaettava tuotteille. Kuten tähänkin asti, sama operaatio toistetaan jokaiselle 16 listalle. 
#Muutetaan tuotejakauma listoittain hehtaareista prosenteiksi


#Jakaumamuunto: tuotesarake jaetaan sen rivisummalla

jakaumaTuotteille<-function(x){x/sum(x)} #sarakkeen luku per sarakesumma = jakauma% 

jak<-list() #tyhjä lista tuloksille
for(i in Listat)
{jak[[i]]<-apply(Combined_list[[i]],2, jakaumaTuotteille)
}

nollat<-function(x){ #joillain riveillä tulee 0/0 tilanteita, Nan tai NA. Muunto nolliin
x %>% replace(is.nan(x), 0)
}

jak<-map(jak, nollat)

#newColSums sisältää uudet, korotetut sarakesummat listakohtaisesti. Kerrotaan jokaisen listan sarakekohtaista jakaumaa lasketulla, korotetulla sarakesummalla. 

uudetAlat<-list() #korotettujen alojen tallennus

for(i in Listat){
  
uudetAlat[[i]]<- sweep(jak[[i]],2,newColSums[[i]], FUN = "*")   
  
}



#Työkirjan luonti

korjatutAlat<-createWorkbook()

for (i in seq_along(listaNimet)){
  
  korjatutAlat %>% addWorksheet(listaNimet[i])
  korjatutAlat %>% writeData(listaNimet[i], as.data.frame(uudetAlat[i]), colNames = T, rowNames = T)
}

saveWorkbook(korjatutAlat, file=here("kokeilu.xlsx"), overwrite = T)

print("Tasokorjattu tuloste muodostettu")

rm(list=ls())

