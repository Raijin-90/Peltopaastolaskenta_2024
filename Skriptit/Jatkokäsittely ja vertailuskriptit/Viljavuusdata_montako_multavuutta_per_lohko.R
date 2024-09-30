
#Viljavuusdatan tulisi toimia niin, että 1 peruslohkokoodia vastaa vain 1 multavuuden luokka, joka määrää maalajin. Onko näin?
Tarkistus<-filter(Viljavuus_aggregointi_multavuus, duplicated(PLTUNNUS)) #Toistuva PLTUNNUS kuvaa eri kasvulohkoja, vaikka sen kirjainkoodi ei vaihdukaan. Viljavuusdata ei tunne kirjainmuuttujaa natiivisti, vaan kuvaa eri kasvulohkot riveinä joilla sama pltunnus mutta muut asiat eriäviä. 
Tarkistus<-select(Tarkistus, PLTUNNUS, Multavuusluokka)
Tarkistus$laskuri<-1
x<-Tarkistus %>% group_by(PLTUNNUS, Multavuusluokka) %>% summarise(yht = sum(laskuri)) #Montaako eri multavuusluokkaa samalle pltunnukselle on tarjottu?
y<-x %>% filter(duplicated(PLTUNNUS)) #Jos Pltunnus toistuu, sille tarjotaan useampaa kuin yhtä multavuutta. Näin ei tulisi olla. 
# 0 riviä. 1 lohko = 1 multavuus
