# Sanulin alkumuuvit

> This is about "hacking" the Finnish word game
> [Sanuli](https://sanuli.fi/) which is a clone of
> [Wordle](https://www.powerlanguage.co.uk/wordle/) with Finnish
> language words.

Tällä näpsäkällä tiedonmurskaajalla saa pyöräytettyä Sanuliin sopivat
aloitussanat eli alkumuuvit. Hyvä alkumuuvi on sellainen joka käy läpi
yleisimmät kirjaimet eikä sisällä samaa kirjainta useampaan
kertaan. Ideaalisti kahdella sanalla saa testattua 10 eri kirjainta ja
kolmella 15, kun viiden sanan peli on käytössä.

## Algoritmi

1. Luetaan sanat Kotuksen [nykysuomen
   sanalistasta](https://kaino.kotus.fi/sanat/nykysuomi/).
2. Lisätään ja/tai poistetaan sanoja, jotta lista saadaan vastaamaan
   Sanulin käyttämää listaa. Erot määritetty tiedostossa
   [sanuli-patch.yaml](sanuli-patch.yaml).
3. Poimitaan tietyn mittaiset sanat, esim. viisi kirjainta pitkät.
4. Suodatetaan pois sanalistasta merkit, joita sanulissa ei käytetä,
   kuten *Š*.
5. Lasketaan kirjainten frekvenssi näistä tietyn mittaisista sanoista
6. Poimitaan useimmin esiintyvät kirjaimet. Esim. kolmen sanan haussa
   poimitaan 15 yleisintä kirjainta.
7. Poimitaan sanalistasta sanat, jotka koostuvat näistä kirjaimista.
8. Suodatetaan pois vielä lopuksi sanat, joissa sama kirjain esiintyy
   useamman kuin kerran
9. Käydään läpi kaikki yhdistelmät ja poimitaan sellaiset, joissa
   kukin kirjain esiintyy vain kerran.

Algoritmi löytyy tiedostosta [Main.hs](Main.hs).

Sanoja on verrattain paljon ja ylläoleva algoritmi on melko hidas
ilman tiettyjä optimointeja. Testattavien sanojen määrää vähennetään
yhdistämällä sanat, jotka ovat toistensa anagrammeja samaksi
testialkioksi. Esimerkiksi sanat *kotva* ja *votka* käsitellään yhtenä
ja vasta tulostusvaiheessa näytetään erikseen.

## Esimerkkitulosteita

### Viisikirjaimiset sanat

* [Kaksi sanaa](https://zouppen.iki.fi/poista/sanuli5-2.txt)
* [Kolme sanaa](https://zouppen.iki.fi/poista/sanuli5-3.txt)
* [Neljä sanaa](https://zouppen.iki.fi/poista/sanuli5-4.txt)

## Parannettavaa

Listatuille avausmuuveille voisi laskea vielä jonkinlaisen
hyvyysarvon, jolloin voisi löytää sen parhaan.

## Taustaa

Olen huono sanapeleissä ja jotta en häviäisi, piti keksiä jotakin. Ei
kiinnostanut kuitenkaan tehdä täysin automaattisesti pelaavaa bottia,
vaan ajattelin että riittäisi kehitellä hyvät aloitussiirrot, jonka
jälkeen peli helpottuu huomattavasti.

Ja helpottuuhan se.

Softan on kirjoittanut Joel Lehtonen, joel.lehtonen ät
iki.fi. Lisenssi on GNU GPL versio 3 tai myöhempi versio.
