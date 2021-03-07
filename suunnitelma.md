
# TIES448 Kääntäjätekniikka - harjoitustyön suunnitelma

## Ryhmä
- Aleksi Tarvainen <a.aleksi.tarvainen@student.jyu.fi>

## Lähdekieli
- työnimi: Krapu
- imperatiivinen, C:n sukuinen
- syntaksin ja joidenkin ominaisuuksien mallina Rust
    - ilman borrow checker, pattern matching, traits ym. hienouksia
    - lausekeorientoituneisuus pyrkimyksenä (esim. silmukka voi palauttaa arvon)

### Pakolliset ominaisuudet (3op):

- [ ] lukukelpoinen (ei binäärimössöä)
- [ ] kommentit
- [ ] kokonaislukuaritmetiikka (infix-syntaksilla, ellei erikseen muuta sovita) 
- [ ] valintojen tekeminen (if tms)
- [ ] toisto (silmukat, rekursio tms)
- [ ] muuttujat
- [ ] jonkinlainen syötteen välitys vähintään ohjelman alussa, esim. muuttujille alkuarvot
- [ ] jonkinlainen tulostus vähintään ohjelman lopussa, esim. muuttujien loppuarvot

### Suunnitellut lisäominaisuudet:

 - [ ] Aliohjelmat (0,5op)
    - [ ] rekursiolla (+0,5op)
- [ ] tietueet ja variantit (1op)
- [ ] staattinen tyypintarkastus (jos aikaa) (1op)

Kohdekieli/tulkkityyppi:
- tulkki (ilman JIT) (0op)

## Työkalut
- isäntäkieli: Haskell
- [BNFC](http://bnfc.digitalgrammars.com/) (mikäli sallittu)
    - vaihtoehtoisesti parser combinators, [trifecta](https://hackage.haskell.org/package/trifecta)
- versionhallinta: Git

## Testaus
Useita esimerkkiohjelmia ja paljon käsin ajelua. Automaattitestit hspec kirjastolla hyödyntäen QuickCheckiä mahdollisuuksien mukaan. Testejä kirjoitetaan jatkuvasti sitä mukaa kuin ominaisuuksia ja kääntäjän osia valmistuu.

## Työnjako
Projekti on kokonaisuudessaan ryhmän ainoan jäsenen, Aleksi Tarvaisen, vastuulla.

## Opintopisteet

Tavoitteena on tienata 5 opintopistettä ja mahdollisesti myös suoritusmerkintä TIES341 Funktio-ohjelmointi 2 -kurssista, koska toteutuskielenä on Haskell (arvioijan harkinnan mukaan).

## Aikataulu
Inkrementit
1. viikot 9-10 (8.3. - 14.3.)
    - kurssimateriaaliin perehtyminen
    - suunnittelu
    - työkaluihin tutustumista
    - suunnitelman palautus 12.3.
2. viikot 11-12 (15.3. - 28.3.)
    - vähintään jäsennin seuraaville
        - kokonaislukuaritmetiikka
        - totuusarvot ja ehtolauseet
    - kommentit
    - testejä jäsentimelle
3. viikot 13-14 (29.3. - 11.4.)
    - muuttujat
    - silmukat (`loop`, `break`, `continue`, ehkä `while`)
    - tulkin toteuttaminen edellisen viikon ja ym. ominaisuuksilla
    - testejä
4. viikot 15-16 (12.4. - 25.4.)
    - jonkinlainen syötteen välitys ja tulostus
    - aliohjelmat (myös rekursiiviset)
    - välitsekki ohjaajalle 16.4. mennessä
5. viikot 17-18 (26.4. - 9.5.)
    - tietueet ja variantit
    - lisää testejä
    - esimerkkiohjelmia
6. viikot 19-21 (10.5. - 28.5.)
    - mahdollisesti (/oletettavasti) esiintyvien puutteiden paikkaamista
    - dokumentaation ja testien täydennys
    - viimeiset esimerkkiohjelmat
    - staattinen tyypitys (jos aikaa)
    - harjoitustyön palautus 28.5. klo 23.59 mennessä