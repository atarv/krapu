
# TIES448 Kääntäjätekniikka - harjoitustyön suunnitelma

## Ryhmä
- Aleksi Tarvainen <a.aleksi.tarvainen@student.jyu.fi>

## Lähdekieli
- työnimi: Krapu
- paradigma: imperatiivinen
- syntaksin ja joidenkin ominaisuuksien mallina Rust
    - lausekeorientoituneisuus pyrkimyksenä (esim. silmukka voi palauttaa arvon)
    - ilman borrow checker, pattern matching, traits ym. hienouksia
- tietotyypit: 64bit etumerkilliset kokonaisluvut, totuusarvot, tietueet, variantit, merkkijonot

### Pakolliset ominaisuudet (3op):

- [ ] lukukelpoinen (ei binäärimössöä)
- [ ] kommentit
- [ ] kokonaislukuaritmetiikka (infix-syntaksilla) 
- [ ] valintojen tekeminen (if)
- [ ] toisto (silmukat)
- [ ] muuttujat
- [ ] jonkinlainen syötteen välitys vähintään ohjelman alussa, esim. muuttujille alkuarvot
- [ ] jonkinlainen tulostus vähintään ohjelman lopussa, esim. muuttujien loppuarvot

Kohdekieli/tulkkityyppi: 
- tulkki (ilman välikieltä) (0op)

Tuetut alustat:
- GNU/Linux AMD64:llä

### Suunnitellut lisäominaisuudet:

 - [ ] aliohjelmat (0,5op)
    - [ ] rekursiolla (+0,5op)
- [ ] merkkijonosyöte ja -tulostus (0,5op)
- [ ] tietueet ja variantit (1op)
- [ ] yksinkertainen staattinen tyypintarkastus (jos aikaa) (1op)


## Työkalut
- isäntäkieli: Haskell (GHC 8.10)
- ~[BNFC](http://bnfc.digitalgrammars.com/) (mikäli sallittu)~
    - vaihtoehtoisesti parser combinators, Megaparsec
- versionhallinta: Git
    - etävaraston osoite: [https://github.com/atarv/krapu](https://github.com/atarv/krapu)

## Testaus
Useita esimerkkiohjelmia ja paljon käsin ajelua. Automaattitestit hspec kirjastolla hyödyntäen QuickCheckiä mahdollisuuksien mukaan. Testejä kirjoitetaan jatkuvasti sitä mukaa kuin ominaisuuksia ja kääntäjän osia valmistuu.

## Työnjako
Projekti on kokonaisuudessaan ryhmän ainoan jäsenen, Aleksi Tarvaisen, vastuulla.

## Opintopisteet

Tavoitteena on tienata 6 opintopistettä ja mahdollisesti myös suoritusmerkintä TIES341 Funktio-ohjelmointi 2 -kurssista, koska toteutuskielenä on Haskell (arvioijan harkinnan mukaan).

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

## Esimerkkiohjelmia

```rust
/* Klassikko.
 * Samalla esimerkki monirivisestä kommentista.
 */
fn main() {
    println("Hei, maailma!");
}
```

```rust
fn main() {
    // Rustista eroten muuttujat on oletuksena mutable. 
    // Vakioiden toteutus mahdollisesti staattisen tyypityksen yhteydessä.
    let x: I64 = 0;

    loop {
        if x > 100 {
            break;
        }

        println(
            // Tässä if-lausekkeen tuloksena on merkkijono
            if x % 3 == 0 && x % 5 == 0 {
                "FizzBuzz" // puuttuva puolipiste merkkaa palautettavaa arvoa
            } else if x % 5 == 0 {
                "Buzz"
            } else if x % 3 == 0 {
                "Fizz"
            } else {
                // Mahdollisesti toteutettava staattinen tyypintarkastus
                // pitää huolen siitä, että tämä ei kääntyisi ilman
                // kokonaisluvun muuttamista merkkijonoksi.
                i64_to_str(x)
            }
        );

        x = x + 1;
    }
}
```

```rust
// Tietue
struct Henkilo {
    ika: I64,
    nimi: String
}

// Aliohjelma
fn max(a: I64, b: I64) -> I64 { // Tyyppinimet alkaa isolla kirjaimella
    if a >= b {
        a
    } else {
        b
    }
}

fn main() {
    let a: Henkilo = Henkilo { ika: 20, nimi: "Janna-Petteri" };
    let b: Henkilo = Henkilo { ika: 30, nimi: "Joni-Jossu" };

    a.ika = a.ika + 5;

    println(max(a.ika, b.ika));
}
```

```rust
fn kertoma(n: I64) -> I64 {
    if n <= 1 {
        1
    } else {
        // rekursiivinen kutsu
        n + kertoma(n - 1)
    }
}

fn main() {
    println(i64_to_str(kertoma(10)));
}
```

```rust
fn main() {                                                                      
    let x: I64 = 1;                                                               
                                                                                
    // loop on myös lauseke. Jos breakille ei anna arvoa, palautetaan Unit
    let y: I64 = loop {                                                               
        if x > 100 {                                                             
            break x;  
        }                                                                        
                                                                                
        x = x + x + 3;                                                              
    };                                                                           
                                                                                
    println(i64_to_str(y));
 }                                                                                
```
