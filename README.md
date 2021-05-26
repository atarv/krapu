# krapu

## Ryhmä

-   Aleksi Tarvainen <a.aleksi.tarvainen@student.jyu.fi>
    -   Vastuut:
        -   koko projekti

## Kääntäminen ja ajaminen

Varmista, että sinulla on asennettuna `stack`. Jos ei, noudata näitä
[asennusohjeita](https://docs.haskellstack.org/en/stable/README/#how-to-install).
Toimivuus on todettu ainoastaan Linuxilla (AMD64), mutta hyvällä tuurilla toimii
luultavasti muillakin alustoilla.

Suorittamalla komennon `stack run -- -r` Git-hakemiston juuressa tulkki kääntyy
ja käynnistyy REPL:iin, johon voi kirjoittaa esimerkiksi lauseen `println("Hei,
maailma!");` varmistuakseen siitä, että ohjelma tosiaan kääntyi.

Esimerkkiohjelmia löytyy kansiosta `test/examples` ja niitä voi ajaa antamatta
yhtäkään lippua tulkille, esim. `stack run -- test/examples/factorial.krap`.

Tulkin testit voi ajaa komennolla `stack test`.

## Lähdekieli

Lähdekieleen toteutetut ominaisuudet on listattu tiedostossa `suunnitelma.md`
(vain merkityt valintaruudut toteutettu). Suunnitelmassa esitellyt
esimerkkiohjelmat eivät välttämättä käänny, sillä joitakin ominaisuuksia
puuttuu. Johdatus lähdekieleen ja sen ominaisuuksiin löytyy tiedostosta
`test/examples/learn.krap`.