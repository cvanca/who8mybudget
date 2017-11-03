# README
# Who8mybudget - Analýza dat o rozpočtu Brna

## Struktura repositáře

`load.R` - prvotní vyčištění dat načtených z [datahubu](https://datahub.io/dataset/rozpoctova-data) pro použití v dalších skriptech.

`mc_rozpocet.R` - skript pro generování grafů o příjmech a výdajích městských částí v Brně 
(v absolutních číslech i per capita), rozdílu mezi schváleným a upraveným rozpočtem městských 
částí, příjmy a výdají městských částí na obyvatele, čeprání výdajů celého Brna i 
městských částí v čase, podílu kapitálových výdajů na celkových výdajích a čerpání rozpočtu
jednotlivých odborů magistrátu města Brna.
Grafy jsou dostupné ve složce `graphs/`.

`PoliticalPartiesEfficiency.R` - skript pro rychlou explorační analýzu dat o vztahu politického složení městských rad a "efektivitou hospodaření" městských částí za jejíž proxy byly považovány podíl kapitálových výdajů a rozdíl mezi schváleným a upraveným rozpočtem městských částí. Vzhledem k nízkému množství dat (pět kvartálů, jeden celý finanční rok) nelze potvrdit žádnou hypotézu. Sesbíraná data o politickém rozložení stran v městkých radách (přístupná v `data/`) a vizualizace politické blízkosti jednotlivých stran a politické podobnosti jednotlivých městských částí podle složení městkých rad pomocí PCA a dendogramů (přístupná v `graphs/`) jsou hezkou ukázkou z politické geografie Brna

`efektivita_politici.R`

`aggregate_city.R` - ?

`aggregate_municipality.R` - ?

`data/` - zdrojová a zpracovaná data použitá ve skriptech.

`graphs/` - grafy.
