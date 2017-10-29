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

`efektivita_politici.R`

`aggregate_city.R` - ?

`aggregate_municipality.R` - ?

`data/` - zdrojová a zpracovaná data použitá ve skriptech.

`graphs/` - grafy.
