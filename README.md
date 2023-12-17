# Advent of Code
Rešitve za naloge Advent of Code (napisane za OCaml 4.14.1). Rešitve posameznih dnevov so `aoc/days/dayi.ml`.

Vhodne datoteke so na lokaciji z `in/day_i.in`, izhodne pa na `out/day_i_1.out` in `out/day_i_2.out`. 
Program se nahaja na `bin/main.exe` in mora biti zagnan iz zunanje direktorije projekta. Morda je treba exectutable znova generirati z `dune build`. 
Alternativno je lahko program compilan in pognan z ukazom `make`. Ukaz `make run-all` ustvari outpute za vse dneve, ukaz `make run day=...` pa požene le podan dan.

Navdih za strukturo projekta, ki pa je posplošen in na novo napisan je iz "[Primer ogrodja za lažje reševanje](https://github.com/jO-Osko/AdventOfCodeOcaml)".
