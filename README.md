# openpogo

Scripts for maintaining and utilizing a Pokemon Go database. Example data included in pokedata.rda.
These things helped me learn R, so functions are poorly named, poorly specified, and generally poorly written. I'm sure many useful changes could be made. Suggestions welcome.

mymons = list of your mons
<br>agenda = next steps you'd like to take

Some of the more important functions:
<br>moncat shows you all the mons in your list of breeds that include the specified string
<br>mylist.new adds pokemon to the list
<br>- mylist.add was an old version, from when we couldn't tell stats right off, was used with possibles()
<br>mylist.subtract can't tell you what this does
<br>mylist.raidcounters gives you a list of your mons that are most powerful vs. a specified raid boss
<br>mylist.dpslvl tells you how a mon would fare against a boss at different levels

Examples:
## moncat("Mew")
```
...
255            Mew 1378 122 1900     15      11      12  15.0 Poison Jab  Flame Charge                84.4 FALSE          3216 15.47
301         Mewtwo 4138 178   NA     14      15      12  40.0 Psycho Cut   Shadow Ball Focus Blast    91.1 FALSE          4138 20.37
...
```

## mylist.new("Trubbish", 423, 6,7,8)
```
         Name   CP  HP Dust Attack Defense Stamina Level Fast Charge Charge2 Percent Shiny Notes
1104 Trubbish  411  78 2200      7       0       5    17                        26.7 FALSE
1105 Basculin 1393 119 4000      1      13       4    26                        40.0 FALSE
1106  Tympole   80  37  400      8      13      10     4                        68.9 FALSE
1107  Tympole   85  37  400     15      14       9     4                        84.4 FALSE
1108 Trubbish  423  79 2200      6       7       8    17                        46.7 FALSE
1109 Trubbish  423  79 2200      6       7       8    17                        46.7 FALSE
```

## mylist.raidcounters("Machamp")
```
 Number          Name         Fast  F.Type         Charge   C.Type Vs.DPS Atk Def Sta Level   CP Net.DPS
    922        Mewtwo    Confusion Psychic      Psystrike  Psychic  39.41 313 197 227  40.0 4134   38.73
    469        Mewtwo    Confusion Psychic        Psychic  Psychic  33.27 315 194 229  40.0 4146   32.85
     77      Alakazam    Confusion Psychic   Future Sight  Psychic  36.39 286 181 161  40.0 3049   32.79
    240      Alakazam    Confusion Psychic   Future Sight  Psychic  36.39 286 182 161  40.0 3057   32.79
     68      Alakazam    Confusion Psychic   Future Sight  Psychic  36.39 285 182 161  36.5 2894   31.92
     33        Espeon    Confusion Psychic   Future Sight  Psychic  36.39 276 190 177  40.0 3161   31.27
    617 Deoxys Attack Zen Headbutt Psychic     Zap Cannon Electric  25.20 425  60 150  25.0 1798   28.37
    183     Gardevoir        Charm   Fairy Dazzling Gleam    Fairy  35.52 252 209 182  40.0 3069   28.29
    421       Moltres  Wing Attack  Flying     Sky Attack   Flying  34.10 266 194 222  28.5 2807   28.19
    437        Mewtwo    Confusion Psychic        Psychic  Psychic  33.27 315 195 224  25.0 2937   27.72
```

## mylist.subtract(1109)
```
[1] "Eliminating:"
         Name  CP HP Dust Attack Defense Stamina Level Fast Charge Charge2 Percent Shiny Notes
1109 Trubbish 423 79 2200      6       7       8    17                        46.7 FALSE
```

## mylist.dpslvl(617, "Machamp")
```
[1] "Deoxys Attack vs. Machamp"
[1] "Zen Headbutt & Zap Cannon"
[1] "Total.DPS: 19.32"
[1] "Vs.DPS: 25.2"
[1] "Multi: 1.3"
    Level Net.DPS PctChg Fst.Dmg Chg.Dmg Stardust Candy
 1:  25.0   28.37     NA      26     157        0     0
 2:  25.5   29.12   2.58      27     158     4000     3
 3:  26.0   29.25   0.44      27     160     8000     6
 4:  26.5   29.39   0.48      27     162    12000    10
 5:  27.0   29.46   0.24      27     163    16000    14
 6:  27.5   30.27   2.68      28     165    20500    18
 7:  28.0   30.34   0.23      28     166    25000    22
 8:  28.5   30.48   0.46      28     168    29500    26
 9:  29.0   30.54   0.20      28     169    34000    30
10:  29.5   30.61   0.23      28     170    39000    34
11:  30.0   31.43   2.61      29     172    44000    38
12:  30.5   31.50   0.22      29     173    49000    42
13:  31.0   31.50   0.00      29     173    54000    46
14:  31.5   31.56   0.19      29     174    60000    52
15:  32.0   31.63   0.22      29     175    66000    58
16:  32.5   31.63   0.00      29     175    72000    64
17:  33.0   31.70   0.22      29     176    78000    70
18:  33.5   32.45   2.31      30     177    85000    78
19:  34.0   32.45   0.00      30     177    92000    86
20:  34.5   32.52   0.22      30     178    99000    94
21:  35.0   32.59   0.21      30     179   106000   102
22:  35.5   32.65   0.18      30     180   114000   112
23:  36.0   32.65   0.00      30     180   122000   122
24:  36.5   32.72   0.21      30     181   130000   132
25:  37.0   32.79   0.21      30     182   138000   142
26:  37.5   32.79   0.00      30     182   147000   154
27:  38.0   33.54   2.24      31     183   156000   166
28:  38.5   33.61   0.21      31     184   165000   178
29:  39.0   33.61   0.00      31     184   174000   190
30:  39.5   33.67   0.18      31     185   184000   205
31:  40.0   33.74   0.21      31     186   194000   220
    Level Net.DPS PctChg Fst.Dmg Chg.Dmg Stardust Candy
```
