---
title: "Caso"
author: "Roberto Ruz Campos"
date: "30/11/2019"
output:
  html_document:
    keep_md: true
---


```r
library(tidyverse)
library(lubridate)
```

Se importa la tabla "Compustat Global Daily" con los tipos de datos correctos.

```r
global_daily <- read_csv("Compustat_Global_Daily.csv",
  col_types = cols(
    sedol = col_character(), 
    datadate = col_date(format = "%Y%m%d") 
  )
)
```

Tipo de variable de cada columna

```r
global_daily %>%
  summarise_all(class) %>%
  pivot_longer(everything(), names_to = "column", values_to = "type")
```

```
## # A tibble: 19 x 2
##    column   type     
##    <chr>    <chr>    
##  1 gvkey    character
##  2 iid      character
##  3 datadate Date     
##  4 conm     character
##  5 curcdd   character
##  6 ajexdi   numeric  
##  7 cshoc    numeric  
##  8 cshtrd   numeric  
##  9 prccd    numeric  
## 10 prcstd   numeric  
## 11 qunit    numeric  
## 12 trfd     numeric  
## 13 exchg    numeric  
## 14 isin     character
## 15 sedol    character
## 16 fic      character
## 17 monthend numeric  
## 18 gsector  numeric  
## 19 sic      numeric
```

Vista general

```r
global_daily
```

```
## # A tibble: 784,102 x 19
##    gvkey iid   datadate   conm  curcdd ajexdi  cshoc cshtrd  prccd prcstd qunit
##    <chr> <chr> <date>     <chr> <chr>   <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl>
##  1 0058~ 01W   1996-09-18 IEM-~ MXN         1 1.65e7     NA   3.86     10     1
##  2 0058~ 01W   1997-11-11 IEM-~ MXN         1 1.65e7   2000   3.6      10     1
##  3 0058~ 01W   1997-11-28 IEM-~ MXN         1 1.65e7    109   3.6      10     1
##  4 0107~ 01W   1986-01-03 TUBO~ MXP         5 1.68e7     NA 320        10     1
##  5 0107~ 01W   1986-01-10 TUBO~ MXP         5 1.68e7     NA 320        10     1
##  6 0107~ 01W   1986-01-31 TUBO~ MXP         5 1.68e7     NA 320        10     1
##  7 0107~ 01W   1986-02-25 TUBO~ MXP         5 1.68e7     NA 320        10     1
##  8 0107~ 01W   1986-03-03 TUBO~ MXP         5 1.68e7     NA 310        10     1
##  9 0107~ 01W   1986-03-11 TUBO~ MXP         5 1.68e7     NA 310        10     1
## 10 0107~ 01W   1986-05-06 TUBO~ MXP         5 1.68e7     NA 310        10     1
## # ... with 784,092 more rows, and 8 more variables: trfd <dbl>, exchg <dbl>,
## #   isin <chr>, sedol <chr>, fic <chr>, monthend <dbl>, gsector <dbl>,
## #   sic <dbl>
```


```r
global_daily %>% 
  count(curcdd) 
```

```
## # A tibble: 5 x 2
##   curcdd      n
##   <chr>   <int>
## 1 EUR     76063
## 2 MXN    675066
## 3 MXP     32456
## 4 USD         1
## 5 <NA>      516
```
observaciones de cada compañía

```r
global_daily %>% 
  count(conm) %>% 
  arrange(conm)
```

```
## # A tibble: 291 x 2
##    conm                             n
##    <chr>                        <int>
##  1 A C MEXICANA SA DE CV            1
##  2 ACCEL SAB DE CV               4244
##  3 ACCIONES VALORES MEXICO SA      97
##  4 ACER COMPUTEC LATINO AMERICA   584
##  5 ACTINVER CASA DE BOL TR 1938  1216
##  6 ACTINVER CASA DE BOLSA SA     1371
##  7 ACTINVER SA DE CV             2467
##  8 ACTINVER TRACS SERIES 16       966
##  9 AEROVIAS DE MEXICO SA DE CV    951
## 10 ALFA SAB DE CV                7440
## # ... with 281 more rows
```

```r
global_daily %>% 
  count(conm) %>% 
  arrange(desc(n))
```

```
## # A tibble: 291 x 2
##    conm                             n
##    <chr>                        <int>
##  1 WAL MART DE MEXICO SA        16815
##  2 KIMBERLY-CLARK DE MEXICO SA  15082
##  3 GRUPO KUO SAB DE CV          12584
##  4 INDUSTRIAS PENOLES SAB DE CV 12346
##  5 FOMENTO ECONOMICO MEXICANO   12331
##  6 GRUPO MEXICO SAB DE CV       12057
##  7 CEMEX SAB DE CV              11515
##  8 TELMEX-TELEFONOS DE MEXICO   10801
##  9 GRUPO BIMBO SA DE CV         10109
## 10 GRUPO CARSO SA DE CV         10011
## # ... with 281 more rows
```


```r
global_daily %>% 
  filter(curcdd == "EUR") %>% 
  count(conm)
```

```
## # A tibble: 28 x 2
##    conm                             n
##    <chr>                        <int>
##  1 ALSEA SA DE CV                3047
##  2 ARCA CONTINENTAL SAB DE CV    2992
##  3 AXTEL SA DE CV                3039
##  4 BANCO COMPARTAMOS SA          1037
##  5 CARSO GLOBAL TELECOM           784
##  6 CARSO INFRAESTRUCTURA Y CONS  1113
##  7 GENOMMA LAB INTERNACIONAL     2972
##  8 GRUPO AEROPORTUARIO DEL PACI  3047
##  9 GRUPO AEROPORTUARIO SURESTE   3040
## 10 GRUPO BIMBO SA DE CV          3046
## # ... with 18 more rows
```



```r
global_daily %>% 
  filter(is.na(qunit)) %>% 
  count(conm)
```

```
## # A tibble: 112 x 2
##    conm                             n
##    <chr>                        <int>
##  1 ALFA SAB DE CV                   1
##  2 ALSEA SA DE CV                   2
##  3 AMERICA MOVIL SA DE CV           3
##  4 APASCO SA DE CV                  2
##  5 BANCO COMPARTAMOS SA             1
##  6 BANCO SANTANDER MEXICO -ADR      9
##  7 BCO MERC DEL NORTE               1
##  8 BUFETE INDUSTRIAL SA             1
##  9 CASA DE BOLSA FINAMEX SAB DE     6
## 10 CELANESE                         2
## # ... with 102 more rows
```



```r
global_daily %>% 
  count(gsector)
```

```
## # A tibble: 12 x 2
##    gsector      n
##      <dbl>  <int>
##  1      10  10720
##  2      15 124331
##  3      20 116114
##  4      25 117444
##  5      30 159695
##  6      35  11649
##  7      40 111733
##  8      45   2359
##  9      50  71491
## 10      55   1743
## 11      60  26875
## 12      NA  29948
```
industries

```r
global_daily %>%
  group_by(conm) %>% 
  count(sic) %>% 
  arrange(sic)
```

```
## # A tibble: 292 x 3
## # Groups:   conm [291]
##    conm                           sic     n
##    <chr>                        <dbl> <int>
##  1 SAVIA SA DE CV                 100  2857
##  2 CIA MINERA AUTLAN SA DE CV    1000  5189
##  3 EMPRESAS FRISCO SA DE CV      1000   150
##  4 GRUPO MEXICO SAB DE CV        1000 12057
##  5 MINERA FRISCO SAB DE CV       1000  2320
##  6 VISTA OIL & GAS CO            1311   597
##  7 GRUPO ICONSA SA DE CV         1500    87
##  8 CONSORCIO ARA SA DE CV        1520  5607
##  9 VINTE VIVIENDAS INTEGRALES    1520   823
## 10 CONSTRUCTORA CONSORCIO HOGAR  1531  4047
## # ... with 282 more rows
```

```r
global_daily %>% 
  count(sic)
```

```
## # A tibble: 103 x 2
##      sic     n
##    <dbl> <int>
##  1   100  2857
##  2  1000 19716
##  3  1311   597
##  4  1500    87
##  5  1520  6430
##  6  1531  4047
##  7  1600 30112
##  8  1623  2781
##  9  2000 14822
## 10  2015  5207
## # ... with 93 more rows
```





```r
global_daily %>% 
  count(conm) %>% 
  arrange(n) %>% 
  filter(n == 1)
```

```
## # A tibble: 8 x 2
##   conm                            n
##   <chr>                       <int>
## 1 A C MEXICANA SA DE CV           1
## 2 EPN SA DE CV                    1
## 3 GRUPO CALINDA SA DE CV          1
## 4 GRUPO FINANCIERO MEXIVAL SA     1
## 5 HULERA EUZKADI SA               1
## 6 PLAVICO SA DE CV                1
## 7 REAL TURISMO SA DE CV           1
## 8 TUBACERO SA                     1
```

```r
global_daily %>% 
  select(conm, datadate) %>% 
  group_by(conm) %>%
  mutate(n = n()) %>% 
  filter(n == 1)
```

```
## # A tibble: 8 x 3
## # Groups:   conm [8]
##   conm                        datadate       n
##   <chr>                       <date>     <int>
## 1 TUBACERO SA                 1995-10-03     1
## 2 GRUPO FINANCIERO MEXIVAL SA 1995-12-07     1
## 3 REAL TURISMO SA DE CV       1995-06-16     1
## 4 A C MEXICANA SA DE CV       1997-10-30     1
## 5 EPN SA DE CV                1997-06-04     1
## 6 GRUPO CALINDA SA DE CV      1995-10-03     1
## 7 PLAVICO SA DE CV            1998-11-12     1
## 8 HULERA EUZKADI SA           1993-02-09     1
```


```r
global_daily %>% 
  select(datadate) %>%
  distinct(datadate) %>% 
  arrange(datadate)
```

```
## # A tibble: 8,745 x 1
##    datadate  
##    <date>    
##  1 1986-01-02
##  2 1986-01-03
##  3 1986-01-06
##  4 1986-01-07
##  5 1986-01-08
##  6 1986-01-09
##  7 1986-01-10
##  8 1986-01-13
##  9 1986-01-14
## 10 1986-01-15
## # ... with 8,735 more rows
```

```r
global_daily %>% 
  filter(conm == "WAL MART DE MEXICO SA") %>% 
  select(conm, datadate, cshoc, cshtrd, prccd, prcstd, gsector) %>% 
  mutate(year = year(datadate)) %>% 
  group_by(year) %>% 
  summarise(n = n())
```

```
## # A tibble: 34 x 2
##     year     n
##    <dbl> <int>
##  1  1986   240
##  2  1987    47
##  3  1988    87
##  4  1989   196
##  5  1990   264
##  6  1991   523
##  7  1992   627
##  8  1993   562
##  9  1994   612
## 10  1995   581
## # ... with 24 more rows
```

```r
global_daily %>% 
  filter(conm == "WAL MART DE MEXICO SA") %>% 
  select(conm, datadate, cshoc, cshtrd, prccd, prcstd, gsector) %>% 
  arrange(datadate)
```

```
## # A tibble: 16,815 x 7
##    conm                  datadate       cshoc cshtrd prccd prcstd gsector
##    <chr>                 <date>         <dbl>  <dbl> <dbl>  <dbl>   <dbl>
##  1 WAL MART DE MEXICO SA 1986-01-02 200905050     NA   470     10      30
##  2 WAL MART DE MEXICO SA 1986-01-03 200905050     NA   470     10      30
##  3 WAL MART DE MEXICO SA 1986-01-03  85300000     NA   475     10      30
##  4 WAL MART DE MEXICO SA 1986-01-06 200905050     NA   460     10      30
##  5 WAL MART DE MEXICO SA 1986-01-07 200905050     NA   495     10      30
##  6 WAL MART DE MEXICO SA 1986-01-07  85300000     NA   480     10      30
##  7 WAL MART DE MEXICO SA 1986-01-08 200905050     NA   520     10      30
##  8 WAL MART DE MEXICO SA 1986-01-08  85300000     NA   500     10      30
##  9 WAL MART DE MEXICO SA 1986-01-09 200905050     NA   570     10      30
## 10 WAL MART DE MEXICO SA 1986-01-09  85300000     NA   560     10      30
## # ... with 16,805 more rows
```

```r
global_daily %>% 
  filter(conm == "WAL MART DE MEXICO SA") %>%  
  mutate(year = year(datadate)) %>% 
  filter(year == 1993) %>% 
  arrange(datadate) %>% 
  filter(curcdd == "MXN") 
```

```
## # A tibble: 561 x 20
##    gvkey iid   datadate   conm  curcdd ajexdi  cshoc cshtrd prccd prcstd qunit
##    <chr> <chr> <date>     <chr> <chr>   <dbl>  <dbl>  <dbl> <dbl>  <dbl> <dbl>
##  1 1051~ 01W   1993-01-04 WAL ~ MXN      1.12 1.22e9     NA     7     10     1
##  2 1051~ 02W   1993-01-04 WAL ~ MXN      4.47 1.18e9     NA     7     10     1
##  3 1051~ 03W   1993-01-04 WAL ~ MXN      1    8.00e8     NA     6     10     1
##  4 1051~ 01W   1993-01-05 WAL ~ MXN      1.12 1.22e9     NA     7     10     1
##  5 1051~ 02W   1993-01-05 WAL ~ MXN      4.47 1.18e9     NA     7     10     1
##  6 1051~ 03W   1993-01-05 WAL ~ MXN      1    8.00e8     NA     6     10     1
##  7 1051~ 02W   1993-01-06 WAL ~ MXN      4.47 1.18e9     NA     7     10     1
##  8 1051~ 03W   1993-01-06 WAL ~ MXN      1    8.00e8     NA     6     10     1
##  9 1051~ 02W   1993-01-07 WAL ~ MXN      4.47 1.18e9     NA     7     10     1
## 10 1051~ 03W   1993-01-07 WAL ~ MXN      1    8.00e8     NA     6     10     1
## # ... with 551 more rows, and 9 more variables: trfd <dbl>, exchg <dbl>,
## #   isin <chr>, sedol <chr>, fic <chr>, monthend <dbl>, gsector <dbl>,
## #   sic <dbl>, year <dbl>
```

```r
global_daily %>% 
  count(exchg)
```

```
## # A tibble: 5 x 2
##   exchg      n
##   <dbl>  <int>
## 1   115   1510
## 2   154  61875
## 3   208 708038
## 4   212   5949
## 5   257   6730
```


```r
global_daily %>% 
  mutate(year = year(datadate)) %>% 
  #filter(conm == "CEMEX SAB DE CV") %>%
  group_by(year, conm, isin) %>% 
  summarise(num = n())
```

```
## # A tibble: 4,467 x 4
## # Groups:   year, conm [3,473]
##     year conm                      isin           num
##    <dbl> <chr>                     <chr>        <int>
##  1  1986 APASCO SA DE CV           MXP041021512   173
##  2  1986 CELANESE                  MXP200351114   172
##  3  1986 CEMEX SAB DE CV           MXP225612300   158
##  4  1986 CERVECERIA MOCTEZ         <NA>           172
##  5  1986 CYDSA SA                  MXP339881098   174
##  6  1986 EL PUERTO DE LIVERPOOL SA MXP369181112   170
##  7  1986 GRUPO CONDUMEX SA DE CV   MXP2862F1364   159
##  8  1986 GRUPO MEXICO SAB DE CV    MX52IX210008   170
##  9  1986 GRUPO SANBORN SA DE CV    MXP842181580    90
## 10  1986 INDS RESISTOL SA          <NA>            84
## # ... with 4,457 more rows
```

