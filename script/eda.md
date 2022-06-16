---
title: "NAEE_Daramola"
author: "BBosa Robert"
date: "2022-06-16"
output:
  html_document:
    toc: yes
    number_sections: yes
    keep_md: yes
    df_print: tibble
  word_document:
    toc: yes
---

# Load required packages

```r
install.packages("ggrepel")
install.packages("fBasics")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("vtable")
```

# Call the libraries

```r
library(ggplot2)
library(ggrepel)
library(fBasics)
```

```
## Loading required package: timeDate
```

```
## Loading required package: timeSeries
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:timeSeries':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(vtable)
```

```
## Loading required package: kableExtra
```

```
## Warning in !is.null(rmarkdown::metadata$output) && rmarkdown::metadata$output
## %in% : 'length(x) = 2 > 1' in coercion to 'logical(1)'
```

```
## 
## Attaching package: 'kableExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     group_rows
```

# LOad the Data Set


```r
naeedata1 <- read.csv("~/NAEE_Data/naee_data/data/data1.csv", 
                header = TRUE,
                check.names = FALSE)

naeedata1 <- naeedata1[1:336,1:24]

View(naeedata1)
names(naeedata1)
```

```
##  [1] "Country"                                                                   
##  [2] "Year"                                                                      
##  [3] "Electricity installed capacity in Total renewable energy (MW)"             
##  [4] "Electricity installed capacity in Hydropower (MW)"                         
##  [5] "Electricity installed capacity in Wind (MW)"                               
##  [6] "Electricity installed capacity in Solar (MW)"                              
##  [7] "Electricity installed capacity in Bioenergy (MW)"                          
##  [8] "Electricity installed capacity in Geothermal (MW)"                         
##  [9] "Electricity export (GWh)"                                                  
## [10] "Electricity import (GWh)"                                                  
## [11] "Electricity final consumption (GWh)"                                       
## [12] "Electricity generation, Total (GWh)"                                       
## [13] "Electricity generated from fossil fuels (GWh)"                             
## [14] "Electricity generated from nuclear power (GWh)"                            
## [15] "Electricity generated from renewable sources (GWh)"                        
## [16] "Electricity generated from hydropower (GWh)"                               
## [17] "Electricity generated from solar, wind, tide, wave and other sources (GWh)"
## [18] "Electricity generated from biofuels and waste (GWh)"                       
## [19] "Electricity generated from geothermal energy (GWh)"                        
## [20] "Population access to electricity-National (% of population)"               
## [21] "Population access to electricity-Urban (% of population)"                  
## [22] "Population access to electricity-Rural (% of population)"                  
## [23] "Electricity generation per Capita (KWh)"                                   
## [24] "Electricity final consumption per capita (KWh)"
```

# Rename variables


```r
colnames(naeedata1)[2:24] <- c("year",
                               "in_tre",
                               "in_hyd",
                               "in_wind",
                               "in_solar",
                               "in_bio",
                               "in_geo",
                               "exp",
                               "imp",
                               "cons",
                               "tgen",
                               "gen_foss",
                               "gen_nuc",
                               "gen_ren",
                               "gen_hyd",
                               "gen_solar",
                               "gen_bio",
                               "gen_geo",
                               "access",
                               "acc_urban",
                               "acc_rural",
                               "perk_gen",
                               "perk_cons")
                               
names(naeedata1)
```

```
##  [1] "Country"   "year"      "in_tre"    "in_hyd"    "in_wind"   "in_solar" 
##  [7] "in_bio"    "in_geo"    "exp"       "imp"       "cons"      "tgen"     
## [13] "gen_foss"  "gen_nuc"   "gen_ren"   "gen_hyd"   "gen_solar" "gen_bio"  
## [19] "gen_geo"   "access"    "acc_urban" "acc_rural" "perk_gen"  "perk_cons"
```




```r
round(fBasics::basicStats(naeedata1[,-c(1:2)]),2)
```

```
## # A tibble: 16 × 22
##        in_tre  in_hyd in_wind in_solar in_bio in_geo    exp    imp   cons   tgen
##         <dbl>   <dbl>   <dbl>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
##  1     336     3.36e2  3.36e2   3.36e2 3.36e2 3.36e2 3.36e2 3.36e2 3.36e2 3.36e2
##  2       0     1   e0  0        0      0      0      0      0      0      0     
##  3       0     0       0        0      0      0      0      0      1.25e1 2.85e1
##  4    9639.    3.70e3  2.64e3   5.99e3 2.66e2 8.24e2 1.69e4 1.64e4 2.09e5 2.78e5
##  5       8.93  0       0        1.15e0 0      0      0      0      3.64e2 4.06e2
##  6     742.    7.02e2  1.3 e0   2.27e1 3.38e1 0      4.28e1 5.08e2 8.25e3 9.72e3
##  7     697.    4.50e2  9.21e1   1.14e2 2.83e1 1.42e1 8.21e2 9.86e2 1.38e4 1.76e4
##  8     169     6.66e1  0        6.15e0 0      0      0      1.8 e1 1.73e3 2.29e3
##  9  234100.    1.51e5  3.09e4   3.82e4 9.50e3 4.77e3 2.76e5 3.31e5 4.64e6 5.90e6
## 10      70.2   3.90e1  1.82e1   3.09e1 3.02e0 5.25e0 1.68e2 1.41e2 2.11e3 2.67e3
## 11     559.    3.73e2  5.62e1   5.29e1 2.24e1 3.89e0 4.92e2 7.09e2 9.67e3 1.23e4
## 12     835.    5.27e2  1.28e2   1.75e2 3.42e1 2.45e1 1.15e3 1.26e3 1.80e4 2.28e4
## 13 1658371.    5.11e5  1.12e5   3.21e5 3.06e3 9.25e3 9.43e6 6.64e6 1.49e9 2.40e9
## 14    1288.    7.15e2  3.34e2   5.67e2 5.53e1 9.62e1 3.07e3 2.58e3 3.86e4 4.90e4
## 15       3.36  2.09e0  4.59e0   7.73e0 2.52e0 6.99e0 4.23e0 3.59e0 3.96e0 3.98e0
## 16      14.4   4.05e0  2.32e1   6.50e1 6.44e0 4.88e1 1.65e1 1.32e1 1.49e1 1.54e1
## # … with 12 more variables: gen_foss <dbl>, gen_nuc <dbl>, gen_ren <dbl>,
## #   gen_hyd <dbl>, gen_solar <dbl>, gen_bio <dbl>, gen_geo <dbl>, access <dbl>,
## #   acc_urban <dbl>, acc_rural <dbl>, perk_gen <dbl>, perk_cons <dbl>
```

```r
st(naeedata1)
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>Summary Statistics</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Variable </th>
   <th style="text-align:left;"> N </th>
   <th style="text-align:left;"> Mean </th>
   <th style="text-align:left;"> Std. Dev. </th>
   <th style="text-align:left;"> Min </th>
   <th style="text-align:left;"> Pctl. 25 </th>
   <th style="text-align:left;"> Pctl. 75 </th>
   <th style="text-align:left;"> Max </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> year </td>
   <td style="text-align:left;"> 336 </td>
   <td style="text-align:left;"> 2016.5 </td>
   <td style="text-align:left;"> 2.295 </td>
   <td style="text-align:left;"> 2013 </td>
   <td style="text-align:left;"> 2014.75 </td>
   <td style="text-align:left;"> 2018.25 </td>
   <td style="text-align:left;"> 2020 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> in_tre </td>
   <td style="text-align:left;"> 336 </td>
   <td style="text-align:left;"> 696.725 </td>
   <td style="text-align:left;"> 1287.778 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 8.925 </td>
   <td style="text-align:left;"> 742.1 </td>
   <td style="text-align:left;"> 9638.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> in_hyd </td>
   <td style="text-align:left;"> 335 </td>
   <td style="text-align:left;"> 449.798 </td>
   <td style="text-align:left;"> 714.751 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 701.7 </td>
   <td style="text-align:left;"> 3700.9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> in_wind </td>
   <td style="text-align:left;"> 336 </td>
   <td style="text-align:left;"> 92.078 </td>
   <td style="text-align:left;"> 334.417 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1.3 </td>
   <td style="text-align:left;"> 2636 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> in_solar </td>
   <td style="text-align:left;"> 336 </td>
   <td style="text-align:left;"> 113.696 </td>
   <td style="text-align:left;"> 566.838 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1.15 </td>
   <td style="text-align:left;"> 22.7 </td>
   <td style="text-align:left;"> 5989.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> in_bio </td>
   <td style="text-align:left;"> 336 </td>
   <td style="text-align:left;"> 28.287 </td>
   <td style="text-align:left;"> 55.325 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 33.8 </td>
   <td style="text-align:left;"> 265.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> in_geo </td>
   <td style="text-align:left;"> 336 </td>
   <td style="text-align:left;"> 14.204 </td>
   <td style="text-align:left;"> 96.162 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 823.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> exp </td>
   <td style="text-align:left;"> 336 </td>
   <td style="text-align:left;"> 821.346 </td>
   <td style="text-align:left;"> 3071.526 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 42.825 </td>
   <td style="text-align:left;"> 16894.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> imp </td>
   <td style="text-align:left;"> 336 </td>
   <td style="text-align:left;"> 985.904 </td>
   <td style="text-align:left;"> 2576.23 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 508.325 </td>
   <td style="text-align:left;"> 16369 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cons </td>
   <td style="text-align:left;"> 336 </td>
   <td style="text-align:left;"> 13821.592 </td>
   <td style="text-align:left;"> 38644.065 </td>
   <td style="text-align:left;"> 12.5 </td>
   <td style="text-align:left;"> 363.6 </td>
   <td style="text-align:left;"> 8249.75 </td>
   <td style="text-align:left;"> 208913.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tgen </td>
   <td style="text-align:left;"> 336 </td>
   <td style="text-align:left;"> 17565.653 </td>
   <td style="text-align:left;"> 49020.7 </td>
   <td style="text-align:left;"> 28.5 </td>
   <td style="text-align:left;"> 406.225 </td>
   <td style="text-align:left;"> 9723.8 </td>
   <td style="text-align:left;"> 278140.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> gen_foss </td>
   <td style="text-align:left;"> 336 </td>
   <td style="text-align:left;"> 14690.549 </td>
   <td style="text-align:left;"> 44923.659 </td>
   <td style="text-align:left;"> 0.7 </td>
   <td style="text-align:left;"> 220.425 </td>
   <td style="text-align:left;"> 4053.1 </td>
   <td style="text-align:left;"> 247301 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> gen_nuc </td>
   <td style="text-align:left;"> 336 </td>
   <td style="text-align:left;"> 348.851 </td>
   <td style="text-align:left;"> 2243.843 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 15814.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> gen_ren </td>
   <td style="text-align:left;"> 336 </td>
   <td style="text-align:left;"> 2526.252 </td>
   <td style="text-align:left;"> 4125.505 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 11.175 </td>
   <td style="text-align:left;"> 3189.575 </td>
   <td style="text-align:left;"> 16574.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> gen_hyd </td>
   <td style="text-align:left;"> 336 </td>
   <td style="text-align:left;"> 2055.777 </td>
   <td style="text-align:left;"> 3562.845 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0.9 </td>
   <td style="text-align:left;"> 2597 </td>
   <td style="text-align:left;"> 16398.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> gen_solar </td>
   <td style="text-align:left;"> 336 </td>
   <td style="text-align:left;"> 323.27 </td>
   <td style="text-align:left;"> 1244.532 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0.2 </td>
   <td style="text-align:left;"> 29.1 </td>
   <td style="text-align:left;"> 8792.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> gen_bio </td>
   <td style="text-align:left;"> 336 </td>
   <td style="text-align:left;"> 43.795 </td>
   <td style="text-align:left;"> 110.948 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 12.25 </td>
   <td style="text-align:left;"> 625.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> gen_geo </td>
   <td style="text-align:left;"> 336 </td>
   <td style="text-align:left;"> 103.407 </td>
   <td style="text-align:left;"> 680.575 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 5343 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> access </td>
   <td style="text-align:left;"> 294 </td>
   <td style="text-align:left;"> 54.232 </td>
   <td style="text-align:left;"> 28.587 </td>
   <td style="text-align:left;"> 3.6 </td>
   <td style="text-align:left;"> 32.075 </td>
   <td style="text-align:left;"> 76.325 </td>
   <td style="text-align:left;"> 100 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> acc_urban </td>
   <td style="text-align:left;"> 294 </td>
   <td style="text-align:left;"> 74.93 </td>
   <td style="text-align:left;"> 21.532 </td>
   <td style="text-align:left;"> 1.9 </td>
   <td style="text-align:left;"> 61.9 </td>
   <td style="text-align:left;"> 91.25 </td>
   <td style="text-align:left;"> 100 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> acc_rural </td>
   <td style="text-align:left;"> 294 </td>
   <td style="text-align:left;"> 35.725 </td>
   <td style="text-align:left;"> 33.564 </td>
   <td style="text-align:left;"> 0.9 </td>
   <td style="text-align:left;"> 8.25 </td>
   <td style="text-align:left;"> 61.1 </td>
   <td style="text-align:left;"> 100 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> perk_gen </td>
   <td style="text-align:left;"> 336 </td>
   <td style="text-align:left;"> 830.083 </td>
   <td style="text-align:left;"> 1354.753 </td>
   <td style="text-align:left;"> 10.9 </td>
   <td style="text-align:left;"> 87.7 </td>
   <td style="text-align:left;"> 710.175 </td>
   <td style="text-align:left;"> 6120.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> perk_cons </td>
   <td style="text-align:left;"> 336 </td>
   <td style="text-align:left;"> 701.538 </td>
   <td style="text-align:left;"> 1060.787 </td>
   <td style="text-align:left;"> 11.4 </td>
   <td style="text-align:left;"> 64.175 </td>
   <td style="text-align:left;"> 949.975 </td>
   <td style="text-align:left;"> 6467.5 </td>
  </tr>
</tbody>
</table>


```r
naeedata1 %>% 
  filter(year == "2013") %>%
  ggplot(aes(x = in_tre, y = tgen)) +
  geom_point()+
  geom_text(aes(label = Country), size = 4)+
  labs(title = "2013",
    x="Electricity installed capacity in Total renewable energy (MW)",
    y="Electricity generation, Total (GWh)")
```

![](eda_files/figure-html/unnamed-chunk-6-1.png)<!-- -->



```r
 naeedata1 %>% 
  filter(year == "2020") %>%
  ggplot(aes(x = in_tre, y = tgen)) +
  geom_point()+
  geom_text(aes(label = Country), size = 4)+
  labs(title = "2020",
    x="Electricity installed capacity in Total renewable energy (MW)",
    y="Electricity generation, Total (GWh)")
```

![](eda_files/figure-html/unnamed-chunk-7-1.png)<!-- -->



```r
 naeedata1 %>% 
  filter(year == "2013") %>%
  mutate(ln_intre=log(in_tre)) %>% 
  mutate(ln_tgen=log(tgen)) %>% 
  ggplot(aes(x = ln_intre, y = ln_tgen)) +
  geom_point()+
  geom_text(aes(label = Country), size = 4)+
  labs(title = "2013",
    x="Electricity installed capacity in Total renewable energy (ln(MW))",
    y="Electricity generation, Total (ln(GWh)")
```

![](eda_files/figure-html/unnamed-chunk-8-1.png)<!-- -->




```r
 naeedata1 %>% 
  filter(year == "2020") %>%
  mutate(ln_intre=log(in_tre)) %>% 
  mutate(ln_tgen=log(tgen)) %>% 
  ggplot(aes(x = ln_intre, y = ln_tgen)) +
  geom_point()+
  geom_text(aes(label = Country), size = 4)+
  labs(title = "2020",
    x="Electricity installed capacity in Total renewable energy (ln(MW))",
    y="Electricity generation, Total (ln(GWh)")
```

![](eda_files/figure-html/unnamed-chunk-9-1.png)<!-- -->




```r
naeedata1 %>% 
  filter(year == "2013") %>% 
   mutate(ln_intre=log(in_tre)) %>% 
  mutate(ln_tgen=log(tgen)) %>% 
  ggplot(aes(x = ln_intre, y = ln_tgen)) +
  geom_point()+
  geom_label_repel(aes(label = Country), size = 4)+
  labs(title = "2013",
    x="Electricity installed capacity in Total renewable energy (ln(MW))",
    y="Electricity generation, Total (ln(GWh)")
```

```
## Warning: ggrepel: 8 unlabeled data points (too many overlaps). Consider
## increasing max.overlaps
```

![](eda_files/figure-html/unnamed-chunk-10-1.png)<!-- -->



```r
naeedata1 %>% 
  filter(year == "2020") %>% 
   mutate(ln_intre=log(in_tre)) %>% 
  mutate(ln_tgen=log(tgen)) %>% 
  ggplot(aes(x = ln_intre, y = ln_tgen)) +
  geom_point()+
  geom_label_repel(aes(label = Country), size = 4)+
  labs(title = "2020",
    x="Electricity installed capacity in Total renewable energy (ln(MW))",
    y="Electricity generation, Total (ln(GWh)")
```

```
## Warning: ggrepel: 16 unlabeled data points (too many overlaps). Consider
## increasing max.overlaps
```

![](eda_files/figure-html/unnamed-chunk-11-1.png)<!-- -->




```r
naeedata1 %>% 
  filter(year == "2013") %>% 
   mutate(ln_intre=log(in_tre)) %>% 
  mutate(ln_tgen=log(tgen)) %>% 
  ggplot(aes(x = ln_intre, y = ln_tgen)) +
  geom_point() +
  geom_text_repel(aes(label = Country), size = 3)+
  labs(title = "2013",
    x="Electricity installed capacity in Total renewable energy (ln(MW))",
    y="Electricity generation, Total (ln(GWh)")
```

![](eda_files/figure-html/unnamed-chunk-12-1.png)<!-- -->



```r
naeedata1 %>% 
  filter(year == "2020") %>% 
  mutate(ln_intre=log(in_tre)) %>% 
  mutate(ln_tgen=log(tgen)) %>% 
  ggplot(aes(x = ln_intre, y = ln_tgen, col = Country)) +
  geom_point(aes(color= Country)) +
  geom_label_repel(aes(label = Country), size = 3)+
  labs(title = "2020",
    x="Electricity installed capacity in Total renewable energy (ln(MW))",
    y="Electricity generation, Total (ln(GWh)") +
  scale_color_discrete() +
  theme(legend.position = "none")
```

```
## Warning: ggrepel: 8 unlabeled data points (too many overlaps). Consider
## increasing max.overlaps
```

![](eda_files/figure-html/unnamed-chunk-13-1.png)<!-- -->




