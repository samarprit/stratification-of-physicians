

```R
library(readr)
library(lme4)
## load the aggregated data 
dataf <- read_delim("C:/User/aggregated.csv", 
    "\t", escape_double = FALSE, trim_ws = TRUE)
```

    Loading required package: Matrix
    Parsed with column specification:
    cols(
      affiliation = col_character(),
      `service_dt (Year)` = col_integer(),
      gastrophysician_scrambled = col_character(),
      setting = col_character(),
      colonoscopy_type = col_character(),
      `Count of episode_id` = col_integer(),
      `Average of episode_allwd_amt` = col_double(),
      percentile = col_double(),
      decile = col_integer()
    )
    


```R
head(dataf) # preview a part of the data
```


<table>
<thead><tr><th scope=col>affiliation</th><th scope=col>service_dt (Year)</th><th scope=col>gastrophysician_scrambled</th><th scope=col>setting</th><th scope=col>colonoscopy_type</th><th scope=col>Count of episode_id</th><th scope=col>Average of episode_allwd_amt</th><th scope=col>percentile</th><th scope=col>decile</th></tr></thead>
<tbody>
	<tr><td>MSHS        </td><td>2015        </td><td>Provider 123</td><td>Office      </td><td>therapeutic </td><td> 1          </td><td>320.3100    </td><td>0.2659574   </td><td>1           </td></tr>
	<tr><td>NonMSHS     </td><td>2015        </td><td>Provider 28 </td><td>Office      </td><td>diagnostic  </td><td>15          </td><td>391.1233    </td><td>0.5319149   </td><td>1           </td></tr>
	<tr><td>NonMSHS     </td><td>2016        </td><td>Provider 28 </td><td>Office      </td><td>diagnostic  </td><td> 9          </td><td>393.5211    </td><td>0.7978723   </td><td>1           </td></tr>
	<tr><td>MSHS        </td><td>2015        </td><td>Provider 32 </td><td>Office      </td><td>diagnostic  </td><td> 1          </td><td>401.3400    </td><td>1.0638298   </td><td>1           </td></tr>
	<tr><td>NonMSHS     </td><td>2016        </td><td>Provider 20 </td><td>Office      </td><td>therapeutic </td><td> 1          </td><td>456.7100    </td><td>1.3297872   </td><td>1           </td></tr>
	<tr><td>NonMSHS     </td><td>2015        </td><td>Provider 28 </td><td>Office      </td><td>therapeutic </td><td>19          </td><td>556.0742    </td><td>1.5957447   </td><td>1           </td></tr>
</tbody>
</table>




```R
# renaming the columns for sake of analysis 
```


```R
colnames(dataf)[colnames(dataf)=="Average of episode_allwd_amt"]<-'avg_epi_amt'
```


```R
colnames(dataf)[colnames(dataf)=="gastrophysician_scrambled"]<-'gs'
```


```R
colnames(dataf)[colnames(dataf)=="Count of episode_id"]<-'cnt_epi'
```


```R
colnames(dataf)[colnames(dataf)=="service_dt (Year)"]<-'Yr'
```


```R
head(dataf)
```


<table>
<thead><tr><th scope=col>affiliation</th><th scope=col>Yr</th><th scope=col>gs</th><th scope=col>setting</th><th scope=col>colonoscopy_type</th><th scope=col>cnt_epi</th><th scope=col>avg_epi_amt</th><th scope=col>percentile</th><th scope=col>decile</th></tr></thead>
<tbody>
	<tr><td>MSHS        </td><td>2015        </td><td>Provider 123</td><td>Office      </td><td>therapeutic </td><td> 1          </td><td>320.3100    </td><td>0.2659574   </td><td>1           </td></tr>
	<tr><td>NonMSHS     </td><td>2015        </td><td>Provider 28 </td><td>Office      </td><td>diagnostic  </td><td>15          </td><td>391.1233    </td><td>0.5319149   </td><td>1           </td></tr>
	<tr><td>NonMSHS     </td><td>2016        </td><td>Provider 28 </td><td>Office      </td><td>diagnostic  </td><td> 9          </td><td>393.5211    </td><td>0.7978723   </td><td>1           </td></tr>
	<tr><td>MSHS        </td><td>2015        </td><td>Provider 32 </td><td>Office      </td><td>diagnostic  </td><td> 1          </td><td>401.3400    </td><td>1.0638298   </td><td>1           </td></tr>
	<tr><td>NonMSHS     </td><td>2016        </td><td>Provider 20 </td><td>Office      </td><td>therapeutic </td><td> 1          </td><td>456.7100    </td><td>1.3297872   </td><td>1           </td></tr>
	<tr><td>NonMSHS     </td><td>2015        </td><td>Provider 28 </td><td>Office      </td><td>therapeutic </td><td>19          </td><td>556.0742    </td><td>1.5957447   </td><td>1           </td></tr>
</tbody>
</table>




```R
# adding column where Ambulatory sercvies and Hospital Outpatient are grouped as Hospital 
dataf$fix_setting<-ifelse(dataf$setting=='Office','Office','Hospital')
```


```R
tail(dataf)
```


<table>
<thead><tr><th scope=col>affiliation</th><th scope=col>Yr</th><th scope=col>gs</th><th scope=col>setting</th><th scope=col>colonoscopy_type</th><th scope=col>cnt_epi</th><th scope=col>avg_epi_amt</th><th scope=col>percentile</th><th scope=col>decile</th><th scope=col>fix_setting</th></tr></thead>
<tbody>
	<tr><td>MSHS            </td><td>2015            </td><td>Provider 122    </td><td>Hosp. Outpatient</td><td>therapeutic     </td><td>1               </td><td> 8842.87        </td><td> 98.67021       </td><td>10              </td><td>Hospital        </td></tr>
	<tr><td>MSHS            </td><td>2016            </td><td>Provider 51     </td><td>Hosp. Outpatient</td><td>therapeutic     </td><td>1               </td><td> 9147.12        </td><td> 98.93617       </td><td>10              </td><td>Hospital        </td></tr>
	<tr><td>MSHS            </td><td>2015            </td><td>Provider 6      </td><td>Hosp. Outpatient</td><td>therapeutic     </td><td>1               </td><td> 9389.47        </td><td> 99.20213       </td><td>10              </td><td>Hospital        </td></tr>
	<tr><td>NonMSHS         </td><td>2016            </td><td>Provider 56     </td><td>Hosp. Outpatient</td><td>therapeutic     </td><td>1               </td><td>10023.80        </td><td> 99.46809       </td><td>10              </td><td>Hospital        </td></tr>
	<tr><td>MSHS            </td><td>2016            </td><td>Provider 6      </td><td>Hosp. Outpatient</td><td>therapeutic     </td><td>2               </td><td>10237.34        </td><td> 99.73404       </td><td>10              </td><td>Hospital        </td></tr>
	<tr><td>MSHS            </td><td>2016            </td><td>Provider 99     </td><td>Hosp. Outpatient</td><td>therapeutic     </td><td>1               </td><td>10371.75        </td><td>100.00000       </td><td>10              </td><td>Hospital        </td></tr>
</tbody>
</table>




```R
#Building models 
```


```R
# the null model accounts for individual setting and provider effects 
m1_null<-lmer(avg_epi_amt~colonoscopy_type+(1|gs)+(1|setting)+cnt_epi,data = dataf,REML = FALSE)
```


```R
summary(m1_null)
```


    Linear mixed model fit by maximum likelihood  ['lmerMod']
    Formula: avg_epi_amt ~ colonoscopy_type + (1 | gs) + (1 | setting) + cnt_epi
       Data: dataf
    
         AIC      BIC   logLik deviance df.resid 
      6384.0   6407.5  -3186.0   6372.0      370 
    
    Scaled residuals: 
        Min      1Q  Median      3Q     Max 
    -2.9303 -0.4011 -0.0563  0.3985  3.7352 
    
    Random effects:
     Groups   Name        Variance Std.Dev.
     gs       (Intercept)  749888   866.0  
     setting  (Intercept) 1858727  1363.4  
     Residual              855526   924.9  
    Number of obs: 376, groups:  gs, 128; setting, 3
    
    Fixed effects:
                                Estimate Std. Error t value
    (Intercept)                 2827.770    796.854   3.549
    colonoscopy_typetherapeutic  416.821    108.267   3.850
    cnt_epi                        2.262     10.999   0.206
    
    Correlation of Fixed Effects:
                (Intr) clnsc_
    clnscpy_typ -0.084       
    cnt_epi     -0.034 -0.115



```R
coefficients(m1_null)
```


    $gs
                 (Intercept) colonoscopy_typetherapeutic  cnt_epi
    Provider 1      2948.201                    416.8211 2.262004
    Provider 10     4560.058                    416.8211 2.262004
    Provider 100    2899.639                    416.8211 2.262004
    Provider 101    2739.372                    416.8211 2.262004
    Provider 102    3149.114                    416.8211 2.262004
    Provider 103    1591.141                    416.8211 2.262004
    Provider 104    2739.883                    416.8211 2.262004
    Provider 105    2482.782                    416.8211 2.262004
    Provider 106    2715.517                    416.8211 2.262004
    Provider 107    2169.030                    416.8211 2.262004
    Provider 108    2492.546                    416.8211 2.262004
    Provider 109    3616.234                    416.8211 2.262004
    Provider 11     1906.630                    416.8211 2.262004
    Provider 110    1967.160                    416.8211 2.262004
    Provider 111    2920.997                    416.8211 2.262004
    Provider 112    1930.349                    416.8211 2.262004
    Provider 113    2768.718                    416.8211 2.262004
    Provider 114    3079.595                    416.8211 2.262004
    Provider 115    2069.714                    416.8211 2.262004
    Provider 116    3397.237                    416.8211 2.262004
    Provider 117    2422.206                    416.8211 2.262004
    Provider 118    3244.853                    416.8211 2.262004
    Provider 119    2629.078                    416.8211 2.262004
    Provider 12     2447.673                    416.8211 2.262004
    Provider 120    1974.803                    416.8211 2.262004
    Provider 121    2696.303                    416.8211 2.262004
    Provider 122    4610.472                    416.8211 2.262004
    Provider 123    2165.239                    416.8211 2.262004
    Provider 124    2232.464                    416.8211 2.262004
    Provider 125    2801.625                    416.8211 2.262004
    Provider 126    3341.056                    416.8211 2.262004
    Provider 127    1631.927                    416.8211 2.262004
    Provider 128    2745.942                    416.8211 2.262004
    Provider 13     2932.778                    416.8211 2.262004
    Provider 14     3079.874                    416.8211 2.262004
    Provider 15     2556.316                    416.8211 2.262004
    Provider 16     2908.533                    416.8211 2.262004
    Provider 17     2521.120                    416.8211 2.262004
    Provider 18     2739.043                    416.8211 2.262004
    Provider 19     2636.436                    416.8211 2.262004
    Provider 2      2624.445                    416.8211 2.262004
    Provider 20     2771.016                    416.8211 2.262004
    Provider 21     2611.436                    416.8211 2.262004
    Provider 22     2673.188                    416.8211 2.262004
    Provider 23     3172.644                    416.8211 2.262004
    Provider 24     2765.792                    416.8211 2.262004
    Provider 25     2514.952                    416.8211 2.262004
    Provider 26     1827.310                    416.8211 2.262004
    Provider 27     4207.117                    416.8211 2.262004
    Provider 28     2000.856                    416.8211 2.262004
    Provider 29     2550.779                    416.8211 2.262004
    Provider 3      2731.372                    416.8211 2.262004
    Provider 30     2762.440                    416.8211 2.262004
    Provider 31     2642.202                    416.8211 2.262004
    Provider 32     1939.677                    416.8211 2.262004
    Provider 33     2189.266                    416.8211 2.262004
    Provider 34     3047.439                    416.8211 2.262004
    Provider 35     4114.111                    416.8211 2.262004
    Provider 36     3849.493                    416.8211 2.262004
    Provider 37     3437.633                    416.8211 2.262004
    Provider 38     2914.367                    416.8211 2.262004
    Provider 39     2020.876                    416.8211 2.262004
    Provider 4      2964.610                    416.8211 2.262004
    Provider 40     2705.084                    416.8211 2.262004
    Provider 41     3661.610                    416.8211 2.262004
    Provider 42     3826.946                    416.8211 2.262004
    Provider 43     2555.817                    416.8211 2.262004
    Provider 44     2830.183                    416.8211 2.262004
    Provider 45     3625.535                    416.8211 2.262004
    Provider 46     2699.334                    416.8211 2.262004
    Provider 47     2749.911                    416.8211 2.262004
    Provider 48     2301.327                    416.8211 2.262004
    Provider 49     3234.038                    416.8211 2.262004
    Provider 5      3011.746                    416.8211 2.262004
    Provider 50     3373.653                    416.8211 2.262004
    Provider 51     5313.164                    416.8211 2.262004
    Provider 52     2305.253                    416.8211 2.262004
    Provider 53     2078.627                    416.8211 2.262004
    Provider 54     3550.955                    416.8211 2.262004
    Provider 55     2195.043                    416.8211 2.262004
    Provider 56     5162.084                    416.8211 2.262004
    Provider 57     3033.666                    416.8211 2.262004
    Provider 58     3491.442                    416.8211 2.262004
    Provider 59     2966.242                    416.8211 2.262004
    Provider 6      4727.571                    416.8211 2.262004
    Provider 60     2656.026                    416.8211 2.262004
    Provider 61     2345.592                    416.8211 2.262004
    Provider 62     3217.066                    416.8211 2.262004
    Provider 63     2835.277                    416.8211 2.262004
    Provider 64     3170.909                    416.8211 2.262004
    Provider 65     2648.930                    416.8211 2.262004
    Provider 66     2231.106                    416.8211 2.262004
    Provider 67     2458.772                    416.8211 2.262004
    Provider 68     2974.826                    416.8211 2.262004
    Provider 69     3388.486                    416.8211 2.262004
    Provider 7      2546.035                    416.8211 2.262004
    Provider 70     2761.842                    416.8211 2.262004
    Provider 71     2600.396                    416.8211 2.262004
    Provider 72     2344.205                    416.8211 2.262004
    Provider 73     2221.880                    416.8211 2.262004
    Provider 74     2109.032                    416.8211 2.262004
    Provider 75     4012.557                    416.8211 2.262004
    Provider 76     2732.253                    416.8211 2.262004
    Provider 77     2498.257                    416.8211 2.262004
    Provider 78     2723.654                    416.8211 2.262004
    Provider 79     2232.675                    416.8211 2.262004
    Provider 8      2561.944                    416.8211 2.262004
    Provider 80     2419.709                    416.8211 2.262004
    Provider 81     3065.029                    416.8211 2.262004
    Provider 82     2207.896                    416.8211 2.262004
    Provider 83     2302.519                    416.8211 2.262004
    Provider 84     3014.926                    416.8211 2.262004
    Provider 85     4196.506                    416.8211 2.262004
    Provider 86     2828.120                    416.8211 2.262004
    Provider 87     3086.846                    416.8211 2.262004
    Provider 88     2247.091                    416.8211 2.262004
    Provider 89     1298.752                    416.8211 2.262004
    Provider 9      2616.772                    416.8211 2.262004
    Provider 90     1961.061                    416.8211 2.262004
    Provider 91     3975.217                    416.8211 2.262004
    Provider 92     2937.718                    416.8211 2.262004
    Provider 93     1616.240                    416.8211 2.262004
    Provider 94     2437.877                    416.8211 2.262004
    Provider 95     2704.150                    416.8211 2.262004
    Provider 96     3028.088                    416.8211 2.262004
    Provider 97     3565.960                    416.8211 2.262004
    Provider 98     2260.108                    416.8211 2.262004
    Provider 99     4718.309                    416.8211 2.262004
    
    $setting
                      (Intercept) colonoscopy_typetherapeutic  cnt_epi
    Amb. Surg. Center    2556.440                    416.8211 2.262004
    Hosp. Outpatient     4607.250                    416.8211 2.262004
    Office               1319.619                    416.8211 2.262004
    
    attr(,"class")
    [1] "coef.mer"



```R
# introducing affiliation as a fixed effect in the model to compare between MSHS and nonMSHS
m1_affiiliation=lmer(avg_epi_amt~affiliation+colonoscopy_type+(1|gs)+(1|setting)+cnt_epi,data = dataf,REML = FALSE)
```


```R
summary(m1_affiiliation)
```


    Linear mixed model fit by maximum likelihood  ['lmerMod']
    Formula: avg_epi_amt ~ affiliation + colonoscopy_type + (1 | gs) + (1 |  
        setting) + cnt_epi
       Data: dataf
    
         AIC      BIC   logLik deviance df.resid 
      6379.1   6406.7  -3182.6   6365.1      369 
    
    Scaled residuals: 
        Min      1Q  Median      3Q     Max 
    -2.8131 -0.4111 -0.0491  0.4032  3.6587 
    
    Random effects:
     Groups   Name        Variance Std.Dev.
     gs       (Intercept)  723570   850.6  
     setting  (Intercept) 1695944  1302.3  
     Residual              845272   919.4  
    Number of obs: 376, groups:  gs, 128; setting, 3
    
    Fixed effects:
                                Estimate Std. Error t value
    (Intercept)                 3295.513    782.479   4.212
    affiliationNonMSHS          -622.280    237.178  -2.624
    colonoscopy_typetherapeutic  416.721    107.539   3.875
    cnt_epi                        3.052     10.925   0.279
    
    Correlation of Fixed Effects:
                (Intr) aNMSHS clnsc_
    affltnNMSHS -0.228              
    clnscpy_typ -0.087  0.005       
    cnt_epi     -0.027 -0.031 -0.115



```R
#comparison between a pair of models to seewhether inclusion of new variable made the model(m1_affiliation) different and better than previous one(m1_null)
anova(m1_null,m1_affiiliation)
```


<table>
<thead><tr><th></th><th scope=col>Df</th><th scope=col>AIC</th><th scope=col>BIC</th><th scope=col>logLik</th><th scope=col>deviance</th><th scope=col>Chisq</th><th scope=col>Chi Df</th><th scope=col>Pr(&gt;Chisq)</th></tr></thead>
<tbody>
	<tr><th scope=row>m1_null</th><td>6         </td><td>6383.951  </td><td>6407.529  </td><td>-3185.976 </td><td>6371.951  </td><td>      NA  </td><td>NA        </td><td>        NA</td></tr>
	<tr><th scope=row>m1_affiiliation</th><td>7         </td><td>6379.147  </td><td>6406.654  </td><td>-3182.573 </td><td>6365.147  </td><td>6.804604  </td><td> 1        </td><td>0.00909231</td></tr>
</tbody>
</table>




```R
m2_affiliation=lmer(avg_epi_amt~(1+affiliation|gs)+(1+affiliation|setting)+colonoscopy_type,data = dataf,REML = FALSE)
```


```R
summary(m2_affiliation)
```


    Linear mixed model fit by maximum likelihood  ['lmerMod']
    Formula: avg_epi_amt ~ (1 + affiliation | gs) + (1 + affiliation | setting) +  
        colonoscopy_type
       Data: dataf
    
         AIC      BIC   logLik deviance df.resid 
      6356.7   6392.1  -3169.4   6338.7      367 
    
    Scaled residuals: 
        Min      1Q  Median      3Q     Max 
    -2.9254 -0.4146 -0.0544  0.3809  4.3693 
    
    Random effects:
     Groups   Name               Variance Std.Dev. Corr 
     gs       (Intercept)        2238620  1496.2        
              affiliationNonMSHS 1405707  1185.6   -0.95
     setting  (Intercept)        3091127  1758.2        
              affiliationNonMSHS  150345   387.7   -1.00
     Residual                     856805   925.6        
    Number of obs: 376, groups:  gs, 128; setting, 3
    
    Fixed effects:
                                Estimate Std. Error t value
    (Intercept)                   2168.0      674.3   3.215
    colonoscopy_typetherapeutic    413.8      106.0   3.904
    
    Correlation of Fixed Effects:
                (Intr)
    clnscpy_typ -0.100



```R
anova(m1_affiiliation,m2_affiliation)
```


<table>
<thead><tr><th></th><th scope=col>Df</th><th scope=col>AIC</th><th scope=col>BIC</th><th scope=col>logLik</th><th scope=col>deviance</th><th scope=col>Chisq</th><th scope=col>Chi Df</th><th scope=col>Pr(&gt;Chisq)</th></tr></thead>
<tbody>
	<tr><th scope=row>m1_affiiliation</th><td>7           </td><td>6379.147    </td><td>6406.654    </td><td>-3182.573   </td><td>6365.147    </td><td>      NA    </td><td>NA          </td><td>          NA</td></tr>
	<tr><th scope=row>m2_affiliation</th><td>9           </td><td>6356.718    </td><td>6392.084    </td><td>-3169.359   </td><td>6338.718    </td><td>26.42905    </td><td> 2          </td><td>1.823916e-06</td></tr>
</tbody>
</table>




```R
m3_interaction<-lmer(avg_epi_amt~(1+1|gs:affiliation)+colonoscopy_type+cnt_epi,data = dataf,REML = FALSE)
```


```R
summary(m3_interaction)
```


    Linear mixed model fit by maximum likelihood  ['lmerMod']
    Formula: avg_epi_amt ~ (1 + 1 | gs:affiliation) + colonoscopy_type + cnt_epi
       Data: dataf
    
         AIC      BIC   logLik deviance df.resid 
      6580.6   6600.3  -3285.3   6570.6      371 
    
    Scaled residuals: 
        Min      1Q  Median      3Q     Max 
    -2.7286 -0.4501 -0.1127  0.2791  3.8676 
    
    Random effects:
     Groups         Name        Variance Std.Dev.
     gs:affiliation (Intercept) 1846492  1359    
     Residual                   1347973  1161    
    Number of obs: 376, groups:  gs:affiliation, 128
    
    Fixed effects:
                                Estimate Std. Error t value
    (Intercept)                  2938.52     167.24  17.571
    colonoscopy_typetherapeutic   511.16     137.35   3.722
    cnt_epi                       -32.16      13.80  -2.330
    
    Correlation of Fixed Effects:
                (Intr) clnsc_
    clnscpy_typ -0.518       
    cnt_epi     -0.186 -0.110



```R
anova(m1_null,m3_interaction)
```


<table>
<thead><tr><th></th><th scope=col>Df</th><th scope=col>AIC</th><th scope=col>BIC</th><th scope=col>logLik</th><th scope=col>deviance</th><th scope=col>Chisq</th><th scope=col>Chi Df</th><th scope=col>Pr(&gt;Chisq)</th></tr></thead>
<tbody>
	<tr><th scope=row>m3_interaction</th><td>5           </td><td>6580.621    </td><td>6600.269    </td><td>-3285.310   </td><td>6570.621    </td><td>      NA    </td><td>NA          </td><td>          NA</td></tr>
	<tr><th scope=row>m1_null</th><td>6           </td><td>6383.951    </td><td>6407.529    </td><td>-3185.976   </td><td>6371.951    </td><td>198.6694    </td><td> 1          </td><td>4.075629e-45</td></tr>
</tbody>
</table>




```R
# in exploratory analysis we found office costs are lower than facility costs so grouped setting(3 level) into hospital and office values (2 level)
m3_interaction_settingfix<-lmer(avg_epi_amt~(1+1|gs:affiliation)+(1|gs:fix_setting)+cnt_epi+(1+colonoscopy_type|fix_setting),data = dataf,REML = FALSE)
```


```R
summary(m3_interaction_settingfix) # as observed the AIC value has dropped from previous model design. 
```


    Linear mixed model fit by maximum likelihood  ['lmerMod']
    Formula: avg_epi_amt ~ (1 + 1 | gs:affiliation) + (1 | gs:fix_setting) +  
        cnt_epi + (1 + colonoscopy_type | fix_setting)
       Data: dataf
    
         AIC      BIC   logLik deviance df.resid 
      6520.0   6551.4  -3252.0   6504.0      368 
    
    Scaled residuals: 
        Min      1Q  Median      3Q     Max 
    -3.0124 -0.4492 -0.1137  0.2532  3.9934 
    
    Random effects:
     Groups         Name                        Variance  Std.Dev.  Corr
     gs:fix_setting (Intercept)                 2.589e-08 1.609e-04     
     gs:affiliation (Intercept)                 1.437e+06 1.199e+03     
     fix_setting    (Intercept)                 1.458e+06 1.207e+03     
                    colonoscopy_typetherapeutic 1.320e+05 3.633e+02 1.00
     Residual                                   1.122e+06 1.059e+03     
    Number of obs: 376, groups:  
    gs:fix_setting, 142; gs:affiliation, 128; fix_setting, 2
    
    Fixed effects:
                Estimate Std. Error t value
    (Intercept)  1552.03     462.54   3.355
    cnt_epi       -17.45      12.63  -1.382
    
    Correlation of Fixed Effects:
            (Intr)
    cnt_epi 0.012 



```R
coefficients(m3_interaction_settingfix)
```


    $`gs:affiliation`
                         fix_settingOffice colonoscopy_typetherapeutic (Intercept)
    Provider 1:NonMSHS                   0                           0    1227.102
    Provider 10:MSHS                     0                           0    1227.102
    Provider 100:MSHS                    0                           0    1227.102
    Provider 101:NonMSHS                 0                           0    1227.102
    Provider 102:NonMSHS                 0                           0    1227.102
    Provider 103:NonMSHS                 0                           0    1227.102
    Provider 104:NonMSHS                 0                           0    1227.102
    Provider 105:NonMSHS                 0                           0    1227.102
    Provider 106:NonMSHS                 0                           0    1227.102
    Provider 107:NonMSHS                 0                           0    1227.102
    Provider 108:NonMSHS                 0                           0    1227.102
    Provider 109:MSHS                    0                           0    1227.102
    Provider 11:NonMSHS                  0                           0    1227.102
    Provider 110:MSHS                    0                           0    1227.102
    Provider 111:NonMSHS                 0                           0    1227.102
    Provider 112:NonMSHS                 0                           0    1227.102
    Provider 113:NonMSHS                 0                           0    1227.102
    Provider 114:NonMSHS                 0                           0    1227.102
    Provider 115:NonMSHS                 0                           0    1227.102
    Provider 116:NonMSHS                 0                           0    1227.102
    Provider 117:NonMSHS                 0                           0    1227.102
    Provider 118:NonMSHS                 0                           0    1227.102
    Provider 119:NonMSHS                 0                           0    1227.102
    Provider 12:NonMSHS                  0                           0    1227.102
    Provider 120:NonMSHS                 0                           0    1227.102
    Provider 121:NonMSHS                 0                           0    1227.102
    Provider 122:MSHS                    0                           0    1227.102
    Provider 123:MSHS                    0                           0    1227.102
    Provider 124:MSHS                    0                           0    1227.102
    Provider 125:MSHS                    0                           0    1227.102
    Provider 126:NonMSHS                 0                           0    1227.102
    Provider 127:NonMSHS                 0                           0    1227.102
    Provider 128:NonMSHS                 0                           0    1227.102
    Provider 13:NonMSHS                  0                           0    1227.102
    Provider 14:NonMSHS                  0                           0    1227.102
    Provider 15:NonMSHS                  0                           0    1227.102
    Provider 16:NonMSHS                  0                           0    1227.102
    Provider 17:NonMSHS                  0                           0    1227.102
    Provider 18:NonMSHS                  0                           0    1227.102
    Provider 19:NonMSHS                  0                           0    1227.102
    Provider 2:NonMSHS                   0                           0    1227.102
    Provider 20:NonMSHS                  0                           0    1227.102
    Provider 21:NonMSHS                  0                           0    1227.102
    Provider 22:NonMSHS                  0                           0    1227.102
    Provider 23:NonMSHS                  0                           0    1227.102
    Provider 24:NonMSHS                  0                           0    1227.102
    Provider 25:NonMSHS                  0                           0    1227.102
    Provider 26:MSHS                     0                           0    1227.102
    Provider 27:MSHS                     0                           0    1227.102
    Provider 28:NonMSHS                  0                           0    1227.102
    Provider 29:NonMSHS                  0                           0    1227.102
    Provider 3:NonMSHS                   0                           0    1227.102
    Provider 30:MSHS                     0                           0    1227.102
    Provider 31:NonMSHS                  0                           0    1227.102
    Provider 32:MSHS                     0                           0    1227.102
    Provider 33:NonMSHS                  0                           0    1227.102
    Provider 34:NonMSHS                  0                           0    1227.102
    Provider 35:MSHS                     0                           0    1227.102
    Provider 36:NonMSHS                  0                           0    1227.102
    Provider 37:NonMSHS                  0                           0    1227.102
    Provider 38:MSHS                     0                           0    1227.102
    Provider 39:NonMSHS                  0                           0    1227.102
    Provider 4:NonMSHS                   0                           0    1227.102
    Provider 40:NonMSHS                  0                           0    1227.102
    Provider 41:MSHS                     0                           0    1227.102
    Provider 42:NonMSHS                  0                           0    1227.102
    Provider 43:NonMSHS                  0                           0    1227.102
    Provider 44:MSHS                     0                           0    1227.102
    Provider 45:NonMSHS                  0                           0    1227.102
    Provider 46:NonMSHS                  0                           0    1227.102
    Provider 47:NonMSHS                  0                           0    1227.102
    Provider 48:NonMSHS                  0                           0    1227.102
    Provider 49:NonMSHS                  0                           0    1227.102
    Provider 5:NonMSHS                   0                           0    1227.102
    Provider 50:NonMSHS                  0                           0    1227.102
    Provider 51:MSHS                     0                           0    1227.102
    Provider 52:MSHS                     0                           0    1227.102
    Provider 53:MSHS                     0                           0    1227.102
    Provider 54:MSHS                     0                           0    1227.102
    Provider 55:NonMSHS                  0                           0    1227.102
    Provider 56:NonMSHS                  0                           0    1227.102
    Provider 57:NonMSHS                  0                           0    1227.102
    Provider 58:NonMSHS                  0                           0    1227.102
    Provider 59:NonMSHS                  0                           0    1227.102
    Provider 6:MSHS                      0                           0    1227.102
    Provider 60:NonMSHS                  0                           0    1227.102
    Provider 61:NonMSHS                  0                           0    1227.102
    Provider 62:NonMSHS                  0                           0    1227.102
    Provider 63:NonMSHS                  0                           0    1227.102
    Provider 64:NonMSHS                  0                           0    1227.102
    Provider 65:MSHS                     0                           0    1227.102
    Provider 66:NonMSHS                  0                           0    1227.102
    Provider 67:NonMSHS                  0                           0    1227.102
    Provider 68:MSHS                     0                           0    1227.102
    Provider 69:NonMSHS                  0                           0    1227.102
    Provider 7:NonMSHS                   0                           0    1227.102
    Provider 70:NonMSHS                  0                           0    1227.102
    Provider 71:NonMSHS                  0                           0    1227.102
    Provider 72:NonMSHS                  0                           0    1227.102
    Provider 73:NonMSHS                  0                           0    1227.102
    Provider 74:MSHS                     0                           0    1227.102
    Provider 75:NonMSHS                  0                           0    1227.102
    Provider 76:NonMSHS                  0                           0    1227.102
    Provider 77:NonMSHS                  0                           0    1227.102
    Provider 78:NonMSHS                  0                           0    1227.102
    Provider 79:NonMSHS                  0                           0    1227.102
    Provider 8:NonMSHS                   0                           0    1227.102
    Provider 80:NonMSHS                  0                           0    1227.102
    Provider 81:NonMSHS                  0                           0    1227.102
    Provider 82:NonMSHS                  0                           0    1227.102
    Provider 83:MSHS                     0                           0    1227.102
    Provider 84:NonMSHS                  0                           0    1227.102
    Provider 85:NonMSHS                  0                           0    1227.102
    Provider 86:NonMSHS                  0                           0    1227.102
    Provider 87:NonMSHS                  0                           0    1227.102
    Provider 88:MSHS                     0                           0    1227.102
    Provider 89:MSHS                     0                           0    1227.102
    Provider 9:NonMSHS                   0                           0    1227.102
    Provider 90:NonMSHS                  0                           0    1227.102
    Provider 91:MSHS                     0                           0    1227.102
    Provider 92:NonMSHS                  0                           0    1227.102
    Provider 93:NonMSHS                  0                           0    1227.102
    Provider 94:NonMSHS                  0                           0    1227.102
    Provider 95:NonMSHS                  0                           0    1227.102
    Provider 96:NonMSHS                  0                           0    1227.102
    Provider 97:MSHS                     0                           0    1227.102
    Provider 98:NonMSHS                  0                           0    1227.102
    Provider 99:MSHS                     0                           0    1227.102
                           cnt_epi
    Provider 1:NonMSHS   -15.41203
    Provider 10:MSHS     -15.41203
    Provider 100:MSHS    -15.41203
    Provider 101:NonMSHS -15.41203
    Provider 102:NonMSHS -15.41203
    Provider 103:NonMSHS -15.41203
    Provider 104:NonMSHS -15.41203
    Provider 105:NonMSHS -15.41203
    Provider 106:NonMSHS -15.41203
    Provider 107:NonMSHS -15.41203
    Provider 108:NonMSHS -15.41203
    Provider 109:MSHS    -15.41203
    Provider 11:NonMSHS  -15.41203
    Provider 110:MSHS    -15.41203
    Provider 111:NonMSHS -15.41203
    Provider 112:NonMSHS -15.41203
    Provider 113:NonMSHS -15.41203
    Provider 114:NonMSHS -15.41203
    Provider 115:NonMSHS -15.41203
    Provider 116:NonMSHS -15.41203
    Provider 117:NonMSHS -15.41203
    Provider 118:NonMSHS -15.41203
    Provider 119:NonMSHS -15.41203
    Provider 12:NonMSHS  -15.41203
    Provider 120:NonMSHS -15.41203
    Provider 121:NonMSHS -15.41203
    Provider 122:MSHS    -15.41203
    Provider 123:MSHS    -15.41203
    Provider 124:MSHS    -15.41203
    Provider 125:MSHS    -15.41203
    Provider 126:NonMSHS -15.41203
    Provider 127:NonMSHS -15.41203
    Provider 128:NonMSHS -15.41203
    Provider 13:NonMSHS  -15.41203
    Provider 14:NonMSHS  -15.41203
    Provider 15:NonMSHS  -15.41203
    Provider 16:NonMSHS  -15.41203
    Provider 17:NonMSHS  -15.41203
    Provider 18:NonMSHS  -15.41203
    Provider 19:NonMSHS  -15.41203
    Provider 2:NonMSHS   -15.41203
    Provider 20:NonMSHS  -15.41203
    Provider 21:NonMSHS  -15.41203
    Provider 22:NonMSHS  -15.41203
    Provider 23:NonMSHS  -15.41203
    Provider 24:NonMSHS  -15.41203
    Provider 25:NonMSHS  -15.41203
    Provider 26:MSHS     -15.41203
    Provider 27:MSHS     -15.41203
    Provider 28:NonMSHS  -15.41203
    Provider 29:NonMSHS  -15.41203
    Provider 3:NonMSHS   -15.41203
    Provider 30:MSHS     -15.41203
    Provider 31:NonMSHS  -15.41203
    Provider 32:MSHS     -15.41203
    Provider 33:NonMSHS  -15.41203
    Provider 34:NonMSHS  -15.41203
    Provider 35:MSHS     -15.41203
    Provider 36:NonMSHS  -15.41203
    Provider 37:NonMSHS  -15.41203
    Provider 38:MSHS     -15.41203
    Provider 39:NonMSHS  -15.41203
    Provider 4:NonMSHS   -15.41203
    Provider 40:NonMSHS  -15.41203
    Provider 41:MSHS     -15.41203
    Provider 42:NonMSHS  -15.41203
    Provider 43:NonMSHS  -15.41203
    Provider 44:MSHS     -15.41203
    Provider 45:NonMSHS  -15.41203
    Provider 46:NonMSHS  -15.41203
    Provider 47:NonMSHS  -15.41203
    Provider 48:NonMSHS  -15.41203
    Provider 49:NonMSHS  -15.41203
    Provider 5:NonMSHS   -15.41203
    Provider 50:NonMSHS  -15.41203
    Provider 51:MSHS     -15.41203
    Provider 52:MSHS     -15.41203
    Provider 53:MSHS     -15.41203
    Provider 54:MSHS     -15.41203
    Provider 55:NonMSHS  -15.41203
    Provider 56:NonMSHS  -15.41203
    Provider 57:NonMSHS  -15.41203
    Provider 58:NonMSHS  -15.41203
    Provider 59:NonMSHS  -15.41203
    Provider 6:MSHS      -15.41203
    Provider 60:NonMSHS  -15.41203
    Provider 61:NonMSHS  -15.41203
    Provider 62:NonMSHS  -15.41203
    Provider 63:NonMSHS  -15.41203
    Provider 64:NonMSHS  -15.41203
    Provider 65:MSHS     -15.41203
    Provider 66:NonMSHS  -15.41203
    Provider 67:NonMSHS  -15.41203
    Provider 68:MSHS     -15.41203
    Provider 69:NonMSHS  -15.41203
    Provider 7:NonMSHS   -15.41203
    Provider 70:NonMSHS  -15.41203
    Provider 71:NonMSHS  -15.41203
    Provider 72:NonMSHS  -15.41203
    Provider 73:NonMSHS  -15.41203
    Provider 74:MSHS     -15.41203
    Provider 75:NonMSHS  -15.41203
    Provider 76:NonMSHS  -15.41203
    Provider 77:NonMSHS  -15.41203
    Provider 78:NonMSHS  -15.41203
    Provider 79:NonMSHS  -15.41203
    Provider 8:NonMSHS   -15.41203
    Provider 80:NonMSHS  -15.41203
    Provider 81:NonMSHS  -15.41203
    Provider 82:NonMSHS  -15.41203
    Provider 83:MSHS     -15.41203
    Provider 84:NonMSHS  -15.41203
    Provider 85:NonMSHS  -15.41203
    Provider 86:NonMSHS  -15.41203
    Provider 87:NonMSHS  -15.41203
    Provider 88:MSHS     -15.41203
    Provider 89:MSHS     -15.41203
    Provider 9:NonMSHS   -15.41203
    Provider 90:NonMSHS  -15.41203
    Provider 91:MSHS     -15.41203
    Provider 92:NonMSHS  -15.41203
    Provider 93:NonMSHS  -15.41203
    Provider 94:NonMSHS  -15.41203
    Provider 95:NonMSHS  -15.41203
    Provider 96:NonMSHS  -15.41203
    Provider 97:MSHS     -15.41203
    Provider 98:NonMSHS  -15.41203
    Provider 99:MSHS     -15.41203
    
    $gs
                 fix_settingOffice colonoscopy_typetherapeutic (Intercept)
    Provider 1          -57.663360                           0  1289.39098
    Provider 10       -2893.273145                           0  4352.48845
    Provider 100        489.436642                           0   698.39971
    Provider 101        611.288248                           0   566.77252
    Provider 102         42.777316                           0  1180.89242
    Provider 103        786.484214                           0   377.52141
    Provider 104        659.407823                           0   514.79254
    Provider 105        104.703951                           0  1113.99770
    Provider 106        -30.380151                           0  1259.91897
    Provider 107       1279.668023                           0  -155.22823
    Provider 108        887.420438                           0   268.48754
    Provider 109       -339.362419                           0  1593.68944
    Provider 11         381.071763                           0   815.45820
    Provider 110        -28.660533                           0  1258.06140
    Provider 111        438.420307                           0   753.50885
    Provider 112        366.060934                           0   831.67327
    Provider 113        565.868500                           0   615.83608
    Provider 114        254.695950                           0   951.97255
    Provider 115        152.398756                           0  1062.47657
    Provider 116        -94.364334                           0  1329.03630
    Provider 117        966.112325                           0   183.48257
    Provider 118         76.112789                           0  1144.88260
    Provider 119        687.324066                           0   484.63671
    Provider 12        -533.172819                           0  1803.04834
    Provider 120        393.032805                           0   802.53758
    Provider 121        706.011367                           0   464.45021
    Provider 122      -2955.757701                           0  4419.98585
    Provider 123        132.965459                           0  1083.46891
    Provider 124       -408.907655                           0  1668.81396
    Provider 125       -763.105970                           0  2051.42795
    Provider 126        -61.833165                           0  1293.89531
    Provider 127        382.990938                           0   813.38506
    Provider 128         -8.419117                           0  1236.19610
    Provider 13        -163.804951                           0  1404.04782
    Provider 14         -62.189845                           0  1294.28061
    Provider 15           6.801019                           0  1219.75493
    Provider 16         430.791675                           0   761.74949
    Provider 17          99.218370                           0  1119.92337
    Provider 18         343.904959                           0   855.60672
    Provider 19         695.303018                           0   476.01764
    Provider 2           75.850376                           0  1145.16607
    Provider 20         307.807310                           0   894.60031
    Provider 21         778.219711                           0   386.44893
    Provider 22         714.193009                           0   455.61220
    Provider 23         189.566065                           0  1022.32750
    Provider 24         -63.495155                           0  1295.69064
    Provider 25         106.641340                           0  1111.90489
    Provider 26         922.298398                           0   230.81149
    Provider 27       -2473.926552                           0  3899.49965
    Provider 28         360.069818                           0   838.14503
    Provider 29           9.951848                           0  1216.35132
    Provider 3          656.562900                           0   517.86570
    Provider 30          98.137186                           0  1121.09129
    Provider 31         749.995737                           0   416.93718
    Provider 32          60.634650                           0  1161.60248
    Provider 33        1227.874820                           0   -99.27990
    Provider 34         296.965114                           0   906.31233
    Provider 35       -2413.894482                           0  3834.65148
    Provider 36        -902.276166                           0  2201.76312
    Provider 37        -388.229129                           0  1646.47649
    Provider 38        -314.055276                           0  1566.35202
    Provider 39         527.870180                           0   656.88283
    Provider 4          398.398877                           0   796.74101
    Provider 40         -89.020707                           0  1323.26398
    Provider 41       -1851.060164                           0  3226.66361
    Provider 42       -1305.979822                           0  2637.85402
    Provider 43         817.860928                           0   343.62749
    Provider 44         438.068987                           0   753.88835
    Provider 45        -338.271522                           0  1592.51102
    Provider 46         669.770723                           0   503.59827
    Provider 47         574.321271                           0   606.70519
    Provider 48        1073.084095                           0    67.92896
    Provider 49         137.383513                           0  1078.69641
    Provider 5          342.754256                           0   856.84974
    Provider 50         -24.529938                           0  1253.59942
    Provider 51       -3729.025381                           0  5255.28917
    Provider 52        -478.177255                           0  1743.64074
    Provider 53        -227.705336                           0  1473.07463
    Provider 54       -1733.562679                           0  3099.73985
    Provider 55        1092.267664                           0    47.20638
    Provider 56       -3639.440520                           0  5158.51734
    Provider 57        -186.201317                           0  1428.24094
    Provider 58        -970.035355                           0  2274.95831
    Provider 59         -66.767788                           0  1299.22582
    Provider 6        -2198.813292                           0  3602.31534
    Provider 60         414.119110                           0   779.75962
    Provider 61        1046.074883                           0    97.10499
    Provider 62        -211.895767                           0  1455.99673
    Provider 63         557.136527                           0   625.26858
    Provider 64          15.764165                           0  1210.07271
    Provider 65         725.479719                           0   443.42001
    Provider 66        1179.901507                           0   -47.45791
    Provider 67         955.525696                           0   194.91852
    Provider 68         -67.471794                           0  1299.98630
    Provider 69       -1068.895999                           0  2381.75008
    Provider 7          797.260224                           0   365.88089
    Provider 70        -664.514511                           0  1944.92696
    Provider 71         826.368978                           0   334.43687
    Provider 72         733.102773                           0   435.18539
    Provider 73        1092.599059                           0    46.84840
    Provider 74        -164.506630                           0  1404.80579
    Provider 75       -1472.494480                           0  2817.72737
    Provider 76         658.628249                           0   515.63466
    Provider 77         898.761465                           0   256.23668
    Provider 78         663.241568                           0   510.65123
    Provider 79         -57.933048                           0  1289.68231
    Provider 8          754.599740                           0   411.96382
    Provider 80         955.148513                           0   195.32596
    Provider 81         270.991401                           0   934.36979
    Provider 82        1205.867372                           0   -75.50690
    Provider 83        -478.977090                           0  1744.50474
    Provider 84         353.006165                           0   845.77537
    Provider 85       -2474.377368                           0  3899.98663
    Provider 86         561.066102                           0   621.02376
    Provider 87        -153.851205                           0  1393.29553
    Provider 88        -274.890151                           0  1524.04486
    Provider 89         607.490037                           0   570.87545
    Provider 9          740.994270                           0   426.66079
    Provider 90         278.712411                           0   926.02936
    Provider 91       -2238.517789                           0  3645.20514
    Provider 92          55.460109                           0  1167.19215
    Provider 93        1422.925602                           0  -309.97869
    Provider 94         949.439942                           0   201.49250
    Provider 95         674.717857                           0   498.25425
    Provider 96         251.677143                           0   955.23354
    Provider 97       -1671.392391                           0  3032.58193
    Provider 98         185.515402                           0  1026.70313
    Provider 99       -3030.152780                           0  4500.34930
                   cnt_epi
    Provider 1   -15.41203
    Provider 10  -15.41203
    Provider 100 -15.41203
    Provider 101 -15.41203
    Provider 102 -15.41203
    Provider 103 -15.41203
    Provider 104 -15.41203
    Provider 105 -15.41203
    Provider 106 -15.41203
    Provider 107 -15.41203
    Provider 108 -15.41203
    Provider 109 -15.41203
    Provider 11  -15.41203
    Provider 110 -15.41203
    Provider 111 -15.41203
    Provider 112 -15.41203
    Provider 113 -15.41203
    Provider 114 -15.41203
    Provider 115 -15.41203
    Provider 116 -15.41203
    Provider 117 -15.41203
    Provider 118 -15.41203
    Provider 119 -15.41203
    Provider 12  -15.41203
    Provider 120 -15.41203
    Provider 121 -15.41203
    Provider 122 -15.41203
    Provider 123 -15.41203
    Provider 124 -15.41203
    Provider 125 -15.41203
    Provider 126 -15.41203
    Provider 127 -15.41203
    Provider 128 -15.41203
    Provider 13  -15.41203
    Provider 14  -15.41203
    Provider 15  -15.41203
    Provider 16  -15.41203
    Provider 17  -15.41203
    Provider 18  -15.41203
    Provider 19  -15.41203
    Provider 2   -15.41203
    Provider 20  -15.41203
    Provider 21  -15.41203
    Provider 22  -15.41203
    Provider 23  -15.41203
    Provider 24  -15.41203
    Provider 25  -15.41203
    Provider 26  -15.41203
    Provider 27  -15.41203
    Provider 28  -15.41203
    Provider 29  -15.41203
    Provider 3   -15.41203
    Provider 30  -15.41203
    Provider 31  -15.41203
    Provider 32  -15.41203
    Provider 33  -15.41203
    Provider 34  -15.41203
    Provider 35  -15.41203
    Provider 36  -15.41203
    Provider 37  -15.41203
    Provider 38  -15.41203
    Provider 39  -15.41203
    Provider 4   -15.41203
    Provider 40  -15.41203
    Provider 41  -15.41203
    Provider 42  -15.41203
    Provider 43  -15.41203
    Provider 44  -15.41203
    Provider 45  -15.41203
    Provider 46  -15.41203
    Provider 47  -15.41203
    Provider 48  -15.41203
    Provider 49  -15.41203
    Provider 5   -15.41203
    Provider 50  -15.41203
    Provider 51  -15.41203
    Provider 52  -15.41203
    Provider 53  -15.41203
    Provider 54  -15.41203
    Provider 55  -15.41203
    Provider 56  -15.41203
    Provider 57  -15.41203
    Provider 58  -15.41203
    Provider 59  -15.41203
    Provider 6   -15.41203
    Provider 60  -15.41203
    Provider 61  -15.41203
    Provider 62  -15.41203
    Provider 63  -15.41203
    Provider 64  -15.41203
    Provider 65  -15.41203
    Provider 66  -15.41203
    Provider 67  -15.41203
    Provider 68  -15.41203
    Provider 69  -15.41203
    Provider 7   -15.41203
    Provider 70  -15.41203
    Provider 71  -15.41203
    Provider 72  -15.41203
    Provider 73  -15.41203
    Provider 74  -15.41203
    Provider 75  -15.41203
    Provider 76  -15.41203
    Provider 77  -15.41203
    Provider 78  -15.41203
    Provider 79  -15.41203
    Provider 8   -15.41203
    Provider 80  -15.41203
    Provider 81  -15.41203
    Provider 82  -15.41203
    Provider 83  -15.41203
    Provider 84  -15.41203
    Provider 85  -15.41203
    Provider 86  -15.41203
    Provider 87  -15.41203
    Provider 88  -15.41203
    Provider 89  -15.41203
    Provider 9   -15.41203
    Provider 90  -15.41203
    Provider 91  -15.41203
    Provider 92  -15.41203
    Provider 93  -15.41203
    Provider 94  -15.41203
    Provider 95  -15.41203
    Provider 96  -15.41203
    Provider 97  -15.41203
    Provider 98  -15.41203
    Provider 99  -15.41203
    
    $fix_setting
             fix_settingOffice colonoscopy_typetherapeutic (Intercept)   cnt_epi
    Hospital                 0                   501.94770    3250.834 -15.41203
    Office                   0                    55.20937    1449.692 -15.41203
    
    attr(,"class")
    [1] "coef.mer"



```R
 # to select which physician is better suited to the program. Select provider_num:MSHS with low intercept value
coefficients(m3_interaction)
```


    $`gs:affiliation`
                         (Intercept) colonoscopy_typetherapeutic   cnt_epi
    Provider 1:NonMSHS     2117.1451                    511.1646 -32.15806
    Provider 10:MSHS       6012.1385                    511.1646 -32.15806
    Provider 100:MSHS      2688.1024                    511.1646 -32.15806
    Provider 101:NonMSHS   2547.5682                    511.1646 -32.15806
    Provider 102:NonMSHS   3080.6898                    511.1646 -32.15806
    Provider 103:NonMSHS   2338.1486                    511.1646 -32.15806
    Provider 104:NonMSHS   2530.9360                    511.1646 -32.15806
    Provider 105:NonMSHS   1358.8814                    511.1646 -32.15806
    Provider 106:NonMSHS   1667.7318                    511.1646 -32.15806
    Provider 107:NonMSHS   1856.8349                    511.1646 -32.15806
    Provider 108:NonMSHS   2263.5958                    511.1646 -32.15806
    Provider 109:MSHS      3521.8347                    511.1646 -32.15806
    Provider 11:NonMSHS    1907.6223                    511.1646 -32.15806
    Provider 110:MSHS      3221.3314                    511.1646 -32.15806
    Provider 111:NonMSHS   2721.9683                    511.1646 -32.15806
    Provider 112:NonMSHS   2757.9129                    511.1646 -32.15806
    Provider 113:NonMSHS   2606.5457                    511.1646 -32.15806
    Provider 114:NonMSHS   2917.0432                    511.1646 -32.15806
    Provider 115:NonMSHS   3009.9589                    511.1646 -32.15806
    Provider 116:NonMSHS   3269.8804                    511.1646 -32.15806
    Provider 117:NonMSHS   2182.6696                    511.1646 -32.15806
    Provider 118:NonMSHS   3094.5629                    511.1646 -32.15806
    Provider 119:NonMSHS   2437.1533                    511.1646 -32.15806
    Provider 12:NonMSHS    2673.9058                    511.1646 -32.15806
    Provider 120:NonMSHS   2795.5437                    511.1646 -32.15806
    Provider 121:NonMSHS   2462.3690                    511.1646 -32.15806
    Provider 122:MSHS      6074.5251                    511.1646 -32.15806
    Provider 123:MSHS      1148.2421                    511.1646 -32.15806
    Provider 124:MSHS      3631.3975                    511.1646 -32.15806
    Provider 125:MSHS      3890.6377                    511.1646 -32.15806
    Provider 126:NonMSHS   3239.8017                    511.1646 -32.15806
    Provider 127:NonMSHS   2788.4816                    511.1646 -32.15806
    Provider 128:NonMSHS   1866.8530                    511.1646 -32.15806
    Provider 13:NonMSHS    3036.5214                    511.1646 -32.15806
    Provider 14:NonMSHS    2655.6918                    511.1646 -32.15806
    Provider 15:NonMSHS    1700.2772                    511.1646 -32.15806
    Provider 16:NonMSHS    2778.6356                    511.1646 -32.15806
    Provider 17:NonMSHS    1392.7770                    511.1646 -32.15806
    Provider 18:NonMSHS    2866.5972                    511.1646 -32.15806
    Provider 19:NonMSHS    2564.0423                    511.1646 -32.15806
    Provider 2:NonMSHS     2927.7777                    511.1646 -32.15806
    Provider 20:NonMSHS    2297.8988                    511.1646 -32.15806
    Provider 21:NonMSHS    2405.0651                    511.1646 -32.15806
    Provider 22:NonMSHS    2487.5561                    511.1646 -32.15806
    Provider 23:NonMSHS    3009.6333                    511.1646 -32.15806
    Provider 24:NonMSHS    1746.0254                    511.1646 -32.15806
    Provider 25:NonMSHS    1371.5193                    511.1646 -32.15806
    Provider 26:MSHS       2251.8489                    511.1646 -32.15806
    Provider 27:MSHS       5729.2739                    511.1646 -32.15806
    Provider 28:NonMSHS     958.3078                    511.1646 -32.15806
    Provider 29:NonMSHS    1694.2594                    511.1646 -32.15806
    Provider 3:NonMSHS     2121.2786                    511.1646 -32.15806
    Provider 30:MSHS       3124.9053                    511.1646 -32.15806
    Provider 31:NonMSHS    2429.9931                    511.1646 -32.15806
    Provider 32:MSHS       2415.7351                    511.1646 -32.15806
    Provider 33:NonMSHS    1936.3945                    511.1646 -32.15806
    Provider 34:NonMSHS    2867.4398                    511.1646 -32.15806
    Provider 35:MSHS       5741.1500                    511.1646 -32.15806
    Provider 36:NonMSHS    4186.9406                    511.1646 -32.15806
    Provider 37:NonMSHS    3649.7630                    511.1646 -32.15806
    Provider 38:MSHS       2575.0289                    511.1646 -32.15806
    Provider 39:NonMSHS    2702.1242                    511.1646 -32.15806
    Provider 4:NonMSHS     2833.3991                    511.1646 -32.15806
    Provider 40:NonMSHS    1957.2093                    511.1646 -32.15806
    Provider 41:MSHS       5076.4543                    511.1646 -32.15806
    Provider 42:NonMSHS    4625.1159                    511.1646 -32.15806
    Provider 43:NonMSHS    2396.3745                    511.1646 -32.15806
    Provider 44:MSHS       2686.0177                    511.1646 -32.15806
    Provider 45:NonMSHS    3604.6880                    511.1646 -32.15806
    Provider 46:NonMSHS    1751.4690                    511.1646 -32.15806
    Provider 47:NonMSHS    2694.6510                    511.1646 -32.15806
    Provider 48:NonMSHS    2071.3573                    511.1646 -32.15806
    Provider 49:NonMSHS    3089.8441                    511.1646 -32.15806
    Provider 5:NonMSHS     2867.9251                    511.1646 -32.15806
    Provider 50:NonMSHS    3232.7833                    511.1646 -32.15806
    Provider 51:MSHS       7011.1170                    511.1646 -32.15806
    Provider 52:MSHS       3714.5940                    511.1646 -32.15806
    Provider 53:MSHS       3444.5508                    511.1646 -32.15806
    Provider 54:MSHS       4961.7547                    511.1646 -32.15806
    Provider 55:NonMSHS    1585.9289                    511.1646 -32.15806
    Provider 56:NonMSHS    6757.1364                    511.1646 -32.15806
    Provider 57:NonMSHS    3416.7787                    511.1646 -32.15806
    Provider 58:NonMSHS    3640.0681                    511.1646 -32.15806
    Provider 59:NonMSHS    1792.5773                    511.1646 -32.15806
    Provider 6:MSHS        5523.9232                    511.1646 -32.15806
    Provider 60:NonMSHS    2794.0926                    511.1646 -32.15806
    Provider 61:NonMSHS    2146.0582                    511.1646 -32.15806
    Provider 62:NonMSHS    2269.9771                    511.1646 -32.15806
    Provider 63:NonMSHS    2641.8929                    511.1646 -32.15806
    Provider 64:NonMSHS    3107.6607                    511.1646 -32.15806
    Provider 65:MSHS       2453.5274                    511.1646 -32.15806
    Provider 66:NonMSHS    1962.8103                    511.1646 -32.15806
    Provider 67:NonMSHS    2190.1807                    511.1646 -32.15806
    Provider 68:MSHS       1912.3674                    511.1646 -32.15806
    Provider 69:NonMSHS    4314.3541                    511.1646 -32.15806
    Provider 7:NonMSHS     2359.6921                    511.1646 -32.15806
    Provider 70:NonMSHS    3786.8731                    511.1646 -32.15806
    Provider 71:NonMSHS    2174.2882                    511.1646 -32.15806
    Provider 72:NonMSHS    2467.4771                    511.1646 -32.15806
    Provider 73:NonMSHS    1661.8610                    511.1646 -32.15806
    Provider 74:MSHS       3391.0865                    511.1646 -32.15806
    Provider 75:NonMSHS    4759.4719                    511.1646 -32.15806
    Provider 76:NonMSHS    2520.7349                    511.1646 -32.15806
    Provider 77:NonMSHS    2260.8247                    511.1646 -32.15806
    Provider 78:NonMSHS    2520.4160                    511.1646 -32.15806
    Provider 79:NonMSHS    3186.5700                    511.1646 -32.15806
    Provider 8:NonMSHS     2519.4643                    511.1646 -32.15806
    Provider 80:NonMSHS    2255.7205                    511.1646 -32.15806
    Provider 81:NonMSHS    2900.2851                    511.1646 -32.15806
    Provider 82:NonMSHS    1936.1071                    511.1646 -32.15806
    Provider 83:MSHS       3699.9405                    511.1646 -32.15806
    Provider 84:NonMSHS    2843.7710                    511.1646 -32.15806
    Provider 85:NonMSHS    5777.2535                    511.1646 -32.15806
    Provider 86:NonMSHS    2617.9334                    511.1646 -32.15806
    Provider 87:NonMSHS    2044.5116                    511.1646 -32.15806
    Provider 88:MSHS       3461.6659                    511.1646 -32.15806
    Provider 89:MSHS       2569.5462                    511.1646 -32.15806
    Provider 9:NonMSHS     2447.6075                    511.1646 -32.15806
    Provider 90:NonMSHS    2850.4522                    511.1646 -32.15806
    Provider 91:MSHS       5484.4232                    511.1646 -32.15806
    Provider 92:NonMSHS    3166.8597                    511.1646 -32.15806
    Provider 93:NonMSHS    1730.5978                    511.1646 -32.15806
    Provider 94:NonMSHS    2240.0727                    511.1646 -32.15806
    Provider 95:NonMSHS    2485.0955                    511.1646 -32.15806
    Provider 96:NonMSHS    2926.9000                    511.1646 -32.15806
    Provider 97:MSHS       4801.8525                    511.1646 -32.15806
    Provider 98:NonMSHS    1090.0872                    511.1646 -32.15806
    Provider 99:MSHS       6356.5321                    511.1646 -32.15806
    
    attr(,"class")
    [1] "coef.mer"



```R

```


    Error in eval(expr, envir, enclos): object 'affiliation' not found
    Traceback:
    



```R

```
