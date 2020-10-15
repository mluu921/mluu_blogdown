---
title: "Publication Quality Kaplan-Meier Survival Curves using ggplot2"
author: ''
date: '2020-10-14'
slug: km_ggplot
categories: [rstats, survival, ggplot2, datavis]
tags: [rstats, survival, ggplot2, datavis]
subtitle: ''
summary: 'The Kaplan-Meier (KM) survival curves are a hallmark figure that is commonly used to illustrate "time to event" analysis in clinical research. In this illustrative example, I will be using the `veterans` data from the `survival` package to construct KM survival curves using `ggplot2` and building the figure from basic geoms within the package.'
authors: []
reading_time: false  # Show estimated reading time?
# lastmod: '2020-09-28T20:32:33-07:00'
featured: yes
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---




The Kaplan-Meier (KM) survival curves are a hallmark figure that is commonly used to illustrate "time to event" analysis in clinical research. In this illustrative example, I will be using the `veterans` data from the `survival` package to construct KM survival curves using `ggplot2` and building the figure from basic geoms within the package. I will also provide examples of other publicly available packages that can facilitate in constructing KM figures that utilizes `ggplot2`. I believe this is a good exercise and illustrative example in potentially more advanced and little known techniques in `ggplot2`, as well as provide insight in the flexibility and capabilities that are available in this package.

The `veterans` data comes from a randomised trial of two treatment regimens for lung cancer. Let's start off by loading the `veterans` data from the `survival` and have a look at the data that we are currently working with.


```r
df <- survival::veteran %>% as_tibble()

glimpse(df)
```

```
## Rows: 137
## Columns: 8
## $ trt      <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
## $ celltype <fct> squamous, squamous, squamous, squamous, squamous, squamous...
## $ time     <dbl> 72, 411, 228, 126, 118, 10, 82, 110, 314, 100, 42, 8, 144,...
## $ status   <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1...
## $ karno    <dbl> 60, 70, 60, 60, 70, 20, 40, 80, 50, 70, 60, 40, 30, 80, 70...
## $ diagtime <dbl> 7, 5, 3, 9, 11, 5, 10, 29, 18, 6, 4, 58, 4, 9, 11, 3, 9, 2...
## $ age      <dbl> 69, 64, 38, 63, 65, 49, 69, 68, 43, 70, 81, 63, 63, 52, 48...
## $ prior    <dbl> 0, 10, 0, 10, 10, 0, 10, 0, 0, 0, 0, 10, 0, 10, 10, 0, 0, ...
```

```r
df
```

```
## # A tibble: 137 x 8
##      trt celltype  time status karno diagtime   age prior
##    <dbl> <fct>    <dbl>  <dbl> <dbl>    <dbl> <dbl> <dbl>
##  1     1 squamous    72      1    60        7    69     0
##  2     1 squamous   411      1    70        5    64    10
##  3     1 squamous   228      1    60        3    38     0
##  4     1 squamous   126      1    60        9    63    10
##  5     1 squamous   118      1    70       11    65    10
##  6     1 squamous    10      1    20        5    49     0
##  7     1 squamous    82      1    40       10    69    10
##  8     1 squamous   110      1    80       29    68     0
##  9     1 squamous   314      1    50       18    43     0
## 10     1 squamous   100      0    70        6    70     0
## # ... with 127 more rows
```

The codebook for the dataset is provided below as follows: 

* **trt**: 1=standard 2=test
* **celltype**: 1=squamous, 2=smallcell, 3=adeno, 4=large
* **time**: survival time (days)
* **status**: censoring status
* **karno**: Karnofsky performance score (100=good)
* **diagtime**: months from diagnosis to randomisation
* **age**: in years
* **prior**: prior therapy 0=no, 10=yes

To handle 'time to event' data in R, we will first need to construct a survival object that encapsulates both the time to event information `time` in our dataset as well as the event/censoring variable `status`. We can then fit the data using the `survfit()` function by constructing a formula with our response variable (survival object) on the left of the `~` and the explanatory variable `trt` on the right. The `summary()` of the object from `survfit()` provides us the probability of survival for a given treatment over time.


```r
fit <- survfit(Surv(time, status) ~ trt, data = df)

summary(fit)
```

```
## Call: survfit(formula = Surv(time, status) ~ trt, data = df)
## 
##                 trt=1 
##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
##     3     69       1   0.9855  0.0144      0.95771        1.000
##     4     68       1   0.9710  0.0202      0.93223        1.000
##     7     67       1   0.9565  0.0246      0.90959        1.000
##     8     66       2   0.9275  0.0312      0.86834        0.991
##    10     64       2   0.8986  0.0363      0.83006        0.973
##    11     62       1   0.8841  0.0385      0.81165        0.963
##    12     61       2   0.8551  0.0424      0.77592        0.942
##    13     59       1   0.8406  0.0441      0.75849        0.932
##    16     58       1   0.8261  0.0456      0.74132        0.921
##    18     57       2   0.7971  0.0484      0.70764        0.898
##    20     55       1   0.7826  0.0497      0.69109        0.886
##    21     54       1   0.7681  0.0508      0.67472        0.874
##    22     53       1   0.7536  0.0519      0.65851        0.862
##    27     51       1   0.7388  0.0529      0.64208        0.850
##    30     50       1   0.7241  0.0539      0.62580        0.838
##    31     49       1   0.7093  0.0548      0.60967        0.825
##    35     48       1   0.6945  0.0556      0.59368        0.812
##    42     47       1   0.6797  0.0563      0.57782        0.800
##    51     46       1   0.6650  0.0570      0.56209        0.787
##    52     45       1   0.6502  0.0576      0.54649        0.774
##    54     44       2   0.6206  0.0587      0.51565        0.747
##    56     42       1   0.6059  0.0591      0.50040        0.734
##    59     41       1   0.5911  0.0595      0.48526        0.720
##    63     40       1   0.5763  0.0598      0.47023        0.706
##    72     39       1   0.5615  0.0601      0.45530        0.693
##    82     38       1   0.5467  0.0603      0.44049        0.679
##    92     37       1   0.5320  0.0604      0.42577        0.665
##    95     36       1   0.5172  0.0605      0.41116        0.651
##   100     34       1   0.5020  0.0606      0.39615        0.636
##   103     32       1   0.4863  0.0607      0.38070        0.621
##   105     31       1   0.4706  0.0608      0.36537        0.606
##   110     30       1   0.4549  0.0607      0.35018        0.591
##   117     29       2   0.4235  0.0605      0.32017        0.560
##   118     27       1   0.4079  0.0602      0.30537        0.545
##   122     26       1   0.3922  0.0599      0.29069        0.529
##   126     24       1   0.3758  0.0596      0.27542        0.513
##   132     23       1   0.3595  0.0592      0.26031        0.496
##   139     22       1   0.3432  0.0587      0.24535        0.480
##   143     21       1   0.3268  0.0582      0.23057        0.463
##   144     20       1   0.3105  0.0575      0.21595        0.446
##   151     19       1   0.2941  0.0568      0.20151        0.429
##   153     18       1   0.2778  0.0559      0.18725        0.412
##   156     17       1   0.2614  0.0550      0.17317        0.395
##   162     16       2   0.2288  0.0527      0.14563        0.359
##   177     14       1   0.2124  0.0514      0.13218        0.341
##   200     12       1   0.1947  0.0501      0.11761        0.322
##   216     11       1   0.1770  0.0486      0.10340        0.303
##   228     10       1   0.1593  0.0468      0.08956        0.283
##   250      9       1   0.1416  0.0448      0.07614        0.263
##   260      8       1   0.1239  0.0426      0.06318        0.243
##   278      7       1   0.1062  0.0400      0.05076        0.222
##   287      6       1   0.0885  0.0371      0.03896        0.201
##   314      5       1   0.0708  0.0336      0.02793        0.180
##   384      4       1   0.0531  0.0295      0.01788        0.158
##   392      3       1   0.0354  0.0244      0.00917        0.137
##   411      2       1   0.0177  0.0175      0.00256        0.123
##   553      1       1   0.0000     NaN           NA           NA
## 
##                 trt=2 
##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
##     1     68       2   0.9706  0.0205      0.93125        1.000
##     2     66       1   0.9559  0.0249      0.90830        1.000
##     7     65       2   0.9265  0.0317      0.86647        0.991
##     8     63       2   0.8971  0.0369      0.82766        0.972
##    13     61       1   0.8824  0.0391      0.80900        0.962
##    15     60       2   0.8529  0.0429      0.77278        0.941
##    18     58       1   0.8382  0.0447      0.75513        0.930
##    19     57       2   0.8088  0.0477      0.72056        0.908
##    20     55       1   0.7941  0.0490      0.70360        0.896
##    21     54       1   0.7794  0.0503      0.68684        0.884
##    24     53       2   0.7500  0.0525      0.65383        0.860
##    25     51       3   0.7059  0.0553      0.60548        0.823
##    29     48       1   0.6912  0.0560      0.58964        0.810
##    30     47       1   0.6765  0.0567      0.57394        0.797
##    31     46       1   0.6618  0.0574      0.55835        0.784
##    33     45       1   0.6471  0.0580      0.54289        0.771
##    36     44       1   0.6324  0.0585      0.52754        0.758
##    43     43       1   0.6176  0.0589      0.51230        0.745
##    44     42       1   0.6029  0.0593      0.49717        0.731
##    45     41       1   0.5882  0.0597      0.48216        0.718
##    48     40       1   0.5735  0.0600      0.46724        0.704
##    49     39       1   0.5588  0.0602      0.45244        0.690
##    51     38       2   0.5294  0.0605      0.42313        0.662
##    52     36       2   0.5000  0.0606      0.39423        0.634
##    53     34       1   0.4853  0.0606      0.37993        0.620
##    61     33       1   0.4706  0.0605      0.36573        0.606
##    73     32       1   0.4559  0.0604      0.35163        0.591
##    80     31       2   0.4265  0.0600      0.32373        0.562
##    84     28       1   0.4112  0.0597      0.30935        0.547
##    87     27       1   0.3960  0.0594      0.29509        0.531
##    90     25       1   0.3802  0.0591      0.28028        0.516
##    95     24       1   0.3643  0.0587      0.26560        0.500
##    99     23       2   0.3326  0.0578      0.23670        0.467
##   111     20       2   0.2994  0.0566      0.20673        0.434
##   112     18       1   0.2827  0.0558      0.19203        0.416
##   133     17       1   0.2661  0.0550      0.17754        0.399
##   140     16       1   0.2495  0.0540      0.16326        0.381
##   164     15       1   0.2329  0.0529      0.14920        0.363
##   186     14       1   0.2162  0.0517      0.13538        0.345
##   201     13       1   0.1996  0.0503      0.12181        0.327
##   231     12       1   0.1830  0.0488      0.10851        0.308
##   242     10       1   0.1647  0.0472      0.09389        0.289
##   283      9       1   0.1464  0.0454      0.07973        0.269
##   340      8       1   0.1281  0.0432      0.06609        0.248
##   357      7       1   0.1098  0.0407      0.05304        0.227
##   378      6       1   0.0915  0.0378      0.04067        0.206
##   389      5       1   0.0732  0.0344      0.02912        0.184
##   467      4       1   0.0549  0.0303      0.01861        0.162
##   587      3       1   0.0366  0.0251      0.00953        0.140
##   991      2       1   0.0183  0.0180      0.00265        0.126
##   999      1       1   0.0000     NaN           NA           NA
```

The base R plotting method provides us with a basic KM figure. We can generate the figure by using the `plot()` function on the `fit` object.


```r
plot(fit)
```

<img src="/post/ggplot2km/index_files/figure-html/unnamed-chunk-3-1.png" width="576" style="display: block; margin: auto;" />

The `GGally` package also includes `ggsurv()` which actually uses the `ggplot2` in the backend to construct the figure.


```r
GGally::ggsurv(fit)
```

<img src="/post/ggplot2km/index_files/figure-html/unnamed-chunk-4-1.png" width="576" style="display: block; margin: auto;" />

An even further improved KM figure comes from the `survminer` package that includes a 'Number at risk' table that is commonly show in combination with the KM figure.


```r
survminer::ggsurvplot(fit, data = df, risk.table = T)
```

<img src="/post/ggplot2km/index_files/figure-html/unnamed-chunk-5-1.png" width="576" style="display: block; margin: auto;" />

The KM figure I'm constructing is going to be based on the `survminer` figure, that includes the secondary 'Number at risk' table. 

We can start by estimating the survival estimates from day 0 to day 500, I chose 500 since it appears that survival trails off after 500 days and this is a method of truncating the figure. Then we can extract the survival estimates into a a structured tidy tibble.


```r
s <- summary(fit, times = seq(0, 500, 1), extend = T)

plot_data <- tibble(
  'time' = s$time,
  'n.risk' = s$n.risk,
  'n.event' = s$n.event,
  'n.censor' = s$n.censor,
  'estimate' = s$surv,
  'std.error' = s$std.err,
  'strata' = s$strata
)
```

Now that we have the 'tidied' data, we can start by constructing the base plot we will use to build from. We will map the x axis to `time`, the y axis to `estimate`, and the fill to `strata`.


```r
p <- ggplot(plot_data, aes(x = time, y = estimate, color = strata))

p
```

<img src="/post/ggplot2km/index_files/figure-html/unnamed-chunk-7-1.png" width="576" style="display: block; margin: auto;" />

The primary geom in building the figure is `geom_step()`


```r
p <- p + geom_step(aes(linetype = strata), size = 1)

p
```

<img src="/post/ggplot2km/index_files/figure-html/unnamed-chunk-8-1.png" width="576" style="display: block; margin: auto;" />

This is pretty close to the plot that is provided from the `GGally` package already, we just need a few more steps to further clean up the axes, adjust the aesthetics, and to add a theme. I also further expanded the x axes to 550 to provide some additional room for curve annotations.


```r
p <- p + 
  scale_x_continuous(breaks = seq(0, 500, 50)) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic() +
  theme(legend.position = 'top',
        axis.title = element_text(face = 'bold')) +
  labs(x = 'Days', y = 'Probability of Survival') + 
  coord_cartesian(xlim = c(0, 550)) + 
  ggsci::scale_color_d3()


p
```

<img src="/post/ggplot2km/index_files/figure-html/unnamed-chunk-9-1.png" width="576" style="display: block; margin: auto;" />

We can further supplement this figure by adding markers on the curves using the `ggrepel` package. The simplest method I found to identify the coordinates of the ideal location of the annotations is by taking the last point of the curves by strata. Then we can use the `geom_text_repel()` function to add the text label to the curves accordingly. Now that we have the annotations on the figure, we can remove the legends to give the actual figure some additional room.


```r
annotate_data <- plot_data %>%
  group_by(strata) %>%
  slice_tail(1)

p <- p + 
  ggrepel::geom_text_repel(data = annotate_data, aes(x = time, y = estimate, label = strata),
                           xlim = c(500, NA)) + 
  theme(legend.position = 'none')

p
```

<img src="/post/ggplot2km/index_files/figure-html/unnamed-chunk-10-1.png" width="576" style="display: block; margin: auto;" />

Next, we can construct the 'At risk' table below the figure we just constructed. The table is actually a ggplot, where we are constructing a table of number of at risk plotted by `time` on the x axis and `strata` on the y axis. Since the time interval for the KM figure is per every 50 days, we will extract the 'At risk' data similarly on a per 100 days basis. The most important concept to remember is to make the scale of the x axis `scale_x_continuous()` is identical to the KM figure to have the alignment match between the two. The number at risk is then plotted using `geom_text()` with `n.risk` as the label.


```r
table_data <- plot_data %>% 
  filter(
    time %in% seq(0, 500, 50)
  ) 

t <- ggplot(table_data, aes(y = fct_rev(strata), x = time)) + 
  geom_text(aes(label = n.risk)) + 
  scale_x_continuous(breaks = seq(0, 500, 50), limits = c(0, 550))

t
```

<img src="/post/ggplot2km/index_files/figure-html/unnamed-chunk-11-1.png" width="576" style="display: block; margin: auto;" />

Now that we have a basis of the plot for the table, we can further customize it by adding a theme, and then further clean up the axes and the labels of the figure.


```r
t <- t + 
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(face = 'bold'))

t
```

<img src="/post/ggplot2km/index_files/figure-html/unnamed-chunk-12-1.png" width="576" style="display: block; margin: auto;" />

We now have 2 ggplot objects, `p` and `t`. The `patchwork` package is the 'glue' we need to put the two plots together. 


```r
library(patchwork)

km <- (p / t) + plot_layout(height = c(1, .25))

km
```

<img src="/post/ggplot2km/index_files/figure-html/unnamed-chunk-13-1.png" width="576" style="display: block; margin: auto;" />

There you have it - a publication quality KM figure ready for submission.
































































