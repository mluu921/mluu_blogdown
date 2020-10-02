---
title: "Natural Language Processing (NLP) and developing a machine learning classifier on Beyonce and Taylor Swift lyrics #TidyTuesday"
author: ''
date: '2020-10-02'
slug: taylor_swift_and_beyonce
categories: [rstats, tidymodels, nlp, textrecipes, tidytuesday]
tags: [rstats, tidymodels, nlp, textrecipes, tidytuesday]
subtitle: ''
summary: 'NLP and building a machine learning clasifier on Beyonce and Taylor Swift Lyrics #TidyTuesday'
authors: []
# lastmod: '2020-09-28T20:32:33-07:00'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---




Let's start off by loading the data from the tidytuesday github repository.

# Reading in data


```r
beyonce_lyrics <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv'
  ) %>%
  janitor::clean_names()

taylor_swift_lyrics <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv'
  )  %>%
  janitor::clean_names()
```

# Data pre-processing and feature engineering

It appears the **beyonce_lyrics** and the **taylor_swift_lyrics** are the pertinent data sets for building our machine learning classifier. Let's have a closer look at the two datasets.


```r
beyonce_lyrics 
```

```
## # A tibble: 22,616 x 6
##    line                        song_id song_name artist_id artist_name song_line
##    <chr>                         <dbl> <chr>         <dbl> <chr>           <dbl>
##  1 If I ain't got nothing, I ~   50396 1+1             498 Beyoncé             1
##  2 If I ain't got something, ~   50396 1+1             498 Beyoncé             2
##  3 'Cause I got it with you      50396 1+1             498 Beyoncé             3
##  4 I don't know much about al~   50396 1+1             498 Beyoncé             4
##  5 And it's me and you           50396 1+1             498 Beyoncé             5
##  6 That's all we'll have when~   50396 1+1             498 Beyoncé             6
##  7 'Cause baby, we ain't got ~   50396 1+1             498 Beyoncé             7
##  8 Darling, you got enough fo~   50396 1+1             498 Beyoncé             8
##  9 So come on, baby, make lov~   50396 1+1             498 Beyoncé             9
## 10 When my days look low         50396 1+1             498 Beyoncé            10
## # ... with 22,606 more rows
```

```r
glimpse(beyonce_lyrics)
```

```
## Rows: 22,616
## Columns: 6
## $ line        <chr> "If I ain't got nothing, I got you", "If I ain't got so...
## $ song_id     <dbl> 50396, 50396, 50396, 50396, 50396, 50396, 50396, 50396,...
## $ song_name   <chr> "1+1", "1+1", "1+1", "1+1", "1+1", "1+1", "1+1", "1+1",...
## $ artist_id   <dbl> 498, 498, 498, 498, 498, 498, 498, 498, 498, 498, 498, ...
## $ artist_name <chr> "Beyoncé", "Beyoncé", "Beyoncé", "Beyoncé", "Beyoncé", ...
## $ song_line   <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, ...
```

```r
taylor_swift_lyrics
```

```
## # A tibble: 132 x 4
##    artist    album     title           lyrics                                   
##    <chr>     <chr>     <chr>           <chr>                                    
##  1 Taylor S~ Taylor S~ Tim McGraw      "He said the way my blue eyes shinx\nPut~
##  2 Taylor S~ Taylor S~ Picture to Burn "State the obvious, I didn't get my perf~
##  3 Taylor S~ Taylor S~ Teardrops on m~ "Drew looks at me,\nI fake a smile so he~
##  4 Taylor S~ Taylor S~ A Place in Thi~ "I don't know what I want, so don't ask ~
##  5 Taylor S~ Taylor S~ Cold As You     "You have a way of coming easily to me\n~
##  6 Taylor S~ Taylor S~ The Outside     "I didn't know what I would find\nWhen I~
##  7 Taylor S~ Taylor S~ Tied Together ~ "Seems the only one who doesn't see your~
##  8 Taylor S~ Taylor S~ Stay Beautiful  "Cory's eyes are like a jungle\nHe smile~
##  9 Taylor S~ Taylor S~ Should’ve Said~ "It's strange to think the songs we used~
## 10 Taylor S~ Taylor S~ Mary’s Song     "She said\n\"I was seven, and you were n~
## # ... with 122 more rows
```

```r
glimpse(taylor_swift_lyrics)
```

```
## Rows: 132
## Columns: 4
## $ artist <chr> "Taylor Swift", "Taylor Swift", "Taylor Swift", "Taylor Swif...
## $ album  <chr> "Taylor Swift", "Taylor Swift", "Taylor Swift", "Taylor Swif...
## $ title  <chr> "Tim McGraw", "Picture to Burn", "Teardrops on my Guitar", "...
## $ lyrics <chr> "He said the way my blue eyes shinx\nPut those Georgia stars...
```

The **beyonce_lyrics** appears to be structured differently than the **taylor_swift_lyrics**. The lyrics from Taylor Swift is stored 1 line per title/song name, while Beyonce's lyrics are stored by song lines. This is a problem, and we'll have to rectify this prior to building our classifier.

My idea of rectifying this would be to collapse the data from Beyonce's lyrics to get them into the same structure as Taylor Swift's lyrics.


```r
beyonce_lyrics <- beyonce_lyrics %>%
  group_by(
    artist_name, song_name
  ) %>%
  summarise(
    lyrics = paste0(line, collapse = ' ')
  ) %>% 
  ungroup() %>%
  select(
    'artist' = artist_name,
    'title' = song_name,
    lyrics
  )

beyonce_lyrics
```

```
## # A tibble: 391 x 3
##    artist  title                        lyrics                                  
##    <chr>   <chr>                        <chr>                                   
##  1 Beyoncé "\"Self-Titled\" Part 1 . T~ "I see music. It's more than just...wha~
##  2 Beyoncé "\"Self-Titled\" Part 2 . I~ "There's a moment where things click. W~
##  3 Beyoncé "***Flawless (Ft. Chimamand~ "Your challengers are a young group fro~
##  4 Beyoncé "***Flawless (Remix) (Ft. N~ "Dum-da-de-da Do, do, do, do, do, do (C~
##  5 Beyoncé "<U+200B>come home (nala interlude)" "You have to come home We've really nee~
##  6 Beyoncé "<U+200B>war (nala interlude)"       "Your reign is over, Scar If you wanna ~
##  7 Beyoncé "<U+200E>blind trust"                "Go out tonight Feel nice tonight Stay ~
##  8 Beyoncé "1+1"                        "If I ain't got nothing, I got you If I~
##  9 Beyoncé "2017 Grammy's Best Urban C~ "Thank you so much. Hi, baby. Thank you~
## 10 Beyoncé "6 Inch (Ft. The Weeknd)"    "Six inch heels, she walked in the club~
## # ... with 381 more rows
```

Okay - this appears to be much better, and will allow us to merge them together with the Taylor Swift data. Our outcome *y* that we are trying to predict will be the 'artist' column. The features *x* will be the song lyrics. In order to get them to a usable state for our model, we will have to perform some preprocessing and feature engineering.


```r
taylor_swift_lyrics <- taylor_swift_lyrics %>% 
  select(
    artist, title, lyrics
  )

data <- bind_rows(
  taylor_swift_lyrics, beyonce_lyrics
)

data
```

```
## # A tibble: 523 x 3
##    artist     title             lyrics                                          
##    <chr>      <chr>             <chr>                                           
##  1 Taylor Sw~ Tim McGraw        "He said the way my blue eyes shinx\nPut those ~
##  2 Taylor Sw~ Picture to Burn   "State the obvious, I didn't get my perfect fan~
##  3 Taylor Sw~ Teardrops on my ~ "Drew looks at me,\nI fake a smile so he won't ~
##  4 Taylor Sw~ A Place in This ~ "I don't know what I want, so don't ask me\n'Ca~
##  5 Taylor Sw~ Cold As You       "You have a way of coming easily to me\nAnd whe~
##  6 Taylor Sw~ The Outside       "I didn't know what I would find\nWhen I went l~
##  7 Taylor Sw~ Tied Together Wi~ "Seems the only one who doesn't see your beauty~
##  8 Taylor Sw~ Stay Beautiful    "Cory's eyes are like a jungle\nHe smiles; it's~
##  9 Taylor Sw~ Should’ve Said No "It's strange to think the songs we used to sin~
## 10 Taylor Sw~ Mary’s Song       "She said\n\"I was seven, and you were nine\nI ~
## # ... with 513 more rows
```

After merging the data together, we will split our data into a separate training and testing dataset.

# Model building


```r
set.seed(1)
splits <- initial_split(data, strata = artist)

train <- training(splits)
test <- testing(splits)
```

The data has now been split, where 75% of the data we have available will be used to train our classifier, and the remaining 25% will be left for validation of the model and to estimate the overall performance.

We will next create a 'recipe' and perform feature engineering on our training data. We will do this in various steps, including tokenizing the lyrics, removing stop words, excluding words that appear less than 20 times, performing term-frequency inverse-document-frequency (TF-IDF), and finally normalization.


```r
rec <- recipe(artist ~ lyrics, data = train) %>%
  step_tokenize(lyrics) %>%
  step_stopwords(lyrics) %>%
  step_tokenfilter(lyrics, min_times = 20, max_tokens = 500) %>%
  step_tfidf(lyrics) %>%
  step_normalize(all_predictors())

rec
```

```
## Data Recipe
## 
## Inputs:
## 
##       role #variables
##    outcome          1
##  predictor          1
## 
## Operations:
## 
## Tokenization for lyrics
## Stop word removal for lyrics
## Text filtering for lyrics
## Term frequency-inverse document frequency with lyrics
## Centering and scaling for all_predictors()
```

Now that we have a 'recipe' for pre-processing our data into a usable state to feed into our model, we will create a specification of the classifier. In this instance we will be building a support vector machine (SVM) classifier from the kernlab package. 


```r
svm_spec <- svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_engine('kernlab') %>%
  set_mode('classification')

svm_spec
```

```
## Radial Basis Function Support Vector Machine Specification (classification)
## 
## Main Arguments:
##   cost = tune()
##   rbf_sigma = tune()
## 
## Computational engine: kernlab
```

## Model paremter tuning

The model parameters cost and rbf_sigma will be tuned via a grid search of 25 values


```r
svm_wf <- workflow() %>%
  add_model(svm_spec) %>%
  add_recipe(rec)

svm_tune_folds <- vfold_cv(train, strata = artist)

set.seed(1)
svm_tune_res <- tune_grid(
  svm_wf,
  resamples = svm_tune_folds,
  grid = 25
)

tune_metrics <- svm_tune_res %>% collect_metrics()

tune_metrics %>%
  filter(., .metric == 'accuracy') %>%
  ggplot(.,
         aes(y = rbf_sigma, x = cost, color = mean)) +
  geom_point() +
  scale_color_viridis_c()
```

<img src="/post/taylor_swift_and_beyonce/index_files/figure-html/unnamed-chunk-8-1.png" width="672" style="display: block; margin: auto;" />

```r
svm_tune_res %>% show_best(metric = 'accuracy')
```

```
## # A tibble: 5 x 7
##      cost   rbf_sigma .metric  .estimator  mean     n std_err
##     <dbl>       <dbl> <chr>    <chr>      <dbl> <int>   <dbl>
## 1 4.92    0.00122     accuracy binary     0.822    10 0.0143 
## 2 9.16    0.000508    accuracy binary     0.812    10 0.0151 
## 3 3.54    0.000220    accuracy binary     0.789    10 0.0122 
## 4 0.00120 0.00000211  accuracy binary     0.748    10 0.00195
## 5 0.00219 0.000000329 accuracy binary     0.748    10 0.00195
```

```r
best_accuracy <- svm_tune_res %>% select_best(., metric = 'accuracy')
```

# Final Model

The optimal tuning parameters for accuracy appears to be 4.916 for cost and 0.001 for rbf_sigma. We will use these parameters for our final model.


```r
svm_final_wf <- finalize_workflow(
  svm_wf,
  best_accuracy
)

final_res <- svm_final_wf %>%
  last_fit(splits)

final_metrics <- final_res %>% collect_metrics()

final_metrics
```

```
## # A tibble: 2 x 3
##   .metric  .estimator .estimate
##   <chr>    <chr>          <dbl>
## 1 accuracy binary         0.869
## 2 roc_auc  binary         0.918
```

Our final model using the tuned parameters optimizing for accuracy allowed us to achieve a model accuracy of 0.8692308 and ROC of binary, binary

Let's have a closer look at the performance of the model via a confusion matrix


```r
final_preds <- final_res %>%
  collect_predictions()


final_preds %>%
  conf_mat(
    ., artist, .pred_class
  ) 
```

```
##               Truth
## Prediction     Beyoncé Taylor Swift
##   Beyoncé           93           13
##   Taylor Swift       4           20
```

```r
final_preds %>%
  conf_mat(
    ., artist, .pred_class
  ) %>%
  summary()
```

```
## # A tibble: 13 x 3
##    .metric              .estimator .estimate
##    <chr>                <chr>          <dbl>
##  1 accuracy             binary         0.869
##  2 kap                  binary         0.621
##  3 sens                 binary         0.959
##  4 spec                 binary         0.606
##  5 ppv                  binary         0.877
##  6 npv                  binary         0.833
##  7 mcc                  binary         0.634
##  8 j_index              binary         0.565
##  9 bal_accuracy         binary         0.782
## 10 detection_prevalence binary         0.815
## 11 precision            binary         0.877
## 12 recall               binary         0.959
## 13 f_meas               binary         0.916
```

# Closing

The model appears to be doing a decent job classifying the artist by the lyrics of the songs with an overall accuracy of 86.9%. Furthermore, the model appears to be doing a better job at classifying Beyonce lyrics than Taylor Swift's

Let's have a closer look at the songs that were misclassified from our model


```r
test %>%
  select(., -artist) %>%
  bind_cols(final_preds) %>%
  select(
    artist, title,
    .pred_Beyoncé, `.pred_Taylor Swift`,
    .pred_class
  ) %>%
  filter(
    artist != .pred_class
  ) %>%
  mutate(
    across(c(.pred_Beyoncé, `.pred_Taylor Swift`), ~ paste0(format(round(.x*100, 1), 1), '%'))
  ) %>%
  print(50)
```

```
## # A tibble: 17 x 5
##    artist     title                 .pred_Beyoncé `.pred_Taylor Swi~ .pred_class
##    <fct>      <chr>                 <chr>         <chr>              <fct>      
##  1 Taylor Sw~ Tell Me Why           68.3%         31.7%              Beyoncé    
##  2 Taylor Sw~ Long Live             82.4%         17.6%              Beyoncé    
##  3 Taylor Sw~ Red                   69.1%         30.9%              Beyoncé    
##  4 Taylor Sw~ Holy Ground           58.0%         42.0%              Beyoncé    
##  5 Taylor Sw~ Sad Beautiful Tragic  78.9%         21.1%              Beyoncé    
##  6 Taylor Sw~ Come Back Be Here     65.9%         34.1%              Beyoncé    
##  7 Taylor Sw~ Blank Space           64.9%         35.1%              Beyoncé    
##  8 Taylor Sw~ How You Get The Girl  66.6%         33.4%              Beyoncé    
##  9 Taylor Sw~ New Romantics         74.1%         25.9%              Beyoncé    
## 10 Taylor Sw~ Death By A Thousand ~ 80.4%         19.6%              Beyoncé    
## 11 Taylor Sw~ Afterglow             82.8%         17.2%              Beyoncé    
## 12 Taylor Sw~ ME                    83.5%         16.5%              Beyoncé    
## 13 Taylor Sw~ my tear ricochet      76.3%         23.7%              Beyoncé    
## 14 Beyoncé    Back to Black (Ft. A~ 34.0%         66.0%              Taylor Swi~
## 15 Beyoncé    New Shoes             6.6%          93.4%              Taylor Swi~
## 16 Beyoncé    Pretty Hurts          32.2%         67.8%              Taylor Swi~
## 17 Beyoncé    Stop Sign             38.5%         61.5%              Taylor Swi~
```






















