Statistical assignment 3
================
Francesca Hume 660024948
12/02/2020

In this assignment we will explore political interest (*vote6*) and how
it changes over time.

## Read data

First we want to read and join the data for the first 7 waves of the
Understanding Society. (Wave 8 does not have a variable for political
interest). We only want five variables: personal identifier, sample
origin, sex, age and political interest. It is tedious to join all the
seven waves manually, and it makes sense to use a loop in this case.
Since you don’t yet know about iteration I’ll provide the code for you;
please see the explanation of the code here:
<http://abessudnov.net/dataanalysis3/iteration.html>.

The only thing you need to do for this code to work on your computer is
to provide a path to the directory where the data are stored on your
computer.

``` r
library(tidyverse)
library(data.table)

# data.table is faster compared to readr so we'll use it in this case (the function fread()). You need to install this package first to be able to run this code.

# create a vector with the file names and paths

files <- dir(
             # Select the folder where the files are stored.
             "/Users/chescamae98/Desktop/UKDA-6614-tab/tab/",
             # Tell R which pattern you want present in the files it will display.
             pattern = "indresp",
             # We want this process to repeat through the entire folder.
             recursive = TRUE,
             # And finally want R to show us the entire file path, rather than just
             # the names of the individual files.
             full.names = TRUE)

# Select only files from the UKHLS.
files <- files[stringr::str_detect(files, "ukhls")]
files
```

    ## [1] "/Users/chescamae98/Desktop/UKDA-6614-tab/tab//ukhls_w1/a_indresp.tab"
    ## [2] "/Users/chescamae98/Desktop/UKDA-6614-tab/tab//ukhls_w2/b_indresp.tab"
    ## [3] "/Users/chescamae98/Desktop/UKDA-6614-tab/tab//ukhls_w3/c_indresp.tab"
    ## [4] "/Users/chescamae98/Desktop/UKDA-6614-tab/tab//ukhls_w4/d_indresp.tab"
    ## [5] "/Users/chescamae98/Desktop/UKDA-6614-tab/tab//ukhls_w5/e_indresp.tab"
    ## [6] "/Users/chescamae98/Desktop/UKDA-6614-tab/tab//ukhls_w6/f_indresp.tab"
    ## [7] "/Users/chescamae98/Desktop/UKDA-6614-tab/tab//ukhls_w7/g_indresp.tab"
    ## [8] "/Users/chescamae98/Desktop/UKDA-6614-tab/tab//ukhls_w8/h_indresp.tab"
    ## [9] "/Users/chescamae98/Desktop/UKDA-6614-tab/tab//ukhls_w9/i_indresp.tab"

``` r
# create a vector of variable names
vars <- c("memorig", "sex_dv", "age_dv", "vote6")

for (i in 1:7) {
        # Create a vector of the variables with the correct prefix.
        varsToSelect <- paste(letters[i], vars, sep = "_")
        # Add pidp to this vector (no prefix for pidp)
        varsToSelect <- c("pidp", varsToSelect)
        # Now read the data. 
        data <- fread(files[i], select = varsToSelect)
        if (i == 1) {
                all7 <- data  
        }
        else {
                all7 <- full_join(all7, data, by = "pidp")
        }
        # Now we can remove data to free up the memory.
        rm(data)
} 
```

## Reshape data (20 points)

Now we have got the data from all 7 waves in the same data frame
**all7** in the wide format. Note that the panel is unbalanced, i.e. we
included all people who participated in at least one wave of the survey.
Reshape the data to the long format. The resulting data frame should
have six columns for six variables.

``` r
Long <- all7 %>%
  pivot_longer(a_memorig:g_vote6, names_to = "variable", values_to = "value") %>%
  separate(variable, into = c("wave", "variable"), sep = "_", extra = "merge") %>%
  pivot_wider(names_from = variable, values_from = value) 

  
Long
```

    ## # A tibble: 584,234 x 6
    ##        pidp wave  memorig sex_dv age_dv vote6
    ##       <int> <chr>   <int>  <int>  <int> <int>
    ##  1 68001367 a           1      1     39     3
    ##  2 68001367 b          NA     NA     NA    NA
    ##  3 68001367 c          NA     NA     NA    NA
    ##  4 68001367 d          NA     NA     NA    NA
    ##  5 68001367 e          NA     NA     NA    NA
    ##  6 68001367 f          NA     NA     NA    NA
    ##  7 68001367 g          NA     NA     NA    NA
    ##  8 68004087 a           1      1     59     2
    ##  9 68004087 b           1      1     60     2
    ## 10 68004087 c           1      1     61     2
    ## # … with 584,224 more rows

## Filter and recode (20 points)

Now we want to filter the data keeping only respondents from the
original UKHLS sample for Great Britain (memorig == 1). We also want to
clean the variables for sex (recoding it to “male” or “female”) and
political interest (keeping the values from 1 to 4 and coding all
negative values as missing). Tabulate *sex* and *vote6* to make sure
your recodings were correct.

``` r
table(Long$vote6)
```

    ## 
    ##    -10     -9     -7     -2     -1      1      2      3      4 
    ##   4615    366  24725    431    358  30903 102110  86712  84295

``` r
Long <- Long %>%
        filter(memorig==1) %>%
        mutate(sex_dv =ifelse(sex_dv == 2, "female",
                           ifelse(sex_dv == 1, "male", NA))) %>%
        mutate(vote6 = case_when(
          vote6 %in% 1:4 ~ vote6,
          vote6 < 1 ~ NA_integer_
        ))


table(Long$sex_dv, Long$vote6)
```

    ##         
    ##              1     2     3     4
    ##   female  8154 36104 34172 33824
    ##   male   13506 34848 21961 18314

## Calculate mean political interest by sex and wave (10 points)

Political interest is an ordinal variable, but we will treat it as
interval and calculate mean political interest for men and women in each
wave.

``` r
table(Long$vote6)
```

    ## 
    ##     1     2     3     4 
    ## 21660 70952 56134 52145

``` r
meanVote6 <- Long %>%
  group_by(wave, sex_dv) %>%
  filter(!is.na(sex_dv)) %>%
  summarise(mean(vote6, na.rm = TRUE))
  
  
        
meanVote6
```

    ## # A tibble: 14 x 3
    ## # Groups:   wave [7]
    ##    wave  sex_dv `mean(vote6, na.rm = TRUE)`
    ##    <chr> <chr>                        <dbl>
    ##  1 a     female                        2.84
    ##  2 a     male                          2.53
    ##  3 b     female                        2.82
    ##  4 b     male                          2.51
    ##  5 c     female                        2.87
    ##  6 c     male                          2.54
    ##  7 d     female                        2.89
    ##  8 d     male                          2.55
    ##  9 e     female                        2.87
    ## 10 e     male                          2.51
    ## 11 f     female                        2.81
    ## 12 f     male                          2.47
    ## 13 g     female                        2.73
    ## 14 g     male                          2.42

## Reshape the data frame with summary statistics (20 points)

Your resulting data frame with the means is in the long format. Reshape
it to the wide format. It should look like this:

| sex\_dv | a | b | c | d | e | f | g |
| ------- | - | - | - | - | - | - | - |
| female  |   |   |   |   |   |   |   |
| male    |   |   |   |   |   |   |   |

In the cells of this table you should have mean political interest by
sex and wave.

Write a short interpretation of your findings.

``` r
names(meanVote6)<- c("Wave", "Sex", "mean_vote" )



meanVote6<- meanVote6 %>%
  pivot_wider(names_from = Wave, values_from = mean_vote)

meanVote6
```

    ## # A tibble: 2 x 8
    ##   Sex        a     b     c     d     e     f     g
    ##   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 female  2.84  2.82  2.87  2.89  2.87  2.81  2.73
    ## 2 male    2.53  2.51  2.54  2.55  2.51  2.47  2.42

``` r
## Noticable from the data is that, in all 7 of the waves looked at, the mean policitical interest of females is always a higher value (suggesting lesser political interest) than males. 
## Over the waves, the mean values of both males and females does appear to fluctuate, however, it appears that in general, political interest is increasing for both sexes as the mean values get lower. 
```

## Estimate stability of political interest (30 points)

Political scientists have been arguing how stable the level of political
interest is over the life course. Imagine someone who is not interested
in politics at all so that their value of *vote6* is always 4. Their
level of political interest is very stable over time, as stable as the
level of political interest of someone who is always very interested in
politics (*vote6* = 1). On the other hand, imagine someone who changes
their value of *votes6* from 1 to 4 and back every other wave. Their
level of political interest is very unstable.

Let us introduce a measure of stability of political interest that is
going to be equal to the sum of the absolute values of changes in
political interest from wave to wave. Let us call this measure Delta. It
is difficult for me to typeset a mathematical formula in Markdown, but
I’ll explain this informally.

Imagine a person with the level of political interest that is constant
over time: {1, 1, 1, 1, 1, 1, 1}. For this person, Delta is zero.

Now imagine a person who changes once from “very interested in politics”
to “fairly interested in politics”: {1, 1, 1, 1, 2, 2, 2}. For them,
Delta = (1 - 1) + (1 - 1) + (1 - 1) + (2 - 1) + (2 - 2) + (2 - 2) = 1.

Now imagine someone who changes from “very interested in politics” to
“not at all interested” every other wave: {1, 4, 1, 4, 1, 4, 1}. Delta
= (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) = 3
\* 6 = 18.

Large Delta indicates unstable political interest. Delta = 0 indicates a
constant level of political interest.

Write the R code that does the following.

1.  To simplify interpretation, keep only the respondents with
    non-missing values for political interest in all seven waves.

2.  Calculate Delta for each person in the data set.

3.  Calculate mean Delta for men and women.

4.  Calculate mean Delta by age (at wave 1) and plot the local
    polynomial curve showing the association between age at wave 1 and
    mean Delta. You can use either **ggplot2** or the *scatter.smooth()*
    function from base R.

5.  Write a short interpretation of your findings.

<!-- end list -->

``` r
#1 
Completeall<- Long

Completeall<- Completeall %>% 
  group_by(pidp) %>%
  filter(!is.na(vote6)) %>%
  mutate(delta = order_by(wave, vote6 - lag(vote6))) %>%
  mutate(delta = ifelse(is.na(delta), 0, delta)) %>% 
  filter(n()>= 7) 
  
Completeall$delta<-abs(Completeall$delta)

Completeall<- Completeall %>%
  group_by(pidp) %>%
  mutate(
    totaldelta = sum(delta)
  )
```

``` r
## Gender

mfmean2<- tapply(Completeall$totaldelta, Completeall$sex_dv, mean, na.rm=TRUE)

mfmean2
```

    ##   female     male 
    ## 2.494727 2.527760

``` r
## age

table(Completeall$age_dv)
```

    ## 
    ##   -9   15   16   17   18   19   20   21   22   23   24   25   26   27   28   29 
    ##    1    1  143  224  298  377  477  555  642  646  688  789  857  943 1043 1191 
    ##   30   31   32   33   34   35   36   37   38   39   40   41   42   43   44   45 
    ## 1300 1359 1411 1468 1485 1555 1584 1592 1688 1776 1904 1980 2083 2069 2134 2156 
    ##   46   47   48   49   50   51   52   53   54   55   56   57   58   59   60   61 
    ## 2145 2111 2156 2117 2110 2141 2125 2130 2065 2048 2036 2017 2043 2019 1999 2046 
    ##   62   63   64   65   66   67   68   69   70   71   72   73   74   75   76   77 
    ## 2138 2206 2248 2166 2210 2130 2019 1861 1714 1577 1473 1367 1289 1193 1100 1005 
    ##   78   79   80   81   82   83   84   85   86   87   88   89   90   91   92   93 
    ##  925  815  751  669  597  529  456  379  307  249  192  132  106   78   56   33 
    ##   94   95   96   97   98   99  100  101 
    ##   23   12    7    4    1    1    1    1

``` r
Completeall.age<- Completeall %>%
  mutate(age_dv = case_when(
          age_dv %in% 15:104 ~ age_dv,
          age_dv < 1 ~ NA_integer_))

Completeall.age<- Completeall.age %>% filter(!is.na(age_dv))


Completeall.age<- subset(Completeall.age, wave == 'a')

agemean<-as.data.frame(tapply(Completeall.age$totaldelta, Completeall.age$age_dv, mean,na.rm=TRUE))

agemean<- agemean %>%
  rownames_to_column("Age")

colnames(agemean)<- c("Age", "Mean")

agemean$Age<- as.numeric(agemean$Age)


ggplot<- ggplot(agemean, aes(x = agemean$Age, y= agemean$Mean))+
  geom_point()+
  geom_smooth()+
  labs(title="Average Delta Score by Age",
  x="Age",
  y="Delta")

ggplot
```

![](assignment3_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
## Looking at the plot, it shows that up until the age of around 50, the detal score, and this political interest, was relatively stable; decreasing slightly if anything between ages 15 to 50. From age 50 however, it appears that the delta score average increases as age increases, suggesting that political interest varies a lot more for these individuals. 
```
