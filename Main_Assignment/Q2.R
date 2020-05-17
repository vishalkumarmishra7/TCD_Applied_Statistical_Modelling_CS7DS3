# Title     : TODO
# Objective : TODO
# Created by: vishal
# Created on: 14-05-2020

# 1. Import package library and data
# library(readr)
# library(dplyr)
# library(ggplot2)
# library(ggjoy)
# library(gridExtra)
# library(ggridges)
# library(knitr)
# library(purrr)
# library(tidyr)
# library(tibble)
# library(magrittr)
# library(plotly)
# library(stringr)
# library(dplyr)
# library(quanteda)
# library(stringr)
#
suppressMessages(library(tidyverse))
suppressMessages(library(tidytext))
suppressMessages(library(gridExtra))
suppressMessages(library(ggridges))
suppressMessages(library(igraph))
suppressMessages(library(RColorBrewer))
suppressMessages(library(widyr))

#2. Load dataset
full_dataset <- read_csv("wwinemag-data-130k-v2.csv")
glimpse(full_dataset)
us_dataset<-filter(full_dataset, country == "US")

#3. Data preprocessing
# How many records have no NA’s in them?
sum(complete.cases(us_dataset))
# [1] 22387

# 2. NA remediation
# There are quite a few missing values there. We're not really interested in regions, designations and twitter handles,
# so we can drop these variables and then get rid of the remaining NAs.

# What is the distribution of the NA’s?
missing_data = us_dataset %>%
    map_df(function(x) sum(is.na(x))) %>%
    gather(feature, num_nulls)%>%
    arrange(desc(num_nulls))%>%
    mutate(percent_missing = num_nulls/nrow(us_dataset)*100)

kable(missing_data, digits = c(0,0,0))
# ggplot(missing_data, aes(reorder(feature,percent_missing), percent_missing)) +
#   geom_bar(stat = "identity")  +
#   coord_flip() +
#   labs(title = "Feature By Percent of Data Missing", y = "Percent of Observations with Feature Missing", x = "Feature") +
#   scale_y_continuous(limits = c(0, 100))

# |feature               | num_nulls| percent_missing|
# |:---------------------|---------:|---------------:|
# |taster_twitter_handle |     19763|              36|
# |designation           |     17596|              32|
# |taster_name           |     16774|              31|
# |region_2              |      3993|               7|
# |region_1              |       278|               1|
# |price                 |       239|               0|
# |X1                    |         0|               0|
# |country               |         0|               0|
# |description           |         0|               0|
# |points                |         0|               0|
# |province              |         0|               0|
# |title                 |         0|               0|
# |variety               |         0|               0|
# |winery                |         0|               0|

# Price is important and there are only about 239 that don’t have a price. Let’s drop those rows and see how it impacts the other NA’s.
us_dataset_02 <- us_dataset %>%
    filter(!is.na(price))

# us_dataset_03 <- na.omit(us_dataset)
# Where is the year? ADD year
# noticed that the dataset doesn’t have a year, but I observed the year is in all of the titles.
# us_dataset_03 <-
#   us_dataset_02 %>%
#     extract(title,
#             'year',
#             "(20\\d\\d)",
#             convert = T,
#             remove = F) %>%
#     mutate(year=ifelse(year<1900,NA,year))
colSums(is.na(us_dataset_03))

library(tidyverse)
# us_dataset_03 <- us_dataset_02 %>%extract(title,'year',"(20\\d\\d)",convert = T,remove = F)%>%mutate(year=ifelse(year<1900,NA,year))

#                    X1               country           description           designation                points
#                     0                     0                     0                 17514                     0
#                 price              province              region_1              region_2           taster_name
#                     0                     0                   276                  3973                 16754
# taster_twitter_handle                 title                  year               variety                winery
#                 19730                     0                  1859                     0                     0

# drop columns in which we're not interested and drop rows with missing data
# us_dataset_04 <-
#   us_dataset_03 %>%
#   select(-X,
#          -country,
#          -region_2,
#          -taster_twitter_handle,
#          -designation,
#   -title)

# Word count
us_dataset_03$wordcount <- as.numeric(sapply(gregexpr("\\S+", us_dataset_03$description), length))
# us_dataset_03$wordcount <- as.numeric(us_dataset_03$wordcount)
summary(us_dataset_03$wordcount)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#    6.00   32.00   40.00   40.79   48.00  135.00

# us_dataset_03$freq<-str_count(us_dataset_03$description,'\\w+')
us_dataset_03$sent_count<-nsentence(us_dataset_03$description)
us_dataset_03<- us_dataset_03 %>% mutate(log_price = log(price))
us_dataset_03<- us_dataset_03 %>% mutate(desc_len = str_length(description))

#
# us_dataset_04$description[which(us_dataset_04$wordcount == 135)]
# us_dataset_04$description[which(us_dataset_04$wordcount == 3)]
# ggplot(data = us_dataset_04, aes(x= wordcount))+
#   geom_histogram(binwidth = 3)+
#   labs(x = "Word Count", y= "Frequency", title = "Distribution of word count of description")

ggplot(data = us_dataset_03, aes(x= points, colour = I('black'), fill = I('cyan')))+
  geom_histogram(binwidth = 2)+
  labs(x = "Points", y= "Frequency");#, title = "Distribution of points")


# 3.  Stats
# Lets see which wine Varietals are most popular with our tasters.
v_vec<- us_dataset_03 %>% count(variety, sort = T) %>% head(20) %>% pull(variety)

us_dataset_03 %>%
  filter(variety %in% v_vec) %>%
  ggplot(aes(x = reorder(variety, points), y = points))+
  geom_boxplot(aes(fill = variety), show.legend = F, alpha = .7)+
  coord_flip()+
  theme_bw()+
  labs(x = ''
       #title = 'Wine Ratings by Varietal'
  )+
  theme(plot.title = element_text(hjust = 0.5, vjust = 2.5))


# Lets see what correlations are between points and price, as well as Price and Wine Description length.

cor_price_points<- us_dataset_03 %>% filter(!is.na(price)) %>%
  summarize(cor(price, points)) %>% round(3) %>% pull()

cor_price_desc_len<- us_dataset_03 %>% filter(!is.na(price)) %>%
  summarize(cor(desc_len, points)) %>% round(3) %>% pull()

p1<- us_dataset_03 %>%
  filter(!is.na(price)) %>%
  ggplot(aes(x = log_price, y = points))+
  geom_point(size = 2,alpha = .3)+
  geom_smooth(method = 'lm')+
  annotate('text', x = 2, y = 100, label = 'Cor = 0.395')#+  labs(title = 'Price vs Points Awarded' )

p2<- us_dataset_03 %>%
  filter(!is.na(price)) %>%
  ggplot(aes(x = desc_len, y = points))+
  geom_point(seize = 2, alpha = .3)+
  geom_smooth(method = 'lm')+
  annotate('text', x = 2, y= 100, label = 'Cor = 0.549')#+  labs(title = "Description Length vs points")

grid.arrange(p1, p2)
# Interestingly enough, our tasters seems to be a little more wordy when reviewing expensive wines. It also seems that
# the higher the price goes, the less likely it becomes to get a good points/price value on the wine. I wonder if some
# of the tasters are tougher then others.

# I’m going to unnest our descriptions down to only words to try to find some interesting insights.
unnested_us_dataset_03<- us_dataset_03 %>%
  mutate(description = str_to_lower(description)) %>%
   unnest_tokens(word, description) %>%
   anti_join(stop_words, by = 'word')
unnested_us_dataset_03<- unnested_us_dataset_03 %>% filter(word != 'wine')
# What words typically show up in good/bad reviews. This is when its nice to be working with such a large dataset, as
# it contains a large enough number of words to be interesting.
mean_point<- mean(us_dataset_03$points) %>% round(2)

tib1<- unnested_us_dataset_03 %>%
  filter(str_detect(word, "[:alpha:]+")) %>%
  group_by(word) %>%
  summarize(mean = mean(points), num = n(), sd = sd(points)) %>%
  filter(num > 50) %>%
  arrange(desc(mean)) %>% head(14)

tib2<- unnested_us_dataset_03 %>%
  filter(str_detect(word, "[:alpha:]+")) %>%
  group_by(word) %>%
  summarize(mean = mean(points), num = n(), sd = sd(points)) %>%
  filter(num > 50) %>%
  arrange(mean) %>% head(14)

full<- rbind(tib1, tib2)

full %>%
  mutate( num_words = str_to_title(paste0(word ," ", '(',num,')'))) %>%
  ggplot(aes(x = reorder(num_words,mean), y = mean))+
  geom_point(aes(colour = mean > mean_point), show.legend = F, size = 3)+
  geom_hline(yintercept = mean_point, lty = 3)+
  geom_segment(aes(y = mean_point, x = num_words, yend = mean, xend = num_words, colour = mean> mean_point),
               show.legend = F, linetype = 1, size = 1.3)+
  coord_flip()+
  scale_colour_manual(values = c('grey40','slateblue1'))+
  theme_minimal()+
  labs(x = '',
       y= "Average Points",
       #title = 'What Words Show up In Good/Poor Wine Reviews',
       subtitle = '# times word is used in brackets'
       )


# 4. encoding
colSums(is.na(us_dataset_03))
#                    X1               country           description           designation                points
#                     0                     0                     0                 17514                     0
#                 price              province              region_1              region_2           taster_name
#                     0                     0                   276                  3973                 16754
# taster_twitter_handle                 title               variety                winery             wordcount
#                 19730                     0                     0                     0                     0
#            sent_count             log_price              desc_len
#                     0                     0                     0
us_dataset_04<-select (us_dataset_03,-c(X1,country, designation, region_2, taster_twitter_handle, title))
colSums(is.na(us_dataset_04))
# description      points       price    province    region_1 taster_name     variety      winery   wordcount  sent_count
#           0           0           0           0         276       16754           0           0           0           0
#   log_price    desc_len
#           0           0

encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
}

us_dataset_04$province <- encode_ordinal(us_dataset_03[["province"]])
us_dataset_04$region_1<-encode_ordinal(us_dataset_04[["region_1"]])
us_dataset_04$taster_name<-encode_ordinal(us_dataset_04[["taster_name"]])
# us_dataset_04$year<-encode_ordinal(us_dataset_04[["year"]])
us_dataset_04$variety<-encode_ordinal(us_dataset_04[["variety"]])
us_dataset_04$winery<-encode_ordinal(us_dataset_04[["winery"]])

# Checking Correlation matrix
# cor(subset(us_dataset_04, select=c(points,wordcount,log(price))))#,variety,region_2,province,region_1,taster_name,year)))
# Error (Q2_research.R#130): Must subset columns with a valid subscript vector.
# x Lossy cast from `vars` <double> to <integer>.

cor(subset(us_dataset_04, select=c(points,wordcount,price,province,region_1,taster_name,winery)))

# cor(subset(us_dataset_04, select=c(points,wordcount,log(price))))
glimpse(us_dataset_04)
# Rows: 54,265
# Columns: 12
# $ description <chr> "Tart and snappy, the flavors of lime flesh and rind dominate. Some green pineapple pokes through, with ...
# $ points      <dbl> 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 86, 86, 86, 86, 86, 86, 86, 86, 86, 85, 85, 86, 86, 86, 86, ...
# $ price       <dbl> 14, 13, 65, 19, 34, 12, 32, 23, 20, 22, 69, 16, 50, 20, 50, 22, 14, 40, 13, 16, 30, 14, 55, 100, 25, 26,...
# $ province    <dbl> 1, 2, 1, 3, 3, 3, 4, 4, 1, 3, 3, 3, 3, 3, 1, 1, 3, 4, 3, 4, 3, 3, 5, 3, 5, 3, 5, 3, 5, 3, 3, 3, 3, 1, 3,...
# $ region_1    <dbl> 1, 2, 1, 3, 4, 5, 6, 6, 7, 8, 9, 10, 11, 12, 13, 1, 8, 6, 14, 15, 3, 16, 17, 3, 17, 18, 17, 19, 17, 4, 2...
# $ taster_name <dbl> 1, 2, 1, 3, 3, 4, 2, 2, 1, 4, 3, 3, 5, 5, 1, 1, 5, 5, 5, 5, 5, 3, 6, 3, 6, 4, 6, 7, 6, 3, 3, 3, 3, 1, 3,...
# $ variety     <dbl> 1, 2, 3, 4, 4, 5, 6, 7, 3, 8, 3, 9, 7, 10, 3, 3, 10, 7, 2, 10, 5, 5, 11, 4, 12, 10, 13, 7, 5, 4, 4, 14, ...
# $ winery      <dbl> 1, 2, 3, 4, 5, 6, 7, 7, 8, 9, 10, 11, 12, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27...
# $ wordcount   <dbl> 28, 33, 41, 36, 28, 48, 55, 36, 43, 45, 38, 50, 19, 35, 41, 28, 42, 47, 19, 50, 31, 25, 28, 28, 32, 34, ...
# $ sent_count  <int> 3, 2, 2, 2, 1, 2, 3, 2, 2, 2, 2, 4, 2, 3, 3, 2, 3, 3, 1, 3, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 2, 4, 1,...
# $ log_price   <dbl> 2.639057, 2.564949, 4.174387, 2.944439, 3.526361, 2.484907, 3.465736, 3.135494, 2.995732, 3.091042, 4.23...
# $ desc_len    <int> 186, 199, 249, 243, 171, 308, 315, 210, 233, 251, 259, 302, 124, 220, 242, 163, 259, 278, 109, 328, 172,...


# Individual lm
us_dataset_lm<-select (us_dataset_04,c(points, price,log_price, wordcount, sent_count, variety, taster_name, province, winery))
lm_base <- lm(points ~ price+wordcount+sent_count+variety+taster_name+province+winery,data = us_dataset_lm)
summary(lm_base)
plot(lm_base)
AIC_step_forward <- step(lm_base)

lm_price <- lm(points ~ log_price, data = us_dataset_lm)
summary(lm_price)
# Multiple R-squared:  0.3018,	Adjusted R-squared:  0.3018
lm_wordcount <- lm(points ~ log(wordcount), data = us_dataset_lm)
summary(lm_wordcount)
# Multiple R-squared:  0.3722,	Adjusted R-squared:  0.3722
lm_sent_count <- lm(points ~ sent_count, data = us_dataset_lm)
summary(lm_sent_count)

# MODEL 1:
lm_1 <- lm(points ~ log(wordcount)+log_price, data = us_dataset_lm)
summary(lm_1)
plot(lm_1)
# Call:
# lm(formula = points ~ log(wordcount) + log_price, data = us_dataset_05)
#
# Residuals:
#      Min       1Q   Median       3Q      Max
# -10.7091  -1.4788   0.0955   1.5790   8.3969
#
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)
# (Intercept)    64.78823    0.11162   580.4   <2e-16 ***
# log(wordcount)  4.60814    0.03243   142.1   <2e-16 ***
# log_price       2.01704    0.01791   112.6   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 2.223 on 54262 degrees of freedom
# Multiple R-squared:  0.4911,	Adjusted R-squared:  0.4911
# F-statistic: 2.618e+04 on 2 and 54262 DF,  p-value: < 2.2e-16

# MODEL 2:
lm_2 <- lm(points ~ log(wordcount)+log_price+sent_count, data = us_dataset_lm)
summary(lm_2)
plot(lm_2)
# Call:
# lm(formula = points ~ log(wordcount) + log_price + sent_count,
#     data = us_dataset_lm2)
#
# Residuals:
#      Min       1Q   Median       3Q      Max
# -10.7120  -1.4812   0.0874   1.5750   8.3811
#
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)
# (Intercept)    64.50097    0.12080 533.961  < 2e-16 ***
# log(wordcount)  4.74343    0.03906 121.435  < 2e-16 ***
# log_price       2.02199    0.01792 112.806  < 2e-16 ***
# sent_count     -0.08931    0.01439  -6.209 5.38e-10 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 2.223 on 54261 degrees of freedom
# Multiple R-squared:  0.4915,	Adjusted R-squared:  0.4915
# F-statistic: 1.748e+04 on 3 and 54261 DF,  p-value: < 2.2e-16

# MODEL 3:
lm_3 <- lm(points ~ log(wordcount)+log_price+variety+taster_name+province+winery, data = us_dataset_lm)
summary(lm_3)
# Call:
# lm(formula = points ~ log(wordcount) + log_price + sent_count +
#     variety + taster_name + province + winery, data = us_dataset_lm)
#
# Residuals:
#     Min      1Q  Median      3Q     Max
# -9.8182 -1.4729  0.0895  1.5553  7.8608
#
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)
# (Intercept)     6.574e+01  1.279e-01 513.950  < 2e-16 ***
# log(wordcount)  4.668e+00  3.897e-02 119.796  < 2e-16 ***
# log_price       1.946e+00  1.810e-02 107.497  < 2e-16 ***
# sent_count     -7.320e-02  1.431e-02  -5.116 3.13e-07 ***
# variety        -4.020e-03  4.497e-04  -8.939  < 2e-16 ***
# taster_name    -6.192e-02  4.856e-03 -12.751  < 2e-16 ***
# province       -4.254e-02  6.557e-03  -6.488 8.76e-11 ***
# winery         -1.945e-04  7.875e-06 -24.702  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 2.202 on 54257 degrees of freedom
# Multiple R-squared:  0.5008,	Adjusted R-squared:  0.5008
# F-statistic:  7777 on 7 and 54257 DF,  p-value: < 2.2e-16

# MODEL 4:

top_30df <- us_dataset_03 %>%
  group_by(variety) %>%
  summarise(count = n())%>%
  arrange(desc(count))

top_30df <- top_30df[1:30,1:2]

top_30 <- top_30df$variety
us_dataset_lm_new <- subset(us_dataset_03, variety %in% top_30)

kable(top_30df)

lm_4 <- lm(points ~ log(wordcount)+log_price+sent_count+variety, data = us_dataset_lm_new)
summary(lm_4)
plot(lm_4)
# Call:
# lm(formula = points ~ log(wordcount) + log_price + sent_count +
#     variety, data = us_dataset_lm_new)
#
# Residuals:
#      Min       1Q   Median       3Q      Max
# -10.3495  -1.4521   0.0955   1.5450   7.5788
#
# Coefficients:
#                                 Estimate Std. Error t value Pr(>|t|)
# (Intercept)                     63.68053    0.18797 338.775  < 2e-16 ***
# log(wordcount)                   4.72547    0.04002 118.077  < 2e-16 ***
# log_price                        2.18096    0.02061 105.835  < 2e-16 ***
# sent_count                      -0.07816    0.01463  -5.342 9.22e-08 ***
# varietyBordeaux-style Red Blend -0.23802    0.15070  -1.579  0.11425
# varietyCabernet Franc           -0.38928    0.15720  -2.476  0.01328 *
# varietyCabernet Sauvignon       -0.03668    0.14389  -0.255  0.79878
# varietyChardonnay                0.77971    0.14370   5.426 5.79e-08 ***
# varietyGewürztraminer            0.81826    0.17814   4.593 4.37e-06 ***
# varietyGrenache                  0.43382    0.17059   2.543  0.01099 *
# varietyMalbec                    0.20567    0.16993   1.210  0.22617
# varietyMeritage                 -0.84909    0.19679  -4.315 1.60e-05 ***
# varietyMerlot                    0.03950    0.14839   0.266  0.79009
# varietyMourvèdre                 0.57146    0.20582   2.776  0.00550 **
# varietyPetit Verdot             -0.64338    0.20412  -3.152  0.00162 **
# varietyPetite Sirah              0.06695    0.16225   0.413  0.67986
# varietyPinot Blanc               0.94946    0.21052   4.510 6.50e-06 ***
# varietyPinot Grigio              0.27669    0.18242   1.517  0.12933
# varietyPinot Gris                1.33137    0.15971   8.336  < 2e-16 ***
# varietyPinot Noir                0.45530    0.14319   3.180  0.00148 **
# varietyRed Blend                -0.40015    0.14685  -2.725  0.00643 **
# varietyRhône-style Red Blend     0.27172    0.16393   1.657  0.09743 .
# varietyRhône-style White Blend   0.23480    0.20481   1.146  0.25164
# varietyRiesling                  1.12702    0.15076   7.476 7.80e-14 ***
# varietyRosé                      0.37466    0.15893   2.357  0.01841 *
# varietySangiovese               -0.23394    0.17611  -1.328  0.18407
# varietySauvignon Blanc           0.66921    0.14895   4.493 7.04e-06 ***
# varietySparkling Blend           0.81391    0.16419   4.957 7.18e-07 ***
# varietySyrah                     0.68274    0.14646   4.662 3.15e-06 ***
# varietyTempranillo              -0.40764    0.18385  -2.217  0.02661 *
# varietyViognier                  0.46413    0.16122   2.879  0.00399 **
# varietyWhite Blend               0.05472    0.16575   0.330  0.74128
# varietyZinfandel                 0.10946    0.14732   0.743  0.45748
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 2.179 on 50996 degrees of freedom
# Multiple R-squared:  0.5154,	Adjusted R-squared:  0.5151
# F-statistic:  1695 on 32 and 50996 DF,  p-value: < 2.2e-16



















