# Title     : TODO
# Objective : TODO
# Created by: vishal
# Created on: 12-05-2020

library(jsonlite)
library(ggplot2)
#library(MCMCpack)
library(RColorBrewer)
library(dplyr)
#library(LabelEncoder)
library(plyr)

#1. Load Dataset & Pre-processing of Data
winedata_full <- read.csv("wwinemag-data-130k-v2.csv")
desc(winedata_full)
# Select useful data
winedata <- winedata_full[c('country','points','price','region_1','variety')]

# Select only Sauvignon Blanc & Chardonnay variety
test_winedata_variety <- filter(winedata,
                  winedata$variety == "Sauvignon Blanc" &
                    winedata$country == "South Africa" |
                    winedata$variety == "Chardonnay" &
                      winedata$country == "Chile")

test_winedata_variety_price <- filter(test_winedata_variety,test_winedata_variety$price == 15)
head(test_winedata_variety_price,10)
#         country points price region_1         variety
# 1         Chile     85    15               Chardonnay
# 2         Chile     83    15               Chardonnay
# 3         Chile     80    15               Chardonnay
# 4         Chile     82    15               Chardonnay
# 5  South Africa     89    15          Sauvignon Blanc
# 6  South Africa     85    15          Sauvignon Blanc
# 7         Chile     85    15               Chardonnay
# 8  South Africa     90    15          Sauvignon Blanc
# 9         Chile     83    15               Chardonnay
# 10        Chile     85    15               Chardonnay

test_winedata = test_winedata_variety_price[c("country","points","variety")]

# Change the class of variety object to be a factor to treat variety as a index column (not as a measurement we have)
test_winedata$variety <- factor(test_winedata$variety)
summary(test_winedata)
#          country       points                 variety
#  Chile       :37   Min.   :80.00   Chardonnay     :37
#  South Africa:14   1st Qu.:85.00   Sauvignon Blanc:14
#              : 0   Median :86.00
#  Argentina   : 0   Mean   :85.67
#  Armenia     : 0   3rd Qu.:87.00
#  Australia   : 0   Max.   :90.00
#  (Other)     : 0

head(test_winedata,5)
#        country points         variety
# 1        Chile     85      Chardonnay
# 2        Chile     83      Chardonnay
# 3        Chile     80      Chardonnay
# 4        Chile     82      Chardonnay
# 5 South Africa     89 Sauvignon Blanc

# 2. Stats & Plots & Analysis
# plot box plots
ggplot(test_winedata) + geom_boxplot(aes(variety, points, fill = variety)) +
  geom_jitter(aes(variety, points, shape = test_winedata$variety)) +
  scale_fill_manual(values=c("red", "green"))
# ggplot(test_winedata) + geom_boxplot(aes(points,variety, fill = variety)) + geom_jitter(aes(points, variety, shape = test_winedata$variety)) + scale_fill_manual(values=c("#999999", "#E69F00"))
#      Chardonnay Sauvignon Blanc
#        85.08108        87.21429
#      Chardonnay Sauvignon Blanc
#              85              87
#      Chardonnay Sauvignon Blanc
#        2.203260        1.717716
# the average rating of Sauvignon Blanc is better than Chardonnay wine


# performing t-test for both sample
t.test(points ~ variety, data=test_winedata, var.equal = TRUE)
# 	Two Sample t-test
#
# data:  points by variety
# t = -3.2599, df = 49, p-value = 0.00203
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -3.4482245 -0.8181847
# sample estimates:
#      mean in group Chardonnay mean in group Sauvignon Blanc
#                      85.08108                      87.21429


# The 95% confidence interval is providing a range that we are 95% confident the true difference in means of Chardonnay wine
# rating and Sauvignon Blanc wine rating is between 0.8181847 and 3.4482245

# The t-test statistic signifies that the average rating of both wine (Chardona and savoru blanc) are different(signifcant)
# As p < 0.05 and t statitistic > critical value,it means we can reject null hypothesis --> alternative hypothesis is true
# Or means of two samples are significant(different)

#selecting the variety of wine based on country
Sauvignon_wine <- test_winedata %>%
  filter(country == "South Africa", variety == "Sauvignon Blanc")

Chardonnay_wine <- test_winedata %>%
  filter(country == "Chile",  variety == "Chardonnay")

mean_diff <- mean(Sauvignon_wine$points) - mean(Chardonnay_wine$points)
mean_diff
# [1] 2.133205









# 1.a.2
compare_gibbs <- function(y, ind, mu0 = 85, tau0 = 1/25, del0 = 0, gamma0 = 1/25, a0 = 289, b0 = 3.4, maxiter = 5000)
{
y1 <- y[ind == 1]
y2 <- y[ind == 2]
n1 <- length(y1)
n2 <- length(y2)
##### starting values
mu <- (mean(y1) + mean(y2)) / 2
del <- (mean(y1) - mean(y2)) / 2
mat_store <- matrix(0, nrow = maxiter, ncol = 3)
#####
##### Gibbs sampler
an <- a0 + (n1 + n2)/2
for(s in 1 : maxiter)
{
##update tau
bn <- b0 + 0.5 * (sum((y1 - mu - del) ^ 2) + sum((y2 - mu + del) ^ 2))
tau <- rgamma(1, an, bn)
##
##update mu
taun <- tau0 + tau * (n1 + n2)
mun <- (tau0 * mu0 + tau * (sum(y1 - del) + sum(y2 + del))) / taun
mu <- rnorm(1, mun, sqrt(1/taun))
##
##update del
gamman <- tau0 + tau*(n1 + n2)
deln <- ( del0 * tau0 + tau * (sum(y1 - mu) - sum(y2 - mu))) / gamman
del<-rnorm(1, deln, sqrt(1/gamman))
##
## store parameter values
mat_store[s, ] <- c(mu, del, tau)
}
colnames(mat_store) <- c("mu", "del", "tau")
return(mat_store)
}

library('MCMCpack')
fit_model <- compare_gibbs(test_winedata$points, as.factor(test_winedata$variety))
plot(as.mcmc(fit_model))

raftery.diag(as.mcmc(fit_model))
# Quantile (q) = 0.025
# Accuracy (r) = +/- 0.005
# Probability (s) = 0.95
#
#      Burn-in  Total Lower bound  Dependence
#      (M)      (N)   (Nmin)       factor (I)
#  mu  2        3866  3746         1.030
#  del 2        3741  3746         0.999
#  tau 2        3803  3746         1.020
# Here, Lower values of dependence factor(Close to zero) explains that performance of the sampler is satisfactory.

head(fit_model,10)
#             mu        del      tau
#  [1,] 89.75368 -4.6610405 87.96566
#  [2,] 83.82378 -6.1515224 82.24293
#  [3,] 83.58380  1.1397175 81.94757
#  [4,] 88.55353  4.0555183 76.32479
#  [5,] 86.21064 -0.5076133 73.85806
#  [6,] 75.71622  3.0858340 75.83246
#  [7,] 93.09777 -1.2406144 84.55164
#  [8,] 91.74747  2.0471623 78.95766
#  [9,] 83.77054 -5.8123003 84.00604
# [10,] 82.20979 -2.1387136 79.67127

#Generating summary statistics, to understand paramters of the posterior distribution
apply(fit_model, 2, mean)
#          mu         del         tau
# 85.01930855  0.03680618 85.09654029
apply(fit_model, 2, sd)
#       mu      del      tau
# 4.932126 4.975741 5.007844

# convert tau to sd
mean(1/sqrt(fit[, 3]))
# [1] 0.1086151
sd(1/sqrt(fit[, 3]))
# [1] 0.00320712

apply(fit_model, 2, function(x) quantile(x, c(0.05, 0.95)))
#           mu       del      tau
# 5%  76.99656 -8.182970 76.95587
# 95% 93.14027  8.240437 93.42174

#Making posterior params
rating1_simu <- rnorm(5000, fit[, 1] + fit[, 2], sd = 1/sqrt(fit[, 3]))
rating2_simu <- rnorm(5000, fit[, 1] - fit[, 2], sd = 1/sqrt(fit[, 3]))

fit_model <- as.mcmc(rating1_simu)

fit_model

acf(rating1_simu,lag.max = 100)
acf(rating2_simu,lag.max = 100)

## set up ggplot for histogram and density plots
pdf_plot <- ggplot(data.frame(rating_simu_diff = rating1_simu-rating2_simu),aes(rating1_simu-rating2_simu, ..density..))

## add histogram
pdf_plot <- pdf_plot + geom_histogram(bins = 30, aes(rating_simu_diff,alpha = 0.5, colour = "blue"), show.legend = FALSE)
pdf_plot <- pdf_plot + geom_density(fill = "cyan", aes(rating_simu_diff,alpha = 0.5), show.legend = FALSE)
pdf_plot <- pdf_plot + xlab("Difference between simulated samples") + ylab("probability density")# + ggtitle("PDF for variation in two simulated data sample")
pdf_plot

## set up ggplot for histogram and density plots
pdf_plot <- ggplot(data.frame(rating1_simu),aes(rating1_simu, ..density..))
pdf_plot <- ggplot(data.frame(rating2_simu),aes(rating2_simu, ..density..))

install.packages('LaplacesDemon')

library(LaplacesDemon)

joint.density.plot(rating1_simu, rating2_simu, Title='Joint PDF of 2 wine samples' , contour=TRUE, color=FALSE, Trace=c(1,10))


ggplot(data.frame(rating_simu_diff = rating1_simu - rating2_simu), aes(x=rating_simu_diff)) + stat_bin(aes(rating_simu_diff)) +geom_histogram(color="blue", fill="white")

mean(rating1_simu > rating2_simu)
# [1] 0.7102

print(" 0.7102")
ggplot(data.frame(rating1_simu, rating2_simu)) + geom_point(color='#8dd3c7',fill="white",aes(rating1_simu, rating2_simu), alpha = 0.3) + geom_abline(slope = 1, intercept = 0)


#1.b

test_winedata_2 <- filter(winedata,winedata$country == "Italy")

test_winedata_3 <- filter(test_winedata_2,test_winedata_2$price < 20)

head(test_winedata_3,10)

test_winedata_4 = ddply(test_winedata_3, "region_1", function(d) {if(nrow(d)>3) d else NULL})

head(test_winedata_4,20)

test_winedata_4[test_winedata_4 == ""] <- NA

head(test_winedata_4,10)

nrow(test_winedata_4)

summary(test_winedata_4)

sum(is.na(test_winedata_4$region_1))

ggplot(test_winedata_4) +
  geom_boxplot(aes(x = reorder(region_1, points, median),
                   points,
                   fill = reorder(region_1, points, median)),
               show.legend=FALSE) +
  labs(#title = "Ratings for Wines with regions as Italy",
       x = "", y = "Wine Ratings") +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7,angle = 90, face = "bold", vjust = 0.2, hjust = 0.98))

#plotting the number of ratings for wines in the regions within Italy
test_winedata_4 %>% ggplot(aes(x = reorder(region_1, region_1, length)),color='red') + stat_count() +
labs(#title = "Variety of Wines with regions as Italy",
     x = "", y = "Ratings Count") +theme_gray() +
theme(panel.background = element_rect(fill = "cyan",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "white"),
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "white"), axis.text.x = element_text(size = 7,angle = 90, face = "bold", vjust = 0.2, hjust = 0.98))

# In above plot we can see that there is not much sample size(count of reviews) for many wines. However there are some wines for which
# there are plent of reviews given

#plotting the range and frequency of wine ratings in regions within Italy
test_winedata_4 %>% ggplot(aes(points),color='cyan') + stat_bin(bins = 10) +
labs(#title = "Range and Frequency of Wine Ratings in Italy",
     x = "Wine Ratings",y = "Count") +theme_gray()

# Above plot shows the count of rating given by cutomers are maximumu between 83-89 i.e. more than 2000
# Very few people have given rating between 75 - 83 or between 89-95

ggplot(data.frame(size = tapply(test_winedata_4$points, test_winedata_4$region_1, length),
                  mean_score = tapply(test_winedata_4$points, test_winedata_4$region_1, mean)),
aes(size, mean_score)) + geom_point() + xlab("Region sample size") + ylab("Mean Score") +
ggtitle("Effect size vs Sample size")

gibbs_m_categories <- function(y,
                               categories,
                               mu_0 = 85,
                               tau_0 = 1/100,
                               a_0 = 1,
                               b_0 = 50,
                               alpha_0 =1,
                               beta_0 = 50,
                               max_iterations = 5000)

#     mu0 = 85, tau0 = 1/25, del0 = 0, gamma0 = 1/25,
# a0 = 289, b0 = 3.4, maxiter = 5000
{
  a_0 <- 289
  b_0 <- 3.4
  alpha_0 <-289
  beta_0 <- 3.4
  mu_0 <- 85
  tau_0 <- 1/25

  m <- nlevels(categories)
  ybar <- theta <- tapply(y, categories, mean)
  tau_w <- mean(1 / tapply(y, categories, var))

  mu <- mean(theta)
  tau_b <-var(theta)

  n_m <- tapply(y, categories, length)
  alpha_n <- alpha_0 + sum(n_m)/2

  theta_matrix <- matrix(0, nrow = max_iterations, ncol = m)
  matrix_store <- matrix(0, nrow = max_iterations, ncol = 3)

  for(iter in 1:max_iterations)
  {
    for(j in 1:m)
    {
      tau_n <- n_m[j] * tau_w + tau_b
      theta_n <- (ybar[j] * n_m[j] * tau_w + mu * tau_b) / tau_n
      theta[j] <- rnorm(1, theta_n, 1/sqrt(tau_n))
    }

    ss <- 0
    for(j in 1:m){
      ss <- ss + sum((y[categories == j] - theta[j])^2)
    }

    beta_n <- beta_0 + ss/2
    tau_w <- rgamma(1, alpha_n, beta_n)

    tau_m <- m * tau_b + tau_0
    mu_m <- (mean(theta) * m * tau_b + mu_0 * tau_0) / tau_m
    mu <- rnorm(1, mu_m, 1/sqrt(tau_m))

    a_m <- a_0 + m/2
    b_m <- b_0 + sum((theta - mu)^2) / 2
    tau_b <- rgamma(1, a_m, b_m)

    theta_matrix[iter,] <- theta
    matrix_store[iter, ] <- c(mu, tau_w, tau_b)
  }
  colnames(matrix_store) <- c("mean", "precision(w)", "precision(b)")
  colnames(theta_matrix) <- levels(categories)
  return(list(params = matrix_store, theta = theta_matrix))
}

sum(is.na(test_winedata_4$region_1))

test_winedata_5 <- na.omit(test_winedata_4)

head(test_winedata_5,5)

sum(is.na(test_winedata_5))

test_winedata_5$region_1 <- as.character(test_winedata_5$region_1)
test_winedata_5 <- test_winedata_5[order(test_winedata_5$region_1), ]
test_winedata_5$region_1 <- as.factor(test_winedata_5$region_1)

test_winedata_5$points <- test_winedata_5$points + rnorm(nrow(test_winedata_5), 1, 1)/1000

tail(test_winedata_5,10)

gibbs_fit2 <- gibbs_m_categories(test_winedata_5$points, test_winedata_5$region_1)

# gibbs_fit2

plot(as.mcmc(gibbs_fit2$params))

apply(gibbs_fit2$params, 2, mean)
apply(gibbs_fit2$params, 2, sd)

mean(1/sqrt(gibbs_fit2$params[, 2]))
sd(1/sqrt(gibbs_fit2$params[, 2]))

mean(1/sqrt(gibbs_fit2$params[, 3]))
sd(1/sqrt(gibbs_fit2$params[, 3]))

theta_hat <- apply(gibbs_fit2$theta, 2, mean)

theta_hat

names(theta_hat) <- colnames(gibbs_fit2$theta)

head(data.frame(sort(theta_hat, decreasing = TRUE)),7)

#getting the upper and lower bounds for the samples generated for all regions
theta_quantile_bounds <- apply(gibbs_fit2$theta, 2, quantile, prob = c(0.05, .95))

theta_quantile_bounds

theta_df_error <- data.frame(lower = theta_quantile_bounds[1, ],
                       upper = theta_quantile_bounds[2, ],
                       mean = theta_hat,
                       region = colnames(gibbs_fit2$theta))

theta_df_error

theta_df <- data.frame(samples = as.numeric(gibbs_fit2$theta),
                       region = rep(colnames(gibbs_fit2$theta),
                                    each = nrow(gibbs_fit2$theta)))

#reformatting the samples as per the order of the median
ggplot(theta_df) +
  geom_boxplot(aes(x = reorder(region, samples, median),
                   samples,
                   fill = reorder(region, samples, median)),
               show.legend=FALSE) +
  labs(#title = "Ratings for Wines for Italy region from generated samples",
       x = "",
       y = "Mean") +
  theme_gray() +
  theme(axis.text.x = element_text(size = 7,angle = 90, face = "bold", vjust = 0.2, hjust = 0.98))

## reformat samples for ggplot
theta_df <- data.frame(samples = as.numeric(gibbs_fit2$theta),
region = rep(1:ncol(gibbs_fit2$theta), each = nrow(gibbs_fit2$theta)))
ggplot(theta_df) +
  geom_boxplot(aes(x = reorder(region, samples, median),
                   samples,
                   fill = reorder(region, samples, median)), show.legend=FALSE)

ggplot(data.frame(size = tapply(test_winedata_5$points, test_winedata_5$region_1, length),
                  theta_hat = theta_hat),
       aes(size, theta_hat)) + geom_point()

ggplot(data.frame(mean = tapply(test_winedata_5$points, test_winedata_5$region_1, mean), theta_hat = theta_hat),
       aes(mean, theta_hat)) + geom_point() +
  labs(#title = "Sample Mean vs Population Mean",
       x = "Mean Population",
       y = "Mean Sample") +
  theme_gray()

theta_hat_df <- data.frame(theta_hat)

head(theta_hat_df,10)

theta_hat_df$region_1 <- unique(sort(test_winedata_5$region_1))

head(theta_hat_df,5)

# Below are the regions that produce better then average wine

theta_hat_df[theta_hat_df$theta_hat > mean(gibbs_fit2$params[, 1]), 2]





































