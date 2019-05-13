





library(tidyverse)
library(missMDA)
library(scales)



## 1. Data, loading two months earlier to allow for second difference.
## 2. Attaching transformation code vector
current <- read_csv("current.csv", col_types = cols(sasdate = col_date(format = "%m/%d/%Y"))) %>%
  filter(sasdate >= as.Date("1989-11-01"), sasdate < as.Date("2019-01-01")) ## 1.

DateVec <- current[,1]

current <- current[,-1]

Transform <- read_csv("current.csv") %>% .[1,] %>% .[,-1]

current <- rbind(Transform, current) ## 2.


## 1. Raw data for loop
## 2. Removing first 2 observations to allow for differences - defining the y dataset
## 3. Number of variables + Date-vec
## 4. Number of observations + the transformation code

RawData <- current                
x = RawData                       ## 1.
y = RawData[-c(2:3),]             ## 2.
y = y[FALSE,]                     
n <- ncol(x)                      ## 3.
t <- as.integer(nrow(x))          ## 4.


# Stationarity ------------------------------------------------------------


for (i in 1:n) {
  
  if (x[1,i] == 1) {       ## no Transformation/already stationary series
    y[4:t,i] <- x[4:t,i]
  }
  
  else  if (x[1,i] == 2) { ## First difference
    y[4:t,i] <- x[4:t,i]-x[3:(t-1),i]
  }
  
  else if (x[1,i] == 3) {  ## Second difference
    y[4:t,i] <- x[4:t,i]-2*x[3:(t-1),i]+x[2:(t-2)]
  }
  
  else if (x[1,i] == 4) {  ## Ln
    y[4:t,i] <- log(x[4:t,i])
  }
  
  else if (x[1,i] == 5) {  ## First difference of ln
    y[4:t,i] <- log(x[4:t,i])-log(x[3:(t-1),i])
  }
  
  else if (x[1,i] == 6) {  ## Second difference of ln
    y[4:t,i] <- log(x[4:t,i])-2*log(x[3:(t-1),i])+log(x[2:(t-2),i])
  }
  
  else if (x[1,i] == 7) {  ## First difference of percentage change
    y[4:t,i] <- (x[4:t,i]-x[3:(t-1),i])/x[3:(t-1),i]-(x[3:(t-1),i]-x[2:(t-2),i])/x[2:(t-2),i]
  } 
}

rm(x, Transform) ## Clearing uneeded data

y = y[4:t,] ## Removing the first 2 months because of differencing leading to NA's



##########################
## Determining outliers ##
##########################
## Outliers: abs(x-median)>10*interquartile_ranget
## 1. Determining the median of each series
## 2. Increasing the dimensions of the median set, by repeating 1,i entries t times
## 3. Determining the quantiles
## 4. Sample Interquantile range X_0.75-X_0.25 
## 5. Increasing dimensions of interquantile range, by repeating 1,i entries t times
## 6. Critical value for each entry
## 7. Defining the binary outlier dataset 1=outlier

Median_y  <- y[FALSE,]
qnt       <- y[FALSE,]
IQR_y     <- y[FALSE,]
Z         <- y[FALSE,]
outlier_y <- y[FALSE,]
for (i in 1:n) {
  Median_y[1,i] <- median(as.matrix(y[1:t, i]), na.rm = TRUE)                 ## 1.
  Median_y[1:t,i] <- rep(Median_y[1,i], each = t)                             ## 2.
  qnt[1:2,i] <- quantile(as.matrix(y[,i]), probs=c(0.25, 0.75), na.rm = TRUE) ## 3.
  IQR_y[1,i] = 10*(qnt[2,i]-qnt[1,i])                                         ## 4.
  IQR_y[1:t,i] = rep(IQR_y[1,i], each = t)                                    ## 5.
  Z[1:t,i]=abs(y[1:t,i]-Median_y[1:t,i])                                      ## 6. 
  outlier_y[1:t,i] = Z[1:t,i]>IQR_y[1:t,i]                                    ## 7.
}
rm(Median_y, qnt, Z, IQR_y)                                                  

#######################
## Removing outliers ##
#######################
y_no = y
outlier_y[is.na(outlier_y)] <- 0
for (i in 1:n) {
  for (k in 1:t) {
    if (outlier_y[k,i] == 1){
      y_no[k,i]  <- NA ## Replacing all values of y considered an outlier by NA
    }
  }
}
rm(outlier_y)

## EM - Algorithm for missing data ## 
y_no <- as.data.frame(y_no)
EM_up = imputePCA(y_no, maxiter = 150)
yt = EM_up$completeObs
yt = as.data.frame(yt)
colnames(yt) = colnames(y_no)

############################
## Demean and standardize ##
############################
yt_z = yt[FALSE,]
for (i in 1:ncol(yt)){
  yt_z[1:nrow(yt),i] <- (yt[,i]-mean(yt[,i]))/sd(yt[,i])
}
## These lines checks if the mean and variance of each column indeed is
## 0 and 1 respectively
# colMeans(yt_z)   
# apply(yt_z, 2, sd) 
## yt_z is hence the Z-standardized dataset.

#########
## PCA ##
#########

yt_z1 <- as.matrix(yt_z)
cov_yt <- t(yt_z1) %*% yt_z1 / t ## Defining the 128x128 covariance matrix
## Eigen decomposition, identical to SVD here (symmetric square matrix)
eig = eigen(cov_yt) 
eig_val <- as.data.frame(eig[1]$values) ## Eigenvalue
colnames(eig_val) <- "Eigenvalue"
rownames(eig_val) <- 1:nrow(eig_val)
eig_vec <- as.data.frame(eig[2]$vectors) ## Eigenvector
colnames(eig_vec) <- 1:ncol(eig_vec)
rownames(eig_val) <- 1:nrow(eig_vec)

Factor_loadings <- sqrt(ncol(yt_z)) * eig_vec
Factor_loadings <-as.data.frame(Factor_loadings)
Factor_components <- as.matrix(yt_z) %*% as.matrix(Factor_loadings/ncol(yt_z))
Factor_components <- as.data.frame(Factor_components)

Fhat <- Factor_components[,1:7] ## Using the first 7 factors as indicated by the IC2
Fhat_d <- cbind(DateVec[3:nrow(DateVec),1], Fhat) ## Reattaching Datevector



var_fac = eig_val/sum(eig_val)
# sum(var_fac[1:7,1])*100 ## Total variation explained by 7 factors
colnames(var_fac) <- "var"
no_fac <- 1:nrow(eig_val)

screeplot = ggplot() + 
  geom_point(aes(x=no_fac, y = var_fac$var), shape=21, fill= "#014d64", size = 2.5) +
  labs(title = "Scree plot - 128 factors",
       caption = "Rasmus M. Jensen" ) +
  scale_y_continuous(expand = c(0, 0),
                     limits=c(0,0.16),
                     breaks=seq(0, 0.16, 0.02), 
                     name = "Variance explained",
                     labels = scales::percent_format()) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 130),
                     breaks = seq(0,128, 16),
                     name = "Factor") +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank())
screeplot



screeplot2 = ggplot() + 
  geom_point(aes(x=no_fac[1:15], y = var_fac$var[1:15]), shape=21, 
             fill="#014d64", size = 2.5) +
  labs(title = "Scree plot - 15 factors",
       caption = "Rasmus M. Jensen"
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits=c(0,0.16),
                     breaks=seq(0, 0.16, 0.02), 
                     name = "Variance explained",
                     labels = scales::percent_format()) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 15),
                     breaks = seq(0,15, 3),
                     name = "Factor") +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank())
screeplot2
