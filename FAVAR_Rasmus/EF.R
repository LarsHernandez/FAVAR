graphics.off()
gc()
set.seed(5)

#unload all packages
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
#sessionInfo()

#Check if packages in this list is installed and if not install 
list.of.packages <-   c("dplyr", "lubridate", "stargazer", "cowplot", "gridExtra", 
                        "xtable", "missMDA", "vars", "forecast", "Quandl", "readr",
                        "fredr","ggplot2", "ggthemes", "xts", "urca", "tsDyn", 
                        "tseries", "nowcasting")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Clearing variables
rm(list=ls())

fredr_set_key("0ed565f681fd4eed054d20207c3962f3")
Quandl.api_key("NBrW5T3wJVcx5QJ4tfY7")


#getwd()

ptm <- proc.time()

## 1. Data, loading two months earlier to allow for second difference.
## 2. Attaching transformation code vector
current <- read_csv("current.csv", 
                    col_types = cols(sasdate = col_date(format = "%m/%d/%Y"))) %>%
  filter(sasdate >= as.Date("1989-11-01"), 
         sasdate < as.Date("2019-01-01")) ## 1.
DateVec <- current[,1]
current <- current[,-1]
Transform <- read_csv("current.csv") %>% 
  .[1,] %>% .[,-1]
current = rbind(Transform, current) ## 2.

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


##################
## Stationarity ##
##################

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

## Thus all series has been converted to stationary series and saved in y.
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

#######################
## Number of factors ##
#######################

 ## Scree plots ##
library(ggplot2)
library(ggthemes)
var_fac = eig_val/sum(eig_val)
# sum(var_fac[1:7,1])*100 ## Total variation explained by 7 factors
colnames(var_fac) <- "var"
no_fac <- 1:nrow(eig_val)

screeplot = ggplot() + 
  geom_point(aes(x=no_fac, y = var_fac$var), shape=21, fill= "#014d64", size = 2.5) +
  labs(title = "Scree plot - 128 factors",
       caption = "Rasmus M. Jensen"
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits=c(0,0.16),
                     breaks=seq(0, 0.16, 0.02), 
                     name = "Variance explained",
                     labels = scales::percent_format(accuracy = 1)) +
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
# ggsave("screeplot128.pdf", plot = screeplot, width = 14, height = 8, units = "in")


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
                     labels = scales::percent_format(accuracy = 1)) +
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
# ggsave("screeplot15.pdf", plot = screeplot2, width = 14, height = 8, units = "in")

 ## Bai & Ng Information criteria ## 
library(nowcasting)
IC1 <- ICfactors(yt_z1, rmax = 14, 1)
IC2 <- ICfactors(yt_z1, rmax = 14, 2)
IC3 <- ICfactors(yt_z1, rmax = 14, 3)
factor <- as.matrix(1:nrow(as.matrix(IC1$IC)), nrow = 1, ncol = 14)

IC1plot = ggplot() + 
  geom_point(aes(x=factor, y = IC1$IC), shape=21, size = 2.5) +
  geom_point(aes(x=which.min(IC1$IC), y = min(IC1$IC)), shape=21, 
             fill="#014d64", size = 3) +
  labs(title = "Number of factors",
       subtitle = "Bai & Ng information criterion 1",
       caption = "Rasmus M. Jensen",
       x = "Factor",
       y = "Information Criterion"
  ) +
  scale_x_continuous(breaks=seq(0,nrow(as.matrix(IC1$IC)),1)) + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank())
IC1plot

IC2plot = ggplot() + 
  geom_point(aes(x=factor, y = IC2$IC), shape=21, size = 2.5) +
  geom_point(aes(x=which.min(IC2$IC), y = min(IC2$IC)), shape=21, 
             fill="#014d64", size = 3) +
  labs(title = "Number of factors",
       subtitle = "Bai & Ng information criterion 2",
       caption = "Rasmus M. Jensen",
       x = "Factor",
       y = "Information Criterion"
  ) +
  scale_x_continuous(breaks=seq(0,nrow(as.matrix(IC2$IC)),1)) + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank())
IC2plot

IC3plot = ggplot() + 
  geom_point(aes(x=factor, y = IC3$IC), shape=21, size = 2.5) +
  geom_point(aes(x=which.min(IC3$IC), y = min(IC3$IC)), shape=21, 
             fill="#014d64", size = 3) +
  labs(title = "Number of factors",
       subtitle = "Bai & Ng information criterion 3",
       caption = "Rasmus M. Jensen",
       x = "Factor",
       y = "Information Criterion"
  ) +
  scale_x_continuous(breaks=seq(0,nrow(as.matrix(IC3$IC)),1)) + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank())
IC3plot

# ggsave("BaiNg1.pdf", plot = IC1plot, width = 14, height = 8, units = "in")
# ggsave("BaiNg2.pdf", plot = IC2plot, width = 14, height = 8, units = "in")
# ggsave("BaiNg3.pdf", plot = IC3plot, width = 14, height = 8, units = "in")

BaiNgIC = cbind(IC1$IC, IC2$IC, IC3$IC)
rownames(BaiNgIC) = 1:nrow(BaiNgIC)
colnames(BaiNgIC) <- c("Information Criterion 1", "Information Criterion 2", 
                       "Information Criterion 3")
stargazer(BaiNgIC)

rm(eig, eig_val, eig_vec, yt, y_no, cov_yt, i, k, n, t, Factor_components,
   Factor_loadings, screeplot, screeplot2, yt_z, no_fac, var_fac, EM_up,
   factors, IC1plot, IC2plot, IC3plot, IC1, IC2, IC3, BaiNgIC)


########################
##  Data for forecast ##
########################
## FAVAR ##
SHADOWRATE <- Quandl("SHADOWS/US", ##(Wu-Xia 2016)
                     order = "asc",
                     start_date = "1990-01-01",
                     force_irregular = TRUE)

FEDFUNDS <- fredr("FEDFUNDS",      ##(FRED 2019)
                  frequency = "m",
                  observation_start = as.Date("1989-12-01"),
                  observation_end = as.Date("2018-12-01"))

FEDFUNDSFUTURES <- Quandl("CHRIS/CME_FF6",
                          order = "asc",
                          start_date = "1989-11-30",
                          end_date = "2018-11-30",
                          collapse = "monthly",
                          force_irregular = TRUE) ##(CME Group, 2019)

IP <- fredr("INDPRO",  ## Industrial Productivity Index 2012=100
            frequency = "m",
            observation_start = as.Date("1989-12-01"),
            observation_end = as.Date("2018-12-01"))
IP = xts(IP$value, order.by = IP$date)
IP = diff(log(IP))
IP = na.omit(IP)

CPI = fredr("CPIAUCSL", ## Consumer price index 1982-1984=100
            frequency = "m",
            observation_start = as.Date("1989-12-01"),
            observation_end = as.Date("2018-12-01"))
CPI = xts(CPI$value, order.by = CPI$date)
CPI = diff(log(CPI))
CPI = na.omit(CPI)

FEDFUNDS <- FEDFUNDS[,-2] 
Datevec <- FEDFUNDS[,1]

FEDFUNDSFUTURES = FEDFUNDSFUTURES[c(1, 7)]
FEDFUNDSFUTURES <- add_row(FEDFUNDSFUTURES, .after = 6)
FEDFUNDSFUTURES[7,1] <- Datevec[7,1] 
FEDFUNDSFUTURES[7,2] <- (FEDFUNDSFUTURES[6,2] + FEDFUNDSFUTURES[8,2]) / 2

 ## Interpolating the missing observation and constructing dataframe.
FedFunds <- as.xts(FEDFUNDS$value, order.by = FEDFUNDS$date)
 # saveRDS(FedFunds, file = 'CONSrate.Rds')

FedFundsPlot = ggplot() + 
  geom_line(aes(x=index(FedFunds), 
                y = FedFunds)) +
  labs(title = "Effective Federal Funds rate", 
       caption = "Rasmus M. Jensen",
       y = "Rate",
       x = "Year"
  ) +
  scale_color_economist() + scale_y_continuous() +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16)
  ) 

FedFundsPlot














# ggsave("Effective_fed.pdf", plot = FedFundsPlot, width = 14, height = 8, units = "in")

Shadowrate <- Quandl("SHADOWS/US", ##(Wu-Xia 2016)
                     order = "asc",
                     start_date = "1990-01-01",
                     type = "xts",
                     force_irregular = TRUE)

ShaFedPlot = ggplot() + 
  geom_line(aes(x=index(FedFunds), 
                y = FedFunds, colour = "Federal Funds Rate"), size=1.2) +
  geom_line(aes(x = index(Shadowrate),
                y = Shadowrate, colour = "WX Shadow Rate"), size = 1.05) +
  labs(title = "Effective Federal Funds rate vs. Wu & Xia Shadow Rate", 
       caption = "Rasmus M. Jensen",
       colour = "Series",
       y = "Rate",
       x = "Year"
  ) +
  scale_color_economist() + scale_y_continuous() +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16),
        legend.position = c(0.8, 1.025),
        legend.background = element_blank(),
        legend.direction = "horizontal"
  ) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE, direction = "horizontal")
)
ShaFedPlot
# ggsave("Shadow_rate_vs_FEDFUNDS.pdf", plot = ShaFedPlot, width = 14, height = 8, units = "in")


rm(FedFundsPlot, FedFunds, Shadowrate, ShaFedPlot)

## Loop replacing FED fund observations at the effective lower bound
## with the estimated shadow rate
CONSrate <- FEDFUNDS[,2]
for (i in 1:nrow(SHADOWRATE)){
  if (SHADOWRATE[i,2] != FEDFUNDS[i,2]){
    CONSrate[i,1] <- SHADOWRATE[i,2]
  }
} 
CONSrate[1,1] <- FEDFUNDS$value[1]

CONSrate <- as.xts(CONSrate, order.by = Datevec$date)

PolicyRatePlot = ggplot() + 
  geom_line(aes(x=index(CONSrate), 
                y = CONSrate), colour = "#014d64") + 
  geom_hline(yintercept = 0, colour = "#00887d", lty = 2, size = 1.1) +
  labs(title = "Policy Rate", 
       caption = "Rasmus M. Jensen",
       y = "Rate",
       x = "Year"
  ) +
  scale_color_economist() + scale_y_continuous() +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16)
)
PolicyRatePlot
# ggsave("Policy_rate.pdf", plot = PolicyRatePlot, width = 14, height = 8, units = "in")


FEDFUNDSFUTURES <- as.xts(FEDFUNDSFUTURES$Settle, order.by = Datevec$date)
FUTURESplot = ggplot() + 
  geom_line(aes(x=index(FEDFUNDSFUTURES), 
                y = FEDFUNDSFUTURES), colour = "#014d64") + 
  labs(title = "Fed Fund Futures price", 
       caption = "Rasmus M. Jensen",
       y = "Price",
       x = "Year"
  ) +
  scale_color_economist() + scale_y_continuous() +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16)
  )
FUTURESplot 
# ggsave("Futures.pdf", plot = FUTURESplot, width = 14, height = 8, units = "in")

# saveRDS(CONSrate, file = 'CONSrate.Rds')
# saveRDS(FEDFUNDSFUTURES, file = 'FEDFUNDSFUTRES.Rds')

# Stationarity testing:
adf.test(CONSrate, k = 1)   # non-stationary p-value>0.05
adf.test(FEDFUNDSFUTURES, k = 1) # non-stationary p-value>0.05


# Hence differencing:
DFEDFUNDSFUTURES <- diff(FEDFUNDSFUTURES)
DFEDFUNDSFUTURES <- na.omit(DFEDFUNDSFUTURES)
adf.test(DFEDFUNDSFUTURES, k = 1)
DFunds = diff(CONSrate)
DFunds = na.omit(DFunds)
adf.test(DFunds, k = 1) 
# P-value lower than 0.05 hence accept null stationary at significance<1% 

DiffFundsPlot = ggplot() + 
  geom_line(aes(x=index(DFunds), 
                y = DFunds), colour = "#014d64") + 
  geom_hline(yintercept = 0, colour = "#00887d", lty = 2, size = 1.1) +
  labs(title = expression(Delta*"Policy Rate"), 
       caption = "Rasmus M. Jensen",
       y = expression(Delta*"Policy Rate"), 
       x = "Year"
  ) +
  scale_color_economist() + scale_y_continuous() +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16)
  )
# DiffFundsPlot
# ggsave("DeltaPolicy_rate.pdf", plot = DiffFundsPlot, width = 14, height = 8, units = "in")

DiffFutures = ggplot() + 
  geom_line(aes(x=index(DFEDFUNDSFUTURES), 
                y = DFEDFUNDSFUTURES), colour = "#014d64") + 
  geom_hline(yintercept = 0, colour = "#00887d", lty = 2, size = 1.1) +
  labs(title = expression(Delta*"Fed Funds Futures prices"), 
       caption = "Rasmus M. Jensen",
       y = expression(Delta*"Futures price"), 
       x = "Year"
  ) +
  scale_color_economist() + scale_y_continuous() +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16)
  )
# DiffFutures
# ggsave("DeltaFutures.pdf", plot = DiffFutures, width = 14, height = 8, units = "in")



Datevec <- Datevec[2:nrow(Datevec),]
Factors <- xts(Fhat, order.by = Datevec$date)
colnames(Factors) <- 1:7
Fed.Funds <- xts(DFunds, order.by = Datevec$date)
colnames(Fed.Funds) <- "Diff.Funds"
colnames(DFEDFUNDSFUTURES) <- "Diff.Fed.Funds.Futures"
rm(FEDFUNDS, SHADOWRATE, DateVec, Datevec, Fhat, i, DFunds, Fhat_d)

Data.Frame <- merge.xts(Fed.Funds, DFEDFUNDSFUTURES, Factors)
write.csv(Data.Frame,'Data.Frame.csv')
colnames(Data.Frame) <- c("Diff.Funds", "Diff.Fed.Funds.Futures", 
                          "F1", "F2", "F3", "F4", "F5", "F6", "F7")
rm(Factors, Fed.Funds)
# plot.xts(Data.Frame$Diff.Funds)

################
## Data Plots ##
################

F1plot = ggplot() + 
  geom_line(aes(x=index(Data.Frame), 
                y = Data.Frame$F1), colour = "#70AD47") +
  labs(title = "Factor 1", 
       colour = "Series",
       x = "Year"
  ) +
  scale_y_continuous() +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 12),
        legend.position = c(0.8, 1.025),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        axis.title.y = element_blank()
  ) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE, direction = "horizontal")
  )

F2plot = ggplot() + 
  geom_line(aes(x=index(Data.Frame), 
                y = Data.Frame$F2), colour = "#70AD47") +
  labs(title = "Factor 2", 
       colour = "Series",
       x = "Year"
  ) +
  scale_y_continuous() +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 12),
        legend.position = c(0.8, 1.025),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        axis.title.y = element_blank()
  ) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE, direction = "horizontal")
  )

F3plot = ggplot() + 
  geom_line(aes(x=index(Data.Frame), 
                y = Data.Frame$F3), colour = "#70AD47") +
  labs(title = "Factor 3", 
       colour = "Series",
       x = "Year"
  ) +
  scale_y_continuous() +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 12),
        legend.position = c(0.8, 1.025),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        axis.title.y = element_blank()
  ) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE, direction = "horizontal")
  )

F4plot = ggplot() + 
  geom_line(aes(x=index(Data.Frame), 
                y = Data.Frame$F4), colour = "#70AD47") +
  labs(title = "Factor 4", 
       colour = "Series",
       x = "Year"
  ) +
  scale_y_continuous() +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 12),
        legend.position = c(0.8, 1.025),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        axis.title.y = element_blank()
  ) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE, direction = "horizontal")
  )

F5plot = ggplot() + 
  geom_line(aes(x=index(Data.Frame), 
                y = Data.Frame$F5), colour = "#70AD47") +
  labs(title = "Factor 5", 
       colour = "Series",
       x = "Year"
  ) +
  scale_y_continuous() +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 12),
        legend.position = c(0.8, 1.025),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        axis.title.y = element_blank()
  ) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE, direction = "horizontal")
  )

F6plot = ggplot() + 
  geom_line(aes(x=index(Data.Frame), 
                y = Data.Frame$F6), colour = "#70AD47") +
  labs(title = "Factor 6", 
       colour = "Series",
       x = "Year"
  ) +
  scale_y_continuous() +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 12),
        legend.position = c(0.8, 1.025),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        axis.title.y = element_blank()
  ) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE, direction = "horizontal")
  )

F7plot = ggplot() + 
  geom_line(aes(x=index(Data.Frame), 
                y = Data.Frame$F7), colour = "#70AD47") +
  labs(title = "Factor 7", 
       colour = "Series",
       x = "Year"
  ) +
  scale_y_continuous() +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 12),
        legend.position = c(0.8, 1.025),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        axis.title.y = element_blank()
  ) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE, direction = "horizontal")
  )

Annot = ggplot() + 
  geom_line(aes(x=index(Data.Frame), 
                y = Data.Frame$F7), colour = "#FFFFFF", size=1.0) +
  labs(title = "Factor 7", 
       colour = "Series",
       x = "Year",
       caption = "Rasmus M. Jensen"
  ) +
  scale_y_continuous() +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_blank(),
        axis.title = element_blank(),
        legend.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.text = element_blank(),
        panel.border = element_blank(),
        line = element_blank()
  ) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE, direction = "horizontal")
  )

Factor.plots = grid.arrange(F1plot, F2plot, F3plot, F4plot, F5plot, F6plot, F7plot, Annot, ncol = 2, 
                            layout_matrix = rbind(c(1, 2),
                                                  c(3, 4),
                                                  c(5, 6),
                                                  c(7, 8)))
# ggsave("Factor_Plots.pdf", plot = Factor.plots, width = 14, height = 8, units = "in")
rm(F1plot, F2plot, F3plot, F4plot, F5plot, F6plot, F7plot, Annot)


###########
## FAVAR ##
###########
# Using AIC and BIC to determine the optimal lag and factor structure
## Model 1 ##
## AIC ##
max_lag = 5
max_fac = 7
AkaikeVAR1 = matrix(0, nrow = max_lag, ncol = max_fac+1)
row.names(AkaikeVAR1) <- c("VAR(1)", "VAR(2)", "VAR(3)", "VAR(4)", "VAR(5)")
colnames(AkaikeVAR1) <- c("FA(0)", "FA(1)", "FA(2)", "FA(3)", "FA(4)", "FA(5)",
                         "FA(6)", "FA(7)")
for (i in 1:max_lag){
  for (z in 0:max_fac){
    AkaikeVAR1[i,(z+1)] <-  AIC(VAR(Data.Frame[, 1:(2+z)], type = "none", p=i))
  }
}

print(paste("Model 1: Akaike Information Criterion suggests a:", 
            colnames(AkaikeVAR)[which(AkaikeVAR1==AkaikeVAR1[which.min(AkaikeVAR1)], 
                                      arr.ind=TRUE)[2]],
            rownames(AkaikeVAR1)[which(AkaikeVAR1==AkaikeVAR1[which.min(AkaikeVAR1)], 
                                      arr.ind=TRUE)[1]], sep=" "))

## BIC ##
BICVAR1 = matrix(0, nrow = max_lag, ncol = max_fac+1)
row.names(BICVAR1) <- c("VAR(1)", "VAR(2)", "VAR(3)", "VAR(4)", "VAR(5)")
colnames(BICVAR1) <- c("FA(0)", "FA(1)", "FA(2)", "FA(3)", "FA(4)", "FA(5)",
                      "FA(6)", "FA(7)")
for (i in 1:max_lag){
  for (z in 0:max_fac){
    BICVAR1[i,(z+1)] <-  BIC(VAR(Data.Frame[, 1:(2+z)], type = "none", p=i))
  }
}
print(paste("Model 1: Bayesian Information Criterion suggests a:", 
            colnames(BICVAR1)[which(BICVAR1==BICVAR1[which.min(BICVAR1)], 
                                   arr.ind=TRUE)[2]],
            rownames(BICVAR1)[which(BICVAR1==BICVAR1[which.min(BICVAR1)], 
                                   arr.ind=TRUE)[1]], sep=" "))
# stargazer(AkaikeVAR1, summary = FALSE)
# stargazer(BICVAR1, summary = FALSE)

## Model 2 ##

VAR.Data = merge.xts(IP, CPI, Data.Frame$Diff.Funds) ##VAR Dataframce
FAVAR.dat = merge.xts(VAR.Data, Data.Frame[,3:ncol(Data.Frame)])

AkaikeVAR2 = matrix(0, nrow = max_lag, ncol = max_fac+1)
row.names(AkaikeVAR2) <- c("VAR(1)", "VAR(2)", "VAR(3)", "VAR(4)", "VAR(5)")
colnames(AkaikeVAR2) <- c("FA(0)", "FA(1)", "FA(2)", "FA(3)", "FA(4)", "FA(5)",
                         "FA(6)", "FA(7)")
for (i in 1:max_lag){
  for (z in 0:max_fac){
    AkaikeVAR2[i,(z+1)] <-  AIC(VAR(FAVAR.dat[, 1:(3+z)], type = "none", p=i))
  }
}

print(paste("Model 2: Akaike Information Criterion suggests a:", 
            colnames(AkaikeVAR2)[which(AkaikeVAR2==AkaikeVAR2[which.min(AkaikeVAR2)], 
                                      arr.ind=TRUE)[2]],
            rownames(AkaikeVAR2)[which(AkaikeVAR2==AkaikeVAR2[which.min(AkaikeVAR2)], 
                                      arr.ind=TRUE)[1]], sep=" "))

## BIC ##
BICVAR2 = matrix(0, nrow = max_lag, ncol = max_fac+1)
row.names(BICVAR2) <- c("VAR(1)", "VAR(2)", "VAR(3)", "VAR(4)", "VAR(5)")
colnames(BICVAR2) <- c("FA(0)", "FA(1)", "FA(2)", "FA(3)", "FA(4)", "FA(5)",
                      "FA(6)", "FA(7)")
for (i in 1:max_lag){
  for (z in 0:max_fac){
    BICVAR2[i,(z+1)] <-  BIC(VAR(FAVAR.dat[, 1:(3+z)], type = "none", p=i))
  }
}
print(paste("Model 2: Bayesian Information Criterion suggests a:", 
            colnames(BICVAR2)[which(BICVAR2==BICVAR2[which.min(BICVAR2)], 
                                   arr.ind=TRUE)[2]],
            rownames(BICVAR2)[which(BICVAR2==BICVAR2[which.min(BICVAR2)], 
                                   arr.ind=TRUE)[1]], sep=" "))
# stargazer(AkaikeVAR2,BICVAR2, summary = FALSE)

rm(AkaikeVAR1, AkaikeVAR2, BICVAR1, BICVAR2)

FAVAR <- VAR(Data.Frame, type = "none", p = 1)

## Data Summary Table ## 
summarydat = merge.xts(Data.Frame[,2], FAVAR.dat[,1:3])
summarydat

# stargazer(summarydat, summary = TRUE) ## Data summary
adf.test(IP, k = 1) ## P-value for fata summary
adf.test(CPI, k = 1) ## P-value for data summary

###############
## Fit-plots ##
###############
    ## Main Plot ##
## Fit ##
Fit.Funds = ggplot() + 
  geom_line(aes(x=index(Data.Frame[2:nrow(Data.Frame),]), 
                y = FAVAR$varresult$Diff.Funds$fitted.values, colour = "Fitted")) +
  geom_line(aes(x = index(Data.Frame[2:nrow(Data.Frame),]), 
                y = FAVAR$datamat$Diff.Funds, colour = "Actual")) + 
  labs(title = expression(Delta*"Policy Rate"), 
       subtitle = "Actual vs. Fitted",
       color="Series",
       y = expression(Delta*"Policy Rate")
  ) +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11),
        legend.position = c(0.9, 1.05),
        legend.direction = "horizontal",
        axis.title.x = element_blank()
        ) +
        guides(fill=guide_legend(nrow=1,byrow=TRUE, direction = "horizontal"))
Fit.Funds
## Residuals ##
Fit.Funds.Res = ggplot() + 
  geom_line(aes(x=index(Data.Frame[2:nrow(Data.Frame),]), 
                y = FAVAR$varresult$Diff.Funds$residuals)) +
  geom_hline(yintercept=0, col = "coral", size = 0.25)+
  labs(title = "Residuals",
       caption = "Rasmus M. Jensen",
       y = "Residuals",
       x = "Year"
  ) +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 11))
##
F_funds <- plot_grid(Fit.Funds, Fit.Funds.Res, align="v", nrow = 2, 
                     rel_heights = c(0.7, 0.3))
# ggsave("Fit.Funds_FAVAR.pdf", plot = F_funds, width = 14, height = 8, units = "in")

#######################
## Fed Funds Futures ##
#######################
Fit.Funds.Futures = ggplot() + 
  geom_line(aes(x=index(Data.Frame[2:nrow(Data.Frame),]), 
                y = FAVAR$varresult$Diff.Fed.Funds.Futures$fitted.values, 
                colour = "Fitted")) +
  geom_line(aes(x = index(Data.Frame[2:nrow(Data.Frame),]), 
                y = Data.Frame$Diff.Fed.Funds.Futures[2:nrow(Data.Frame)], 
                colour = "Actual")) + 
  labs(title = expression(Delta*"FED.FUNDS Futures"), 
       subtitle = "Actual vs. Fitted",
       color="Series",
       y = expression(Delta*"FED.FUNDS Futures")
  ) +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11),
        legend.position = c(0.9, 1.05),
        legend.direction = "horizontal",
        axis.title.x = element_blank()
  ) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE, direction = "horizontal"))
## Residuals ##
Fit.Funds.Futures.Res = ggplot() + 
  geom_line(aes(x=index(Data.Frame[2:nrow(Data.Frame),]), 
                y = FAVAR$varresult$Diff.Fed.Funds.Futures$residuals)) +
  geom_hline(yintercept=0, col = "coral", size = 0.25)+
  labs(title = "Residuals",
       caption = "Rasmus M. Jensen",
       y = "Residuals",
       x = "Year"
  ) +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 11))
##
F_Futures_funds <- plot_grid(Fit.Funds.Futures, Fit.Funds.Futures.Res, align="v", nrow = 2, 
                     rel_heights = c(0.7, 0.3))
# F_Futures_funds
# ggsave("Fit.Funds.Futures_FAVAR.pdf", plot = F_Futures_funds, width = 14, height = 8, units = "in")

##############
## Factor 1 ##
##############
## Fit ##
Fit.F1 = ggplot() + 
  geom_line(aes(x=index(Data.Frame[2:nrow(Data.Frame),]), 
                y = FAVAR$varresult$F1$fitted.values, colour = "Fitted")) +
  geom_line(aes(x = index(Data.Frame[2:nrow(Data.Frame),]), 
                y = Data.Frame$F1[2:nrow(Data.Frame)], colour = "Actual")) + 
  labs(title = "Factor 1", 
       subtitle = "Actual vs. Fitted",
       color="Series",
       y = "Factor 1"
  ) +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11),
        legend.position = c(0.9, 1.05),
        legend.direction = "horizontal",
        axis.title.x = element_blank()
  ) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE, direction = "horizontal"))
## Residuals ##
Fit.F1.Res = ggplot() + 
  geom_line(aes(x=index(Data.Frame[2:nrow(Data.Frame),]), 
                y = FAVAR$varresult$F1$residuals)) +
  geom_hline(yintercept=0, col = "coral", size = 0.25)+
  labs(title = "Residuals",
       caption = "Rasmus M. Jensen",
       y = "Residuals",
       x = "Year"
  ) +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 11))

Plot.F1 <- plot_grid(Fit.F1, Fit.F1.Res, align="v", nrow = 2, 
                     rel_heights = c(0.7, 0.3))
# Plot.F1

##############
## Factor 2 ##
##############
## Fit ##
Fit.F2 = ggplot() + 
  geom_line(aes(x=index(Data.Frame[2:nrow(Data.Frame),]), 
                y = FAVAR$varresult$F2$fitted.values, colour = "Fitted")) +
  geom_line(aes(x = index(Data.Frame[2:nrow(Data.Frame),]), 
                y = Data.Frame$F2[2:nrow(Data.Frame)], colour = "Actual")) + 
  labs(title = "Factor 2", 
       subtitle = "Actual vs. Fitted",
       color="Series",
       y = "Factor 2"
  ) + 
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11),
        legend.position = c(0.9, 1.05),
        legend.direction = "horizontal",
        axis.title.x = element_blank()
  ) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE, direction = "horizontal"))
## Residuals ##
Fit.F2.Res = ggplot() + 
  geom_line(aes(x=index(Data.Frame[2:nrow(Data.Frame),]), 
                y = FAVAR$varresult$F2$residuals)) +
  geom_hline(yintercept=0, col = "coral", size = 0.25)+
  labs(title = "Residuals",
       caption = "Rasmus M. Jensen",
       y = "Residuals",
       x = "Year"
  ) +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 11))

Plot.F2 <- plot_grid(Fit.F2, Fit.F2.Res, align="v", nrow = 2, 
                     rel_heights = c(0.7, 0.3))
# Plot.F2

##############
## Factor 3 ##
##############
## Fit ##
Fit.F3 = ggplot() + 
  geom_line(aes(x=index(Data.Frame[2:nrow(Data.Frame),]), 
                y = FAVAR$varresult$F3$fitted.values, colour = "Fitted")) +
  geom_line(aes(x = index(Data.Frame[2:nrow(Data.Frame),]), 
                y = Data.Frame$F3[2:nrow(Data.Frame)], colour = "Actual")) + 
  labs(title = "Factor 3", 
       subtitle = "Actual vs. Fitted",
       color="Series",
       y = "Factor 3"
  ) +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11),
        legend.position = c(0.9, 1.05),
        legend.direction = "horizontal",
        axis.title.x = element_blank()
  ) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE, direction = "horizontal"))
## Residuals ##
Fit.F3.Res = ggplot() + 
  geom_line(aes(x=index(Data.Frame[2:nrow(Data.Frame),]), 
                y = FAVAR$varresult$F3$residuals)) +
  geom_hline(yintercept=0, col = "coral", size = 0.25)+
  labs(title = "Residuals",
       caption = "Rasmus M. Jensen",
       y = "Residuals",
       x = "Year"
  ) +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 11))

Plot.F3 <- plot_grid(Fit.F3, Fit.F3.Res, align="v", nrow = 2, 
                     rel_heights = c(0.7, 0.3))
# Plot.F3

##############
## Factor 4 ##
##############
## Fit ##
Fit.F4 = ggplot() + 
  geom_line(aes(x=index(Data.Frame[2:nrow(Data.Frame),]), 
                y = FAVAR$varresult$F4$fitted.values, colour = "Fitted")) +
  geom_line(aes(x = index(Data.Frame[2:nrow(Data.Frame),]), 
                y = Data.Frame$F4[2:nrow(Data.Frame)], colour = "Actual")) + 
  labs(title = "Factor 4", 
       subtitle = "Actual vs. Fitted",
       color="Series",
       y = "Factor 4"
  ) +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11),
        legend.position = c(0.9, 1.05),
        legend.direction = "horizontal",
        axis.title.x = element_blank()
  ) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE, direction = "horizontal"))
## Residuals ##
Fit.F4.Res = ggplot() + 
  geom_line(aes(x=index(Data.Frame[2:nrow(Data.Frame),]), 
                y = FAVAR$varresult$F4$residuals)) +
  geom_hline(yintercept=0, col = "coral", size = 0.25)+
  labs(title = "Residuals",
       caption = "Rasmus M. Jensen",
       y = "Residuals",
       x = "Year"
  ) +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 11))

Plot.F4 <- plot_grid(Fit.F4, Fit.F4.Res, align="v", nrow = 2, 
                     rel_heights = c(0.7, 0.3))
# Plot.F4

##############
## Factor 5 ##
##############
## Fit ##
Fit.F5 = ggplot() + 
  geom_line(aes(x=index(Data.Frame[2:nrow(Data.Frame),]), 
                y = FAVAR$varresult$F5$fitted.values, colour = "Fitted")) +
  geom_line(aes(x = index(Data.Frame[2:nrow(Data.Frame),]), 
                y = Data.Frame$F5[2:nrow(Data.Frame)], colour = "Actual")) + 
  labs(title = "Factor 5", 
       subtitle = "Actual vs. Fitted",
       color="Series",
       y = "Factor 5"
  ) +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11),
        legend.position = c(0.9, 1.05),
        legend.direction = "horizontal",
        axis.title.x = element_blank()
  ) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE, direction = "horizontal"))
## Residuals ##
Fit.F5.Res = ggplot() + 
  geom_line(aes(x=index(Data.Frame[2:nrow(Data.Frame),]), 
                y = FAVAR$varresult$F5$residuals)) +
  geom_hline(yintercept=0, col = "coral", size = 0.25)+
  labs(title = "Residuals",
       caption = "Rasmus M. Jensen",
       y = "Residuals",
       x = "Year"
  ) +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 11))

Plot.F5 <- plot_grid(Fit.F5, Fit.F5.Res, align="v", nrow = 2, 
                     rel_heights = c(0.7, 0.3))
# Plot.F5

##############
## Factor 6 ##
##############
## Fit ##
Fit.F6 = ggplot() + 
  geom_line(aes(x=index(Data.Frame[2:nrow(Data.Frame),]), 
                y = FAVAR$varresult$F6$fitted.values, colour = "Fitted")) +
  geom_line(aes(x = index(Data.Frame[2:nrow(Data.Frame),]), 
                y = Data.Frame$F6[2:nrow(Data.Frame)], colour = "Actual")) + 
  labs(title = "Factor 6", 
       subtitle = "Actual vs. Fitted",
       color="Series",
       y = "Factor 6"
  ) +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11),
        legend.position = c(0.9, 1.05),
        legend.direction = "horizontal",
        axis.title.x = element_blank()
  ) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE, direction = "horizontal"))
## Residuals ##
Fit.F6.Res = ggplot() + 
  geom_line(aes(x=index(Data.Frame[2:nrow(Data.Frame),]), 
                y = FAVAR$varresult$F6$residuals)) +
  geom_hline(yintercept=0, col = "coral", size = 0.25)+
  labs(title = "Residuals",
       caption = "Rasmus M. Jensen",
       y = "Residuals",
       x = "Year"
  ) +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 11))

Plot.F6 <- plot_grid(Fit.F6, Fit.F6.Res, align="v", nrow = 2, 
                     rel_heights = c(0.7, 0.3))
# Plot.F6

##############
## Factor 7 ##
##############
## Fit ##
Fit.F7 = ggplot() + 
  geom_line(aes(x=index(Data.Frame[2:nrow(Data.Frame),]), 
                y = FAVAR$varresult$F7$fitted.values, colour = "Fitted")) +
  geom_line(aes(x = index(Data.Frame[2:nrow(Data.Frame),]), 
                y = Data.Frame$F7[2:nrow(Data.Frame)], colour = "Actual")) + 
  labs(title = "Factor 7", 
       subtitle = "Actual vs. Fitted",
       color="Series",
       y = "Factor 7"
  ) +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11),
        legend.position = c(0.9, 1.05),
        legend.direction = "horizontal",
        axis.title.x = element_blank()
  ) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE, direction = "horizontal"))
## Residuals ##
Fit.F7.Res = ggplot() + 
  geom_line(aes(x=index(Data.Frame[2:nrow(Data.Frame),]), 
                y = FAVAR$varresult$F7$residuals)) +
  geom_hline(yintercept=0, col = "coral", size = 0.25)+
  labs(title = "Residuals",
       caption = "Rasmus M. Jensen",
       y = "Residuals",
       x = "Year"
  ) +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 11))

Plot.F7 <- plot_grid(Fit.F7, Fit.F7.Res, align="v", nrow = 2, 
                     rel_heights = c(0.7, 0.3))
# Plot.F7

# ggsave("Fit.F1_FAVAR.pdf", plot = Plot.F1, width = 14, height = 8, units = "in")
# ggsave("Fit.F2_FAVAR.pdf", plot = Plot.F2, width = 14, height = 8, units = "in")
# ggsave("Fit.F3_FAVAR.pdf", plot = Plot.F3, width = 14, height = 8, units = "in")
# ggsave("Fit.F4_FAVAR.pdf", plot = Plot.F4, width = 14, height = 8, units = "in")
# ggsave("Fit.F5_FAVAR.pdf", plot = Plot.F5, width = 14, height = 8, units = "in")
# ggsave("Fit.F6_FAVAR.pdf", plot = Plot.F6, width = 14, height = 8, units = "in")
# ggsave("Fit.F7_FAVAR.pdf", plot = Plot.F7, width = 14, height = 8, units = "in")

rm(Fit.F1, Fit.F1.Res, F_funds, Fit.Funds, Fit.Funds.Res, Fit.F2, Fit.F2.Res,
   Fit.F3, Fit.F3.Res, Fit.F4, Fit.F4.Res, Fit.F5, Fit.F5.Res, Fit.F6, Fit.F6.Res,
   Fit.F7, Fit.F7.Res, Plot.F1, Plot.F2, Plot.F3, Plot.F4, Plot.F5, Plot.F6, Plot.F7,
   DiffFundsPlot, F_Futures_funds, DiffFutures, Fit.Funds.Futures, Fit.Funds.Futures.Res,
   PolicyRatePlot, FUTURESplot, AkaikeVAR, BICVAR, DFEDFUNDSFUTURES, max_fac, max_lag,
   z, i, FEDFUNDSFUTURES, factor, current, RawData, y)

#########################
## Performance testing ##
#########################

######################
## Expanding window ##
######################
# VAR.Data = merge.xts(IP, CPI, Data.Frame$Diff.Funds) ##VAR Dataframce
# FAVAR.dat = merge.xts(VAR.Data, Data.Frame[,3:ncol(Data.Frame)])

init_p = 250
nahead = 1
noMods = 7
end_p = nrow(Data.Frame) - nahead
n_windows = end_p - init_p 
fcstErr = data.frame(matrix(0, nrow = (n_windows+1), ncol = noMods))
fcstErr = xts(fcstErr, order.by = index(Data.Frame[(nrow(Data.Frame)-n_windows):nrow(Data.Frame), ]))
colnames(fcstErr) <- c("FAVAR", "FAVAR2", "AR", "RW", "MM", "VAR(1)", "VAR(3)")
fcst = fcstErr
observed = Data.Frame[(init_p+nahead):nrow(Data.Frame),1]

for (i in 1:(n_windows)) { 
  print(sprintf("Expanding window: Iteration: %d", i))
  ## Factor reestimation ##
  Data_set = yt_z1[1:(i+init_p-1), ]
  cov_yt <- t(Data_set) %*% Data_set / nrow(Data_set)
  eig = eigen(cov_yt)
  eig_val <- as.data.frame(eig[1]$values)
  eig_vec <- as.data.frame(eig[2]$vectors)
  Factor_loadings <- sqrt(ncol(Data_set)) * eig_vec
  Factor_loadings <-as.data.frame(Factor_loadings)
  Factor_components <- as.matrix(Data_set) %*% as.matrix(Factor_loadings/ncol(Data_set))
  Factor_components <- as.data.frame(Factor_components)
  Fhat <- Factor_components[,1:7]
  Fhat <- xts(Fhat, order.by = index(Data.Frame[1:(i+init_p-1)]))
  ## Data set ##
  TrainingDat = Data.Frame[1:(i+init_p-1),1:2]
  TrainingDat = merge.xts(TrainingDat, Fhat)
  VARTrainingdat = VAR.Data[1:(i+init_p-1),]
  FAVARtrainingDat = merge.xts(VARTrainingdat, Fhat)
  ## FAVAR ##
  TrainingVAR  = VAR(TrainingDat, type = "none", p = 1)
  fcstFAVAR    = predict(TrainingVAR, n.ahead = nahead)
  fcstFAVAR    = fcstFAVAR$fcst$Diff.Funds[nahead,1]
  fcst[i,1]    = fcstFAVAR
  fcstErr[i,1] = Data.Frame[(init_p+nahead+i-1),1] - fcstFAVAR
  ## FAVAR Y = 3 series##
  TrainingFAVAR2  = VAR(FAVARtrainingDat, type = "none", p = 1)
  fcstFAVAR2    = predict(TrainingFAVAR2, n.ahead = nahead)
  fcstFAVAR2    = fcstFAVAR2$fcst$Diff.Funds[nahead,1]
  fcst[i,2]    = fcstFAVAR2
  fcstErr[i,2] = Data.Frame[(init_p+nahead+i-1),1] - fcstFAVAR2
  ## AR ##
  TrainingAR  = arima(TrainingDat$Diff.Funds, order = c(1, 0, 0))
  fcstAR = predict(TrainingAR, n.ahead = nahead)
  fcstAR = fcstAR$pred[nahead]
  fcst[i,3] <- fcstAR
  fcstErr[i,3] = Data.Frame[(init_p+nahead+i-1),1] - fcstAR
  ## RW ##
  RW = arima(TrainingDat$Diff.Funds, order = c(0, 1, 0))
  fcstRW = predict(RW, n.ahead = nahead)
  fcstRW = fcstRW$pred[nahead]
  fcst[i,4] <- fcstRW
  fcstErr[i,4] = Data.Frame[(init_p+nahead+i-1),1] - fcstRW
  ## Mean model ##
  MM = arima(TrainingDat$Diff.Funds, order = c(0,0,0), include.mean = T)
  fcstMM = predict(MM, n.ahead = nahead)
  fcstMM = fcstMM$pred[nahead]
  fcst[i,5] <- fcstMM
  fcstErr[i,5] = Data.Frame[(init_p+nahead+i-1),1] - fcstMM
  ## VAR(1) ##
  TrainingVAR = VAR(VARTrainingdat, type = "none", p = 1)
  fcstVAR = predict(TrainingVAR, n.ahead = nahead)
  fcstVAR = fcstVAR$fcst$Diff.Funds[nahead, 1]
  fcst[i,6] = fcstVAR
  fcstErr[i,6] = Data.Frame[(init_p+nahead+i-1),1] - fcstVAR
  ## VAR(3) ##
  TrainingVAR2 = VAR(VARTrainingdat, type = "none", p = 3)
  fcstVAR2 = predict(TrainingVAR2, n.ahead = nahead)
  fcstVAR2 = fcstVAR2$fcst$Diff.Funds[nahead, 1]
  fcst[i,7] = fcstVAR2
  fcstErr[i,7] = Data.Frame[(init_p+nahead+i-1),1] - fcstVAR2
  
}

fcst = fcst[-nrow(fcst),]
fcstErr = fcstErr[-nrow(fcstErr),]

FEW1p_plot = ggplot() +
  geom_line(aes(x=index(fcst),
                y = fcst$FAVAR, col = "FAVAR1"), size = 1.0) +
  geom_line(aes(x=index(fcst),
                y = fcst$FAVAR2, col = "FAVAR2"), size = 1.0) +
  geom_line(aes(x=index(fcst),
                y=(fcst$`VAR(1`), col = "VAR(1)"), size = 1.0)+
  geom_line(aes(x=index(fcst),
                y=(fcst$`VAR(3`), col = "VAR(3)"), size = 1.0)+
  geom_line(aes(x=index(fcst),
                y = fcst$AR, col = "AR(1)"), size = 1.0) +
  geom_line(aes(x=index(fcst),
                y = fcst$RW, col = "RW"), size = 1.0) +
  geom_line(aes(x=index(fcst),
                y = fcst$MM, col = "Mean"), size = 1.0) +
  geom_line(aes(x= index(observed),
                y = observed, col = "Observed")) + 
  labs(title = "Forecasts", 
       subtitle = "Actual vs. Forecasts, Expanding window, 1-period ahead",
       color="Series",
       caption = "Rasmus M. Jensen",
       y = "Fed Funds Rate"
  ) +
  scale_y_continuous() +
  scale_color_colorblind()+
  # scale_color_manual(values=c("#90353B", "#2D6D66", "#938DD2", "#1A476F", "#9C8847")) +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11),
        legend.position = c(0.8, 1.0399),
        legend.direction = "horizontal",
        axis.title.x = element_blank(),
        legend.background = element_blank()
  ) +
  guides(colour=guide_legend(nrow=2,byrow=TRUE, direction = "horizontal"))
FEW1p_plot

# ggsave("FEW1p_plot.pdf", plot = FEW1p_plot, width = 14,
     # height = 8, units = "in")

FEW1pErr_plot = ggplot() +
  geom_line(aes(x=index(fcstErr),
                y=(fcstErr$`VAR(1`), col = "VAR(1)"), size = 1.0)+
  geom_line(aes(x=index(fcstErr),
                y=(fcstErr$`VAR(3`), col = "VAR(3)"), size = 1.0)+
  geom_line(aes(x=index(fcstErr),
                y = fcstErr$AR, col = "AR(1)"), size = 1.0) +
  geom_line(aes(x=index(fcstErr),
                y = fcstErr$RW, col = "RW"), size = 1.0) +
  geom_line(aes(x=index(fcstErr),
                y = fcstErr$MM, col = "Mean"), size = 1.0) +
  geom_line(aes(x=index(fcstErr),
                y = fcstErr$FAVAR2, col = "FAVAR2"), size = 1.0) +
  geom_line(aes(x=index(fcstErr),
                y = fcstErr$FAVAR, col = "FAVAR1"), size = 1.0) +
  geom_hline(aes(yintercept = 0), size = 1) +
  labs(title = "Forecast Errors", 
       subtitle = "Expanding window, 1-period ahead",
       color="Series",
       caption = "Rasmus M. Jensen",
       y = expression(epsilon[t+1*"|"*t]~"="~y[t+1]~"-"~f[t+1*"|"*t])) +
  scale_y_continuous() +
  scale_color_colorblind() +
  # scale_color_manual(values=c("#90353B", "#2D6D66", "#938DD2", "#1A476F", "#9C8847")) +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11),
        legend.position = c(0.8, 1.0399),
        legend.direction = "horizontal",
        axis.title.x = element_blank(),
        legend.background = element_blank()
  ) +
  guides(colour=guide_legend(nrow=2,byrow=TRUE, direction = "horizontal"))
# FEW1pErr_plot
# ggsave("FEW1pErr_plot.pdf", FEW1pErr_plot, width = 14, height = 7)

rm(FEW1p_plot)

 ## Forecast measures matrix
fcst.sign = fcst

for (z in 1:noMods){
  for (i in 1:nrow(fcst)){
    if (sign(fcst[i,z]) == sign(observed[i])){
    fcst.sign[i,z] = 100
    }
    else if (sign(fcst[i,z])!=sign(observed[i])){
    fcst.sign[i,z] = 0
    }
  }
}

Bias = colMeans(fcstErr)*100
MSE  = colMeans(fcstErr^2)
MSFE = colMeans(sqrt(fcstErr^2))
Sign = colMeans(fcst.sign)
Std = Sign
for (i in 1:7){
  Std[i] = sd(fcst[,i])
}
Std = Std*100

dm = cbind(Sign, 1:7)
colnames(dm) = c("DM test statistic", "DM p-value")
for (i in 1:7){
  if (i==4){
    dm[i,] = NA
  }
  else {
    dm[i,1] = dm.test(fcstErr[,4], fcstErr[,i], alternative = "g", h = 1, power = 1)$statistic
    dm[i,2] = dm.test(fcstErr[,4], fcstErr[,i], alternative = "g", h = 1, power = 1)$p.value
  }
}
dm ## Alternative: method 2 (RW) less accurate than 1. Low P reject null - hence accept alternative.

f.p.m = cbind(Std, Bias, MSE, MSFE, Sign, dm)
# stargazer(f.p.m, summary = FALSE, digits = 5)
f.p.m

#####################
## 3-periods ahead ##
#####################
init_p = 250
nahead = 3
noMods = 7
end_p = nrow(Data.Frame) - nahead
n_windows = end_p - init_p 
fcstErr = data.frame(matrix(0, nrow = (n_windows+1), ncol = noMods))
fcstErr = xts(fcstErr, order.by = index(Data.Frame[(nrow(Data.Frame)-n_windows):nrow(Data.Frame), ]))
colnames(fcstErr) <- c("FAVAR", "FAVAR2", "AR", "RW", "MM", "VAR(1)", "VAR(3)")
fcst = fcstErr
observed = Data.Frame[(init_p+nahead):nrow(Data.Frame),1]

for (i in 1:(n_windows)) { 
  print(sprintf("Expanding window: Iteration: %d", i))
  ## Factor reestimation ##
  Data_set = yt_z1[1:(i+init_p-1), ]
  cov_yt <- t(Data_set) %*% Data_set / nrow(Data_set)
  eig = eigen(cov_yt)
  eig_val <- as.data.frame(eig[1]$values)
  eig_vec <- as.data.frame(eig[2]$vectors)
  Factor_loadings <- sqrt(ncol(Data_set)) * eig_vec
  Factor_loadings <-as.data.frame(Factor_loadings)
  Factor_components <- as.matrix(Data_set) %*% as.matrix(Factor_loadings/ncol(Data_set))
  Factor_components <- as.data.frame(Factor_components)
  Fhat <- Factor_components[,1:7]
  Fhat <- xts(Fhat, order.by = index(Data.Frame[1:(i+init_p-1)]))
  ## Data set ##
  TrainingDat = Data.Frame[1:(i+init_p-1),1:2]
  TrainingDat = merge.xts(TrainingDat, Fhat)
  VARTrainingdat = VAR.Data[1:(i+init_p-1),]
  FAVARtrainingDat = merge.xts(VARTrainingdat, Fhat)
  ## FAVAR ##
  TrainingVAR  = VAR(TrainingDat, type = "none", p = 1)
  fcstFAVAR    = predict(TrainingVAR, n.ahead = nahead)
  fcstFAVAR    = fcstFAVAR$fcst$Diff.Funds[nahead,1]
  fcst[i,1]    = fcstFAVAR
  fcstErr[i,1] = Data.Frame[(init_p+nahead+i-1),1] - fcstFAVAR
  ## FAVAR Y = 3 series##
  TrainingFAVAR2  = VAR(FAVARtrainingDat, type = "none", p = 1)
  fcstFAVAR2    = predict(TrainingFAVAR2, n.ahead = nahead)
  fcstFAVAR2    = fcstFAVAR2$fcst$Diff.Funds[nahead,1]
  fcst[i,2]    = fcstFAVAR2
  fcstErr[i,2] = Data.Frame[(init_p+nahead+i-1),1] - fcstFAVAR2
  ## AR ##
  TrainingAR  = arima(TrainingDat$Diff.Funds, order = c(1, 0, 0))
  fcstAR = predict(TrainingAR, n.ahead = nahead)
  fcstAR = fcstAR$pred[nahead]
  fcst[i,3] <- fcstAR
  fcstErr[i,3] = Data.Frame[(init_p+nahead+i-1),1] - fcstAR
  ## RW ##
  RW = arima(TrainingDat$Diff.Funds, order = c(0, 1, 0))
  fcstRW = predict(RW, n.ahead = nahead)
  fcstRW = fcstRW$pred[nahead]
  fcst[i,4] <- fcstRW
  fcstErr[i,4] = Data.Frame[(init_p+nahead+i-1),1] - fcstRW
  ## Mean model ##
  MM = arima(TrainingDat$Diff.Funds, order = c(0,0,0), include.mean = T)
  fcstMM = predict(MM, n.ahead = nahead)
  fcstMM = fcstMM$pred[nahead]
  fcst[i,5] <- fcstMM
  fcstErr[i,5] = Data.Frame[(init_p+nahead+i-1),1] - fcstMM
  ## VAR(1) ##
  TrainingVAR = VAR(VARTrainingdat, type = "none", p = 1)
  fcstVAR = predict(TrainingVAR, n.ahead = nahead)
  fcstVAR = fcstVAR$fcst$Diff.Funds[nahead, 1]
  fcst[i,6] = fcstVAR
  fcstErr[i,6] = Data.Frame[(init_p+nahead+i-1),1] - fcstVAR
  ## VAR(3) ##
  TrainingVAR2 = VAR(VARTrainingdat, type = "none", p = 3)
  fcstVAR2 = predict(TrainingVAR2, n.ahead = nahead)
  fcstVAR2 = fcstVAR2$fcst$Diff.Funds[nahead, 1]
  fcst[i,7] = fcstVAR2
  fcstErr[i,7] = Data.Frame[(init_p+nahead+i-1),1] - fcstVAR2
  
}

fcst = fcst[-nrow(fcst),]
fcstErr = fcstErr[-nrow(fcstErr),]

FEW3p_plot = ggplot() +
  geom_line(aes(x=index(fcst),
                y = fcst$FAVAR, col = "FAVAR1"), size = 1.0) +
  geom_line(aes(x=index(fcst),
                y = fcst$FAVAR2, col = "FAVAR2"), size = 1.0) +
  geom_line(aes(x=index(fcst),
                y=(fcst$`VAR(1`), col = "VAR(1)"), size = 1.0)+
  geom_line(aes(x=index(fcst),
                y=(fcst$`VAR(3`), col = "VAR(3)"), size = 1.0)+
  geom_line(aes(x=index(fcst),
                y = fcst$AR, col = "AR(1)"), size = 1.0) +
  geom_line(aes(x=index(fcst),
                y = fcst$RW, col = "RW"), size = 1.0) +
  geom_line(aes(x=index(fcst),
                y = fcst$MM, col = "Mean"), size = 1.0) +
  geom_line(aes(x= index(observed),
                y = observed, col = "Observed")) + 
  labs(title = "Forecasts", 
       subtitle = "Actual vs. Forecasts, Expanding window, 3-periods ahead",
       color="Series",
       caption = "Rasmus M. Jensen",
       y = "Fed Funds Rate"
  ) +
  scale_y_continuous() +
  scale_color_colorblind()+
  # scale_color_manual(values=c("#90353B", "#2D6D66", "#938DD2", "#1A476F", "#9C8847")) +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11),
        legend.position = c(0.8, 1.0399),
        legend.direction = "horizontal",
        axis.title.x = element_blank(),
        legend.background = element_blank()
  ) +
  guides(colour=guide_legend(nrow=2,byrow=TRUE, direction = "horizontal"))
# FEW3p_plot

FEW3pErr_plot = ggplot() +
  geom_line(aes(x=index(fcstErr),
                y = fcstErr$FAVAR, col = "FAVAR1"), size = 1.0) +
  geom_line(aes(x=index(fcstErr),
                y = fcstErr$FAVAR2, col = "FAVAR2"), size = 1.0) +
  geom_line(aes(x=index(fcstErr),
                y=(fcstErr$`VAR(1`), col = "VAR(1)"), size = 1.0)+
  geom_line(aes(x=index(fcstErr),
                y=(fcstErr$`VAR(3`), col = "VAR(3)"), size = 1.0)+
  geom_line(aes(x=index(fcstErr),
                y = fcstErr$AR, col = "AR(1)"), size = 1.0) +
  geom_line(aes(x=index(fcstErr),
                y = fcstErr$RW, col = "RW"), size = 1.0) +
  geom_line(aes(x=index(fcstErr),
                y = fcstErr$MM, col = "Mean"), size = 1.0) +
  geom_hline(aes(yintercept = 0), size = 1) +
  labs(title = "Forecast Errors", 
       subtitle = "Expanding window, 3-periods ahead",
       color="Series",
       caption = "Rasmus M. Jensen",
       y = expression(epsilon[t+3*"|"*t]~"="~y[t+3]~"-"~f[t+3*"|"*t])) +
  scale_y_continuous() +
  scale_color_colorblind() +
  # scale_color_manual(values=c("#90353B", "#2D6D66", "#938DD2", "#1A476F", "#9C8847")) +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11),
        legend.position = c(0.8, 1.0399),
        legend.direction = "horizontal",
        axis.title.x = element_blank(),
        legend.background = element_blank()
  ) +
  guides(colour=guide_legend(nrow=2,byrow=TRUE, direction = "horizontal"))
# FEW3pErr_plot
  # ggsave("FEW3pErr_plot.pdf", plot = FEW3pErr_plot, width = 14,
  #      height = 7, units = "in")

rm(FEW3p_plot)

## Forecast measures matrix
fcst.sign = fcst

for (z in 1:noMods){
  for (i in 1:nrow(fcst)){
    if (sign(fcst[i,z]) == sign(observed[i])){
      fcst.sign[i,z] = 100
    }
    else if (sign(fcst[i,z])!=sign(observed[i])){
      fcst.sign[i,z] = 0
    }
  }
}

Bias = colMeans(fcstErr)*100
MSE  = colMeans(fcstErr^2)
MSFE = colMeans(sqrt(fcstErr^2))
Sign = colMeans(fcst.sign)
Std = Sign
for (i in 1:7){
  Std[i] = sd(fcst[,i])
}
Std = Std*100

dm = cbind(Sign, 1:7)
colnames(dm) = c("DM test statistic", "DM p-value")
for (i in 1:7){
  if (i==4){
    dm[i,] = NA
  }
  else {
    dm[i,1] = dm.test(fcstErr[,4], fcstErr[,i], alternative = "g", h = 1, power = 1)$statistic
    dm[i,2] = dm.test(fcstErr[,4], fcstErr[,i], alternative = "g", h = 1, power = 1)$p.value
  }
}
# dm ## Alternative: method 2 (RW) less accurate than 1. Low P reject null - hence accept alternative.

f.p.m = cbind(Std, Bias, MSE, MSFE, Sign, dm)
# stargazer(f.p.m, summary = FALSE, digits = 5)
f.p.m

################################
## Alternate Split/Robustness ## * Computational heavy
################################

## Note that this loop is computational very heavy, Hence saving and reloading RDS 
## after first time: Hence entire loop is Cmmented away.
# MeanErr = Data.Frame[,FALSE]
# MeanErr = cbind(MeanErr, c(1:nrow(MeanErr)), c(1:nrow(MeanErr)), c(1:nrow(MeanErr)), c(1:nrow(MeanErr)), c(1:nrow(MeanErr)), c(1:nrow(MeanErr)), c(1:nrow(MeanErr)))
# colnames(MeanErr) = c("FAVAR1", "FAVAR2", "AR", "RW", "MM", "VAR", "VAR3")
# MeanErr = MeanErr[180:324]
# jmax=145
# for (j in 1:jmax){
#   print(sprintf("Mean Error Calc: %d", j))
#   init_p = 179+j
#   nahead = 1
#   noMods = 7
#   end_p = nrow(Data.Frame) - nahead
#   n_windows = end_p - init_p 
#   fcstErr = data.frame(matrix(0, nrow = (n_windows+1), ncol = noMods))
#   fcstErr = xts(fcstErr, order.by = index(Data.Frame[(nrow(Data.Frame)-n_windows):nrow(Data.Frame), ]))
#   colnames(fcstErr) <- c("FAVAR", "FAVAR2", "AR", "RW", "MM", "VAR(1)", "VAR(3)")
#   fcst = fcstErr
#   observed = Data.Frame[(init_p+nahead):nrow(Data.Frame),1]
#   
#   for (i in 1:(n_windows)) { 
#     print(sprintf("Expanding window %d", j, "of %d", jmax, ": Iteration: %d", i))
#     ## Factor reestimation ##
#     Data_set = yt_z1[1:(i+init_p-1), ]
#     cov_yt <- t(Data_set) %*% Data_set / nrow(Data_set)
#     eig = eigen(cov_yt)
#     eig_val <- as.data.frame(eig[1]$values)
#     eig_vec <- as.data.frame(eig[2]$vectors)
#     Factor_loadings <- sqrt(ncol(Data_set)) * eig_vec
#     Factor_loadings <-as.data.frame(Factor_loadings)
#     Factor_components <- as.matrix(Data_set) %*% as.matrix(Factor_loadings/ncol(Data_set))
#     Factor_components <- as.data.frame(Factor_components)
#     Fhat <- Factor_components[,1:7]
#     Fhat <- xts(Fhat, order.by = index(Data.Frame[1:(i+init_p-1)]))
#     ## Data set ##
#     TrainingDat = Data.Frame[1:(i+init_p-1),1:2]
#     TrainingDat = merge.xts(TrainingDat, Fhat)
#     VARTrainingdat = VAR.Data[1:(i+init_p-1),]
#     FAVARtrainingDat = merge.xts(VARTrainingdat, Fhat)
#     ## FAVAR ##
#     TrainingVAR  = VAR(TrainingDat, type = "none", p = 1)
#     fcstFAVAR    = predict(TrainingVAR, n.ahead = nahead)
#     fcstFAVAR    = fcstFAVAR$fcst$Diff.Funds[nahead,1]
#     fcst[i,1]    = fcstFAVAR
#     fcstErr[i,1] = Data.Frame[(init_p+nahead+i-1),1] - fcstFAVAR
#     ## FAVAR Y = 3 series##
#     TrainingFAVAR2  = VAR(FAVARtrainingDat, type = "none", p = 1)
#     fcstFAVAR2    = predict(TrainingFAVAR2, n.ahead = nahead)
#     fcstFAVAR2    = fcstFAVAR2$fcst$Diff.Funds[nahead,1]
#     fcst[i,2]    = fcstFAVAR2
#     fcstErr[i,2] = Data.Frame[(init_p+nahead+i-1),1] - fcstFAVAR2
#     ## AR ##
#     TrainingAR  = arima(TrainingDat$Diff.Funds, order = c(1, 0, 0))
#     fcstAR = predict(TrainingAR, n.ahead = nahead)
#     fcstAR = fcstAR$pred[nahead]
#     fcst[i,3] <- fcstAR
#     fcstErr[i,3] = Data.Frame[(init_p+nahead+i-1),1] - fcstAR
#     ## RW ##
#     RW = arima(TrainingDat$Diff.Funds, order = c(0, 1, 0))
#     fcstRW = predict(RW, n.ahead = nahead)
#     fcstRW = fcstRW$pred[nahead]
#     fcst[i,4] <- fcstRW
#     fcstErr[i,4] = Data.Frame[(init_p+nahead+i-1),1] - fcstRW
#     ## Mean model ##
#     MM = arima(TrainingDat$Diff.Funds, order = c(0,0,0), include.mean = T)
#     fcstMM = predict(MM, n.ahead = nahead)
#     fcstMM = fcstMM$pred[nahead]
#     fcst[i,5] <- fcstMM
#     fcstErr[i,5] = Data.Frame[(init_p+nahead+i-1),1] - fcstMM
#     ## VAR(1) ##
#     TrainingVAR = VAR(VARTrainingdat, type = "none", p = 1)
#     fcstVAR = predict(TrainingVAR, n.ahead = nahead)
#     fcstVAR = fcstVAR$fcst$Diff.Funds[nahead, 1]
#     fcst[i,6] = fcstVAR
#     fcstErr[i,6] = Data.Frame[(init_p+nahead+i-1),1] - fcstVAR
#     ## VAR(3) ##
#     TrainingVAR2 = VAR(VARTrainingdat, type = "none", p = 3)
#     fcstVAR2 = predict(TrainingVAR2, n.ahead = nahead)
#     fcstVAR2 = fcstVAR2$fcst$Diff.Funds[nahead, 1]
#     fcst[i,7] = fcstVAR2
#     fcstErr[i,7] = Data.Frame[(init_p+nahead+i-1),1] - fcstVAR2
#     ## Save Err ##
#     for (k in 1:7){
#       MeanErr[j,k] = mean(fcstErr[,k]^2)
#     }
#   }
# }
# 
# saveRDS(MeanErr, "MeanErr.RDS")
MeanErr = readRDS("MeanErr.RDS")

MeanErr.plot = ggplot(data = MeanErr, aes(x=index(MeanErr))) +
  geom_line(aes(y = FAVAR1, col = "FAVAR1"), size = 1.0) +
  geom_line(aes(y = FAVAR2, col = "FAVAR2"), size = 1.0) +
  geom_line(aes(y = VAR, col = "VAR(1)"), size = 1.0) +
  geom_line(aes(y = VAR3, col = "VAR(3)"), size = 1.0) +
  geom_line(aes(y = AR, col = "AR(1)"), size = 1.0) +
  geom_line(aes(y = RW, col = "RW"), size = 1.0) +
  geom_line(aes(y = MM, col = "Mean"), size = 1.0) +
  labs(title = "Mean Square Forecast error: Robustness", 
       subtitle = "MSFE over time, 1-period ahead, expanding window",
       color="Series",
       caption = "Rasmus M. Jensen",
       y = expression(epsilon[t+1*"|"*t]~"="~y[t+1]~"-"~f[t+1*"|"*t])
  ) +
  scale_y_continuous() +
  scale_color_colorblind()+
  # scale_color_manual(values=c("#90353B", "#2D6D66", "#938DD2", "#1A476F", "#9C8847")) +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11),
        legend.position = c(0.85, 1.045),
        legend.direction = "horizontal",
        axis.title.x = element_blank(),
        legend.background = element_blank()
  ) +
  guides(colour=guide_legend(nrow=2, byrow=TRUE, direction = "horizontal"))
MeanErr.plot

# ggsave("MSFE_Robust.pdf", plot = MeanErr.plot, width = 14,
# height = 7, units = "in")
  rm(RW, SFE, TrainingAR, SFE_roll, MSE_exp, MSE_roll, Bias, MSE, MSFE, Sign, dm,
    CPI, IP, MM, TrainingFAVAR2, TrainingVAR, TrainingVAR2, TrainingDat, VARTrainingdat,
    CONSrate, cov_yt, Data_set, eig, eig_val, eig_vec, f.p.m, Factor_components, SE,
    Factor_loadings, Factor.plots, fcst.sign,z, nomods, nahead,n_windows,max_lag,
    max_fac,init_p,i,fcstVAR2, fcstVAR,fcstRW,fcstMM,fcstFAVAR,fcstFAVAR2,fcstAR,
    f.sign,f.MSFE,f.MSE,f.bias,end_p,yt_z1,VAR.Data,Fhat,fcstErr)

FAVAR2dat = merge.xts(VAR.Data, Data.Frame[,3:9])
FAVAR2 = VAR(FAVAR2dat, p = 1, type = "none")

IRF.FAVAR.PR = irf(FAVAR2, impulse = c("CPI", "IP", "Diff.Funds"), response = c("CPI", "IP", "Diff.Funds"), ortho = F)
IRF.FAVAR.PR

 ## Response: Policy Rate ##
IRF.Plots.PR1 = ggplot() +
  geom_ribbon(aes(x=1:11, ymin = IRF.FAVAR.PR$Lower$Diff.Funds[,1], 
                  ymax = IRF.FAVAR.PR$Upper$Diff.Funds[,1], col = "Confidence Interval 95%"),
              fill = "blue", alpha = 0.3, lty = 2) +
  geom_line(aes(x = 1:11, 
                y = IRF.FAVAR.PR$irf$Diff.Funds[,1], col = "IRF")) + 
  geom_hline(aes(yintercept = 0)) +
  labs(title = "Impulse Response",
       colour = "Series",
       subtitle = "Impulse: Industrial Production") +
  scale_y_continuous(expand = c(0, 0),
                     name = expression(Delta*"Policy Rate")) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(1, 11),
                     breaks = seq(0,10, 2),
                     name = "Period") +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_blank(),
        legend.position = "none"
  ) 
# IRF.Plots.PR1

IRF.Plots.PR2 = ggplot() +
  geom_ribbon(aes(x=1:11, ymin = IRF.FAVAR.PR$Lower$Diff.Funds[,2], 
                  ymax = IRF.FAVAR.PR$Upper$Diff.Funds[,2], col = "Confidence Interval 95%"),
              fill = "blue", alpha = 0.3, lty = 2) +
  geom_line(aes(x = 1:11, 
                y = IRF.FAVAR.PR$irf$Diff.Funds[,2], col = "IRF")) + 
  geom_hline(aes(yintercept = 0)) +
  labs(title = "Impulse Response",
       colour = "Series",
       subtitle = "Impulse: Consumer Price Index") +
  scale_y_continuous(expand = c(0, 0),
                     name = expression(Delta*"Policy Rate")) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(1, 11),
                     breaks = seq(0,10, 2),
                     name = "Period") +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_blank(),
        legend.position = "none"
  ) 
# IRF.Plots.PR2

IRF.Plots.PR3 = ggplot() +
  geom_ribbon(aes(x=1:11, ymin = IRF.FAVAR.PR$Lower$Diff.Funds[,3], 
                  ymax = IRF.FAVAR.PR$Upper$Diff.Funds[,3], col = "Confidence Interval 95%"),
              fill = "blue", alpha = 0.3, lty = 2) +
  geom_line(aes(x = 1:11, 
                y = IRF.FAVAR.PR$irf$Diff.Funds[,3], col = "IRF")) + 
  geom_hline(aes(yintercept = 0)) +
  labs(title = "Impulse Response",
       colour = "Series",
       subtitle = expression("Impulse:"~Delta*"Policy Rate")) +
  scale_y_continuous(expand = c(0, 0),
                     name = expression(Delta*"Policy Rate")) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(1, 11),
                     breaks = seq(0,10, 2),
                     name = "Period") +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_blank(),
        legend.position = "none"
  ) 
 # IRF.Plots.PR3

## Response: Industrial Production ##
IRF.Plots.IP1 = ggplot() +
  geom_ribbon(aes(x=1:11, ymin = IRF.FAVAR.PR$Lower$IP[,2], 
                  ymax = IRF.FAVAR.PR$Upper$IP[,2], col = "Confidence Interval 95%"),
              fill = "blue", alpha = 0.3, lty = 2) +
  geom_line(aes(x = 1:11, 
                y = IRF.FAVAR.PR$irf$IP[,2], col = "IRF")) + 
  geom_hline(aes(yintercept = 0)) +
  labs(title = "Impulse Response",
       caption = " ",
       colour = "Series",
       subtitle = "Impulse: Consumer Price Index") +
  scale_y_continuous(expand = c(0, 0),
                     name = "Industrial Production") +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(1, 11),
                     breaks = seq(0,10, 2),
                     name = "Period") +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_blank(),
        legend.position = "none"
  ) 
# IRF.Plots.IP1

IRF.Plots.IP2 = ggplot() +
  geom_ribbon(aes(x=1:11, ymin = IRF.FAVAR.PR$Lower$IP[,3], 
                  ymax = IRF.FAVAR.PR$Upper$IP[,3], col = "Confidence Interval 95%"),
              fill = "blue", alpha = 0.3, lty = 2) +
  geom_line(aes(x = 1:11, 
                y = IRF.FAVAR.PR$irf$IP[,3], col = "IRF")) + 
  geom_hline(aes(yintercept = 0)) +
  labs(title = "Impulse Response",
       colour = "Series",
       caption = " ",
       subtitle = expression("Impulse:"~Delta*"Policy Rate")) +
  scale_y_continuous(expand = c(0, 0),
                     name = "Industrial Production") +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(1, 11),
                     breaks = seq(0,10, 2),
                     name = "Period") +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_blank(),
        legend.position = "none"
  ) 
# IRF.Plots.IP2

IRF.Plots.IP3 = ggplot() +
  geom_ribbon(aes(x=1:11, ymin = IRF.FAVAR.PR$Lower$IP[,1], 
                  ymax = IRF.FAVAR.PR$Upper$IP[,1], col = "Confidence Interval 95%"),
              fill = "blue", alpha = 0.3, lty = 2) +
  geom_line(aes(x = 1:11, 
                y = IRF.FAVAR.PR$irf$IP[,1], col = "IRF")) + 
  geom_hline(aes(yintercept = 0)) +
  labs(title = "Impulse Response",
       colour = "Series",
       caption = "Rasmus M. Jensen",
       subtitle = "Impulse: Industrial Production") +
  scale_y_continuous(expand = c(0, 0),
                     name = "Industrial Production") +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(1, 11),
                     breaks = seq(0,10, 2),
                     name = "Period") +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_blank(),
        legend.position = "none"
  ) 
# IRF.Plots.IP3

## Response: Consumer Price Index ##
IRF.Plots.CPI1 = ggplot() +
  geom_ribbon(aes(x=1:11, ymin = IRF.FAVAR.PR$Lower$CPI[,2], 
                  ymax = IRF.FAVAR.PR$Upper$CPI[,2], col = "Confidence Interval 95%"),
              fill = "blue", alpha = 0.3, lty = 2) +
  geom_line(aes(x = 1:11, 
                y = IRF.FAVAR.PR$irf$CPI[,2], col = "IRF")) + 
  geom_hline(aes(yintercept = 0)) +
  labs(title = "Impulse Response",
       colour = "Series",
       subtitle = "Impulse: Consumer Price Index") +
  scale_y_continuous(expand = c(0, 0),
                     name = "Consumer Price Index") +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(1, 11),
                     breaks = seq(0,10, 2),
                     name = "Period") +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_blank(),
        legend.position = "none"
  ) 
# IRF.Plots.CPI1

IRF.Plots.CPI2 = ggplot() +
  geom_ribbon(aes(x=1:11, ymin = IRF.FAVAR.PR$Lower$CPI[,3], 
                  ymax = IRF.FAVAR.PR$Upper$CPI[,3], col = "Confidence Interval 95%"),
              fill = "blue", alpha = 0.3, lty = 2) +
  geom_line(aes(x = 1:11, 
                y = IRF.FAVAR.PR$irf$CPI[,3], col = "IRF")) + 
  geom_hline(aes(yintercept = 0)) +
  labs(title = "Impulse Response",
       colour = "Series",
       subtitle = expression("Impulse:"~Delta*"Policy Rate")) +
  scale_y_continuous(expand = c(0, 0),
                     name = "Consumer Price Index") +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(1, 11),
                     breaks = seq(0,10, 2),
                     name = "Period") +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_blank(),
        legend.position = "none"
  ) 
# IRF.Plots.CPI2

IRF.Plots.CPI3 = ggplot() +
  geom_ribbon(aes(x=1:11, ymin = IRF.FAVAR.PR$Lower$CPI[,1], 
                  ymax = IRF.FAVAR.PR$Upper$CPI[,1], col = "Confidence Interval 95%"),
              fill = "blue", alpha = 0.3, lty = 2) +
  geom_line(aes(x = 1:11, 
                y = IRF.FAVAR.PR$irf$CPI[,1], col = "IRF")) + 
  geom_hline(aes(yintercept = 0)) +
  labs(title = "Impulse Response",
       colour = "Series",
       subtitle = "Impulse: Industrial Production") +
  scale_y_continuous(expand = c(0, 0),
                     name = "Consumer Price Index") +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(1, 11),
                     breaks = seq(0,10, 2),
                     name = "Period") +
  scale_color_economist() + 
  theme_bw() +
  theme(text = element_text(family = "Times"),
        panel.grid = element_line(colour = "#A9A9A9"),
        panel.grid.minor = element_line(colour = "#A6A6A6"),
        plot.caption = element_text(colour = "#A6A6A6"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_blank(),
        legend.position = "none"
  ) 
# IRF.Plots.CPI3

IRF.Plots = grid.arrange(IRF.Plots.PR1, IRF.Plots.PR2, IRF.Plots.PR3, IRF.Plots.CPI1, IRF.Plots.CPI2, IRF.Plots.CPI3,
             IRF.Plots.IP1, IRF.Plots.IP2, IRF.Plots.IP3, ncol = 3, 
             layout_matrix = rbind(c(3, 2, 1),
                                   c(5, 4, 6),
                                   c(8, 7, 9)))
# ggsave("IRF_plots.pdf", plot = IRF.Plots, width = 28, height = 14)



proc.time() - ptm



