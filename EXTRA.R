
library(gridExtra)
library(tidyverse)

th <- theme(plot.title        = element_text(size = 20),
            plot.background   = element_rect(fill = "white", color = NA),
            panel.background  = element_rect(fill = NA,       color = NA), 
            legend.background = element_rect(fill = NA,       color = NA),
            legend.key        = element_rect(fill = NA,       color = NA),
            strip.background  = element_rect(fill = NA,       color = NA),
            panel.border      = element_rect(fill = NA,       color = "black", size = 0.3),
            panel.grid        = element_line(color = NA),
            title             = element_text(color = "black"),
            plot.subtitle     = element_text(color = "grey40"),
            plot.caption      = element_text(color = "grey70"),
            strip.text        = element_text(face  = "bold"),
            axis.text         = element_text(color = "black"),
            axis.ticks        = element_line(color = "black"),
            plot.margin       = unit(c(0.2, 0.1, 0.2, 0.1), "cm"))



# PCA ---------------------------------------------------------------------

set.seed(14)

x <- rnorm(70) + 0.2*-10:60
y <- rnorm(70) + 0.2*-10:60


PCA <- prcomp(cbind(x,y), rank. = 1)
summary(PCA)


p1 <- ggplot(tibble(x,y), aes(x,y, fill=c(1:71))) + 
  geom_point(size=3, alpha=0.7, color="black", shape=21) +     
  scale_x_continuous(limits=c(-6,16)) + scale_y_continuous(limits=c(-5,15)) +
  labs(title="Data + eigenvektor", tag = "A)") + 
  scale_fill_distiller(palette = "Greys") + 
  geom_abline(aes(intercept=0, slope = PCA$rotation[2,1]/ PCA$rotation[1,1]), linetype = "dashed") + 
  th + theme(legend.position = 'none')

p2 <- ggplot(tibble(x = PCA$x[,1], y = rep(0,71)), aes(x,y, fill=c(1:71))) + 
  geom_point(size=3, alpha=0.7, color="black", shape=21) + 
  scale_x_continuous(limits=c(-11,11)) + scale_y_continuous(limits=c(-10,10)) +
  labs(title="Principale komponent", tag = "B)") + 
  scale_fill_distiller(palette = "Greys") + 
  th + theme(legend.position = 'none')

p3 <- ggplot(as.tibble((PCA$x[,1] %*% t(PCA$rotation[,1]) + mean(x))), aes(x,y, fill=c(1:71))) + 
  scale_x_continuous(limits=c(-6,16)) + scale_y_continuous(limits=c(-5,15)) +
  geom_point(size=3, alpha=0.7, color="black", shape=21) + 
  labs(title="Rekonstruktion", tag = "C)") + 
  scale_fill_distiller(palette = "Greys") + 
  th + theme(legend.position = 'none')


p1 <- grid.arrange(p1, p2, p3, nrow = 1)

ggsave(plot = p1, filename = "GENERATE/EXTRA1.pdf", width = 30, height = 8, units = "cm", dpi = 320)



# FFR ---------------------------------------------------------------------
rece <- read.table(textConnection(
    "Peak, Trough
    1857-06-01, 1858-12-01
    1860-10-01, 1861-06-01
    1865-04-01, 1867-12-01
    1869-06-01, 1870-12-01
    1873-10-01, 1879-03-01
    1882-03-01, 1885-05-01
    1887-03-01, 1888-04-01
    1890-07-01, 1891-05-01
    1893-01-01, 1894-06-01
    1895-12-01, 1897-06-01
    1899-06-01, 1900-12-01
    1902-09-01, 1904-08-01
    1907-05-01, 1908-06-01
    1910-01-01, 1912-01-01
    1913-01-01, 1914-12-01
    1918-08-01, 1919-03-01
    1920-01-01, 1921-07-01
    1923-05-01, 1924-07-01
    1926-10-01, 1927-11-01
    1929-08-01, 1933-03-01
    1937-05-01, 1938-06-01
    1945-02-01, 1945-10-01
    1948-11-01, 1949-10-01
    1953-07-01, 1954-05-01
    1957-08-01, 1958-04-01
    1960-04-01, 1961-02-01
    1969-12-01, 1970-11-01
    1973-11-01, 1975-03-01
    1980-01-01, 1980-07-01
    1981-07-01, 1982-11-01
    1990-07-01, 1991-03-01
    2001-03-01, 2001-11-01
    2007-12-01, 2009-06-01"), sep=',',
    colClasses=c('Date', 'Date'), header=TRUE)

p2 <- Quandl("FRED/FEDFUNDS", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date) %>% 
  ggplot(aes(Date, Value)) +   
  geom_rect(data = rece, aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf), fill = "grey", alpha = 0.2, inherit.aes = F) +
  geom_line() + 
  scale_y_continuous(breaks=c(0,4,8,12,16,20), limits=c(0,21)) + 
  scale_x_date(date_breaks = "3 years", date_labels = "%Y", limits = c(as.Date("1959-01-01"), as.Date("2019-01-01"))) + 
  coord_cartesian(xlim = c(as.Date("1961-01-01"), as.Date("2017-01-01"))) +
  th + theme(axis.title = element_blank())
  
ggsave(plot = p2, filename = "GENERATE/EXTRA2.pdf", width = 30, height = 8, units = "cm", dpi = 320)














