
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
            plot.margin       = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))


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
















