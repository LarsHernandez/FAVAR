
library(gridExtra)

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


set.seed(4)

x <- rnorm(70) + 0.2*-10:60
y <- rnorm(70) + 0.2*-10:60


PCA <- prcomp(cbind(x,y), rank. = 1)
summary(PCA)


p1 <- ggplot(tibble(x,y), aes(x,y)) + 
  geom_point(shape=1, size=2) +     
  scale_x_continuous(limits=c(-5,15)) + scale_y_continuous(limits=c(-5,15)) +
  labs(title="Data + eigenvektor", subtitle = "Første faktor forklarer 97.65% af variationen\n", tag = "A)") + 
  geom_abline(aes(intercept=0, slope = PCA$rotation[2,1]/ PCA$rotation[1,1]), linetype = "dashed") + th

p2 <- ggplot(tibble(x = PCA$x[,1], y = rep(0,71)), aes(x,y)) + 
  geom_point(shape=1, size=2) + 
  scale_x_continuous(limits=c(-10,10)) + scale_y_continuous(limits=c(-10,10)) +
  labs(title="Principale komponent", subtitle = "Faktoren er projektioner af data op mod\neigenvektoren", tag = "B)") + th

p3 <- ggplot(as.tibble((PCA$x[,1] %*% t(PCA$rotation[,1]) + mean(x))), aes(x,y)) + 
  scale_x_continuous(limits=c(-5,15)) + scale_y_continuous(limits=c(-5,15)) +
  geom_point(shape=1, size=2) + 
  labs(title="Rekonstruktion", subtitle="Det originale data kan rekonstrukturers med\nfaktoren, eigenvektoren og gennemsnittet", tag = "C)") + 
  th


grid.arrange(p1,p2,p3,nrow=1)





