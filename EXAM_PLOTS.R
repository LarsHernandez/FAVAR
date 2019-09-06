

obj <- variables %>% dplyr::select(INFL,PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,FFR) %>% VAR(p = 13) %>% irf(n.ahead = 48, impulse="FFR")

#obj <- irf(m, impulse = "FFR", n.ahead = 48, ci = 0.66)
eigen <- FAVAR_PCA$rotation

val <- obj[1]$irf$FFR[,-c(1,10)] %*% t(eigen) %>% 
  cbind(.,obj[1]$irf$FFR[,c(1,10)]) %>% 
  as.data.frame %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

upp <- obj[2]$Lower$FFR[,-c(1,10)] %*% t(eigen) %>% 
  cbind(.,obj[2]$Lower$FFR[,c(1,10)]) %>% 
  as.data.frame %>% 
  dplyr::select(-starts_with("X")) %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

low <- obj[3]$Upper$FFR[,-c(1,10)] %*% t(eigen) %>% 
  cbind(.,obj[3]$Upper$FFR[,c(1,10)]) %>% 
  as.data.frame %>% 
  dplyr::select(-starts_with("X")) %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

AA <- val %>% 
  full_join(.,upp, by=c("N", "variable")) %>% 
  full_join(., low, by=c("N", "variable")) %>%
  filter(variable %in% TOP) %>% 
  gather(type, value, -variable,-N) %>% 
  group_by(type, variable) %>% 
  mutate(value2 = case_when(variable %in% c("M2SL","M2REAL")~ cumsum(value),
                            variable %in% c("IPMANSICS","CUSR0000SAC","T10YFFM","GS5","UEMP15OV","HOUST")~value)) %>% 
  ungroup() %>% 
  mutate(variable = factor(variable, levels=TOP))


  ggplot(aes(N, value2, linetype=type)) + 
  geom_hline(aes(yintercept = 0), color="grey") +
  geom_line(size=0.3) +
  geom_blank(aes(value2=-value2)) + 
  scale_x_continuous("Lags (months)", limits = c(0,48), breaks = seq(0, 48, 6)) +
  scale_linetype_manual(values = c("dotted", "solid", "dotted")) + 
  facet_wrap(~variable, scales = "free", nrow=2) + 
  th  + theme(axis.title.y = element_blank(), legend.position = "none")

obj <- variables %>% dplyr::select(INFL,PC1,PC3,PC4,FFR) %>% VAR(p = 13) %>% irf(n.ahead = 48)

#obj <- irf(m, impulse = "FFR", n.ahead = 48, ci = 0.66)
eigen <- FAVAR_PCA$rotation[,c(1,3,4)]

val <- obj[1]$irf$FFR[,-c(1,5)] %*% t(eigen) %>% 
  cbind(.,obj[1]$irf$FFR[,c(1,5)]) %>% 
  as.data.frame %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

upp <- obj[2]$Lower$FFR[,-c(1,5)] %*% t(eigen) %>% 
  cbind(.,obj[2]$Lower$FFR[,c(1,5)]) %>% 
  as.data.frame %>% 
  dplyr::select(-starts_with("X")) %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

low <- obj[3]$Upper$FFR[,-c(1,5)] %*% t(eigen) %>% 
  cbind(.,obj[3]$Upper$FFR[,c(1,5)]) %>% 
  as.data.frame %>% 
  dplyr::select(-starts_with("X")) %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

BB <- val %>% 
  full_join(.,upp, by=c("N", "variable")) %>% 
  full_join(., low, by=c("N", "variable")) %>%
  filter(variable %in% TOP) %>% 
  gather(type, value, -variable,-N) %>% 
  group_by(type, variable) %>% 
  mutate(value2 = case_when(variable %in% c("M2SL","M2REAL")~ cumsum(value),
                            variable %in% c("IPMANSICS","CUSR0000SAC","T10YFFM","GS5","UEMP15OV","HOUST")~value)) %>% 
  ungroup() %>% 
  mutate(variable = factor(variable, levels=TOP))

  ggplot(aes(N, value2, linetype=type)) + 
  geom_hline(aes(yintercept = 0), color="grey") +
  geom_line(size=0.3) +
  geom_blank(aes(value2=-value2)) + 
  scale_x_continuous("Lags (months)", limits = c(0,48), breaks = seq(0, 48, 6)) +
  scale_linetype_manual(values = c("dotted", "solid", "dotted")) + 
  facet_wrap(~variable, scales = "free", nrow=2) + 
  th  + theme(axis.title.y = element_blank(), legend.position = "none")
  

AA$color <- "VAR(Y=INFL,FFR, K=8)"
BB$color <- "VAR(Y=INFL,FFR, K=3)"

E1 <- rbind(AA,BB) %>% 
ggplot(aes(N, value2, linetype=type, color=color)) + 
  geom_hline(aes(yintercept = 0), color="grey") +
  geom_line(size=0.5) +
  geom_blank(aes(value2=-value2)) + 
  scale_x_continuous("Lags (months)", limits = c(0,48), breaks = seq(0, 48, 6)) +
  scale_linetype_manual(values = c("dotted", "solid", "dotted"), guide=FALSE) + 
  scale_color_brewer("MODEL", palette="Paired") +
  facet_wrap(~variable, scales = "free", nrow=2) + 
  th  + theme(axis.title.y = element_blank(),legend.position="top") +
  labs(title=expression(paste(bold("Figur  "), "Rekonstruktion af FAVAR variablers IRF")), 
       subtitle="Sammenligning mellem K=3 og K=8 model", 
       caption="Source: FRED + Transformationer", x = "", y = "")

  
ggsave(plot = E1, filename = "GENERATE/EXAM1.pdf", width = 25, height = 15, units = "cm", dpi = 320)
  
  
  
  

# FAST PC3 PC4 

# SLOW PC1 PC2 PC5

# fak FFR
# FFR fak
# fast FFR slow
# slow FFR fast


RE   <- variables %>% dplyr::select(INFL, PC1, PC3, PC4, FFR) %>% VAR(p = 13) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 48)
RE1  <- variables %>% dplyr::select(INFL, FFR, PC1, PC3, PC4) %>% VAR(p = 13) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 48)
RE2  <- variables %>% dplyr::select(INFL, PC1, FFR, PC3, PC4) %>% VAR(p = 13) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 48)
RE3  <- variables %>% dplyr::select(INFL, PC3, PC4, FFR, PC1) %>% VAR(p = 13) %>% irf(impulse = "FFR", response = "INFL", n.ahead = 48)

ERE  <-  data.frame(value=as.numeric(RE$irf$FFR),   lower=as.numeric(RE$Lower$FFR),   upper=as.numeric(RE$Upper$FFR),   N=c(0:48), order="INFL, FACTORS, FFR")
ERE1 <-  data.frame(value=as.numeric(RE1$irf$FFR),  lower=as.numeric(RE1$Lower$FFR),  upper=as.numeric(RE1$Upper$FFR),  N=c(0:48), order="INFL, FFR, FACTORS")
ERE2 <-  data.frame(value=as.numeric(RE2$irf$FFR),  lower=as.numeric(RE2$Lower$FFR),  upper=as.numeric(RE2$Upper$FFR),  N=c(0:48), order="INFL, SLOW, FFR, FAST")
ERE3 <-  data.frame(value=as.numeric(RE3$irf$FFR),  lower=as.numeric(RE3$Lower$FFR),  upper=as.numeric(RE3$Upper$FFR),  N=c(0:48), order="INFL, FAST, FFR, SLOW")


E2 <- rbind(ERE,
      ERE1,
      ERE2,
      ERE3) %>% 
  gather(variable, value, -N, -order) %>% 
  ggplot(aes(N, value, linetype=variable)) + 
  scale_linetype_manual(values=c("dotted", "dotted","solid"), guide=FALSE) +
  facet_wrap(~order, nrow=1)+
  scale_x_continuous("Lags (months)", limits = c(0,48), breaks = seq(0, 48, 6)) +
  geom_line(size=0.5, color="#1f78b4") +
  labs(title=expression(paste(bold("Figur  "), "Alternative ordninger FAVAR")), 
       subtitle="FAVAR K=3: FAST = PC3 og PC4, SLOW = PC1", 
       caption="Source: FRED + Transformationer", x = "", y = "") +
  th

ggsave(plot = E2, filename = "GENERATE/EXAM2.pdf", width = 25, height = 10, units = "cm", dpi = 320)











obj <- variables %>% dplyr::select(INFL,PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,FFR) %>% VAR(p = 13) %>% irf(n.ahead = 48, impulse="PC1")

#obj <- irf(m, impulse = "FFR", n.ahead = 48, ci = 0.66)
eigen <- FAVAR_PCA$rotation

val <- obj[1]$irf$PC1[,-c(1,10)] %*% t(eigen) %>% 
  cbind(.,obj[1]$irf$PC1[,c(1,10)]) %>% 
  as.data.frame %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

upp <- obj[2]$Lower$PC1[,-c(1,10)] %*% t(eigen) %>% 
  cbind(.,obj[2]$Lower$PC1[,c(1,10)]) %>% 
  as.data.frame %>% 
  dplyr::select(-starts_with("X")) %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

low <- obj[3]$Upper$PC1[,-c(1,10)] %*% t(eigen) %>% 
  cbind(.,obj[3]$Upper$PC1[,c(1,10)]) %>% 
  as.data.frame %>% 
  dplyr::select(-starts_with("X")) %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

AA <- val %>% 
  full_join(.,upp, by=c("N", "variable")) %>% 
  full_join(., low, by=c("N", "variable")) %>%
  filter(variable %in% TOP) %>% 
  gather(type, value, -variable,-N) %>% 
  group_by(type, variable) %>% 
  mutate(value2 = case_when(variable %in% c("M2SL","M2REAL")~ cumsum(value),
                            variable %in% c("IPMANSICS","CUSR0000SAC","T10YFFM","GS5","UEMP15OV","HOUST")~value)) %>% 
  ungroup() %>% 
  mutate(variable = factor(variable, levels=TOP))


AA %>% ggplot(aes(N, value2, linetype=type)) + 
  geom_hline(aes(yintercept = 0), color="grey") +
  geom_line(size=0.5) +
  geom_blank(aes(value2=-value2)) + 
  scale_x_continuous("Lags (months)", limits = c(0,48), breaks = seq(0, 48, 6)) +
  scale_linetype_manual(values = c("dotted", "solid", "dotted"), guide=FALSE) + 
  scale_color_brewer("MODEL", palette="Paired") +
  facet_wrap(~variable, scales = "free", nrow=2) + 
  th  + theme(axis.title.y = element_blank(),legend.position="top") +
  labs(title=expression(paste(bold("Figur  "), "Rekonstruktion af FAVAR variablers IRF")), 
       subtitle="Sammenligning mellem K=3 og K=8 model", 
       caption="Source: FRED + Transformationer", x = "", y = "")








# safkldanf ---------------------------------------------------------------
FAVAR_PCA <- prcomp(FAVAR_T[,-1], rank. = 8)

v1 <- tibble(INFL =INFL$Value[-c(1,2)])
v2 <- tibble(FFR = FFR$Value[-c(1,2)])
variables <- cbind(FAVAR_T[c("Date")], v1, FAVAR_PCA$x,v2)
m <- variables %>% dplyr::select(INFL, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, FFR) %>% VAR(p = 13)

TOP <- c("IPMANSICS","CUSR0000SAC","T10YFFM","GS5","UEMP15OV","HOUST","M2REAL","M2SL")

obj <- irf(m, impulse = "FFR", n.ahead = 48, ci = 0.66)
eigen <- FAVAR_PCA$rotation

val <- obj[1]$irf$FFR[,-c(1,10)] %*% t(eigen) %>% 
  cbind(.,obj[1]$irf$FFR[,c(1,10)]) %>% 
  as.data.frame %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

upp <- obj[2]$Lower$FFR[,-c(1,10)] %*% t(eigen) %>% 
  cbind(.,obj[2]$Lower$FFR[,c(1,10)]) %>% 
  as.data.frame %>% 
  dplyr::select(-starts_with("X")) %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

low <- obj[3]$Upper$FFR[,-c(1,10)] %*% t(eigen) %>% 
  cbind(.,obj[3]$Upper$FFR[,c(1,10)]) %>% 
  as.data.frame %>% 
  dplyr::select(-starts_with("X")) %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

base <- val %>% 
  full_join(.,upp, by=c("N", "variable")) %>% 
  full_join(., low, by=c("N", "variable")) %>%
  filter(variable %in% TOP) %>% 
  gather(type, value, -variable,-N) %>% 
  group_by(type, variable) %>% 
  mutate(value2 = case_when(variable %in% c("M2SL","M2REAL")~ cumsum(cumsum(value)),
                            variable %in% c("IPMANSICS","CUSR0000SAC","T10YFFM","GS5","UEMP15OV","HOUST") ~ cumsum(value))) %>% 
  ungroup() %>% 
  mutate(variable = factor(variable, levels=TOP),
         spec = "BASE")



m <- variables %>% dplyr::select(INFL, PC3, PC4, PC8, PC7, PC1, PC5, PC6, PC2, FFR) %>% VAR(p = 13)

TOP <- c("IPMANSICS","CUSR0000SAC","T10YFFM","GS5","UEMP15OV","HOUST","M2REAL","M2SL")

obj <- irf(m, impulse = "FFR", n.ahead = 48, ci = 0.66)
eigen <- FAVAR_PCA$rotation

val <- obj[1]$irf$FFR[,-c(1,10)] %*% t(eigen) %>% 
  cbind(.,obj[1]$irf$FFR[,c(1,10)]) %>% 
  as.data.frame %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

upp <- obj[2]$Lower$FFR[,-c(1,10)] %*% t(eigen) %>% 
  cbind(.,obj[2]$Lower$FFR[,c(1,10)]) %>% 
  as.data.frame %>% 
  dplyr::select(-starts_with("X")) %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

low <- obj[3]$Upper$FFR[,-c(1,10)] %*% t(eigen) %>% 
  cbind(.,obj[3]$Upper$FFR[,c(1,10)]) %>% 
  as.data.frame %>% 
  dplyr::select(-starts_with("X")) %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

new <- val %>% 
  full_join(.,upp, by=c("N", "variable")) %>% 
  full_join(., low, by=c("N", "variable")) %>%
  filter(variable %in% TOP) %>% 
  gather(type, value, -variable,-N) %>% 
  group_by(type, variable) %>% 
  mutate(value2 = case_when(variable %in% c("M2SL","M2REAL")~ cumsum(cumsum(value)),
                            variable %in% c("IPMANSICS","CUSR0000SAC","T10YFFM","GS5","UEMP15OV","HOUST") ~ cumsum(value))) %>% 
  ungroup() %>% 
  mutate(variable = factor(variable, levels=TOP),
         spec = "ALT1")

m <- variables %>% dplyr::select(INFL, PC4, PC3, PC1, PC5, PC6, PC2, PC8, PC7, FFR) %>% VAR(p = 13)

TOP <- c("IPMANSICS","CUSR0000SAC","T10YFFM","GS5","UEMP15OV","HOUST","M2REAL","M2SL")

obj <- irf(m, impulse = "FFR", n.ahead = 48, ci = 0.66)
eigen <- FAVAR_PCA$rotation

val <- obj[1]$irf$FFR[,-c(1,10)] %*% t(eigen) %>% 
  cbind(.,obj[1]$irf$FFR[,c(1,10)]) %>% 
  as.data.frame %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

upp <- obj[2]$Lower$FFR[,-c(1,10)] %*% t(eigen) %>% 
  cbind(.,obj[2]$Lower$FFR[,c(1,10)]) %>% 
  as.data.frame %>% 
  dplyr::select(-starts_with("X")) %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

low <- obj[3]$Upper$FFR[,-c(1,10)] %*% t(eigen) %>% 
  cbind(.,obj[3]$Upper$FFR[,c(1,10)]) %>% 
  as.data.frame %>% 
  dplyr::select(-starts_with("X")) %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

new2 <- val %>% 
  full_join(.,upp, by=c("N", "variable")) %>% 
  full_join(., low, by=c("N", "variable")) %>%
  filter(variable %in% TOP) %>% 
  gather(type, value, -variable,-N) %>% 
  group_by(type, variable) %>% 
  mutate(value2 = case_when(variable %in% c("M2SL","M2REAL")~ cumsum(cumsum(value)),
                            variable %in% c("IPMANSICS","CUSR0000SAC","T10YFFM","GS5","UEMP15OV","HOUST") ~ cumsum(value))) %>% 
  ungroup() %>% 
  mutate(variable = factor(variable, levels=TOP),
         spec = "ALT2")


E3 <- rbind(base, new, new2) %>% 
  ggplot(aes(N, value2, linetype=type, color=spec)) + 
  geom_hline(aes(yintercept = 0), color="grey") +
  geom_line(size=0.3) +
  geom_blank(aes(value2=-value2)) + 
  scale_x_continuous("Lags (months)", limits = c(0,48), breaks = seq(0, 48, 6)) +
  scale_linetype_manual(values = c("dotted", "solid", "dotted"), guide=FALSE) + 
  scale_color_brewer("MODEL", palette="Paired") +
  facet_wrap(~variable, scales = "free", nrow=2) + 
  th  + theme(axis.title.y = element_blank(),legend.position="top") +
  labs(title=expression(paste(bold("Figur  "), "Alternative ordninger - akkumulerede")), 
       subtitle="BASE: Y = (INFL, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, FFR)\nALT1 : Y = (INFL, PC3, PC4, PC8, PC7, PC1, PC5, PC6, PC2, FFR)\nALT2 : Y = (INFL, PC4, PC3, PC1, PC5, PC6, PC2, PC8, PC7, FFR)", 
       caption="Source: FRED + Transformationer", x = "", y = "")

ggsave(plot = E3, filename = "GENERATE/EXAM3.pdf", width = 25, height = 15, units = "cm", dpi = 320)



# New length --------------------------------------------------------------

FAVAR_I <- FAVAR %>% filter(Date>"1972-01-01")

FAVAR_S <- FAVAR_I[sapply(FAVAR_I, function(x) !any(is.na(x)))]

# Transform according to auto.arima
degree <- function(x) if(auto.arima(x)$arma[6] == 1) c(NA, diff(x)) else if(auto.arima(x)$arma[6] == 2) c(NA,NA, diff(diff(x))) else x

FAVAR_T <- FAVAR_S %>% 
  mutate_at(vars(-Date), funs(degree)) %>% 
  mutate_at(vars(-Date), funs(scale))

# Remove NA's due to differencing 
FAVAR_T <- FAVAR_T[-c(1,2),]


FAVAR_PCA <- prcomp(FAVAR_T[,-1], rank. = 8)


FFR <- Quandl("FRED/FEDFUNDS", api_key = key) %>% 
  filter(Date > "1972-01-01", Date < "2019-01-01") %>% 
  arrange(Date) %>% 
  mutate(Value = c(NA, diff(Value)))

INFL <- Quandl("FRED/CPIAUCSL", api_key = key) %>% 
  filter(Date > "1972-01-01", Date < "2019-01-01") %>% 
  arrange(Date) %>% 
  mutate(Value = c(NA, diff(log(Value))*100))

PROD <- Quandl("FRED/INDPRO", api_key = key) %>% 
  filter(Date > "1972-01-01", Date < "2019-01-01") %>% 
  arrange(Date) %>% 
  mutate(Value = c(NA, diff(log(Value))*100))


v1 <- tibble(INFL =INFL$Value[-c(1,2)])
v2 <- tibble(FFR = FFR$Value[-c(1,2)])

variables <- cbind(FAVAR_T[c("Date")], v1, FAVAR_PCA$x,v2)

#VARselect(variables[,-1])

m <- VAR(variables[,-c(1)], p = 13)

TOP <- c("EXSZUS","EXJPUS","EXUSUK","EXCAUS")

obj <- irf(m, impulse = "FFR", n.ahead = 48, ci = 0.66)
eigen <- FAVAR_PCA$rotation

val <- obj[1]$irf$FFR[,-c(1,10)] %*% t(eigen) %>% 
  cbind(.,obj[1]$irf$FFR[,c(1,10)]) %>% 
  as.data.frame %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

upp <- obj[2]$Lower$FFR[,-c(1,10)] %*% t(eigen) %>% 
  cbind(.,obj[2]$Lower$FFR[,c(1,10)]) %>% 
  as.data.frame %>% 
  dplyr::select(-starts_with("X")) %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

low <- obj[3]$Upper$FFR[,-c(1,10)] %*% t(eigen) %>% 
  cbind(.,obj[3]$Upper$FFR[,c(1,10)]) %>% 
  as.data.frame %>% 
  dplyr::select(-starts_with("X")) %>% 
  mutate(N = 1:49) %>% 
  gather(variable, value, -N)

E4 <- val %>% 
  full_join(.,upp, by=c("N", "variable")) %>% 
  full_join(., low, by=c("N", "variable")) %>%
  filter(variable %in% TOP) %>% 
  gather(type, value, -variable,-N) %>% 
  group_by(type, variable) %>% 
  mutate(value2 = case_when(variable %in% c("EXSZUS","EXJPUS","EXUSUK","EXCAUS")~ cumsum(value),
                            variable %in% c("IPMANSICS","CUSR0000SAC","T10YFFM","GS5","UEMP15OV","HOUST")~value)) %>% 
  ungroup() %>% 
  mutate(variable = factor(variable, levels=TOP)) %>% 
  ggplot(aes(N, value2, linetype=type)) + 
  geom_hline(aes(yintercept = 0), color="grey") +
  geom_line(size=0.5, color="#1f78b4") +
  geom_blank(aes(value2=-value2)) + 
  scale_x_continuous("Lags (months)", limits = c(0,48), breaks = seq(0, 48, 6)) +
  scale_linetype_manual(values = c("dotted", "solid", "dotted"), guide=F) + 
  facet_wrap(~variable, scales = "free", nrow=1) + 
  th  + theme(axis.title.y = element_blank(),legend.position="top") +
  labs(title=expression(paste(bold("Figur  "), "FAVAR 1972 - 2019 - Valutakurser akkumulerede effekt")), 
       caption="Source: FRED + Transformationer", x = "", y = "")

ggsave(plot = E4, filename = "GENERATE/EXAM4.pdf", width = 25, height = 10, units = "cm", dpi = 320)


# NEXT --------------------------------------------------------------------


irf2 <- irf(V2, impulse = "FFR", response = "DOW", ortho = T, cumulative = F, n.ahead = 48, ci = 0.66)


d2 <- rbind(irf2$irf$FFR   %>% as_tibble %>% mutate(type = "irf", N = 1:49),
            irf2$Upper$FFR %>% as_tibble %>% mutate(type = "upper", N = 1:49),
            irf2$Lower$FFR %>% as_tibble %>% mutate(type = "lower", N = 1:49)) %>% as_tibble %>% mutate(model = "Dow Jones")


E5 <- d2 %>% 
  group_by(type) %>% 
  mutate(value = cumsum(DOW)) %>% 
  ggplot(aes(N, value, linetype=type)) + 
  geom_hline(aes(yintercept = 0), color="grey") +
  geom_line(size=0.5, color="#1f78b4") +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) + 
  scale_x_continuous("Lags (months)", limits = c(0,48), breaks = seq(0, 48, 8)) +
  th + theme(axis.title.y = element_blank(), legend.position = "none") + 
  labs(title=expression(paste(bold("Figur  "), "VAR(13) Response DOW - accumulated")), 
       caption="Source: FRED + Transformationer", x = "", y = "")

ggsave(plot = E5, filename = "GENERATE/EXAM5.pdf", width = 16, height = 12, units = "cm", dpi = 320)

E6 <- d2 %>% 
  ggplot(aes(N, DOW, linetype=type)) + 
  geom_hline(aes(yintercept = 0), color="grey") +
  geom_line(size=0.5, color="#1f78b4") +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) + 
  scale_x_continuous("Lags (months)", limits = c(0,48), breaks = seq(0, 48, 8)) +
  th + theme(axis.title.y = element_blank(), legend.position = "none") + 
  labs(title=expression(paste(bold("Figur  "), "VAR(13) Response DOW - change")), 
       caption="Source: FRED + Transformationer", x = "", y = "")

ggsave(plot = E6, filename = "GENERATE/EXAM6.pdf", width = 16, height = 12, units = "cm", dpi = 320)
