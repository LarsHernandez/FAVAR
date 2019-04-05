

nsbalpanel <- read_table2("//student.aau.dk/Users/lbni13/Desktop/Bernanke R/nsbalpanel.txt", 
                          col_names = FALSE)

TOT <- nsbalpanel %>% 
  mutate_all(funs(scale))


plot(irf(VAR(TOT[,c(16,107,77)], p=2)))
  
TOT %>% mutate(N = 1:511) %>% gather(variable, value, -N) %>% ggplot(aes(N,value)) + geom_line() + facet_wrap(~variable)

summary(prcomp(TOT, rank. = 8))

#   
#   % choose X from balanced panel (remember -- with Y excluded, so -1 after 77)
# xindex=[16;107;77;80;95;92;74;101;17;49;50;51;26;48;117;54;62;71;119];
# 
# 
# %varnames={'IP','CPI','3m TREASURY BILLS','5y TREASURY BONDS','M1','M2','MONETARY BASE','TOTAL INST RESERVES',...
#   %        'NONBORROWED INST R','EXCHANGE RATE YEN','COMMODITY PRICE INDEX','CAPACITY UTIL RATE',...
#   %        'PERSONAL CONSUMPTION','EMPLOYMENT','AVG WEEKLY HOURS',...
#   %        'HOUSING STARTS','NEW ORDERS','NYSE COMPOSITE','AVG HOURLY EARNINGS',...
#   %        'CONSUMER EXPECTATIONS'};

#% Y=nsbalpanel(:,[16 107 77]); % this is IP, CPI and FFR