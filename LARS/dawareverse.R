devtools::install_github("mikkelkrogsholm/dawa")

library("xlsx")
library(dawa)
library(readxl)
library(tidyverse)

CVR <- read_excel("//student.aau.dk/Users/lbni13/Desktop/CVRudtræk_30.04.2019_IVS_Excel.xlsx")



res <- c(NA)
for (i in 1:254) {
  tryCatch?{
res[i] <- do.call(adresser, dawa::datavask(betegnelse = paste(CVR$Adresse[i], CVR$Postnr.[i],CVR$By[i], sep = " "), type = "adgangsadresser")[["resultater"]][[1]][["vaskeresultat"]][["variant"]])[[1]][["adgangsadresse"]][["sogn"]][["kode"]]
Sys.sleep(tim? = 0.05)
  }, error=function(e){})
}


write.xlsx(df, file, sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)