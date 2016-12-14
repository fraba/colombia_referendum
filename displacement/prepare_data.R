library(xlsx)
dat <- read.xlsx('/Users/francesco/Desktop/projects/colombia_referendum/displacement/desplazamiento-1999-2014.xlsx',
                 sheetIndex = 1)
dat$admin2Pcod <- paste0("CO", dat$DIVIPOLA)

dat$DIVIPOLA <- NULL
dat$DEPARTAMENTO <- NULL
dat$MUNICIPIO <- NULL

colnames(dat)[1:16] <- gsub("X","displ_",colnames(dat)[1:16])
dat$displ_mean <- apply(dat[,1:16], 1,  mean, na.rm = T)

write.csv(dat, file = "/Users/francesco/Desktop/projects/colombia_referendum/displacement/desplazamiento-1999-2014_prep.csv",
          row.names = FALSE)

