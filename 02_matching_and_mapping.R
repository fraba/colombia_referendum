library(rgdal)

# Base map
shape <- 
  readOGR(dsn = "/Users/francesco/Desktop/projects/colombia_referendum/Admin 2/", 
          layer = "col_admbnda_adm2_igac_ocha")

adm1_geo_data <- as.data.frame(table(shape$admin1Name), stringsAsFactors = FALSE)
adm1_geo_data$name <- adm1_geo_data$Var1
adm1_geo_data <- 
  merge(adm1_geo_data, unique(as.data.frame(shape[,c('admin1Name', 'admin1Pcod')])), 
        by.x = 'Var1', by.y = 'admin1Name')


# Referendum data
load("/Users/francesco/Desktop/projects/colombia_referendum/scraped_data.RData")
adm1_ref_data <- as.data.frame(table(scraped_data$departamento), stringsAsFactors = FALSE)
adm1_ref_data$name <- adm1_ref_data$Var1

# Match departments
require(fuzzyjoin)
# matching_df <- stringdist_join(adm1_geo_data, adm1_ref_data, by = "name", ignore_case = TRUE,
#                                mode = 'full')
# write.csv(matching_df, file = "/Users/francesco/Desktop/projects/colombia_referendum/matching_df.csv")
matching_df <- read.csv( "/Users/francesco/Desktop/projects/colombia_referendum/matching_df_edited.csv", stringsAsFactors = F)
adm1_ref_data <- merge(adm1_ref_data, matching_df, by.x = 'name', by.y = 'ref_data_name')

adm1_ref_data <- merge(adm1_ref_data, adm1_geo_data, by = 'admin1Pcod')

View(subset(adm1_ref_data, Freq.x != Freq.y))

# Match municipios
library(xlsx)
scraped_data <- merge(scraped_data, adm1_ref_data[,c("name.x", "admin1Pcod")],
                      by.x = 'departamento', by.y = 'name.x', all.x = FALSE)

# for (admin1 in unique(scraped_data$admin1Pcod)) {
#   tmp_df <- subset(scraped_data, admin1Pcod == admin1, select = c("admin1Pcod","municipio"))
#   colnames(tmp_df)[2] <- "admin2Name"
#   matching_admin2 <-
#     stringdist_join(tmp_df, subset(as.data.frame(shape),
#                                     select = c("admin1Pcod","admin1Name",'admin2Name','admin2Pcod'),
#                                     admin1Pcod == admin1),
#                     by = "admin2Name", ignore_case = TRUE, mode = 'full')
#   matching_admin2$duplicated <- 
#     duplicated(matching_admin2$admin2Name.x) | 
#     duplicated(matching_admin2$admin2Name.x, fromLast=TRUE)
#   
#   write.xlsx(matching_admin2, "/Users/francesco/Desktop/projects/colombia_referendum/matching_admin2/coding_admin2.xlsx", 
#              sheetName=admin1, row.names=FALSE, append = TRUE)
#   
# }

# Read from Excel

municipios_by_dep <- data.frame()
for (admin1 in unique(scraped_data$admin1Pcod)) {
  municipios_by_dep <- 
    rbind(municipios_by_dep, 
          read.xlsx("/Users/francesco/Desktop/projects/colombia_referendum/matching_admin2/coding_admin2.xlsx",
                    sheetName = admin1))
}

municipios_by_dep <- municipios_by_dep[,c("admin1Pcod.x","admin2Name.x","admin2Pcod")]

scraped_data_wt_geocode <- 
  merge(scraped_data, municipios_by_dep, 
        by.x = c('admin1Pcod','municipio'),
        by.y = c('admin1Pcod.x','admin2Name.x'),
        all = TRUE)

## CHANGES (due to difference from shapefile)
# sum to
# CO13	NOROSI ->  CO13	RIOVIEJO
# CO19	GUACHENE -> CO19	CALOTO
# CO23	SAN JOSE DE URE -> CO23	AYAPEL
# CO23	TUCHIN -> CO23	SAN ANDRES DE SOTAVENTO
# CO44	ALBANIA -> CO44	URIBIA
# CO88	PROVIDENCIA -> CO88	SAN ANDRES
# 
# # Changed state
# 
# #N/A	#N/A	CO27	Chocó	Belén de Bajirá	CO27086

scraped_data_wt_geocode$admin2Pcod[scraped_data_wt_geocode$admin1Pcod == "CO13" &
                                     scraped_data_wt_geocode$municipio == "NOROSI"] <- "CO13600"

scraped_data_wt_geocode$admin2Pcod[scraped_data_wt_geocode$admin1Pcod == "CO19" &
                                     scraped_data_wt_geocode$municipio == "GUACHENE"] <- "CO19142"

scraped_data_wt_geocode$admin2Pcod[scraped_data_wt_geocode$admin1Pcod == "CO23" &
                                     scraped_data_wt_geocode$municipio == "SAN JOSE DE URE"] <- "CO23068"

scraped_data_wt_geocode$admin2Pcod[scraped_data_wt_geocode$admin1Pcod == "CO23" &
                                     scraped_data_wt_geocode$municipio == "TUCHIN"] <- "CO23670"

scraped_data_wt_geocode$admin2Pcod[scraped_data_wt_geocode$admin1Pcod == "CO44" &
                                     scraped_data_wt_geocode$municipio == "ALBANIA"] <- "CO44847"

scraped_data_wt_geocode$admin2Pcod[scraped_data_wt_geocode$admin1Pcod == "CO88" &
                                     scraped_data_wt_geocode$municipio == "PROVIDENCIA"] <- "CO88001"

scraped_data_wt_geocode$votantes <- NULL

for (v in names(scraped_data_wt_geocode[4:11])) {
  scraped_data_wt_geocode[[v]] <- as.numeric(gsub("\\.", "", scraped_data_wt_geocode[[v]]))  
}

scraped_data_wt_geocode$municipio <- NULL
scraped_data_wt_geocode$departamento <- NULL
scraped_data_wt_geocode$admin1Pcod <- NULL

require(dplyr)
scraped_data_wt_geocode <-
  scraped_data_wt_geocode %>%
  dplyr::group_by(admin2Pcod) %>%
  summarize(mesas_informadas = sum(mesas_informadas),
            mesas_instaladas = sum(mesas_instaladas),
            personas_habilitadas = sum(personas_habilitadas),
            votos_validos = sum(votos_validos),
            votos_no_marcados = sum(votos_no_marcados),
            votos_nulos = sum(votos_nulos),
            votos_si = sum(votos_si),
            votos_no = sum(votos_no))

save(scraped_data_wt_geocode, file = '/Users/francesco/Desktop/projects/colombia_referendum/scraped_data_wt_geocode.RData')
write.csv(scraped_data_wt_geocode, file = '/Users/francesco/Desktop/projects/colombia_referendum/scraped_data_wt_geocode.csv',
          row.names = FALSE)


# 
require(rgdal)
load('/Users/francesco/Desktop/projects/colombia_referendum/scraped_data_wt_geocode.RData')

library(xlsx)
dat <- read.xlsx('/Users/francesco/Desktop/projects/colombia_referendum/displacement/desplazamiento-1999-2014.xlsx',
                 sheetIndex = 1)
dat$admin2Pcod <- paste0("CO", dat$DIVIPOLA)

dat$DIVIPOLA <- NULL
dat$DEPARTAMENTO <- NULL
dat$MUNICIPIO <- NULL

colnames(dat)[1:16] <- gsub("X","displ_",colnames(dat)[1:16])
dat$displ_mean <- apply(dat[,1:16], 1,  mean, na.rm = T)

library(rgdal)
# col_adm1 <- readOGR(dsn = "/Users/francesco/Desktop/projects/colombia_referendum/Admin 1/", 
#                     layer = "col_admbnda_adm1_unodc_ocha")
col_adm2 <- readOGR(dsn = "/Users/francesco/Desktop/projects/colombia_referendum/Admin 2/", 
                    layer = "col_admbnda_adm2_igac_ocha_simp")

col_adm2 <- merge(col_adm2, scraped_data_wt_geocode, by = 'admin2Pcod')
col_adm2 <- merge(col_adm2, dat, by = 'admin2Pcod')

col_adm2$perc_si <- col_adm2$votos_si / col_adm2$votos_validos
col_adm2$perc_displ <- col_adm2$displ_mean / col_adm2$personas_habilitadas
col_adm2$perc_particip <- 
  (col_adm2$votos_validos + col_adm2$votos_nulos + col_adm2$votos_no_marcados) / col_adm2$personas_habilitadas

require(rvest) 
url <- 'https://es.wikipedia.org/wiki/Anexo:Departamentos_de_Colombia_por_pobreza_monetaria'

col_probreza <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/table[1]') %>%
  html_table() %>%
  as.data.frame()
col_probreza <- col_probreza[1:(nrow(col_probreza)-1),]

col_probreza$Capital <- NULL
col_probreza$Población..est..2015..3. <- NULL
col_probreza$Proporción.de.pobreza <- as.numeric(gsub(",", ".", col_probreza$Proporción.de.pobreza))

names(col_probreza) <- c('admin1Name', 'perc_pov')
col_probreza$perc_pov <- col_probreza$perc_pov / 100

require(fuzzyjoin)
col_probreza <- stringdist_join(col_probreza, 
                                unique(as.data.frame(col_adm2)[,c('admin1Pcod','admin1Name')]),
                                by = 'admin1Name', mode = 'full', ignore_case = TRUE)

col_probreza <- col_probreza[complete.cases(col_probreza),c('admin1Pcod', 'perc_pov')]

col_adm2 <- merge(col_adm2, col_probreza, by = 'admin1Pcod', all.x = TRUE)

save(col_adm2, file = "/Users/francesco/Desktop/projects/colombia_referendum/sp_poly_dataframe.RData")

require(ggplot)
require(car)
scatterplot(col_adm2$perc_si ~ sqrt(col_adm2$perc_displ))

require(RColorBrewer)

pal <- colorNumeric(
  palette = brewer.pal(6, "Blues") ,
  domain = c(0,1)
)

require(viridis)

pal <- colorNumeric(
  palette = viridis(7),
  domain = col_adm2$perc_particip
)

library(leaflet)
leaflet(col_adm2) %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addPolygons(
    stroke = TRUE, fillOpacity = 1, color = 'gray', weight = 1,
    fillColor = ~pal(perc_particip)
  )



