load("/Users/francesco/Desktop/projects/colombia_referendum/scraped_data.RData")

scraped_data$votantes <- NULL

for (v in names(scraped_data[3:ncol(scraped_data)])) {
  scraped_data[[v]] <- as.numeric(gsub("\\.", "", scraped_data[[v]]))  
}

sum(scraped_data$votos_si)
sum(scraped_data$votos_no)
sum(scraped_data$votos_validos) + sum(scraped_data$votos_no_marcados) + sum(scraped_data$votos_nulos)
