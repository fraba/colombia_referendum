.libPaths("/Users/francesco/R_packages")

setwd('/Users/francesco/Desktop/projects/colombia_referendum')

# library(sp) 
library(RSelenium)

# COL_adm1 <- readRDS('COL_adm1.rds')
# COL_adm2 <- readRDS('COL_adm2.rds')

# htm_pages <- read.csv('htm_pages.csv',stringsAsFactors = F, header = FALSE)

base_url <- 'http://plebiscito.registraduria.gov.co'

library(rvest)
library(dplyr)
require(stringr)


RSelenium::checkForServer()
RSelenium::startServer()
remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4444
                      , browserName = "firefox"
)
remDr$open()

remDr$navigate(base_url)
src <- remDr$getPageSource()
homepage <- read_html(src[[1]])

dep_pages <- 
  data.frame(url=homepage %>% html_nodes("#combo2 option") %>% html_attr("value"),
             name=homepage %>% html_nodes("#combo2 option") %>% html_text(),
             stringsAsFactors=FALSE)[-1,]
dep_pages$url <- gsub("^\\.\\.", "", dep_pages$url) 

if (!file.exists('scraped_data.RData')) {
  scraped_data <- data.frame(departamento = character(),
                             municipio = character(),
                             mesas_informadas = numeric(),
                             mesas_instaladas = numeric(),
                             votantes = numeric(),
                             personas_habilitadas = numeric(),
                             votos_validos = numeric(),
                             votos_no_marcados = numeric(),
                             votos_nulos = numeric(),
                             votos_si = numeric(),
                             votos_no = numeric(),
                             stringsAsFactors = FALSE) 
} else {
  load('scraped_data.RData')
}

for (i in 1:nrow(dep_pages)) {
  
  remDr$navigate(paste0(base_url, dep_pages$url[i]))
  src <- remDr$getPageSource()
  deppage <- read_html(src[[1]])
  
  mun_pages <- 
    data.frame(url=deppage %>% html_nodes("#combo3 option") %>% html_attr("value"),
               name=deppage %>% html_nodes("#combo3 option") %>% html_text(),
               stringsAsFactors=FALSE)[-1,]
  mun_pages$url <- gsub("^\\.\\.", "", mun_pages$url) 
  
  for (j in 1:nrow(mun_pages)) {
    
    testmun <- which(scraped_data$municipio == mun_pages$name[j])
    testdep <- which(scraped_data$departamento == dep_pages$name[i])
    
    if (!(length(testmun) == 0 | length(testdep) == 0)) {
      if (testmun %in% testdep) {
        next
      }
    }
    
    print(paste0(dep_pages$name[i], ", ", municipio = mun_pages$name[j]))
  
    remDr$navigate(paste0(base_url, mun_pages$url[j]))
    
    tmp_mesas <-
      remDr$findElement(using = 'xpath', value = '//*[@id="sol0"]/div/div[1]/div/div[1]/div[1]/div/div[2]/span[2]')
    tmp_mesas <-
      tmp_mesas$getElementText()[[1]]
    
    tmp_votacion <- 
      remDr$findElement(using = 'xpath', value = '//*[@id="sol0"]/div/div[1]/div/div[1]/div[2]/div/div[2]/span[2]')
    tmp_votacion <-
      tmp_votacion$getElementText()[[1]]
    
    tmp_validos <- 
      remDr$findElement(using = 'xpath', value = '//*[@id="sol0"]/div/div[1]/div/div[3]/div[1]/div[2]')
    tmp_validos <-
      tmp_validos$getElementText()[[1]]
    
    tmp_nomarcados <- 
      remDr$findElement(using = 'xpath', value = '//*[@id="sol0"]/div/div[1]/div/div[3]/div[2]/div[2]')
    tmp_nomarcados <-
      tmp_nomarcados$getElementText()[[1]]
    
    tmp_nulos <- 
      remDr$findElement(using = 'xpath', value = '//*[@id="sol0"]/div/div[1]/div/div[3]/div[3]/div[2]')
    tmp_nulos <-
      tmp_nulos$getElementText()[[1]]
    
    tmp_si <- 
      remDr$findElement(using = 'xpath', value = '//*[@id="graphContainer"]/div[4]')
    tmp_si <-
      tmp_si$getElementText()[[1]]
    
    tmp_no <- 
      remDr$findElement(using = 'xpath', value = '//*[@id="graphContainer"]/div[8]')
    tmp_no <-
      tmp_no$getElementText()[[1]]
    
    df_tmp <- 
      data.frame(departamento = dep_pages$name[i],
                 municipio = mun_pages$name[j],
                 mesas_informadas = str_extract_all(tmp_mesas, "\\d{1,3}(.\\d{1,3})?(.\\d{1,3})?")[[1]][1],
                 mesas_instaladas = str_extract_all(tmp_mesas, "\\d{1,3}(.\\d{1,3})?(.\\d{1,3})?")[[1]][2],
                 votantes = str_extract_all(tmp_votacion, "\\d{1,3}")[[1]][1],
                 personas_habilitadas = str_extract_all(tmp_votacion, "\\d{1,3}(.\\d{1,3})?(.\\d{1,3})?")[[1]][2],
                 votos_validos = str_extract(tmp_validos, "\\d{1,3}(.\\d{1,3})?(.\\d{1,3})?"),
                 votos_no_marcados = str_extract(tmp_nomarcados, "\\d{1,3}(.\\d{1,3})?(.\\d{1,3})?"),
                 votos_nulos = str_extract(tmp_nulos, "\\d{1,3}(.\\d{1,3})?(.\\d{1,3})?"),
                 votos_si = str_extract(tmp_si, "\\d{1,3}(.\\d{1,3})?(.\\d{1,3})?"),
                 votos_no = str_extract(tmp_no, "\\d{1,3}(.\\d{1,3})?(.\\d{1,3})?"),
                 stringsAsFactors = FALSE)
    
    scraped_data <- rbind(scraped_data, df_tmp)
    save(scraped_data, file = 'scraped_data.RData')
    
    Sys.sleep(5)
    
  }
  
}

