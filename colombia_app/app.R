library(shiny)
library(leaflet)
library(RColorBrewer)
library(viridis)
library(car)


sp_data_file <- '/Users/francesco/Desktop/projects/colombia_referendum/sp_poly_dataframe.RData'

load(sp_data_file)

vars_for_selection <- c("Yes (%)" = 'perc_si', 
                        "Participation (%)" = 'perc_particip',
                        "Diplaced (%)" = 'perc_displ',
                        "Poor (%)" = 'perc_pov')

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                tags$h2("Colombian peace agreement referendum mapped by municipality", style = "background-color:rgba(255, 255, 255, 0.75);")),
  absolutePanel(bottom = 38, left = 10,
                selectInput("var", "Variable on the map", vars_for_selection),
                tags$div(htmlOutput('var_info'), 
                         style = 'font-size: 80%; background-color: white; width: 300px;'),
                tags$br(),
                selectInput("cory", "Variables in the plot", vars_for_selection, selected = 'perc_si'),
                selectInput("corx", NULL, vars_for_selection, selected = 'perc_displ'),
                plotOutput('cor_plot', height = '300px', width = '300px'),
                tags$div(id='footer',
                         style='font-size: 70%; position:fixed; width:100%; width: 50%; padding: 0 5px; bottom:-10px;',
                         HTML(paste0(
                           "<p>Design: Francesco Bailo (<a href='https://twitter.com/FrBailo' target='_blank'>@FrBailo</a>) ",
                           "| Based a <a href='https://data.humdata.org/dataset/colombia-admin-level-2-boundaries' target='_blank'>Humanitarian Data Exchange</a> shapefile ",
                           "| Powered by: <a href='http://www.R-project.org/' target='_blank'>R</a> and <a href='http://shiny.rstudio.com/' target='_blank'>Shiny</a> ",
                           "| R packages: <a href='https://CRAN.R-project.org/package=leaflet' target='_blank'>leaflet</a>, <a href='https://CRAN.R-project.org/package=viridis'>viridis</a>, <a href='https://CRAN.R-project.org/package=RColorBrewer'>RColorBrewer</a> ",
                           "| Hosted by: <a href='https://nectar.org.au/research-cloud/'>Nectar Cloud</a></p>"))))
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    col_adm2$selected_var <- as.data.frame(col_adm2)[[input$var]]
    return(col_adm2)
  })
  
  correlationData <- reactive({
    dat <- as.data.frame(col_adm2)[,c(input$corx, input$cory)]
    colnames(dat) <- c('corx', 'cory')
    dat$corx <- dat$corx * 100
    dat$cory <- dat$cory * 100
    return(dat)
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    
    if (input$var == 'perc_si') {
      
      colorNumeric(
        palette = brewer.pal(7, "RdBu"),
        domain = c(0,1)
      )
      
    } else if (input$var == 'perc_particip') {
      
      colorNumeric(
        palette = viridis(7),
        domain = col_adm2$selected_var
      )
      
    } else if (input$var == 'perc_displ') {
      
      colorNumeric(
        palette = brewer.pal(7, "Greens"),
        domain = col_adm2$selected_var
      )
      
    } else if (input$var == 'perc_pov') {
      
      colorNumeric(
        palette = brewer.pal(7, "YlOrRd"),
        domain = col_adm2$selected_var,
        na.color = NA
      )
      
    }
    
    
    
  })
  
  output$var_info <- renderText({
    
    if (input$var == 'perc_si') {
      
      return("'Yes votes' over 'Valid votes' in the 2016 Colombian peace agreement referendum.<br><i>Source: <a href = 'http://plebiscito.registraduria.gov.co'>plebiscito.registraduria.gov.co</a></i>")
      
    } else if (input$var == 'perc_particip') {
      
      return("'Valid votes' over 'Entitled people' in the 2016 Colombian peace agreement referendum.<br><i>Source: <a href = 'http://plebiscito.registraduria.gov.co'>plebiscito.registraduria.gov.co</a></i>")
      
    } else if (input$var == 'perc_displ') {
      
      return("Annual mean of 'Displaced people' in the period 1999-2014 over 'Entitled people' in the 2016 Colombian peace agreement referendum.<br><i>Source: <a href = 'https://data.humdata.org/dataset/idps-data-by-year-and-municipality'>data.humdata.org/</a></i>")
      
    } else if (input$var == 'perc_pov') {
      
      return("'Poor' over 'Entire populaton' in 2014 (by department).<br><i>Source: <a href = 'https://es.wikipedia.org/wiki/Anexo:Departamentos_de_Colombia_por_pobreza_monetaria'>es.wikipedia.org/</a></i>")
      
    }
      
    })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(col_adm2) %>% 
      addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
      fitBounds(-90, -4.24, -66.6, 12.7) 
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addPolygons(
        stroke = TRUE, fillOpacity = 0.7, color = 'gray', weight = .5,
        fillColor = ~pal(selected_var),
        popup = ~paste0("Mun: <b>", admin2RefN, "</b><br>",
                        "Dep: <b>", admin1Name, "</b><br>",
                        "<br>",
                        "Total votes: <b>", formatC(votos_validos + votos_no_marcados + votos_nulos, big.mark = ",", format = "fg"), "</b> (", round(perc_particip*100,2),"%)<br>",
                        "'Yes' votes: <b style = 'font-color: #2166AC;'>", formatC(votos_si, big.mark = ",", format = "fg"), "</b> (", round(perc_si*100,2),"%)<br>",
                        "'No' votes: <b style = 'font-color: #B2182B;'>", formatC(votos_no, big.mark = ",", format = "fg"), "</b> (", round(votos_no/votos_validos*100,2),"%)<br>",
                        "<br>",
                        "Displaced (1999-2014, annual mean): <b>", formatC(round(displ_mean,0), big.mark = ",", format = "fg"), "</b> (", round(perc_displ*100,2),"%)<br>",
                        "<br>",
                        "Poverty: <b>", round(perc_pov*100,2), "%</b>")
        
      )
  })
  
  
  output$cor_plot <- renderPlot({
    
    cex_value = 0.2
    
    if (input$corx == 'perc_displ') {
      fit <- lm(cory ~ corx, data = correlationData())
      scatterplot(cory ~ sqrt(corx), data = correlationData(),
                  xlab = paste0(names(vars_for_selection)[vars_for_selection == input$corx], " (sqrt)"),
                  ylab = names(vars_for_selection)[vars_for_selection == input$cory],
                  cex = cex_value,
                  main = paste0("Municipios: ", nobs(fit), "; R2: ", format(summary(fit)$adj.r.squared, digits=4)))
    } else if(input$cory == 'perc_displ') {
      fit <- lm(sqrt(cory) ~ corx, data = correlationData())
      scatterplot(sqrt(cory) ~ corx, data = correlationData(),
                  xlab = names(vars_for_selection)[vars_for_selection == input$corx],
                  ylab = paste0(names(vars_for_selection)[vars_for_selection == input$cory], " (sqrt)"),
                  cex = cex_value,
                  main = paste0("Municipios: ", nobs(fit), "; R2: ", format(summary(fit)$adj.r.squared, digits=4)))
    } else {
      fit <- lm(cory ~ corx, data = correlationData())
      subtitle <- paste("R2: ", format(summary(fit)$adj.r.squared, digits=4))
      scatterplot(cory ~ corx, data = correlationData(),
                  xlab = names(vars_for_selection)[vars_for_selection == input$corx],
                  ylab = names(vars_for_selection)[vars_for_selection == input$cory],
                  cex = cex_value, 
                  main = paste0("Municipios: ", nobs(fit), "; R2: ", format(summary(fit)$adj.r.squared, digits=4)))
    }
    
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = col_adm2)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls
    
    if (input$var == 'perc_si') {
      
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = c(0,1),
                          title = "Voted 'YES'",
                          labFormat = labelFormat(suffix = "%", transform = function(x) 100 * x),
                          opacity = 1
      )
      
    } else if (input$var == 'perc_particip') {
      
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~perc_particip,
                          title = "Participation",
                          labFormat = labelFormat(suffix = "%", transform = function(x) 100 * x),
                          opacity = 1
      )
      
    } else if (input$var == 'perc_displ') {
      
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~perc_displ,
                          title = "Displaced<br>(annual mean, 1999-2014)",
                          labFormat = labelFormat(suffix = "%", transform = function(x) 100 * x),
                          opacity = 1
      )
      
    } else if (input$var == 'perc_pov') {
      
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = c(0,1),
                          title = "Poverty",
                          labFormat = labelFormat(suffix = "%", transform = function(x) 100 * x),
                          opacity = 1
      )
      
    }
    
  })
}

shinyApp(ui, server)