#' Librerías [Wardo]
####

library(plotly)
#library(dplyr)
library(DT)
library(formattable)
library(kableExtra)
library(purrr)
library(ggplot2)
library(readr)
library(knitr)

#' Librerías [Sam]
####
library(tidyverse)
library(mongolite)
library(jsonlite)
library(data.table)
library(shiny)
library(shinydashboard)
#library(plotly)

#' Librerias [Ajustes]
library(fontawesome)
library(rCharts)
library(shiny)
library(magrittr)


Conexion        <- function(database = "lago",
                            coleccion)                            #' @Conexion.Lago.Productivo
{
  if(database == "lago" || database == "lake"){
    con <- mongo(collection = coleccion,
                 db = "lake",
                 url = "mongodb://devLake:0vX0AiRF0xk!@169.57.123.35:27017/lake")
  } else if(database == "productivo" || database == "prod"){
    con <- mongo(collection = coleccion,
                 db = "visoradl",
                 url = "mongodb://development:4b21272fd9@169.57.10.26:27017/visoradl")
  } else{
    stop("Database incorrecto. Selecciona entre 'lago' o 'productivo'.")
  }
  
  return(con)
  
}

RedondeaMedia <- function(x, n = 100, redondea = 2)
{
  if(!is.numeric(x)){
    try(x <- as.numeric(x), silent = TRUE)
  }
  
  resultado <- round(n*mean(x), redondea)
  
  return(resultado)
}

RandomKPI <- function(rfc.data, seed = 123456,
                      n.KPI = 5, flag = 0.6, campanas)
{
  set.seed(as.numeric(seed))
  
  if(!("Pyme" %in% names(rfc.data) || "RFC" %in% names(rfc.data))){
    stop("Data frame incorrecto. Verificar existencia de columnas 'RFC' o 'Pyme'.")
  }
  
   rfc.data %<>%
     filter(campana %in% campanas)
  
  n     <- dim(rfc.data)[1]
  KPIs  <- NULL
  orden <- NULL
  
  for(i in 1:n.KPI){
    KPIs  <- c(KPIs, paste("KPI", i))
    orden <- c(orden, c(paste("KPI", i), paste("alerta", i)))
  }
  
  nombres <- c("RFC", KPIs)
  orden <- c("RFC", orden)
  
  resultado <- cbind.data.frame(data.frame(rfc.data[, grep("Pyme|RFC", names(rfc.data))[1]],
                                           stringsAsFactors = F),
                                data.frame(replicate(n.KPI, runif(n)), stringsAsFactors = FALSE),
                                stringsAsFactors = FALSE) %>%
    `names<-`(nombres) %>% 
    mutate_at(vars(starts_with("KPI")), list(synth = ~ifelse(. > flag, T, F))) %>% 
    rename_at(vars( contains( "_synth") ), list( ~paste("alerta", gsub("\\D+", "", .)) ) ) %>%
    .[ , orden]
  
  gc()
  
  return(resultado)
  
}

FormatoIconos <- function(data, n.Pymes = 20)
{
  require(fontawesome)
  
  addLink <- function(link.loc = "www.google.com", icon = "check")
  {
    if(icon == "check"){
      fill <- "#008000"
    } else if(icon == "exclamation-triangle"){
      fill <- "#8C001A"
    } else{
      fill <- "#000000"
    }
    
    resultado <- paste0("<a href=\"", link.loc, "\">", as.character(fa(icon, fill = fill)), "</a>")
    
    return(resultado)
    
  }  
  
  if(n.Pymes > dim(data)[1]){
    
    n.Pymes <- dim(data)[1]
    
  }
  
  data %<>% 
    mutate_at(vars(starts_with("alerta")), list( ~ifelse(., addLink(.),
                                                         addLink(., icon = "exclamation-triangle")))) %>%
    mutate_at(vars(starts_with("KPI")), list( ~scales::percent(.))) #%>%
    # `rownames<-`(.$RFC) %>%
    #   mutate(RFC = NULL)
    
  rownames(data) <- data$RFC
  
  data$RFC <- NULL
  
  resultado <- seq(2, ncol(data), 2) %>%
    map(~ select(data,(.-1):.))
  
  require(magrittr)
  
  for(i in 1:length(resultado)){
    
    KPI.colname     <- paste0("KPI ", i)
    .header         <- c("RFC" = 1, KPI.colname = 2)
    names(.header)  <- c("RFC", KPI.colname)
    
    
    resultado[[i]] %<>%
      `names<-`(c("Valor", "Alerta")) %>%
      rownames_to_column('RFC') %>%
      arrange(desc(as.numeric(gsub("%", "", Valor)))) %>%
      column_to_rownames('RFC') %>%
      head(., n = n.Pymes) %>%
      kable(escape = F, align = c("c", "c"), linesep = "", row.names = T) %>%
      kable_styling(bootstrap_options = c("striped", "scale_down", "bordered"),
                    full_width = F, font_size = 11, position='float_left') %>%
      column_spec(1, bold = T) %>%
      column_spec(1:3, color = "black", width =  "7em", include_thead = T) %>%
      add_header_above(header = .header)
    
  }
  
  return(resultado)
}

PloteoSabroso <- function(data)
{
  nombres.alertas <- names(data)[grep("alerta", names(data))]
  
  tabla_conteo <- data %>% 
  {rbind.data.frame(
    summarise_at(., nombres.alertas, sum),
    summarise_at(., nombres.alertas, mean),
    stringsAsFactors = FALSE
  )} %>% t %>% data.frame(., stringsAsFactors = F) %>%
    #transpose %>%
    `colnames<-`(c("conteo", "porcentaje")) %>%
    mutate(alerta = nombres.alertas, status = "Adelante") %>%
    select(alerta, status, conteo,porcentaje) %>%
    {rbind.data.frame(
      mutate(., conteo = nrow(data) - conteo,
             porcentaje = 1 - porcentaje,
             status = "Precaución"),
      .,
      stringsAsFactors = F
    )} %>%
    mutate(alerta = stringi::stri_trans_totitle(gsub("alerta", "alerta ", alerta)))
  
  require(rCharts)
  
  colores.visor <- c("#33B9A5", #aqua
                     "#00A2F3", #azul
                     "#f6A623", #naranja
                     "#27B701", #verde
                     "#002031") #azul noche
  
  colores.alerta      <- c("#FF3B30", "#4CD964") # rojo
  
  
  plot <- nPlot(porcentaje ~ alerta,
                tabla_conteo,
                group = "status",
                type = "multiBarChart")
  plot$chart(stacked = TRUE)
  plot$chart(color = colores.alerta)
  plot$xAxis(rotateLabels = -15)
  plot$yAxis(tickFormat = "#!function(d) {return d3.format('.0%')(d);}!#")
  
  return(plot)
  
}

## Processsss

data.init <- Conexion(coleccion = "sas_final")$find('{}')
maxKPI <- 7
nombres.campanas <- Conexion(coleccion = "sas_final")$find('{}',
                                                           fields = '{"campana" : true,
                                                           "_id": false}') %>%
  unique() %>%
  as.character()

data.main.alertas <- Conexion(coleccion = "sas_final")$find('{}') %>%
   RandomKPI(campanas = "UNIVARPERFIL187")

# alertas.kable <- data.main.alertas %>%
#    FormatoIconos()

PloteoSabroso(data.main.alertas)
