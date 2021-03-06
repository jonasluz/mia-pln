# Importa a biblioteca da API Rosette.
#
library(rosetteApi)
rosette.apiDir <- "..\\R-Binding\\R\\Api.R"
source(rosette.apiDir)

# Chave da aplica��o Rosette.
#
#rosette.apiKey <- "<put Rosette API Key here!"

## Fun��o para executar uma chamada Rosette
#
RosetteCall <- function(x, endpoint, extraParams = NULL) 
{
  # Configura os par�metros default.
  params <- list(language = "por", content = x)
  # Configura par�metros adicionais.
  if (!is.null(extraParams))
  {
    params <- append(params, extraParams)
  }
  
  # Faz a chamada � API Rosette.
  result <- api(rosette.apiKey, endpoint, parameters = params)

  return(result)
} # RosetteCall

PrepareResult <- function(x, index) 
{
  # Prepara vari�vel de resultado.
  result <- list()
  result$index <- index
  result$origin <- gsub("\\s+", " ", trimws(x))
  
  return(result)
} #PrepareResult

EntitiesFrom <- function(x, index = NULL) 
{
  # Vari�vel de resultado.
  result <- PrepareResult(x, index)
  if (result$origin == "") return(result)
  
  # Extrai as entidades usando a API Rosette.
  data <- RosetteCall(x, "entities")
  data.entities <- data$content$entities
  data.merged <- Reduce(function(...) merge(..., all = TRUE), data.entities)
  
  result$entities <- data.merged
  
  return(result)
} # EntitiesFrom

PartsOfSpeech <- function(x, index = NULL) 
{
  # Vari�vel de resultado.
  result <- PrepareResult(x, index)
  if (result$origin == "") return(result)

  # Extrai as partes da fala usando a API Rosette.
  data <- RosetteCall(x, "morphology", c(morphology = "parts-of-speech"))
  result$tokens <- data$content$tokens
  result$posTags <- data$content$posTags
  
  return(result)
} # PartsOfSpeech