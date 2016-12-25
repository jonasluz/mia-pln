## Relatives.R
# 
# Identifica relações de parentesco.
#
# ##

library(stringi)

## Função para encontrar relações.
# 
FindRelations <- function(x, index = NULL) 
{
  ## Relações...
  # 
  relations <- c("pai", "mãe", "irmão", "irmã", "primo", "prima", "tio", "tia", 
                 "neto", "neta", "bisneto", "bisneta", "cunhado", "cunhada", 
                 "marido", "esposo", "mulher", "esposa", "noivo", "noiva", 
                 "namorado", "namorada", "avô", "avõ", "bisavô", "bisavó", 
                 "sobrinho", "sobrinha", "sogro", "sogra", "genro", "nora",
                 "enteado", "enteada", "entiado", "entiada", # (SIC)
                 "companheiro", "companheira", "parceiro", "parceira",
                 "compadre", "comadre", "cumpadre", "cumadre", #(SIC)
                 "amigo", "amiga", "vizinho", "vizinha", "ex", #, "si"
                 "filho", "filha"
  )
  
  # Faz a análise morfológica, encontrando tokens e posTags.
  #
  result <- PartsOfSpeech(x, index)
  if (result$origin == "") return(result)
  
  # Procura por relações de família na lista de tokens.
  #
  indexes <- which(result$tokens %in% relations, arr.ind = TRUE)
  relations <- result$tokens[indexes]
  result$relations <- unique(unlist(relations))
  
  # Limpa o POS do resultado antes de retornar.
  result$tokens <- NULL
  result$posTags <- NULL
  
  return(result)
}