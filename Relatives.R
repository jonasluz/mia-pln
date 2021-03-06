## Relatives.R
# 
# Identifica rela��es de parentesco.
#
# ##

library(stringi)

## Fun��o para encontrar rela��es.
# 
FindRelations <- function(x, index = NULL) 
{
  ## Rela��es...
  # 
  relations <- c("pai", "m�e", "irm�o", "irm�", "primo", "prima", "tio", "tia", 
                 "neto", "neta", "bisneto", "bisneta", "cunhado", "cunhada", 
                 "marido", "esposo", "mulher", "esposa", "noivo", "noiva", 
                 "namorado", "namorada", "av�", "av�", "bisav�", "bisav�", 
                 "sobrinho", "sobrinha", "sogro", "sogra", "genro", "nora",
                 "enteado", "enteada", "entiado", "entiada", # (SIC)
                 "companheiro", "companheira", "parceiro", "parceira",
                 "compadre", "comadre", "cumpadre", "cumadre", #(SIC)
                 "amigo", "amiga", "vizinho", "vizinha", "ex", #, "si"
                 "filho", "filha"
  )
  
  # Faz a an�lise morfol�gica, encontrando tokens e posTags.
  #
  result <- PartsOfSpeech(x, index)
  if (result$origin == "") return(result)
  
  # Procura por rela��es de fam�lia na lista de tokens.
  #
  indexes <- which(result$tokens %in% relations, arr.ind = TRUE)
  relations <- result$tokens[indexes]
  result$relations <- unique(unlist(relations))
  
  # Limpa o POS do resultado antes de retornar.
  result$tokens <- NULL
  result$posTags <- NULL
  
  return(result)
}