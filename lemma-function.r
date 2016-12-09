library(stringr)

## Função para extrair o lema das palavras de dado documento.
lemma <- function(x, dataframe) {
  
  # Função interna, de substituição de determinado padrão.
  subst <- function (x, pattern, replacement) {
    out <- tryCatch(
      {
        # Retira '.' e '*' da expressão procurada para evitar erros de regex.
        pattern <- gsub("\\.", "", pattern, perl = TRUE)                
        pattern <- gsub("\\*", "", pattern, perl = TRUE)
        
        if (nchar(pattern) < 2) { 
          # Desconsidera padrões nulos ou de tamanho 1.
          x
        } else {
          # Redefine o padrão buscado para procurar apenas por palavra inteira.
          pattern <- paste0("\\b", pattern, "\\b")
          
          # Substitui todas as ocorrências encontradas.
          gsub(pattern, replacement, x, perl = TRUE)
        }
      },
      error = function(cond) {
        return (x)
      }, 
      warning = function(cond) {
        return (x)
      }
    )
    return (out)
  }
  
  # Converte a entrada para a caixa baixa.
  result <- str_to_lower(x, locale = "UTF-8")
  
  # Para cada linha no dataframe lematizador, faz a substituição.
  for (i in 1:nrow(dataframe)) {
    pattern <- str_to_lower(dataframe[[i,1]])
    result <- subst(result, pattern, dataframe[[i,3]])
  }
  
  # Retorna o resultado.
  return(result)
} ## function lemma
