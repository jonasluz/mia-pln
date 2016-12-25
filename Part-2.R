## Part-2.R 
#
# Requer a execução prévia do Part-1.R ou leitura dos dados de ambiente R.
#
# ##

#setwd("C:\\Users\\jonas\\JONAS\\MIA\\mia-pln")

# Prepara a lista de documentos para tratamento.
#
# Retira identificação de processos.
docs.clear <- sub(docs.text, pattern = "Proc: P([0-9]{6})/([0-9]{4})", replacement = "", perl=TRUE)
# Converte para caixa baixa.
docs.lower <- stri_trans_tolower(docs.clear)
# Corrige pontuação, em função de se ter detectado erros crassos neste aspecto.
docs.lower <- gsub(docs.lower, pattern = "[.]", replacement = ". ", perl = TRUE, fixed = FALSE)
docs.lower <- gsub(docs.lower, pattern = "[,]", replacement = ", ", perl = TRUE, fixed = FALSE)
docs.lower <- gsub(docs.lower, pattern = "sr:", replacement = "sr. ", perl = TRUE)
docs.lower <- gsub(docs.lower, pattern = "sra:", replacement = "sra. ", perl = TRUE)
# Retira espaços extras.
docs.lower <- gsub(trimws(docs.lower), pattern = "\\s+", replacement = " ")

# Carrega ligações e funções de uso da Rosette API.
source(".\\Rosette.R", encoding = "UTF-8")
# Carrega funções de busca por relações entre as pessoas.
source(".\\Relatives.R", encoding = "UTF-8")

# PROCESSAMENTO DO CORPUS COM EXTRAÇÃO DAS ENTIDADES NOMEADAS 
# E RELAÇÕES ENTRE AS PESSOAS.
# 
library(foreach)
length <- length(docs.lower)
result <- foreach(i = 1 : length) %do% 
  unlist(list( EntitiesFrom(docs.lower[[i]], index = i),
         list( relations = FindRelations(docs.lower[[i]], index = i)$relations)
            ), recursive = FALSE
         )
# ##

# GERAÇÃO DA SAÍDA EM PLANILHA XLSX.
# @see http://www.sthda.com/english/wiki/r2excel-read-write-and-format-easily-excel-files-using-r-software
# 

# Instala o pacote necessário: r2Excel
#
library(rJava)
install.packages("devtools")
devtools::install_github("kassambara/r2excel")

## Faz exportação
#
ExportResult(result, TRUE)
