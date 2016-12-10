###
## UNIVERSIDADE DE FORTALEZA - UNIFOR. DEPARTAMENTO DE PÓS-GRADUAÇÃO EM INFORMÁTICA APLICADA.
## MESTRADO EM INFORMÁTICA APLICADA, TURMA 19. 
## DISCIPLINA: PROCESSAMENTO DE LINGUAGEM NATURAL (PLN) - PROFA. VLÁDIA.
## TRABALHO 1 DE PLN, OUTUBRO DE 2016.
## EQUIPE: Djacir, Jônatas e Jonas
#

## Preparando o ambiente R para fazer PLN.
#
install.packages("ctv")
library("ctv")
install.views(views="NaturalLanguageProcessing")

## Importando o corpus para o R.
#
# Forma o caminho para o arquivo corpus.txt
cname <- file.path("./data")
fname <- file.path(cname, "corpus.txt")
fname # exibe o caminho completo do arquivo.

# Faz a leitura de todas as linhas de corpus.txt
docs <- readLines(fname, encoding = "UTF-8")

# Verifica a leitura do arquivo. Exibe o conteúdo de docs
# Resultado é um vetor de linhas de texto.
docs

## Expurgando conteúdo sem interesse para a análise.
#
docs <- sub("Proc: P([0-9]{6})/([0-9]{4})", "", docs, perl=TRUE)

## Importando tabela de lemas gerada pelo TreeTagger.
#
# Definir o caminho para o arquivo tagged-corpus.csv
csvname <-file.path(cname, "tagged-corpus.csv")
# Ler o arquivo CSV, criando um dataframe
csv <- read.csv2(csvname, header = FALSE, sep = "\t",  encoding = "UTF-8")
# Retira duplicatas do dataframe
lemmadf <- unique(csv)

## Carrega função de lematização.
#
source("lemma-function.r")

## Lematizando o corpus
#
# Gera novo vetor de texto, lematizado.
ldocs <- lemma(docs, lemmadf)

# Carrega a biblioteca de mineração de texto tm
library(tm)

## Criando corpus a partir das linhas de texto.
#
# Gera o corpus a partir das linhas lidas e lematizadas.
# Cada linha corresponderá a um documento
corpus <- Corpus(VectorSource(ldocs))

# Verifica a leitura e geração do corpus.
corpus
# Exemplo de saída: 
# <<VCorpus>>
# Metadata:  corpus specific: 0, document level (indexed): 0
# Content:  documents: 672

## Preprocessamento final.
#
# Remover a palavra "card" adicionada pelo TreeTagger como um marcador
corpus <- tm_map(corpus, removeWords, c("card"))
# Converter o corpus para letras minúsculas.
corpus <- tm_map(corpus, content_transformer(tolower))
#Remover a pontuação do corpus.
corpus <- tm_map(corpus, removePunctuation)
#Remover espaços vazios do corpus.
corpus <- tm_map(corpus, stripWhitespace)
# Remover "stopwords" da língua portuguesa.
corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))

## Extraíndo -gramas.
#
# Gerar a matrix termo-documento, extraindo unigramas.
tdm <- TermDocumentMatrix(corpus)
# Carrega a biblioteca RWeka, que deve ter sido instalada previamente.
library(RWeka)
# Define o Tokenizador para Bigrama
BiTokenizer <- function(x) 
    NGramTokenizer(x, Weka_control(min = 2, max = 2)) # 2 = bigramas
# Gera a matriz termo X documento.
tdm2 <- TermDocumentMatrix(corpus, control = list(tokenize = BiTokenizer))
# Verificando matrix termo-documento para unigramas.
tdm
# Exemplo de resultado: 
# <<TermDocumentMatrix (terms: 2624, documents: 672)>>
# Non-/sparse entries: 22921/1740407
# Sparsity           : 99%
# Maximal term length: 38
# Weighting          : term frequency (tf)

# Verificando matrix termo-documento para bigramas.
tdm2
# <<TermDocumentMatrix (terms: 8431, documents: 672)>>
# Non-/sparse entries: 27488/5638144
# Sparsity           : 100%
# Maximal term length: 45
# Weighting          : term frequency (tf)

## Gerando gráficos de frequências.
# 
# Soma as frequências
freq = colSums(t(as.matrix(tdm)))
freq2 = colSums(t(as.matrix(tdm2)))
# Ordena as frequências em ordem decrescente
ord = order(freq, decreasing = TRUE)
ord2 = order(freq2, decreasing = TRUE)
# Filtra os cem mais frequentes
dt = freq[ord][1:100]
dt2 = freq2[ord2][1:100]
# Converte em dataframes
df <- data.frame(words=names(dt), freq=dt)
df2 <- data.frame(words=names(dt2), freq=dt2)
# Cria uma coluna pelo nome do vetor dado.
df$Col1 <- factor(rownames(df), levels=rownames(df))
df2$Col1 <- factor(rownames(df2), levels=rownames(df2))

# Carrega a biblioteca de geração dos gráficos.
library(ggplot2)

# Gera o gráfico dos 100 mais para os unigramas
p <- ggplot(data=df, aes(x=Col1, y=dt)) 
p <- p + geom_bar(stat = "identity") 
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p <- p + xlab("Termo") + ylab("Frequência")
# Exibe o gráfico de unigramas
p

# Gera o gráfico dos 100 mais para os bigramas
p2 <- ggplot(data=df2, aes(x=Col1, y=dt2)) 
p2 <- p2 + geom_bar(stat = "identity") 
p2 <- p2 + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p2 <- p2 + xlab("Termo") + ylab("Frequência")
# Exibe o gráfico de bigramas
p2

## Gerando espaço LSA.
#
library(lsa)
# Define a função de peso para TF-IDF
Weighting <- function(x) weightTfIdf(x, normalize = FALSE)
# Gera a matriz de termos para unigramas
terms <-DocumentTermMatrix(corpus, control = list(weighting = Weighting))
# Gera a matriz de termos para bigramas
terms2 <-DocumentTermMatrix(corpus, control = list(weighting = Weighting, tokenize = BiTokenizer))
# Gera o espaço LSA para unigramas
lsaspace <- lsa(terms)
# Gera o espaço LSA para bigramas
lsaspace2 <- lsa(terms2)

## Gerando matrizes LSA
#
# Matriz de distribuição dos termos no espaço LSA.
lsamatrix <- as.textmatrix(lsaspace)
lsamatrix2 <- as.textmatrix(lsaspace2)

## Gráficos de correlação
#
# Geração de gráficos de correlação entre os termos. 
plot(cor(lsamatrix), ann = FALSE)
plot(cor(lsamatrix2), ann = FALSE)
