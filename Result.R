library(r2excel)

InsertRecord <- function(x, wb, sheet) 
{
  # Insere os dados.
  #
  index <- x$index[[1]]
  print(index)
  relation <- if(length(x$relations) == 0) "NA" else x$relations[[1]]
  lowHeader <- data.frame(index, relation)
  xlsx.addTable(wb, sheet, lowHeader, startCol = 1, columnWidth = 30)
  #xlsx.addLineBreak(sheet, 1)
  
  text <- if (x$origin == "") "NA" else x$origin
  xlsx.addParagraph(wb, sheet, value = text, isItalic = TRUE, fontSize = 10,
                    colSpan = 5, rowSpan = 4)
  #xlsx.addLineBreak(sheet, 1)
  if (is.null(x$entities))
    xlsx.addParagraph(wb, sheet, value = "NÃO FORAM IDENTIFICADAS ENTIDADES", 
                      fontSize = 10, fontColor = "darkgray", isItalic = TRUE, isBold = TRUE,
                      colSpan = 5, rowSpan = 1)
  else 
  {
    data <- as.data.frame(x$entities)
    xlsx.addTable(wb, sheet, data, startCol = 1, columnWidth = 30)
  }
  xlsx.addLineBreak(sheet, 2)
}

ExportResult <- function(x, open = FALSE) 
{
  # Cria a planilha Excel. 
  #
  xls.filename <- "data\\result.xlsx"
  xls.workbook <- createWorkbook(type = "xlsx")
  xls.sheet <- createSheet(xls.workbook, sheetName = "Resultados")
  
  # Adiciona um cabeçalho
  #
  xlsx.addHeader(xls.workbook, xls.sheet, value = "Trabalho 2 de PLN - Resultados", 
                 level = 1, color = "black", underline = 1)
  xlsx.addLineBreak(xls.sheet, 1)

  # Adiciona registros.
  #
  
  lapply(x, xls.workbook, xls.sheet, FUN = InsertRecord)
  
  # Salva planilha.
  #
  saveWorkbook(xls.workbook, xls.filename)
  if(open) xlsx.openFile(xls.filename)
}
