
#https://nacnudus.github.io/spreadsheet-munging-strategies/tidy-clean.html
#https://github.com/nacnudus/unpivotr
#https://github.com/nacnudus/tidyxl


devtools::install_github("nacnudus/unpivotr", build_vignettes = TRUE)


library(tidyxl)
library(unpivotr)
library(rdrop2)

library(dplyr)
library(tidyverse)

#library(chron)
library(datasets)
library(dygraphs)
library(ggplot2)
library(DT)
library(corrplot)
library(psych)
library(plot3D)
library(scales)
library(stargazer)
library(xts)

library(data.table)
library(RColorBrewer)

library(Hmisc)
library(readxl)
library(tidyxl)
library(unpivotr)

x <- paste()


x <- drop_search("xlsx")


x <- paste(substring(x, 8, 50))



my_files <- x

xx1<- drop_download(my_files$matches[[1]]$metadata$path_lower, 
                    overwrite = TRUE)

xlsname <- my_files$matches[[1]]$metadata$path_lower

name2 <- paste(substring(xlsname, 8, 50))



xlsdat<- xlsx_cells(name2, sheet = "Raw Data")

searchfor <- 
  pit <- print("08-ambatchnb-229.xls")
  searchfor <- paste(pit)

  x <- pit
  pit <- sprintf("%s", x)
  pit
  
  

searchfor <- sprintf('08-am')
print(searchfor)


x <- xlsdat[xlsdat$data_type == "numeric", c("address", "numeric")]

x1 <-xlsdat[xlsdat$col == 10, c("address", "character", "numeric")]

xx <- xlsdat %>%
  filter(row %in% 19:102, col %in% 1:25) %>%
  filter(is_blank == FALSE) %>%
  
  #filter(data_type == "numeric")
  behead("N", header) %>%
  filter(header == "AFDW") %>%
  select(row, col, data_type, header, character, numeric, date)
  

x2 <- xx %>%
  #filter(between(row, top_left$row, bottom_right$row),
   #      between(col, top_left$col, bottom_rightcol)) %>%
  behead("N", header) %>%
  select(row, col, data_type, header, character, numeric) %>%
  #behead("E", header) %>%
  select(-col) %>%
  spatter(header) %>%
  select(-row)

#cells <- tidy_table(xx, col_names = TRUE)
  
  
my_files <- drop_search('xlsx')
xx1<- drop_download(my_files$matches[[1]]$metadata$path_lower, 
                    overwrite = TRUE)
xlsname <- my_files$matches[[1]]$metadata$path_lower
name2 <- paste(substring(xlsnames, 8, 50))
xlsdat<- xlsx_cells(name2, sheet = "Raw Data") 




readxlsdata <- function() {
  my_files <- drop_search('xlsx')
  xx1<- drop_download(my_files$matches[[1]]$metadata$path_lower, 
                      overwrite = TRUE)
  xlsname <- my_files$matches[[1]]$metadata$path_lower
  name2 <- paste(substring(xlsname, 8, 50))
  xlsdat<- xlsx_cells(name2)
  xlsdat
}



 
    
  dfx <- readxlsdata()
  
 
    df <- dfx %>% filter(sheet == "Raw Data") %>%
      filter(row %in% 18:102, col %in% 1:25) %>%
      behead("N", header) %>%
      select(header, data_type)
    
    df1 <- unique(df) %>%
      filter(header == "AFDW") %>%
      spread(header, data_type) 
      
    
    
  
  
    df1 <- dfx %>%
      filter(sheet == "Raw Data") %>%
      filter(row %in% 18:102, col %in% 1:25) %>%
      behead("N", header) %>%
      filter(header == "AFDW"| header == "Date" | header == "Depth") %>%
      filter(col == 11) %>%
      select(row, col, is_blank, error, numeric, date, character)
    
    
      
    
             
             
             | header == "Date" | header == "Depth") %>%
      spread(header, numeric)%>%
      select(AFDW)
      
    
    df2 <- df %>%
      filter(header == "AFDW") %>%
      spread(header, date) %>%
      select(AFDW)
    
    df3 <- df %>%
      filter(header == "AFDW") %>%
      spread(header, character) %>%
      select(AFDW)
    
    df4 <- df %>%
      filter(header == "AFDW") %>%
      spread(header, is_blank) %>%
      select(AFDW)
    
    dft <- cbind(df1, df2, df2, df3, df4) %>%
      
    
    
  }
  
  testme <- tabledf(df)
  
    df1 <- df %>% filter(sheet == "Raw Data") %>%
    filter(row %in% 18:102, col %in% 1:25) %>%
    filter(is_blank == FALSE) %>%
      #filter(data_type == "numeric")
      behead("N", header) %>%
      filter(header == "AFDW") %>%
      select(row, col, data_type, header, character, numeric, date) %>%
      spread(header, numeric)
      
    
    
       df2 <- dfx %>% filter(sheet == "Raw Data") %>%
       filter(row %in% 18:102, col %in% 1:25) %>%
       filter(is_blank == FALSE) %>%
       behead("N", header) %>%
       behead("N", header2) %>%  
       select(header, header2, data_type) 
       
       df3 <- unique(df2) %>%
         unite(col = newheader, header, header2, sep ="_") %>%
         
         arrange(newheader)
         
   dfq <- dfx %>%
     glimpse(dfx)
        
  df3 <- cbind(df1, df2)
  
 question <- as.data.frame(df1) 
 question1 <- question %>% rectify(question)
  
 
 
 df <- dfx %>% 
   select(sheet)
  df1 <- unique(df)
 
  df <- dfx %>%
    count()
 
 
  