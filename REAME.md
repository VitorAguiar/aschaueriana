Open an R session and run the following commands to install required packages:

``` r
install.packages(c("devtools", "magrittr"))

devtools::install_github("hadley/dplyr", "hadley/ggplot2", "hadley/plyr", "hadley/tidyr", "hadley/readr" ,"hadley/readxl")
```

Make sure you create a directory with subdirectories named "R", "data" and "plots". Put the R scripts in the subdirectory "R" and the input data in the "data" subdirectory.

Execute the scripts from subdirectory "R".

    ##  [1] "/Users/vitoraguiar/aschaueriana2"     
    ##  [2] "├── R"                                
    ##  [3] "│   ├── formatar_dados_climaticos.R"  
    ##  [4] "│   ├── formatar_dados_morfologicos.R"
    ##  [5] "│   ├── plotar_dados_climaticos.R"    
    ##  [6] "│   └── plotar_dados_morfologicos.R"  
    ##  [7] "├── data"                             
    ##  [8] "│   ├── FLORIANOPOLIS_SC.xlsx"        
    ##  [9] "│   ├── SALINOPOLIS_PA.xls"           
    ## [10] "│   ├── SANTA_MARTA_SC.xlsx"          
    ## [11] "│   ├── dados_morfologicos.xlsx"      
    ## [12] "│   └── medidas.xlsx"                 
    ## [13] "└── plots"                            
    ## [14] ""                                     
    ## [15] "3 directories, 9 files"
