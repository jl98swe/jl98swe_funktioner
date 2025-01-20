kombinera_filer_func <- function(
    indata_sokvag,                           # Sökväg till katalogen som innehåller filerna.
    utdata_sokvag = NULL,                    # Sökväg för att spara utdatafilerna (valfritt, standard är att inte spara).
    fil_monster = "\\.(xlsx|xls|csv)$",      # Mönster för att matcha Excel- och CSV-filer (standard).
    kolumn_typer = "text",                   # Kolumntyper vid inläsning av Excel-filer.
    kolumn_enkla_namn = TRUE,                # Om kolumnnamnen ska rensas från versaler, ovanliga tecken, mellanrum osv.
    spara_excel = FALSE,                     # Spara kombinerade data som en Excel-fil (standard är FALSE).
    flera_blad = FALSE,                      # Spara i separata blad om TRUE, eller en enda tabell annars.
    spara_csv = FALSE,                       # Spara kombinerade data som en CSV-fil (standard är FALSE).
    csv_filnamn = "sammanstallt_data.csv",   # Filnamn för CSV-filen.
    excel_filnamn = "sammanstallt_data.xlsx" # Filnamn för Excel-filen.
) {
  # -------------------------------------------------------------------
  # Läser in flera filer av samma typ från en mapp och kombinerar dessa, i regel Excel- och CSV-filer.
  # -------------------------------------------------------------------
  #
  # Indata:
  # - En katalog med Excel- och/eller CSV-filer som matchar ett angivet mönster (fil_monster).
  # - Valfria parametrar för att spara resultatet som Excel eller CSV.
  #
  # Utdata:
  # - En kombinerad data frame med innehållet från alla Excel- och CSV-filer.
  # - (Valfritt) Sparar kombinerade data till en Excel-fil (med separata blad eller en samlad tabell).
  # - (Valfritt) Sparar kombinerade data som en CSV-fil.
  # -------------------------------------------------------------------
  
  # Ladda nödvändiga paket.
  if (!require("readxl")) install.packages("readxl")        # För att läsa in xls- och xlsx-filer.
  if (!require("writexl")) install.packages("writexl")      # För att skriva xlsx-filen. 
  if (!require("tidyverse")) install.packages("tidyverse")  # För datamanipulering och för att läsa och skriva CSV-filer.
  if (!require("janitor")) install.packages("janitor")      # För att förenkla kolumnnamnen.
  
  # Läs in filvägar som matchar mönstret.
  filer_sokvag <- list.files(path = indata_sokvag, 
                            pattern = fil_monster, 
                            full.names = TRUE)
  
  # Kontrollera om filer hittas.
  if (length(filer_sokvag) == 0) {
    stop("Inga filer hittades som matchar mönstret: ", fil_monster)
  }
  
  # Initiera en tom lista och en data frame.
  data_list <- list()
  sammanstallt_data <- data.frame()
  
  # Loopa igenom filerna.
  for (fil in filer_sokvag) {
    # Extrahera filnamnet (utan filändelse) för att använda som bladnamn.
    bladnamn <- tools::file_path_sans_ext(basename(fil))
    
    # Läs in filen baserat på dess typ.
    if (grepl("\\.csv$", fil, ignore.case = TRUE)) {
      # Läs in CSV-filen.
      data <- read_csv(fil, col_types = cols(.default = "c")) # Alla kolumner som text.
    } else {
      # Läs in Excel-filen.
      data <- read_excel(fil, col_types = kolumn_typer)
    }
    
    # Lägg till en kolumn för filens namn (för spårbarhet).
    data$kallfil <- bladnamn
    
    # Lägg till data i listan.
    data_list[[bladnamn]] <- data
    
    # Kombinera till en gemensam data frame.
    sammanstallt_data <- bind_rows(sammanstallt_data, data)
  }
  
  # Förenkla kolumnnamnen.
  if (kolumn_enkla_namn) {sammanstallt_data <- sammanstallt_data %>% clean_names()}
  
  # Spara data om användaren har valt det.
  if (!is.null(utdata_sokvag)) {
    if (spara_excel) {
      # Skapa en komplett Excel-fil.
      if (flera_blad) {
        write_xlsx(data_list, path = file.path(utdata_sokvag, excel_filnamn))
      } else {
        write_xlsx(list(KombineradData = sammanstallt_data), path = file.path(utdata_sokvag, excel_filnamn))
      }
    }
    if (spara_csv) {
      # Spara till en CSV-fil.
      write_csv(sammanstallt_data, file = file.path(utdata_sokvag, csv_filnamn))
    }
  }
  
  # Returnera den kombinerade data framen.
  return(sammanstallt_data)
}
