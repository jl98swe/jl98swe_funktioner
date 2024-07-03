### Gör flera bokslut från Bisnisanalys redo för analys genom att kombinera
### dem till samma data frame med endast ett urval av nyckeltalen.

## Ladda paket.
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("readxl")) install.packages("readxl")
if (!require("writexl")) install.packages("writexl")
if (!require("tools")) install.packages("tools")

bearbeta_bokslut <- function(filNamn, nyckeltal) {
  # Funktion för att läsa Excel-filer, lägga till filnamn som en kolumn, filtrera rader och transponera.
  # Indata:
  #   filNamn     Namn på filsökväg inklusive filnamnet (organisationsnumret) plus filtillägget.
  #   nyckeltal   Vektor som innehåller alla nyckeltal av intresse.
  # Utdata:
  #   df          Data frame med kolumner innehållande företagsinformation, årtal och nyckeltal.

  # Identifiera filtyp baserat på filändelse
  filtyp <- tools::file_ext(filNamn)
  
  # Läs fil baserat på filtyp.
  df <- switch(filtyp,
               "xlsx" = read_excel(filNamn, col_names = FALSE),
               "xls" = readxl::read_excel(filNamn, col_names = FALSE),
               "csv" = read.csv(filNamn, header = FALSE),
               "txt" = read.delim(filNamn, header = FALSE),
               stop("Ogiltig filtyp. Stödda typer är 'xlsx', 'csv', och 'txt'.")
  ) %>% suppressMessages()
  
  # Hämta namnet som alltid återfinns på rad 1 kolumn 2.
  namn <- df[1, 2] %>% as.character()
  print(namn)
  
  # Gör så att kolumnnamnen blir årtalen från rad 7, men undantag för den första kolumnen som är nyckeltalen.
  colnames(df) <- c("Nyckeltal", df[7, 2:ncol(df)])
  
  # Ta bort dom 7 första raderna från 'df' (6 tomma rader och den sjunde med årtalen).
  df <- df[-c(1:7), ]
  
  # Extrahera organisationsnummer utan tillägg.
  org_nr <- file_path_sans_ext(basename(filNamn))
  
  # Filtrera rader så att endast de som finns i 'nyckeltal' behålls.
  df <- df %>%
    filter(.[[1]] %in% nyckeltal) # Från första kolumnen.
  
  # Transponera data frame.
  df <- as.data.frame(t(df))
  
  # Sätt första raden som kolumnnamn.
  colnames(df) <- df[1, ]
  
  # Konvertera alla kolumner i 'nyckeltal' till numeriska.
  df[] <- lapply(df, as.numeric)
  
  # Lägg till kolumner med företagets namn och organisationsnummer.
  df <- cbind(org_nr, namn, df)
  df <- df %>%
    rename(Organisationsnummer = org_nr,
           Namn = namn)
  
  # Lägg till en kolumn för årtalet.
  df <- df %>%
    # Gör först om radnamnen (där datumen hamnade efter transponeringen) till datum för att på nästa rad extrahera endast årtalet från datumet samt göra om det till ett numeriskt värde.
    mutate(År = as.Date(rownames(df), format = "%Y-%m-%d"), .after = Namn,
           År = format(År, "%Y") %>% as.numeric())
  
  # Ta bort första raden eftersom den blir konstig vid transponering.
  df <- df[-1, ]
  
  # Gör df lång istället för bred. Första tre kolumnerna undantas.
  df <- df %>%
    pivot_longer(cols = -c(1:3), names_to = "Nyckeltal", values_to = "Värde")
  
  return(df)
}

sammanstall_bokslut <- function(arbetskatalog = "~/FoI-projekt/Bokslut",
                                nyckeltal = c("Nettoomsättning", "Årets resultat", "Antal anställda"),
                                utmapp = "~/FoI-projekt/",
                                sparaExcel = FALSE,
                                filtyp = ".xlsx") {
  ## Arbetsordning:
  ## Först skapas bearbeta_bokslut(), sedan skapas en vektor av de önskade nyckeltalen 
  ## och en lista av alla filer. Vektorn och listan används i sin tur som
  ## argument i bearbeta_bokslut(). Resultatet blir flera data frames som innehåller;
  ## organisationsnummer, företagsnamn, årtal, nyckeltalen som önskas samt värdena
  ## för nyckeltalen. Dessa slås sedan ihop till en data frame i den här funktionen
  ## som returneras, men som också kan skrivas till utmappen.

  ## OBS: Alla filer måste ligga i samma mapp. Alla filer av vald filtyp kommer
  ## att läsas in så låt inte överflödiga filer av samma filtyp ligga i samma mapp.
  
  # Ladda lista med namnen på alla filer med filtillägg 'filtyp'.
  file_list <- list.files(path = arbetskatalog,
                          pattern = paste0("*", filtyp))
  
  # Lägg till filsökvägen (arbetskatalogen) till listan med namn på filer.
  path_list <- paste0(arbetskatalog, "/", file_list)
  
  # Läs och bearbeta Excel-filer.
  df_list <- lapply(path_list, bearbeta_bokslut, nyckeltal = nyckeltal)
  
  # Kombinera alla data frames till en enda data frame.
  df <- do.call(rbind, df_list)
  
  # Återställ radnamnen.
  rownames(df) <- NULL
  
  ## Spara data frame som en fil av vald filtyp.
  if (sparaExcel) {
    write_xlsx(df,
               path = paste0(utmapp, "Sammanställning", filtyp)) # "Sammanställning..." är det valda namnet på filen.
  }
  
  return(df)
}
