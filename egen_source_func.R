## Ladda paket.
if (!require("httr")) install.packages("httr")

egen_source <- function(url, ...) {
  # Funktion för att arbeta runt felmeddelandet "SSL connect error" som kan fås
  # av source() vid anrop av Github-länkar.
  # egen_source() ersätter base::source(), men den har endast ett arbetssätt som
  # skiljer sig från den ursprungliga funktionen när en github-länk skickas som
  # argument.
  
  if (grepl("^https://raw.githubusercontent.com", url)) {
    response <- GET(url)
    if (status_code(response) == 200) {
      
      # Hämta innehållet som text.
      content <- rawToChar(response$content)
      
      # Dela upp innehållet radvis.
      lines <- strsplit(content, "\n")[[1]]
      
      # Trimma varje rad för att ta bort osynliga tecken som mellanslag och tabb.
      trimmedLines <- trimws(lines)
      
      # Filtrera bort alla rader som börjar med "# -" då det uppstod problem när "fjoin_med_klartext2.R" skulle laddas in.
      cleanedLines <- trimmedLines[!grepl("^# -", trimmedLines)]
      
      # Sätt ihop de återstående raderna igen.
      cleanedContent <- paste(cleanedLines, collapse = "\n")
      
      # Tolka och kör den städade koden.
      eval(parse(text = cleanedContent), envir = parent.frame())
      
    } else {
      stop(paste("Fel vid hämtning av fil. Statuskod:", status_code(response)))
    }
  } else {
    # Om det inte är en GitHub URL, använd den vanliga source() funktionen
    base::source(url, ...)
  }
  
  # OBS: Lägg till denna kod i ditt script efter du laddat in denna funktion.
  # assignInNamespace("source", egen_source, ns = "base")
}
