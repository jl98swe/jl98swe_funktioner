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
      content <- rawToChar(response$content)
      eval(parse(text = content), envir = parent.frame())
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
