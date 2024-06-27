# Funktion som returnerar det första elementet från en vektor. Kan även ta bort elementet från vektorn i den globala miljön.
plocka_forsta_element <- function(vektor, taBort = TRUE) {
  
  # Hämta namnet på vektorn som en sträng.
  vektorNamn <- deparse(substitute(vektor))
  
  # Hämta vektorn från det globala miljön.
  vektor <- get(vektorNamn, envir = .GlobalEnv)
  
  # Kontrollera om vektorn är tom.
  if (length(vektor) == 0) {
    print("Vektorn är tom")
    return(NULL) # Returnera NULL om vektorn är tom.
  }
  
  # Ta ut det första elementet.
  forstaElement <- vektor[1]
  
  # Ta bort det första elementet från vektorn om taBort är TRUE.
  if (taBort) {
    vektor <- vektor[-1]
    
    # Uppdatera vektorn i det globala miljön.
    assign(vektorNamn, vektor, envir = .GlobalEnv)
  }
  
  # Returnera det första elementet.
  return(forstaElement)
}
