### Funktioner för att skapa en befolkningspyramid.

## Ladda paket.
if (!require("tidyverse")) install.packages("tidyverse")

forbered_bef_pyramid_func <- function(data, Kon_kolumn, Varde_kolumn){
  # Funktion för att dela skapa negativa värden för kvinnor och positiva för män i skapandet av en befolkningspyramid.
  
  # Ser till att det både går att skicka namnet på en kolumn med eller utan "". Svårt att förstå hur det fungerar.
  Kon_kolumn <- ensym(Kon_kolumn)     # Konvertera Kon_kolumn till symbol.
  Varde_kolumn <- ensym(Varde_kolumn) # Konvertera Varde_kolumn till symbol.
  
  # Returnera 'data' där 'Varde_kolumn' är negativ för kvinnor och fortfarande positiv för män.
  data %>%
    group_by(!!Kon_kolumn) %>%         # Gruppera efter Kon_kolumn (konvertera till symbol med !!).
    mutate(
      multiplikator = cur_group_id(),  # Skapa 'multiplikator' baserat på gruppens id vilket skapas alfabetiskt. 1 - Kvinnor, 2 - Män.
      multiplikator = case_when(
        multiplikator == 2 ~ 1,        # Män får värdet 1 och kvinnor får värdet -1.
        multiplikator == 1 ~ -1,
        TRUE ~ 1  # Gruppen med totalt förblir oförändrad.
      )
    ) %>%
    ungroup() %>%                   # Kom ihåg att alltid avgruppera!
    mutate(!!Varde_kolumn := !!Varde_kolumn * multiplikator) %>%  # Uppdatera Varde_kolumn med multiplikatorn.
    select(-multiplikator) # Ta bort multiplikatorn från data framen.
}
