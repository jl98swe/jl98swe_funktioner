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

plot_bef_pyramid <- function(df, varde, aldersgrupp, kon, antal_breaks = 9){
  # Funktion för att rita en befolkningspyramid baserat på angivna variabler i en data frame.
  # 
  # Input:
  #   df:           En data frame innehållande variablerna för ålder, kön och befolkning.
  #   varde:        Namnet på kolumnen i df som innehåller befolkningens värden.
  #   aldersgrupp:  Namnet på kolumnen i df som representerar åldersgruppen.
  #   kon:          Namnet på kolumnen i df som representerar kön.
  #   antal_breaks: Ungefärligt antal breaks längs x-axeln. pretty() har n = 5 som default-värde.
  # 
  # Output:
  #   En ggplot2-figur som visar befolkningspyramiden.
  
  # Konvertera inmatade variabler till teckensträngar om de inte redan är det.
  varde <- as.character(substitute(varde))
  aldersgrupp <- as.character(substitute(aldersgrupp))
  kon <- as.character(substitute(kon))
  
  # Beräkna intervall för befolkningens värden.
  pop_intervall <- range(df[[varde]])
  pop_granser <- c(-max(abs(pop_intervall)), max(abs(pop_intervall)))
  
  # Skapa en sekvens av befolkningsvärden.
  x_varden <- pretty(pop_granser, n = antal_breaks)
  
  # Plotta med ggplot2.
  ggplot(df,
         aes_string(x = varde,  # aes_string används för att hantera variablerna som strängar.
                    y = aldersgrupp,
                    fill = kon)) +  # Färgkodning baserat på kön.
    geom_col(width = .8) +  # Skapa stapeldiagram med små mellanrum.
    scale_x_continuous(breaks = x_varden,
                       labels = scales::comma(abs(x_varden), big.mark = ".", decimal.mark = ","),  # Lägg till punkt som tusentalsavgränsare.
                       limits = if (min(x_varden) * 0.90 < min(range(df[[varde]])) &  # Justera gränserna för x-axeln.
                                    max(x_varden) * 0.90 > max(range(df[[varde]]))) {
                         c(min(x_varden) * 0.90, max(x_varden) * 0.90)
                       } else {
                         c(min(x_varden), max(x_varden))
                       }
    ) +
    theme(legend.title = element_blank())
}


