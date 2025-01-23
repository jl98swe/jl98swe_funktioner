skapa_paj_donutdiagram <- function(data, 
                                   x_kolumn, 
                                   y_kolumn, 
                                   farg = "gray1",
                                   farg_gradient = NULL,
                                   motsols = TRUE,
                                   sortera_storst_till_minst = FALSE,
                                   storlek_hal = 50,
                                   storlek_text = 4,
                                   flytta_siffror_utat = 1.8, 
                                   placering_legend = "none",
                                   titel = "",
                                   titel_legend = NULL) {
  
  # -------------------------------------------------------------------
  # Skapar ett paj- eller donutdiagram.
  # -------------------------------------------------------------------
  #
  # Indata:
  #   - data: Data frame som innehåller nödvändiga kolumner för kategorier och värden.
  #   - x_kolumn: (Sträng) Namnet på kolumnen för kategorier i diagrammet.
  #   - y_kolumn: (Sträng) Namnet på kolumnen för värden som bestämmer kategoriernas storlek.
  #   - farg: (Sträng eller hexkod) Grundfärgen som segmenten tonas från (t.ex. "blue" eller "#1f78b4").
  #   - farg_gradient: (Vektor med minst två färgsträngar) Skapar en färggradient mellan angivna färger. Finns ingen maxgräns för antalet färger.
  #   - motsols: (Boolean) Om diagrammet ska sorteras motsols (TRUE) eller medsols (FALSE). Standard = TRUE.
  #   - sortera_storst_till_minst: (Boolean) Om diagrammet ska sorteras fallande efter värde i 'y_kolumn' (TRUE) eller efter ordningen på kategorier (FALSE). Standard = FALSE.
  #   - storlek_hal: (Numerisk) Storleken på hålet. Värdet 0 motsvarar ett pajdiagram. Standard = 50.
  #   - storlek_text: (Numerisk) Textstorlek för kategoriernas etiketter. Standard = 4.
  #   - flytta_siffror_utat: (Numerisk) Hur långt ut etiketter placeras från diagrammets centrum. Standard = 1.8.
  #   - placering_legend: (Sträng) Placering av legenden. Kan välja mellan "none" (ingen legend), "left", "right", "bottom", "top", "inside". Standard = "none".
  #   - titel: (Sträng) Titel för diagrammet. Standard är en tom sträng ("").
  #   - titel_legend: (Sträng eller NULL) Titel för legenden. Standard är NULL, och om inte angett sätts det till x_kolumn med stor bokstav.
  #
  # Utdata:
  #   - Ett ggplot-objekt som representerar ett paj- eller donutdiagram.
  # -------------------------------------------------------------------
  
  # Ladda nödvändiga paket.
  if (!require("tidyverse")) install.packages("tidyverse")       # För datamanipulering.
  if (!require("colorspace")) install.packages("colorspace")     # För färghantering.
  
  # ------------ Felhantering ------------
  
  # Kontrollera om kolumnerna finns i data.
  if (!(x_kolumn %in% colnames(data)) || !(y_kolumn %in% colnames(data))) {
    stop("Ange giltiga kolumnnamn för 'x_kolumn' och 'y_kolumn'.")
  }
  
  # Omvandla x_kolumn till faktor om den inte redan är det.
  if (!is.factor(data[[x_kolumn]])) {
    data[[x_kolumn]] <- as.factor(data[[x_kolumn]])
  }
  
  # Omvandla y_kolumn till numerisk om den inte redan är det.
  if (!is.numeric(data[[y_kolumn]])) {
    data[[y_kolumn]] <- as.numeric(data[[y_kolumn]])
  }
  
  # ------------ Databearbetning ------------
  
  # Beräkna procentandelar och etiketter.
  data <- data %>%
    mutate(procent = !!sym(y_kolumn) / sum(!!sym(y_kolumn)) * 100,
           label = paste0(!!sym(y_kolumn))) # Etiketter med antal.
  
  # Om titel_legend är NULL, sätt det till x_kolumn där första bokstaven är stor.
  if (is.null(titel_legend)) {
    titel_legend <- tools::toTitleCase(x_kolumn)
  }
  
  # Skapa färgpalett.
  n_categories <- n_distinct(data[[x_kolumn]]) # Räkna antal kategorier.
  
  if (length(farg_gradient) > 1) {
    # Skapa en gradient baserat på farg_gradient. colorRampPalette skapar en funktion som använder den andra parentesen '(n_categories)' för att avgöra hur många färgkoder den ska returnera.
    custom_palette <- colorRampPalette(farg_gradient)(n_categories)
  } else {
    # om ingen farg_gradient är angedd så utgår den från farg och ger en standardtoning från den färgen.
    custom_palette <- lighten(farg, seq(0, 0.7, length.out = n_categories))
  }
  
  # Sortera data från störst till minst för att säkerställa rätt ordning.
  if (sortera_storst_till_minst) {
    data <- data %>% 
      mutate(!!x_kolumn := fct_reorder(!!sym(x_kolumn), 
                                       desc(!!sym(y_kolumn))))
  }
  
  # ------------ Skapa figur ------------
  
  # Skapa figur.
  donut <- ggplot(data,
                  aes(x = "", y = !!sym(y_kolumn),
                      fill = !!sym(x_kolumn))) +
    geom_bar(stat = "identity", width = 1, color = "white") + # Skapa pajdiagram.
    coord_polar(theta = "y",                                  # Gör det cirkulärt.
                direction = ifelse(motsols, 1, -1)) +         # Ändra mellan motsols och medsols.
    geom_text(aes(label = label, x = flytta_siffror_utat),
              position = position_stack(vjust = 0.5),
              size = storlek_text) +                          # Etiketter.
    scale_fill_manual(values = custom_palette) +              # Använd anpassad färgskala.
    labs(fill = titel_legend,
         title = titel) +                                     # Lägg till titel.
    theme_void() +                                            # Ta bort bakgrund.
    theme(legend.position = placering_legend)                 # Styr legendens position.
  
  # Lägg till en vit cirkel i mitten för att skapa donut-form.
  donut <- donut + 
    annotate("point", x = 0, y = 0, size = storlek_hal, color = "white") # Lägg till ett hål i mitten.
  
  return(donut)
}



