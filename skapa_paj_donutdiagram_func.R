skapa_paj_donutdiagram <- function(data, 
                           x_kolumn, 
                           y_kolumn, 
                           farg = "gray1", 
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
  
  # Kontrollera om kolumnerna finns i data.
  if (!(x_kolumn %in% colnames(data)) || !(y_kolumn %in% colnames(data))) {
    stop("Ange giltiga kolumnnamn för 'x_kolumn' och 'y_kolumn'.")
  }
  
  # Beräkna procentandelar och etiketter.
  data <- data %>%
    mutate(procent = !!sym(y_kolumn) / sum(!!sym(y_kolumn)) * 100,
           label = paste0(!!sym(y_kolumn))) # Etiketter med antal.
  
  # Skapa en anpassad färgskala som tonas till en ljusare version av grundfärgen.
  n_categories <- n_distinct(data[[x_kolumn]])                            # Räkna först antalet kategorier i x-kolumnen.
  custom_palette <- lighten(farg, seq(0, 0.7, length.out = n_categories)) # Tonar mot ljusare färg.
  
  # Om titel_legend är NULL, sätt det till x_kolumn där första bokstaven är stor.
  if (is.null(titel_legend)) {
    titel_legend <- tools::toTitleCase(x_kolumn)
  }
  
  # Skapa diagram.
  donut <- ggplot(data,
                  aes(x = "", y = !!sym(y_kolumn),
                      fill = !!sym(x_kolumn))) +
    geom_bar(stat = "identity", width = 1, color = "white") + # Skapa pajdiagram.
    coord_polar(theta = "y") +                                # Gör det cirkulärt.
    geom_text(aes(label = label, x = flytta_siffror_utat),
              position = position_stack(vjust = 0.5),
              size = storlek_text) +                          # Etiketter.
    scale_fill_manual(values = custom_palette) +              # Använd anpassad färgskala.
    labs(fill = titel_legend,
         title = titel) +                                     # Lägg till eller ta bort legendtitel.
    theme_void() +                                            # Ta bort bakgrund och axlar.
    theme(legend.position = placering_legend)                 # Styr legendens synlighet.
  
  # Lägg till en vit cirkel i mitten för att skapa donut-form.
  donut <- donut + 
    annotate("point", x = 0, y = 0, size = storlek_hal, color = "white") # Lägger till en vit cirkel som ser ut som ett hål i mitten av diagrammet.
  
  return(donut)
}
