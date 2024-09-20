### Funktion för att extrahera e-mejladresser från en lista.

# Din textsträng.
emailString <- ''

# Filsökväg där filen ska sparas (om den lämnas tom så sparas filen i arbetsmappen).
getwd() # Se arbetsmappen.
path <- ''

# Extrahera alla mejladresser inom "<" och ">".
emails <- unlist(regmatches(emailString, gregexpr("(?<=<)[^>]+", emailString, perl=TRUE)))

# Skapa en vektor med HYPERLÄNK-funktionen för varje mejladress (svenska versionen av Excel. HYPERLINK i engelska versionen där ";" byts ut mot ",").
hyperlinks <- paste0('=HYPERLÄNK("mailto:', emails, '"; "', emails, '")')

# Skriv ut resultatet till en textfil.
writeLines(hyperlinks, paste0(path, "email_hyperlankar.txt"))
