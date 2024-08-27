ladda_github_funktioner <- function(repo_owner = "jl98swe",
                                    repo_name = "jl98swe_funktioner",
                                    branch = "main",
                                    folder = NULL,
                                    filtyp = ".R") {
  
  # Skapa URL:en.
  url <- paste0("https://api.github.com/repos/", repo_owner, "/", repo_name, "/contents")
  if (!is.null(folder)) {
    url <- paste0(url, "/", folder)
  }
  
  # Hämta innehållslistan.
  contents <- gh::gh(
    paste0("GET ", url),
    owner = repo_owner,
    repo = repo_name,
    ref = branch
  )
  
  # Ladda varje fil med en viss ändelse.
  for (file in contents) {
    if (endsWith(file$name, filtyp)) {
      source_url(file$download_url)
      cat("Loaded:", file$name, "\n")
    }
  }
}

# Exempel: ladda_github_funktioner(repo_owner = "jl98swe", repo_name = "jl98swe_funktioner")
