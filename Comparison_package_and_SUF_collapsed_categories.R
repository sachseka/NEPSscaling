library(stringr)
library(dplyr)
library(openxlsx)
library(tidyr)

extract_expected_categories <- function(script_path, specific_SC, data) {

  # Skript als Text einlesen
  script_lines <- readLines(script_path)

  # 1. Suche nach SC-Block
  sc_start <- which(str_detect(script_lines, paste0("if \\(SC == \"", specific_SC, "\"\\)")))
  sc_end <- which(str_detect(script_lines, "if \\(SC =="))[-1] # Nächste SC starten
  sc_end <- c(sc_end, length(script_lines)) # Ende der Datei hinzufügen

  block_start <- sc_start[1]
  block_end <- min(sc_end[sc_end > block_start])
  sc_lines <- script_lines[block_start:block_end]

  # Debug: Zeige relevante SC-Zeilen
  print("SC-Block erfolgreich extrahiert:")
  print(sc_lines)

  # 2. Identifiziere Domain-Blöcke
  domain_indices <- which(str_detect(sc_lines, "if \\(domain =="))
  domain_blocks <- lapply(seq_along(domain_indices), function(i) {
    start <- domain_indices[i]
    end <- if (i < length(domain_indices)) domain_indices[i + 1] - 1 else length(sc_lines)
    list(start = start, end = end)
  })

  results <- list()

  for (domain_block in domain_blocks) {
    domain_start <- domain_block$start
    domain_end <- domain_block$end
    domain_line <- sc_lines[domain_start]
    current_domain <- str_match(domain_line, "domain == \"(.*?)\"")[, 2]

    # Debug: Zeige aktuelle Domain
    print(paste("Verarbeite Domain:", current_domain))

    # Extrahiere Wave-Blöcke innerhalb des Domain-Blocks
    wave_indices <- which(str_detect(sc_lines[domain_start:domain_end], "if \\(wave ==")) + domain_start - 1
    wave_blocks <- lapply(seq_along(wave_indices), function(i) {
      start <- wave_indices[i]
      end <- if (i < length(wave_indices)) wave_indices[i + 1] - 1 else domain_end
      list(start = start, end = end)
    })

    for (wave_block in wave_blocks) {
      wave_start <- wave_block$start
      wave_end <- wave_block$end
      wave_line <- sc_lines[wave_start]
      current_wave <- str_match(wave_line, "wave == \"(.*?)\"")[, 2]

      # Debug: Zeige aktuelle Wave
      print(paste("Verarbeite Wave:", current_wave))

      # Suche nach Recodierungszeilen innerhalb des Wave-Blocks
      recode_lines <- sc_lines[wave_start:wave_end][
        str_detect(sc_lines[wave_start:wave_end], "resp\\[\\[\".*?_c\"\\]\\].*?(%in%|==).*?<\\-")
      ]
      if (length(recode_lines) == 0) {
        message("Keine Recodierungszeilen für Domain: ", current_domain, ", Wave: ", current_wave)
        next
      }

      # Debug: Zeige gefundene Recodierungszeilen
      print("Gefundene Recodierungszeilen:")
      print(recode_lines)

      # Parsing der relevanten Informationen
      parsed_data <- recode_lines %>%
        str_match("\\[\\[\"(.*?)\"\\]\\].*?==\\s*(\\d+).*?<\\-\\s*(\\d+)") %>%
        as.data.frame() %>%
        filter(!is.na(V1)) %>%
        mutate(Domain = current_domain, Wave = current_wave)

      colnames(parsed_data) <- c("FullLine", "Itemname", "OriginalValue", "RecodedValue", "Domain", "Wave")

      # Berechnung der maximalen Recoded-Werte pro Item
      item_categories <- parsed_data %>%
        group_by(Itemname, Domain, Wave) %>%
        summarize(MaxCategory = max(RecodedValue)) %>%
        ungroup()

      results <- append(results, list(item_categories))
    }
  }

  # 3. Ergebnisse kombinieren
  combined_results <- bind_rows(results)


  if (nrow(combined_results) == 0) stop("Keine Recodierungsinformationen gefunden.")

  # Debug: Zeige kombinierte Ergebnisse
  print("Kombinierte Ergebnisse:")
  print(combined_results)


  print("Items in Recodierungszeilen:")
  print(unique(combined_results$Itemname))

  missing_items <- setdiff(combined_results$Itemname, names(data))
  if (length(missing_items) > 0) {
    message("Folgende Items fehlen im SPSS-Datensatz: ", paste(missing_items, collapse = ", "))
  }

  suf_categories <- data %>%
    select(all_of(intersect(combined_results$Itemname, names(data)))) %>% # Nur existierende Items auswählen
    summarise(across(everything(), ~ n_distinct(., na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "Itemname", values_to = "Categories_in_SUF")

  suf_categories <- combined_results %>%
    select(Itemname) %>%
    distinct() %>%
    left_join(suf_categories, by = "Itemname") %>%
    mutate(Categories_in_SUF = ifelse(is.na(Categories_in_SUF), NA, Categories_in_SUF))

  # Debug: Zeige berechnete Kategorien in SUF
  print("Kategorien in SUF:")
  print(suf_categories)

  print("Items in SUF:")
  print(unique(suf_categories$Itemname))

  # 5. Daten kombinieren^
  combined_results <- combined_results %>%
  mutate(MaxCategory = as.numeric(MaxCategory)) # Sicherstellen, dass MaxCategory numerisch ist

  output_data <- combined_results %>%
    left_join(suf_categories, by = "Itemname") %>%
    mutate(
      Startingcohort = specific_SC,
      ExpectedNumberOfCategories = MaxCategory + 1
    ) %>%
    select(Startingcohort, Domain, Wave, Itemname, Categories_in_SUF, ExpectedNumberOfCategories)

  if (nrow(output_data) == 0) stop("Die finale Tabelle ist leer.")

  return(output_data)
}
# Funktion zum Exportieren der Tabelle
create_excel_from_script <- function(script_path, output_path, specific_SC, data) {
  # Extrahiere Daten für die spezifische SC
  extracted_data <- extract_expected_categories(script_path, specific_SC, data)

  # Speichere die Daten als Excel
  write.xlsx(extracted_data, file = output_path, overwrite = TRUE)

  cat("Die Datei wurde erfolgreich erstellt:", output_path, "\n")
}


# Beispiel: Anwendung der Funktion
script_file <- "~/Plausible Values/NEPSscaling/R/collapse_categories_pcm.R" # Name des Skripts
output_file <- "~/Plausible Values/Expected_Categories_SC1.xlsx"  # Name der Output-Datei
specific_SC <- "SC1" # Spezifische SC

# Beispiel-Daten (hier kommt dein echter DataFrame hin)
data <- dat

# Funktion aufrufen
create_excel_from_script(script_file, output_file, specific_SC, data)
