# ==========================
# Operational Checklist Generator
# ==========================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# --- 1. Read CSV ---
data <- read_csv("data/checklist.csv", col_types = cols(.default = "c")) %>%
  mutate(
    Category1 = str_trim(Category1),
    Category2 = str_trim(Category2)
  ) %>%
  fill(Category1, Category2, .direction = "down")

# --- 2. Filter actual checklist items and create IDs ---
tasks <- data %>%
  filter(!is.na(ID) & ID != "", !is.na(`Action to perform`) & `Action to perform` != "") %>%
  mutate(
    checkbox_id = paste0("chk_", gsub("[^a-zA-Z0-9]", "_", ID)),
    Category1_id = paste0("cat_", gsub("[^a-zA-Z0-9]", "_", Category1)),
    Category2_id = paste0("sub_", gsub("[^a-zA-Z0-9]", "_", Category2))
  )

# --- 3. Build subtitle cards ---
subtitle_cards <- tasks %>%
  group_by(Category1, Category2, Category1_id, Category2_id) %>%
  summarise(
    subtitle_card = paste0(
      '<div class="subcard" id="', Category2_id, '">',
      '<h3>', Category2, '</h3>',
      paste0(
        '<label><input type="checkbox" id="', checkbox_id, '"> ',
        ID, ' ', `Action to perform`, '</label>',
        collapse = "\n"
      ),
      '</div>'
    ),
    .groups = "drop"
  ) %>% 
  unique()

# --- 4. Build title cards with nested subtitles ---
title_cards <- subtitle_cards %>%
  group_by(Category1, Category1_id) %>%
  summarise(
    title_card = paste0(
      '<div class="card" id="', Category1_id, '">',
      '<h2>', Category1, '</h2>',
      paste(subtitle_card, collapse = "\n"),
      '</div>'
    ),
    .groups = "drop"
  ) %>% 
  unique()

# --- 5. Collapse all title cards ---
tasks_html <- paste(title_cards$title_card, collapse = "\n")

detailed_toc <- F

if (detailed_toc){
  # --- 6. Build TOC (deduplicated) ---
  toc_subs <- subtitle_cards %>%
    select(Category1, Category2, Category1_id, Category2_id) %>%
    unique()
  
  toc_titles <- title_cards %>%
    select(Category1, Category1_id) %>%
    unique()
  
  toc_html <- ""
  for(i in seq_len(nrow(toc_titles))) {
    title_row <- toc_titles[i, ]
    
    # get all unique subtitles under this title
    subs <- toc_subs %>% filter(Category1 == title_row$Category1)
    
    toc_html <- paste0(
      toc_html,
      '<li><a href="#', title_row$Category1_id, '">', title_row$Category1, '</a>'
    )
    
    if(nrow(subs) > 0) {
      toc_html <- paste0(
        toc_html,
        '<ul>',
        paste0(
          '<li><a href="#', subs$Category2_id, '">', subs$Category2, '</a></li>',
          collapse = "\n"
        ),
        '</ul>'
      )
    }
    
    toc_html <- paste0(toc_html, '</li>\n')
  }
  
}else{
  # --- 6. Build TOC (titles only) ---
  toc_titles <- title_cards %>%
    select(Category1, Category1_id) %>%
    unique()
  
  toc_html <- ""
  for(i in seq_len(nrow(toc_titles))) {
    title_row <- toc_titles[i, ]
    toc_html <- paste0(
      toc_html,
      '<li><a href="#', title_row$Category1_id, '">', title_row$Category1, '</a></li>\n'
    )
  }
}


# --- 7. Read template ---
template <- paste(readLines("data/template.html"), collapse = "\n")

# --- 8. Insert placeholders ---
description_text <- "<p style='font-style:italic; color:#555;'>
This operational checklist follows the same structure as the detailed checklist and the article.
It can be stored in your project folder, and allows you to tick the actions that you already performed.
Provided you do not move the file, the ticked items will be saved, and will still be ticked when reopening the file.
NB: for revision, this document is presented as a pdf, but the html, interactive document is 
available on GitHub: 
</p>"

html <- gsub("\\{\\{DESCRIPTION\\}\\}", description_text, template)
html <- gsub("\\{\\{TITLE\\}\\}", "Appendix S2 - Operational Checklist", html)
html <- gsub("\\{\\{TASKS\\}\\}", tasks_html, html)

# Insert TOC HTML into the template
html <- gsub("<!-- TOC items will be inserted here by R -->", toc_html, html)

# --- 9. Write final HTML ---
writeLines(html, "Appendix S2 - Operational checklist.html")
cat("✔ checklist_operational.html generated successfully\n")

# SessionInfo ####

print_session_info <- F
if (print_session_info){
  sink("sessionInfo.txt")
  sessionInfo()
  sink()
}

