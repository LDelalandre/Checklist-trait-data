# ==========================
# Operational Checklist Generator
# ==========================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# --- Helper: map Tier text to a CSS-safe slug ---
tier_slug <- function(x) {
  case_when(
    x == "Simple dataset"               ~ "simple",
    x == "Structured dataset for reuse" ~ "structured",
    x == "Collection of datasets"       ~ "collection",
    TRUE                                 ~ "unknown"
  )
}

# --- 1. Read CSV ---
data <- read_csv("data/checklist.csv", col_types = cols(.default = "c")) %>%
  mutate(
    Category1 = str_trim(Category1),
    Category2 = str_trim(Category2),
    Tier = str_trim(Tier)
  ) %>%
  fill(Category1, Category2, .direction = "down")

# --- 2. Filter actual checklist items, create IDs, and pre-build each row's HTML ---
tasks <- data %>%
  filter(!is.na(ID) & ID != "", !is.na(`Action to perform`) & `Action to perform` != "") %>%
  mutate(
    checkbox_id = paste0("chk_", gsub("[^a-zA-Z0-9]", "_", ID)),
    Category1_id = paste0("cat_", gsub("[^a-zA-Z0-9]", "_", Category1)),
    Category2_id = paste0("sub_", gsub("[^a-zA-Z0-9]", "_", Category2)),
    tier_id = tier_slug(Tier),
    task_html = paste0(
      '<div class="task-row tier-', tier_id, '" data-tier="', Tier, '">',
      '<label><input type="checkbox" id="', checkbox_id, '"> ',
      ID, ' ', `Action to perform`,
      '</label></div>'
    )
  )

# --- 3. Build subtitle cards (group keys pulled with first() to guarantee length 1) ---
subtitle_cards <- tasks %>%
  group_by(Category1, Category2, Category1_id, Category2_id) %>%
  summarise(
    subtitle_card = paste0(
      '<div class="subcard" id="', first(Category2_id), '">',
      '<h3>', first(Category2), '</h3>',
      paste(task_html, collapse = "\n"),
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
      '<div class="card" id="', first(Category1_id), '">',
      '<h2>', first(Category1), '</h2>',
      paste(subtitle_card, collapse = "\n"),
      '</div>'
    ),
    .groups = "drop"
  ) %>% 
  unique()

# --- 5. Collapse all title cards ---
tasks_body_html <- paste(title_cards$title_card, collapse = "\n")

# --- 5b. Tier CSS (Okabe-Ito palette, same tints as the PDF) ---
tier_style_html <- '
<style>
.task-row {
  padding: 3px 8px;
  border-radius: 4px;
  margin-bottom: 3px;
}
.tier-simple     { background-color: rgba(0,158,115,0.25); }
.tier-structured { background-color: rgba(240,228,66,0.35); }
.tier-collection { background-color: rgba(213,94,0,0.20); }
.tier-unknown    { background-color: transparent; }

#tier-filter {
  margin-bottom: 15px;
  padding: 10px 14px;
  border: 1px solid #ccc;
  border-radius: 6px;
  background: #fafafa;
}
#tier-filter label {
  margin-right: 18px;
  white-space: nowrap;
}
.legend-swatch {
  display: inline-block;
  width: 12px;
  height: 12px;
  border-radius: 2px;
  margin-right: 4px;
  vertical-align: middle;
}
.swatch-simple     { background-color: rgb(0,158,115); opacity: 0.55; }
.swatch-structured { background-color: rgb(240,228,66); opacity: 0.7; }
.swatch-collection { background-color: rgb(213,94,0);  opacity: 0.45; }
</style>
'

# --- 5c. Tier filter panel (checkboxes to show/hide tiers) ---
tier_filter_html <- '
<div id="tier-filter">
  <strong>Show tiers:&nbsp;</strong>
  <label><input type="checkbox" class="tier-toggle" value="Simple dataset" checked>
    <span class="legend-swatch swatch-simple"></span>Simple dataset</label>
  <label><input type="checkbox" class="tier-toggle" value="Structured dataset for reuse" checked>
    <span class="legend-swatch swatch-structured"></span>Structured dataset for reuse</label>
  <label><input type="checkbox" class="tier-toggle" value="Collection of datasets" checked>
    <span class="legend-swatch swatch-collection"></span>Collection of datasets</label>
</div>
'

# --- 5d. Filter JS (runs after checkboxes exist in the DOM) ---
tier_script_html <- '
<script>
document.addEventListener("DOMContentLoaded", function() {
  var toggles = document.querySelectorAll(".tier-toggle");
  function updateVisibility() {
    var active = Array.from(toggles).filter(t => t.checked).map(t => t.value);
    document.querySelectorAll(".task-row").forEach(function(row) {
      var tier = row.getAttribute("data-tier");
      row.style.display = active.includes(tier) ? "" : "none";
    });
  }
  toggles.forEach(t => t.addEventListener("change", updateVisibility));
  updateVisibility();
});
</script>
'

# --- 5e. Assemble final tasks HTML block ---
tasks_html <- paste0(
  tier_style_html,
  tier_filter_html,
  tasks_body_html,
  tier_script_html
)

detailed_toc <- F

if (detailed_toc){
  toc_subs <- subtitle_cards %>%
    select(Category1, Category2, Category1_id, Category2_id) %>%
    unique()
  
  toc_titles <- title_cards %>%
    select(Category1, Category1_id) %>%
    unique()
  
  toc_html <- ""
  for(i in seq_len(nrow(toc_titles))) {
    title_row <- toc_titles[i, ]
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
  
} else {
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
\n NB: for the revision, this document is presented as a pdf, but the interactive HTML document is 
available on GitHub: <a href='https://htmlpreview.github.io/?https://github.com/Ldelalandre/Checklist-trait-data/blob/main/Appendix%20S2%20-%20Operational%20checklist.html' 
target='_blank'>https://htmlpreview.github.io/?https://github.com/Ldelalandre/Checklist-trait-data/blob/main/Appendix%20S2%20-%20Operational%20checklist.html</a>.
</p>"

html <- gsub("\\{\\{DESCRIPTION\\}\\}", description_text, template)
html <- gsub("\\{\\{TITLE\\}\\}", "Appendix S2 - Operational Checklist", html)
html <- gsub("\\{\\{TASKS\\}\\}", tasks_html, html)
html <- gsub("<!-- TOC items will be inserted here by R -->", toc_html, html)

# --- 9. Write final HTML ---
writeLines(html, "Appendix S2 - Operational checklist.html")
cat("✔ checklist_operational.html generated successfully\n")

print_session_info <- F
if (print_session_info){
  sink("sessionInfo.txt")
  sessionInfo()
  sink()
}