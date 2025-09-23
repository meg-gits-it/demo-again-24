library(tidyverse)
library(rvest)
#library(polite) #use this to rate limit requests once script works

# rvest (as in "harvest") is a package for web scraping

# The basic process is..
# (1) import HTML from webpage into R (data are imported as XML)
# (2) find where items of interest sit in HTML structure (right click and "Inspect" in chrome)
# (3) use copy function to create "selector" for that HTML location
# (4) split out relevant parts of HTML (e.g. text, links etc)
# (5) compile results into tibble/data frame

# I used this as an example - https://personalpages.manchester.ac.uk/staff/david.selby/rthritis/2021-11-05-web-scraping/resources/webscraping.html#17

scrape_companies <- function(path) {
  
  med_forms_html <- read_html(file.path("https://bnf.nice.org.uk/drugs/", path, "/medicinal-forms/"))
  
  # Line below creates a vector (trade_name) based on all entries at the HTML location (main div div... etc) in the hierarchy. 
  # HTMLtext2 converts the text at that location from xml to plain text  
  
  trade_name <- med_forms_html %>% html_elements("main div div div div div section ol li details summary h3 span") %>% html_text2
  non_proprietary_name <- med_forms_html %>% html_element("h1 span span") %>% html_text2
  
  tibble(non_proprietary_name, trade_name#, manufacturer 
  )
}

# The trade_name vector contains both trade name and manufacturer info on alternate rows
# ... both elements seem to be at the same location in the HTML hierarchy
# There are also a couple of tags that appear occasionally - black triangle and sugar-free
# The code below gets rid of those tags then separates out alternate rows

# The row splitting thing is based on this: https://stackoverflow.com/questions/53843589/split-variable-on-every-other-row-to-form-two-new-columns-in-data-frame

companies <- scrape_companies("simvastatin") |>
  filter(!grepl("black triangle", trade_name)) |>
  filter(trade_name != "Sugar free") |>
  mutate(index = rep(c(1, 2),length.out = n())) |>
  group_by(index) |>
  mutate(id = row_number()) |>
  pivot_wider(names_from = index, values_from = trade_name) |>
  select(-id) |>
  rename("product_name" = "1", "company_name" = "2")

write.table(companies, file = "Companies.csv", quote = FALSE, sep = ",", eol = "\n", col.names = TRUE, row.names = FALSE)

# Stuff to do: 
# turn tidying bit into a function
# check for more exceptions (like "black triangle")