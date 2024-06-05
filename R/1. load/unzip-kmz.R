library(here)
library(archive)
library(purrr)
library(dplyr)
library(unglue)

# find landfill kmz
landfill_input_files <- here("input") |> 
  list.files(full.names = TRUE, recursive = TRUE) 

# from that, derive where to put them in the output
landfills <- unglue_data(landfill_input_files, 
                                 here("input/{country}/Landfills/{landfill}.kmz")) 

output_file_names <- here("output", 
                          landfills$country, 
                          "Landfills", 
                          landfills$landfill)

# unzip to output

# make sure output exists
if (!dir.exists(here("output/Ghana"))) {
  dir.create(here("output/Ghana"))
}

if (!dir.exists(here("output/Ghana/Landfills"))) {
  dir.create(here("output/Ghana/Landfills"))
}

if (!dir.exists(here("output/Kenya/Landfills"))) {
  dir.create(here("output/Kenya/Landfills"))
}

# unzip
walk2(
  .x = landfill_input_files,
  .y = output_file_names,
  .f = ~ archive::archive_extract(
    archive = .x,
    dir = .y
  )
)

# all have a folder now, and inside is a document called "doc.kml".
# let's get that out.

file.copy(
  from = here(output_file_names, "doc.kml"),
  to = here(
    "output/",
    landfills$country, "/Landfills/",
    paste0(landfills$landfill, ".kml")
  ) 
)
