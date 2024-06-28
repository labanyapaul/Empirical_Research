# Ghana summary table of all landfills. Ghana barplot showing landfill area over the years. 
library(sf)
library(tidyverse)
library(maptiles)
library(archive)
library(dplyr)
library(unglue)
library(units)
library(readr)
library(ggplot2)
library(ggspatial)
library(knitr)
library(geos)
library(here)
library(kableExtra)
library(gt)
library(gtsummary)
library(formatR)
library(scales)

Ghana_all_landfills_polygon_nogeometry <- read_csv("output/Ghana_all_landfills_polygon_nogeometry.csv", show_col_types = FALSE)


#############################################################
# Plotting the landfills area over the years - barplot

years <- unique(Ghana_all_landfills_polygon_nogeometry$year)

ggplot(data = Ghana_all_landfills_polygon_nogeometry) +
  geom_col(aes(x = year |> as.character(), y = area_m2, fill = landfill_name)) +
  scale_y_continuous(labels = comma) +  
  facet_wrap(~landfill_name, scales = "free_y") +
  coord_flip() +
  labs(y = "Area (m2)",
       x = "",
       fill = "Landfill") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# plot for only Oti and Agbogbloshie landfills

filtered_data <- Ghana_all_landfills_polygon_nogeometry %>%
  filter(landfill_name %in% c("Oti", "Agbogbloshie"))

# Get unique years for filtering
years_filtered <- unique(filtered_data$year)

# Create the plot
ggplot(data = filtered_data) +
  geom_col(aes(x = as.character(year), y = area_m2, fill = landfill_name)) +
  scale_y_continuous(labels = comma) +  
  facet_wrap(~landfill_name, scales = "free_y") +
  coord_flip() +
  labs(y = "Area (m2)",
       x = "",
       fill = "Landfill") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("report/images/Ghana_landfillarea.png", width = 10, height = 6, dpi = 300)

#############################################################
# Ghana summary table for all years
summary_table <- Ghana_all_landfills_polygon_nogeometry %>%
  group_by(landfill_name) %>%
  summarize(
    Min =  round(min(area_m2, na.rm = TRUE), 2),
    Max = round(max(area_m2, na.rm = TRUE), 2),
    Mean = round(mean(area_m2, na.rm = TRUE),2),
    Stdev = round(sd(area_m2, na.rm = TRUE), 2),
    NAs = sum(is.na(area_m2)),
    Observations = n()
                   
  )


# summary table nicer 
summary_table %>%
  kable() %>%
  kable_styling()

# Print the summary table using gt
gt_table <- summary_table %>%
  gt() %>%
  tab_header(
    title = "Table 1. Summary Statistics of Ghana Landfills 2008- 2014"
  ) %>%
  tab_footnote(
    footnote = "Note: Area is in square meters (m2).",
    locations = cells_title(groups = "title")
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "all",
        color = "black",
        weight = px(1)
      ),
      cell_text(
        weight = "bold"
      )
    ),
    locations = cells_body()
  ) %>%
  tab_options(
    table.font.size = "small"
  ) %>%
  opt_footnote_marks(marks = "standard")

# Print the gt table
print(gt_table)




