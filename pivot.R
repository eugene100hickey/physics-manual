# https://datascienceplus.com/from-wide-to-long-reshaping-world-bank-data-with-pivot_longer/

library(tidyverse)

url <- paste0("https://api.worldbank.org/v2/en/indicator/",
              "SP.DYN.LE00.IN?downloadformat=csv")
zipfile <- file.path(tempdir(), "le.zip")
download.file(url, zipfile, mode = "wb", quiet = TRUE)
files <- unzip(zipfile, exdir = tempdir())
basename(files)

data_file <- files[str_detect(basename(files), "^API_")]
wide <- read_csv(data_file, skip = 4)
dim(wide)

long <- wide |>
  select(-`Indicator Name`, -`Indicator Code`, -last_col()) |>
  pivot_longer(
    cols            = -c(`Country Name`, `Country Code`),
    names_to        = "year",
    values_to       = "life_exp",
    names_transform = as.integer,
    values_drop_na  = TRUE
  )
long

meta_file <- files[grepl("^Metadata_Country", basename(files))]
meta <- read_csv(meta_file)

countries <- long |>
  inner_join(meta |> filter(!is.na(Region)) |>
               select(`Country Code`, Region),
             by = "Country Code")
n_distinct(countries$`Country Code`)

dsp_colors <- c("#0066CC", "#E8862D", "#159A6C", "#7D5BD6",
                "#D64580", "#2AA9B8", "#C9A227")

dsp_theme <- theme_minimal(base_size = 13) +
  theme(
    panel.background = element_rect(fill = "#F5F5F7", color = NA),
    panel.grid       = element_line(color = "white"),
    axis.ticks       = element_blank(),
    plot.title       = element_text(face = "bold")
  )

regions <- c(EAS = "East Asia & Pacific",   ECS = "Europe & Central Asia",
             LCN = "Latin America & Carib.", MEA = "Middle East & N. Africa",
             NAC = "North America",          SAS = "South Asia",
             SSF = "Sub-Saharan Africa")

reg <- long |>
  filter(`Country Code` %in% names(regions)) |>
  mutate(region = regions[`Country Code`])

factor_levels <- reg |> 
  filter(year == "2024") |> 
  arrange(life_exp |> desc())
  

reg |> 
  mutate(region = factor(region, levels = factor_levels$region)) |> 
  ggplot(aes(year, life_exp, color = region)) +
  annotate("rect", xmin = 2019.5, xmax = 2021.5, ymin = -Inf, ymax = Inf,
           alpha = .12, fill = "red") +
  geom_line(linewidth = .8) +
  scale_x_continuous(breaks = seq(1960, 2020, 20)) +
  labs(title    = "Life expectancy by region, 1960–2024",
       subtitle = "Shaded band: the COVID-19 years",
       x = NULL, y = "Life expectancy at birth (years)", color = NULL) +
  scale_color_manual(values = dsp_colors) +
  dsp_theme


reg |> 
  mutate(region = factor(region, levels = factor_levels$region)) |> 
  filter(year >= 2015) |> 
  ggplot(aes(year, life_exp, color = region)) +
  geom_line(linewidth = .9) +
  geom_point(size = 1.6) +
  scale_x_continuous(breaks = seq(2015, 2024, 3)) +
  labs(title = "The dip and the rebound, 2015–2024",
       x = NULL, y = "Life expectancy at birth (years)", color = NULL) +
  scale_color_manual(values = dsp_colors) +
  dsp_theme

recovery <- countries |>
  filter(year %in% c(2019, 2024)) |>
  pivot_wider(names_from = year, values_from = life_exp,
              names_prefix = "y") |>
  mutate(delta = y2024 - y2019)

recovery |>
  arrange(delta) |>
  select(`Country Name`, y2019, y2024, delta) |>
  tail(5)



# Sun Aug 30 13:54:06 2026 ------------------------------

# https://datascienceplus.com/in-r-a-missing-value-is-a-join-key/

library(tidyverse)

owid <- function(slug) {
  read_csv(paste0("https://ourworldindata.org/grapher/", slug,
                  ".csv?v=1&csvType=full&useColumnShortNames=true"),
           show_col_types = FALSE, progress = FALSE)
}

life <- owid("life-expectancy") |>
  rename(life_exp = life_expectancy_0) |>
  filter(year == 2023) |>
  select(entity, code, life_exp)

age <- owid("median-age") |>
  rename(median_age = median_age__sex_all__age_all__variant_estimates) |>
  filter(year == 2023, !is.na(median_age)) |>
  select(entity, code, median_age)

c(life = nrow(life), age = nrow(age))
