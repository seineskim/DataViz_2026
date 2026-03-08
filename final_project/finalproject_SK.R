rm(list = ls())

library(dplyr)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(sf)
library(tibble)
library(modelsummary)

# env
options(
  modelsummary_factory_latex = "kableExtra",
  modelsummary_format_numeric_latex = "plain"
)
setwd("/Users/ines/Desktop/School/0_Lecture/2학기/Data Viz/DataViz_2026/final_project")
pdf_device <- "pdf"

# custom theme
my_custom_theme <- function(base_size = 11, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      text = element_text(family = base_family),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_text(colour = "grey20"),
      plot.title = element_text(face = "bold", size = base_size + 4, hjust = 0),
      plot.subtitle = element_text(size = base_size + 1, colour = "grey30", hjust = 0, margin = margin(t = 6, b = 8)),
      plot.caption = element_text(size = base_size - 1, colour = "grey45", hjust = 1),
      legend.position = "top",
      legend.title = element_text(face = "bold", colour = "grey20"),
      legend.text = element_text(family = base_family, size = base_size - 1, lineheight = 1.1)
    )
}

theme_set(my_custom_theme())

# legend labels
indicator_legend_labels <- c(
  "Total fertility rate index (2015 = 100)\nTFR = 5 x sum of ASFR (ages 15-49)",
  "Crude marriage rate index (2015 = 100)\n(Marriages / mid-year population) x 1,000"
)

# region mapping kr-en
region_map_kr_to_eng <- c(
  "서울특별시" = "Seoul",
  "부산광역시" = "Busan",
  "대구광역시" = "Daegu",
  "인천광역시" = "Incheon",
  "광주광역시" = "Gwangju",
  "대전광역시" = "Daejeon",
  "울산광역시" = "Ulsan",
  "세종특별자치시" = "Sejong-si",
  "경기도" = "Gyeonggi-do",
  "강원특별자치도" = "Gangwon-do",
  "충청북도" = "Chungcheongbuk-do",
  "충청남도" = "Chungcheongnam-do",
  "전북특별자치도" = "Jeollabuk-do",
  "전라북도" = "Jeollabuk-do",
  "전라남도" = "Jellanam-do",
  "경상북도" = "Gyeongsangbuk-do",
  "경상남도" = "Gyeongsangnam-do",
  "제주특별자치도" = "Jeju-do",
  "제주도" = "Jeju-do"
)

# ---------------------------------------------------------
# shared prep - provincial fertility and youth structure
# ---------------------------------------------------------
raw <- read.csv(
  "Total_Fertility_Rates_and_AgeSpecific_Fertility_Rates_for_Provinces.csv",
  header = FALSE, check.names = FALSE, stringsAsFactors = FALSE
)

# tfr cols: 2015(2), 2024(74)
tfr_10y <- setNames(raw[-c(1, 2), c(1, 2, 74)], c("province", "tfr_2015", "tfr_2024")) |>
  as_tibble() |>
  mutate(
    province = trimws(as.character(province)),
    tfr_2015 = as.numeric(tfr_2015),
    tfr_2024 = as.numeric(tfr_2024)
  ) |>
  filter(!is.na(province), province != "", !is.na(tfr_2015), !is.na(tfr_2024))

tfr_region_2024 <- tfr_10y |>
  filter(province != "Whole country") |>
  mutate(
    CTP_ENG_NM = case_when(
      province == "Sejong" ~ "Sejong-si",
      province == "Jeju" ~ "Jeju-do",
      province == "Jeollanam-do" ~ "Jellanam-do",
      TRUE ~ province
    )
  ) |>
  transmute(province, CTP_ENG_NM, tfr_2024)

# optional provincial change table data
provincial_change_data <- tfr_10y |>
  filter(province != "Whole country") |>
  transmute(
    tfr_2015,
    tfr_2024,
    fertility_drop = tfr_2015 - tfr_2024,
    fertility_drop_pct = ((tfr_2024 - tfr_2015) / tfr_2015) * 100
  )

pop_raw <- read.csv("korea-population_2024.csv", check.names = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
col_region <- names(pop_raw)[1]
col_sex <- names(pop_raw)[2]
col_age <- names(pop_raw)[3]
col_value <- names(pop_raw)[4]

pop_total <- pop_raw |>
  filter(.data[[col_sex]] == "계", .data[[col_age]] == "계", .data[[col_region]] != "전국") |>
  transmute(region_kr = .data[[col_region]], total_pop = as.numeric(.data[[col_value]]))

pop_youth <- pop_raw |>
  filter(
    .data[[col_sex]] == "계",
    .data[[col_age]] %in% c("20 - 24세", "25 - 29세", "30 - 34세", "35 - 39세"),
    .data[[col_region]] != "전국"
  ) |>
  group_by(region_kr = .data[[col_region]]) |>
  summarise(youth_pop = sum(as.numeric(.data[[col_value]]), na.rm = TRUE), .groups = "drop")

pop_share <- pop_total |>
  left_join(pop_youth, by = "region_kr") |>
  mutate(
    youth_share = (youth_pop / total_pop) * 100,
    CTP_ENG_NM = unname(region_map_kr_to_eng[region_kr])
  ) |>
  select(CTP_ENG_NM, youth_share)

stopifnot(!any(is.na(pop_share$CTP_ENG_NM)))

regional_data <- tfr_region_2024 |>
  left_join(pop_share, by = "CTP_ENG_NM")

stopifnot(!any(is.na(regional_data$tfr_2024)), !any(is.na(regional_data$youth_share)))

# ---------------------------------------------------------
# fig 1 - pattern: spatial mismatch map
# ---------------------------------------------------------
korea_map <- st_read("koreamap.shp", options = "ENCODING=CP949", quiet = TRUE)

map_youth <- korea_map |>
  left_join(regional_data |> select(CTP_ENG_NM, youth_share), by = "CTP_ENG_NM")

map_tfr <- korea_map |>
  left_join(regional_data |> select(CTP_ENG_NM, tfr_2024), by = "CTP_ENG_NM")

map_panel_theme <- theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.subtitle = element_text(size = 10, hjust = 0),
    legend.position = "bottom",
    legend.key.width = grid::unit(1.4, "cm")
  )

figure1_left <- ggplot(map_youth) +
  geom_sf(aes(fill = youth_share), color = "white", linewidth = 0.35) +
  scale_fill_gradientn(colors = c("#E8F1FA", "#90CAF9", "#1565C0"), name = "Youth share (%)") +
  labs(
    title = "Youth Population Share (Age 20-39), 2024",
    subtitle = "Share of total regional population"
  ) +
  map_panel_theme

figure1_right <- ggplot(map_tfr) +
  geom_sf(aes(fill = tfr_2024), color = "white", linewidth = 0.35) +
  scale_fill_gradientn(colors = c("#FFF3E0", "#FFB74D", "#E65100"), name = "TFR (2024)") +
  labs(
    title = "Total Fertility Rate by Region, 2024",
    subtitle = "Births per woman"
  ) +
  map_panel_theme

figure1 <- (figure1_left + figure1_right + plot_layout(ncol = 2)) +
  plot_annotation(
    title = "Figure 1. Spatial Mismatch Between Youth Concentration and Fertility in Korea",
    subtitle = "Regions with larger young-adult populations do not necessarily show higher fertility levels in 2024.",
    caption = "Source: Population Statistics and KOSIS Fertility Statistics (2024)"
  )

ggsave("fig1.pdf", figure1, device = pdf_device, width = 13, height = 6.8, units = "in")

# ---------------------------------------------------------
# fig 2 - verification: scatter with regression
# ---------------------------------------------------------
plot2_data <- regional_data |>
  transmute(province, youth_share, tfr_2024)

cor_val <- cor(plot2_data$youth_share, plot2_data$tfr_2024, use = "complete.obs")

figure2 <- ggplot(plot2_data, aes(x = youth_share, y = tfr_2024)) +
  geom_point(size = 3, color = "#00897B", alpha = 0.9) +
  geom_smooth(method = "lm", se = TRUE, color = "#1E88E5", linewidth = 1.2) +
  ggrepel::geom_text_repel(aes(label = province), size = 2.8, color = "grey25") +
  labs(
    title = "Figure 2. Youth Population Share and Regional Fertility Levels in 2024",
    subtitle = "Cross-sectional relationship between youth population share and TFR in 2024",
    x = "Youth population share (20-39, %)",
    y = "Total fertility rate (2024)",
    caption = sprintf("Source: Population Statistics and KOSIS Fertility Statistics (2024) | Pearson correlation: %.3f | Line: OLS fit with 95%% CI", cor_val)
  )

ggsave("fig2.pdf", figure2, device = pdf_device, width = 10, height = 6.8, units = "in")

# ---------------------------------------------------------
# fig 3 - mechanism: national marriage & fertility trends
# ---------------------------------------------------------
vital_raw <- read.csv("Vital_Statistics_of_Korea.csv", header = FALSE, check.names = FALSE, stringsAsFactors = FALSE)

# rebase indices (2015=100)
figure3_data <- tibble(
  year = as.integer(unlist(vital_raw[1, 2:11])),
  fertility_rate = as.numeric(unlist(vital_raw[8, 2:11])),
  marriage_rate = as.numeric(unlist(vital_raw[12, 2:11]))
) |>
  mutate(
    fertility_index = (fertility_rate / first(fertility_rate)) * 100,
    marriage_index = (marriage_rate / first(marriage_rate)) * 100,
    band_low = pmin(fertility_index, marriage_index),
    band_high = pmax(fertility_index, marriage_index)
  )

fertility_change_10y <- (last(figure3_data$fertility_rate) / first(figure3_data$fertility_rate) - 1) * 100
marriage_change_10y <- (last(figure3_data$marriage_rate) / first(figure3_data$marriage_rate) - 1) * 100

figure3 <- ggplot(figure3_data, aes(x = year)) +
  geom_ribbon(aes(ymin = band_low, ymax = band_high, fill = "Gap between series"), alpha = 0.8) +
  geom_line(aes(y = fertility_index, color = "Total fertility rate"), linewidth = 1.3) +
  geom_line(aes(y = marriage_index, color = "Crude marriage rate"), linewidth = 1.3) +
  geom_point(aes(y = fertility_index, color = "Total fertility rate"), size = 2.2) +
  geom_point(aes(y = marriage_index, color = "Crude marriage rate"), size = 2.2) +
  geom_text(
    aes(y = fertility_index, label = sprintf("%.3f", fertility_rate)),
    color = "#426f87", size = 2.7, vjust = -1, show.legend = FALSE
  ) +
  geom_text(
    aes(y = marriage_index, label = sprintf("%.1f", marriage_rate)),
    color = "#487571", size = 2.7, vjust = 2, show.legend = FALSE
  ) +
  scale_color_manual(
    name = "",
    values = c("Total fertility rate" = "#0288D1", "Crude marriage rate" = "#26A69A"),
    breaks = c("Total fertility rate", "Crude marriage rate"),
    labels = indicator_legend_labels
  ) +
  scale_fill_manual(name = "", values = c("Gap between series" = "#ECEFF1"), breaks = "Gap between series", labels = "Gap between two index series") +
  guides(
    color = guide_legend(order = 1, override.aes = list(linewidth = 1.3, size = 3)),
    fill = guide_legend(order = 2)
  ) +
  scale_x_continuous(breaks = figure3_data$year) +
  labs(
    title = "Figure 3. Marriage Rates Declined Faster Than Fertility in Korea",
    subtitle = "National trend indices rebased to 2015 = 100",
    x = "Year",
    y = "Index (2015 = 100)",
    caption = paste0(
      "Source: Vital Statistics of Korea | Marriage change (2015-2024): ", sprintf("%+.1f%%", marriage_change_10y),
      " | Fertility change (2015-2024): ", sprintf("%+.1f%%", fertility_change_10y)
    )
  )

ggsave("fig3.pdf", figure3, device = pdf_device, width = 10, height = 6.5, units = "in")

# ---------------------------------------------------------
# latex tables
# ---------------------------------------------------------
print_latex_skim <- function(df, title) {
  tex_out <- capture.output(datasummary_skim(df, output = "latex", fmt = 2, type = "numeric", title = title))
  cat("\n% ", title, "\n")
  cat(paste(tex_out, collapse = "\n"), "\n\n")
}

# summary 1: regional youth share and TFR
print_latex_skim(
  regional_data |> transmute(youth_share, tfr_2024),
  "Descriptive Statistics: Regional Youth Share and TFR (2024)"
)

# summary 2: national marriage/fertility time series
print_latex_skim(
  figure3_data |> transmute(fertility_rate, marriage_rate, fertility_index, marriage_index),
  "Descriptive Statistics: National Marriage and Fertility Time-Series"
)

# summary 3: optional provincial fertility change
print_latex_skim(
  provincial_change_data,
  "Descriptive Statistics: Provincial Fertility Change"
)
