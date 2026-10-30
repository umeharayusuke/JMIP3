#!/usr/bin/env Rscript

# Quick-look visualisation for the AIM/JMIP3 IAMC-template GDX file.
#
# Usage:
#   Rscript R/visualize_1008_gdx.R
#   Rscript R/visualize_1008_gdx.R --gdx=data/1008/JPN_IAMC.gdx \
#     --region=JPN --out=output/1008_gdx_overview

# Rscript launched from a sandboxed shell may not inherit the ordinary Windows
# user-library path, so add it explicitly when it exists.
r_minor <- strsplit(R.version$minor, "\\.", fixed = FALSE)[[1]][1]
user_library <- file.path(
  Sys.getenv("USERPROFILE"), "AppData", "Local", "R", "win-library",
  paste(R.version$major, r_minor, sep = ".")
)
extra_libraries <- c(Sys.getenv("JMIP3_R_LIB", unset = NA_character_), user_library)
extra_libraries <- extra_libraries[!is.na(extra_libraries) & dir.exists(extra_libraries)]
.libPaths(unique(c(extra_libraries, .libPaths())))

suppressPackageStartupMessages({
  library(gdxrrw)
  library(tidyverse)
  library(scales)
})

parse_args <- function(args) {
  defaults <- list(
    gdx = file.path("data", "1008", "JPN_IAMC.gdx"),
    region = "JPN",
    out = file.path("output", "1008_gdx_overview")
  )
  for (arg in args) {
    if (grepl("^--gdx=", arg)) defaults$gdx <- sub("^--gdx=", "", arg)
    if (grepl("^--region=", arg)) defaults$region <- sub("^--region=", "", arg)
    if (grepl("^--out=", arg)) defaults$out <- sub("^--out=", "", arg)
  }
  defaults
}

opt <- parse_args(commandArgs(trailingOnly = TRUE))
if (!file.exists(opt$gdx)) stop("GDX file not found: ", opt$gdx)
dir.create(opt$out, recursive = TRUE, showWarnings = FALSE)
pdf_dir <- file.path(opt$out, "pdf")
csv_dir <- file.path(opt$out, "csv")
dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)

message("Reading ", opt$gdx, " ...")
raw <- rgdx.param(opt$gdx, "IAMC_template") %>%
  transmute(
    scenario = as.character(SCENARIO),
    region = as.character(REMF),
    variable = as.character(VEMF),
    year = as.integer(as.character(YEMF)),
    value = as.numeric(IAMC_Template)
  ) %>%
  filter(region == opt$region, year >= 2010, year <= 2100)

if (nrow(raw) == 0) stop("No records found for region: ", opt$region)

scenario_number <- function(x) {
  ifelse(grepl("BaU", x), 0L, as.integer(str_extract(x, "(?<=CM)\\d+")))
}

scenarios <- raw %>%
  distinct(scenario) %>%
  mutate(
    scenario_id = scenario_number(scenario),
    cm_group = if_else(scenario_id == 0L, 0L, as.integer(ceiling(scenario_id / 6))),
    cm_member = if_else(scenario_id == 0L, 0L, (scenario_id - 1L) %% 6L + 1L),
    group_order = case_when(
      cm_group == 0L ~ 0L, # BASELINE
      cm_group == 2L ~ 1L, # 20by2050
      cm_group == 1L ~ 2L, # 0by2050
      cm_group == 3L ~ 3L, # -5by2050
      cm_group == 4L ~ 4L, # -20by2050
      TRUE ~ 99L
    )
  ) %>%
  arrange(group_order, cm_member, scenario) %>%
  pull(scenario)

years <- seq(2010, 2100, by = 5)
key_years <- c(2050L, 2100L)

short_scenario <- function(x) {
  ifelse(grepl("BaU", x), "BaU", paste0("CM", scenario_number(x)))
}

scenario_levels_short <- short_scenario(scenarios)
scenario_ids <- scenario_number(scenarios)
cm_groups <- ifelse(scenario_ids == 0L, 0L, ceiling(scenario_ids / 6))
cm_members <- ifelse(scenario_ids == 0L, 0L, (scenario_ids - 1L) %% 6L + 1L)

# Colour identifies blocks of six scenarios. Line type and point shape then
# distinguish scenarios within a block, giving every scenario a unique style.
group_colours <- c(
  "0" = "#111111", # BaU
  "1" = "#0072B2", # CM1-6
  "2" = "#009E73", # CM7-12
  "3" = "#D55E00", # CM13-18
  "4" = "#CC79A7"  # CM19-24
)
member_linetypes <- c("solid", "dashed", "dotdash", "longdash", "twodash", "dotted")
member_shapes <- c(16, 17, 15, 18, 3, 4)

scenario_cols <- setNames(unname(group_colours[as.character(cm_groups)]), scenarios)
scenario_linetypes <- setNames(
  ifelse(scenario_ids == 0L, "solid", member_linetypes[cm_members]), scenarios
)
scenario_shapes <- setNames(
  ifelse(scenario_ids == 0L, 8, member_shapes[cm_members]), scenarios
)

# JMIP3.1 target labels supplied for the 24 CM scenarios. The six members in
# each colour block are the 2100 targets; the four colour blocks are the 2050
# targets.
target_2050 <- c("0", "20", "-5", "-20")
target_2100 <- c("20", "0", "-5", "-20", "-50", "-100")
scenario_legend_labels <- ifelse(
  scenario_ids == 0L,
  "BASELINE",
  paste0(
    target_2050[pmax(cm_groups, 1L)], "by2050_",
    target_2100[pmax(cm_members, 1L)], "by2100"
  )
)
scenario_legend_labels <- setNames(scenario_legend_labels, scenarios)

theme_jmip <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.35)),
      plot.subtitle = element_text(color = "grey35"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      plot.margin = margin(10, 16, 10, 10)
    )
}

save_plot <- function(filename, plot, width, height) {
  png_path <- file.path(opt$out, paste0(filename, ".png"))
  pdf_path <- file.path(pdf_dir, paste0(filename, ".pdf"))
  ggsave(png_path, plot, width = width, height = height, dpi = 220, bg = "white")
  ggsave(pdf_path, plot, width = width, height = height, device = cairo_pdf, bg = "white")
  message("  wrote ", png_path)
}

# Missing records in the sparse GDX mean zero for the flow/price variables below.
complete_series <- function(data, variables) {
  data %>%
    filter(variable %in% variables) %>%
    select(scenario, year, variable, value) %>%
    complete(
      scenario = scenarios,
      year = years,
      variable = variables,
      fill = list(value = 0)
    )
}

cdr_codes <- c(
  "Car_Rem_Bio_wit_CCS",
  "Car_Rem_Dir_Air_Cap_wit_CCS",
  "Car_Rem_Enh_Wea",
  "Car_Rem_Bio",
  "Car_Rem_Frs",
  "Car_Rem_Soi_Car_Seq"
)

direct_metrics <- tribble(
  ~variable, ~metric, ~unit,
  "Prc_Car", "Carbon price", "USD_2010/t CO2",
  "Prm_Ene", "Primary energy", "EJ/yr",
  "Sec_Ene_Ele", "Power generation", "EJ/yr",
  "Fin_Ene", "Final energy", "EJ/yr",
  "Car_Cap_Geo_Sto", "CCS geological storage", "Mt CO2/yr",
  "Trd_Emi_All_Vol", "Emissions allowance trade", "Mt CO2-eq/yr",
  "Emi_CO2_Ene_and_Ind_Pro", "Energy and industrial CO2", "Mt CO2/yr"
)

direct_ts <- complete_series(raw, direct_metrics$variable) %>%
  left_join(direct_metrics, by = "variable")

cdr_ts <- complete_series(raw, cdr_codes) %>%
  group_by(scenario, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    variable = "CDR_selected_pathways",
    metric = "CDR (selected pathways)",
    unit = "Mt CO2/yr"
  )

metric_order <- c(
  "Carbon price", "Energy and industrial CO2", "Primary energy",
  "Power generation", "Final energy", "CDR (selected pathways)",
  "CCS geological storage", "Emissions allowance trade"
)

metrics <- bind_rows(direct_ts, cdr_ts) %>%
  mutate(
    scenario = factor(scenario, levels = scenarios),
    scenario_short = factor(short_scenario(as.character(scenario)),
                            levels = scenario_levels_short),
    metric = factor(metric, levels = metric_order),
    panel = paste0(metric, "\n[", unit, "]")
  ) %>%
  arrange(metric, scenario, year)

write_csv(metrics %>% mutate(scenario = as.character(scenario)),
          file.path(csv_dir, "selected_timeseries.csv"))

p_ts <- ggplot(
  metrics,
  aes(
    year, value, group = scenario, color = scenario,
    linetype = scenario, shape = scenario
  )
) +
  geom_line(linewidth = 0.52, alpha = 0.9, na.rm = TRUE) +
  geom_point(
    data = ~filter(.x, year %% 10 == 0),
    size = 1.05, stroke = 0.4, alpha = 0.95, na.rm = TRUE
  ) +
  facet_wrap(~panel, scales = "free_y", ncol = 2) +
  scale_color_manual(
    values = scenario_cols, labels = scenario_legend_labels, drop = FALSE
  ) +
  scale_linetype_manual(
    values = scenario_linetypes, labels = scenario_legend_labels, drop = FALSE
  ) +
  scale_shape_manual(
    values = scenario_shapes, labels = scenario_legend_labels, drop = FALSE
  ) +
  scale_x_continuous(breaks = seq(2020, 2100, 20)) +
  scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1)) +
  labs(
    title = paste0("JMIP3 scenario overview: ", opt$region),
    x = NULL, y = NULL,
    color = "Scenario target", linetype = "Scenario target", shape = "Scenario target"
  ) +
  guides(
    color = guide_legend(
      ncol = 3, byrow = TRUE,
      override.aes = list(linewidth = 1.1, size = 2.2, alpha = 1)
    )
  ) +
  theme_jmip(10) +
  theme(legend.text = element_text(size = 8), legend.key.width = unit(18, "pt"))

save_plot("01_all_scenarios_timeseries", p_ts, 16, 17)

# Key-year table for downstream analysis. Percentiles make metrics with unlike
# units comparable in the CSV without requiring a separate heatmap figure.
endpoints <- metrics %>%
  filter(year %in% key_years) %>%
  group_by(metric, year) %>%
  mutate(percentile = if (n_distinct(value) <= 1) 50 else percent_rank(value) * 100) %>%
  ungroup()

write_csv(endpoints %>% mutate(scenario = as.character(scenario)),
          file.path(csv_dir, "selected_key_years.csv"))

mix_specs <- list(
  primary_energy = tribble(
    ~variable, ~component,
    "Prm_Ene_Bio_w_CCS", "Biomass w/ CCS",
    "Prm_Ene_Bio_wo_CCS", "Biomass w/o CCS",
    "Prm_Ene_Coa_w_CCS", "Coal w/ CCS",
    "Prm_Ene_Coa_wo_CCS", "Coal w/o CCS",
    "Prm_Ene_Gas_w_CCS", "Gas w/ CCS",
    "Prm_Ene_Gas_wo_CCS", "Gas w/o CCS",
    "Prm_Ene_Oil_w_CCS", "Oil w/ CCS",
    "Prm_Ene_Oil_wo_CCS", "Oil w/o CCS",
    "Prm_Ene_Geo", "Geothermal",
    "Prm_Ene_Hyp", "Hydro",
    "Prm_Ene_Nuc", "Nuclear",
    "Prm_Ene_Solar", "Solar",
    "Prm_Ene_Win", "Wind"
  ),
  power_generation = tribble(
    ~variable, ~component,
    "Sec_Ene_Ele_Bio_w_CCS", "Biomass w/ CCS",
    "Sec_Ene_Ele_Bio_wo_CCS", "Biomass w/o CCS",
    "Sec_Ene_Ele_Coa_w_CCS", "Coal w/ CCS",
    "Sec_Ene_Ele_Coa_wo_CCS", "Coal w/o CCS",
    "Sec_Ene_Ele_Gas_w_CCS", "Gas w/ CCS",
    "Sec_Ene_Ele_Gas_wo_CCS", "Gas w/o CCS",
    "Sec_Ene_Ele_Oil_w_CCS", "Oil w/ CCS",
    "Sec_Ene_Ele_Oil_wo_CCS", "Oil w/o CCS",
    "Sec_Ene_Ele_Geo", "Geothermal",
    "Sec_Ene_Ele_Hyp", "Hydro",
    "Sec_Ene_Ele_Nuc", "Nuclear",
    "Sec_Ene_Ele_Solar", "Solar",
    "Sec_Ene_Ele_Win", "Wind"
  ),
  final_energy = tribble(
    ~variable, ~component,
    "Fin_Ene_Ele", "Electricity",
    "Fin_Ene_Gas", "Gas",
    "Fin_Ene_Heat", "Heat",
    "Fin_Ene_Hyd", "Hydrogen",
    "Fin_Ene_Liq", "Liquids",
    "Fin_Ene_Solids", "Solids",
    "Fin_Ene_Geo", "Geothermal",
    "Fin_Ene_Solar", "Solar"
  ),
  cdr = tribble(
    ~variable, ~component,
    "Car_Rem_Bio_wit_CCS", "BECCS",
    "Car_Rem_Dir_Air_Cap_wit_CCS", "DACCS",
    "Car_Rem_Enh_Wea", "Enhanced weathering",
    "Car_Rem_Bio", "Biochar",
    "Car_Rem_Frs", "Afforestation",
    "Car_Rem_Soi_Car_Seq", "Soil carbon"
  ),
  ccs_storage = tribble(
    ~variable, ~component,
    "Car_Cap_Geo_Sto_Bio", "Biomass",
    "Car_Cap_Geo_Sto_Dir_Air_Cap", "Direct air capture",
    "Car_Cap_Geo_Sto_Fos", "Fossil",
    "Car_Cap_Geo_Sto_Ind_Pro", "Industrial processes",
    "Car_Cap_Geo_Sto_Oth_Sou", "Other sources",
    "Car_Cap_Geo_Sto_Syn_Fue", "Synthetic fuels"
  )
)

mix_meta <- tribble(
  ~name, ~title, ~unit, ~file_no,
  "primary_energy", "Primary energy mix", "EJ/yr", "03",
  "power_generation", "Power generation mix", "EJ/yr", "04",
  "final_energy", "Final energy mix", "EJ/yr", "05",
  "cdr", "Carbon dioxide removal by pathway", "Mt CO2/yr", "06",
  "ccs_storage", "CCS geological storage by source", "Mt CO2/yr", "07"
)

mix_palette <- c(
  "#3B6FB6", "#79A7D8", "#3B3B3B", "#8A8A8A", "#D9922E",
  "#F0C36E", "#8B5A2B", "#C49A6C", "#8C6BB1", "#5AAE61",
  "#2CA25F", "#F46D43", "#74ADD1"
)

for (i in seq_len(nrow(mix_meta))) {
  meta <- mix_meta[i, ]
  spec <- mix_specs[[meta$name]]

  mix_data <- complete_series(raw, spec$variable) %>%
    filter(year %in% key_years) %>%
    left_join(spec, by = "variable") %>%
    mutate(
      scenario_short = factor(short_scenario(scenario), levels = scenario_levels_short),
      scenario_target = factor(
        unname(scenario_legend_labels[scenario]),
        levels = rev(unname(scenario_legend_labels[scenarios]))
      ),
      component = factor(component, levels = spec$component)
    )

  write_csv(mix_data, file.path(csv_dir, paste0(meta$file_no, "_", meta$name, "_data.csv")))

  cols <- setNames(mix_palette[seq_len(nrow(spec))], spec$component)
  p_mix <- ggplot(mix_data, aes(value, scenario_target, fill = component)) +
    geom_col(width = 0.82) +
    facet_wrap(~year, ncol = 2, scales = "free_x") +
    scale_fill_manual(values = cols, drop = FALSE) +
    scale_x_continuous(labels = label_number(big.mark = ",", accuracy = 0.1),
                       expand = expansion(mult = c(0, 0.06))) +
    labs(
      title = paste0(meta$title, ": ", opt$region),
      x = meta$unit, y = NULL, fill = NULL
    ) +
    guides(fill = guide_legend(ncol = 4, byrow = TRUE)) +
    theme_jmip(10) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      axis.text.y = element_text(size = 8),
      legend.text = element_text(size = 8)
    )

  save_plot(paste0(meta$file_no, "_", meta$name, "_2050_2100"),
            p_mix, 18, 13)
}

inventory <- tibble(
  item = c(
    "GDX source", "Region", "Scenarios", "Years", "Time-series metrics",
    "CDR definition", "CCS definition", "Sparse records"
  ),
  value = c(
    normalizePath(opt$gdx, winslash = "/", mustWork = TRUE),
    opt$region,
    as.character(length(scenarios)),
    paste0(min(years), "-", max(years), " (5-year steps)"),
    paste(metric_order, collapse = "; "),
    paste("Sum of", paste(cdr_codes, collapse = ", ")),
    "Car_Cap_Geo_Sto (geological storage flow)",
    "Missing selected-variable records are treated as zero"
  )
)
write_csv(inventory, file.path(csv_dir, "run_metadata.csv"))

message("Done: ", normalizePath(opt$out, winslash = "/", mustWork = TRUE))
