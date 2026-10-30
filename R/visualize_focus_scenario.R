#!/usr/bin/env Rscript

# Focused JMIP3 visualisation for one scenario in an IAMC-template GDX file.
#
# Default run (SSP2_CM15_NoCC_No = -5by2050_-5by2100, 1008_3):
#   Rscript R/visualize_focus_scenario.R
#
# Optional arguments:
#   --gdx=data/1008/1008_3/JPN_IAMC.gdx
#   --scenario=SSP2_CM15_NoCC_No
#   --scenario-label=-5by2050_-5by2100
#   --region=JPN
#   --out=output/1008_3_gdx_overview/focus_-5by2050_-5by2100

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
    gdx = file.path("data", "1008", "1008_3", "JPN_IAMC.gdx"),
    scenario = "SSP2_CM15_NoCC_No",
    scenario_label = "-5by2050_-5by2100",
    region = "JPN",
    out = file.path(
      "output", "1008_3_gdx_overview", "focus_-5by2050_-5by2100"
    )
  )
  for (arg in args) {
    if (grepl("^--gdx=", arg)) defaults$gdx <- sub("^--gdx=", "", arg)
    if (grepl("^--scenario=", arg)) {
      defaults$scenario <- sub("^--scenario=", "", arg)
    }
    if (grepl("^--scenario-label=", arg)) {
      defaults$scenario_label <- sub("^--scenario-label=", "", arg)
    }
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
  filter(
    scenario == opt$scenario,
    region == opt$region,
    year >= 2010,
    year <= 2100
  ) %>%
  group_by(scenario, region, variable, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

if (nrow(raw) == 0) {
  stop(
    "No records found for scenario/region: ", opt$scenario, " / ", opt$region
  )
}

years <- sort(unique(raw$year))

theme_jmip <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.3)),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = "bottom",
      legend.text = element_text(size = 9),
      plot.margin = margin(10, 16, 10, 10)
    )
}

save_plot <- function(filename, plot, width = 13, height = 7.5) {
  png_path <- file.path(opt$out, paste0(filename, ".png"))
  pdf_path <- file.path(pdf_dir, paste0(filename, ".pdf"))
  ggsave(png_path, plot, width = width, height = height, dpi = 240, bg = "white")
  ggsave(
    pdf_path, plot, width = width, height = height,
    device = cairo_pdf, bg = "white"
  )
  message("  wrote ", png_path)
}

# IAMC GDX is sparse: for the selected flow variables, absent records are zero.
complete_components <- function(spec) {
  raw %>%
    filter(variable %in% spec$variable) %>%
    select(year, variable, value) %>%
    complete(year = years, variable = spec$variable, fill = list(value = 0)) %>%
    left_join(spec, by = "variable") %>%
    mutate(component = factor(component, levels = spec$component)) %>%
    arrange(year, component)
}

energy_palette <- c(
  "#2F6B9A", "#78A9D1", "#303030", "#8C8C8C", "#C8781A",
  "#EDB458", "#7C4A21", "#C3976B", "#8064A2", "#4E9F6D",
  "#2BAE66", "#EE6A4B", "#69A9D2"
)
process_palette <- c(
  "#2667A8", "#6BAED6", "#2CA25F", "#E6AB02", "#8C6BB1", "#D95F0E"
)
emissions_palette <- c(
  "#4D9221", "#C51B7D", "#E66101", "#5E3C99", "#4393C3", "#B2182B"
)
electricity_palette <- c(
  "#1B9E77", "#D95F02", "#7570B3", "#E6AB02", "#666666"
)

plot_area <- function(data, title, unit, colours, filename, legend_ncol = 3) {
  component_levels <- levels(data$component)
  fill_values <- setNames(colours[seq_along(component_levels)], component_levels)

  p <- ggplot(data, aes(year, value, fill = component)) +
    geom_area(position = "stack", linewidth = 0.12, colour = "white", alpha = 0.96) +
    geom_hline(yintercept = 0, linewidth = 0.35, colour = "grey30") +
    scale_fill_manual(values = fill_values, drop = FALSE) +
    scale_x_continuous(
      breaks = seq(2020, 2100, 20),
      expand = expansion(mult = c(0, 0.01))
    ) +
    scale_y_continuous(
      labels = label_number(big.mark = ",", accuracy = 0.1),
      expand = expansion(mult = c(0.04, 0.08))
    ) +
    labs(
      title = paste0(title, ": ", opt$scenario_label),
      x = NULL, y = unit, fill = NULL
    ) +
    guides(fill = guide_legend(ncol = legend_ncol, byrow = TRUE)) +
    theme_jmip()

  write_csv(
    data %>% mutate(component = as.character(component)),
    file.path(csv_dir, paste0(filename, ".csv"))
  )
  save_plot(filename, p)
}

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
  ccs = tribble(
    ~variable, ~component,
    "Car_Cap_Geo_Sto_Bio", "Biomass",
    "Car_Cap_Geo_Sto_Dir_Air_Cap", "Direct air capture",
    "Car_Cap_Geo_Sto_Fos", "Fossil",
    "Car_Cap_Geo_Sto_Ind_Pro", "Industrial processes",
    "Car_Cap_Geo_Sto_Oth_Sou", "Other sources",
    "Car_Cap_Geo_Sto_Syn_Fue", "Synthetic fuels"
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
  co2_emissions = tribble(
    ~variable, ~component,
    "Emi_CO2_AFO", "AFOLU",
    "Emi_CO2_Ene_Sup", "Energy supply",
    "Emi_CO2_Ene_Dem", "Energy demand",
    "Emi_CO2_Ind_Pro", "Industrial processes",
    "Emi_CO2_Pro_Use", "Product use",
    "Emi_CO2_Cap_and_Rem", "Carbon capture/removal"
  )
)

primary_data <- complete_components(mix_specs$primary_energy)
plot_area(
  primary_data, "Primary energy", "EJ/yr", energy_palette,
  "01_primary_energy_annual", 4
)

power_data <- complete_components(mix_specs$power_generation)
plot_area(
  power_data, "Power generation", "EJ/yr", energy_palette,
  "02_power_generation_annual", 4
)

final_data <- complete_components(mix_specs$final_energy)
plot_area(
  final_data, "Final energy demand", "EJ/yr", energy_palette,
  "03_final_energy_demand_annual", 4
)

ccs_data <- complete_components(mix_specs$ccs)
plot_area(
  ccs_data, "CCS geological storage by source", "Mt CO2/yr", process_palette,
  "04_ccs_annual", 3
)

cdr_data <- complete_components(mix_specs$cdr)
plot_area(
  cdr_data, "Carbon dioxide removal", "Mt CO2/yr", process_palette,
  "05_cdr_annual", 3
)

co2_data <- complete_components(mix_specs$co2_emissions)
plot_area(
  co2_data, "CO2 emissions by source", "Mt CO2/yr", emissions_palette,
  "06_emi_co2_annual", 3
)

# Electricity use: the four reported end-use sectors do not always add up to
# Fin_Ene_Ele. The non-negative residual is retained as Other / unallocated.
electricity_spec <- tribble(
  ~variable, ~component,
  "Fin_Ene_Ind_Ele", "Industry",
  "Fin_Ene_Res_and_Com_Ele", "Residential & commercial",
  "Fin_Ene_Tra_Ele", "Transport",
  "Fin_Ene_Agr_Ele", "Agriculture"
)

electricity_sector_wide <- complete_components(electricity_spec) %>%
  mutate(component = as.character(component)) %>%
  select(year, component, value) %>%
  pivot_wider(names_from = component, values_from = value, values_fill = 0)

electricity_total <- raw %>%
  filter(variable == "Fin_Ene_Ele") %>%
  select(year, total = value) %>%
  complete(year = years, fill = list(total = 0))

electricity_data <- electricity_sector_wide %>%
  left_join(electricity_total, by = "year") %>%
  mutate(
    `Other / unallocated` = pmax(
      total - Industry - `Residential & commercial` - Transport - Agriculture,
      0
    )
  ) %>%
  select(-total) %>%
  pivot_longer(-year, names_to = "component", values_to = "value") %>%
  mutate(
    component = factor(
      component,
      levels = c(
        "Industry", "Residential & commercial", "Transport", "Agriculture",
        "Other / unallocated"
      )
    )
  ) %>%
  arrange(year, component)

plot_area(
  electricity_data, "Electricity use by sector", "EJ/yr",
  electricity_palette, "07_electricity_use_annual", 3
)

scalar_spec <- tribble(
  ~variable, ~metric, ~unit,
  "Prc_Car", "Carbon price", "USD_2010/t CO2",
  "Trd_Emi_All_Vol", "Emissions allowance trade", "Mt CO2-eq/yr"
)

scalar_data <- raw %>%
  filter(variable %in% scalar_spec$variable) %>%
  select(year, variable, value) %>%
  complete(year = years, variable = scalar_spec$variable, fill = list(value = 0)) %>%
  left_join(scalar_spec, by = "variable") %>%
  mutate(
    metric = factor(metric, levels = scalar_spec$metric),
    panel = paste0(metric, "\n[", unit, "]")
  ) %>%
  arrange(metric, year)

write_csv(scalar_data, file.path(csv_dir, "08_carbon_price_and_trade.csv"))

p_scalar <- ggplot(scalar_data, aes(year, value, colour = metric, shape = metric)) +
  geom_hline(yintercept = 0, linewidth = 0.35, colour = "grey40") +
  geom_line(linewidth = 1.05) +
  geom_point(size = 2.25, stroke = 0.7, fill = "white") +
  facet_wrap(~panel, scales = "free_y", ncol = 1) +
  scale_colour_manual(values = c("Carbon price" = "#0072B2", "Emissions allowance trade" = "#D55E00")) +
  scale_shape_manual(values = c("Carbon price" = 16, "Emissions allowance trade" = 17)) +
  scale_x_continuous(
    breaks = seq(2020, 2100, 20),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 0.1)) +
  labs(
    title = paste0("Carbon price and emissions allowance trade: ", opt$scenario_label),
    x = NULL, y = NULL, colour = NULL, shape = NULL
  ) +
  theme_jmip() +
  theme(legend.position = "none")

save_plot("08_carbon_price_and_trade", p_scalar, width = 12, height = 9)

# Reconciliation table for the aggregate series represented by each area plot.
aggregate_specs <- tribble(
  ~chart, ~aggregate_variable,
  "Primary energy", "Prm_Ene",
  "Power generation", "Sec_Ene_Ele",
  "Final energy demand", "Fin_Ene",
  "CCS geological storage", "Car_Cap_Geo_Sto",
  "Electricity use", "Fin_Ene_Ele"
)

component_totals <- bind_rows(
  primary_data %>% group_by(year) %>% summarise(component_sum = sum(value), .groups = "drop") %>% mutate(chart = "Primary energy"),
  power_data %>% group_by(year) %>% summarise(component_sum = sum(value), .groups = "drop") %>% mutate(chart = "Power generation"),
  final_data %>% group_by(year) %>% summarise(component_sum = sum(value), .groups = "drop") %>% mutate(chart = "Final energy demand"),
  ccs_data %>% group_by(year) %>% summarise(component_sum = sum(value), .groups = "drop") %>% mutate(chart = "CCS geological storage"),
  electricity_data %>% group_by(year) %>% summarise(component_sum = sum(value), .groups = "drop") %>% mutate(chart = "Electricity use")
)

aggregate_totals <- raw %>%
  inner_join(aggregate_specs, by = c("variable" = "aggregate_variable")) %>%
  select(chart, year, aggregate_value = value)

reconciliation <- component_totals %>%
  left_join(aggregate_totals, by = c("chart", "year")) %>%
  mutate(difference = component_sum - aggregate_value) %>%
  arrange(chart, year)

write_csv(reconciliation, file.path(csv_dir, "component_reconciliation.csv"))

metadata <- tibble(
  item = c(
    "GDX source", "Region", "Scenario code", "Scenario label", "Years",
    "Output note", "Electricity-use residual"
  ),
  value = c(
    normalizePath(opt$gdx, winslash = "/", mustWork = TRUE),
    opt$region,
    opt$scenario,
    opt$scenario_label,
    paste0(min(years), "-", max(years), " (model periods)"),
    "PNG in output root; PDF in pdf/; CSV in csv/; no subtitles",
    "Fin_Ene_Ele minus Industry, Residential & commercial, Transport, and Agriculture; floored at zero"
  )
)
write_csv(metadata, file.path(csv_dir, "run_metadata.csv"))

message("Done: ", normalizePath(opt$out, winslash = "/", mustWork = TRUE))
