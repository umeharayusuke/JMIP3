#!/usr/bin/env Rscript

# Presentation figures for the offshore-wind sensitivity analysis.
# Figure 01: CO2-emission trajectories and sectoral composition in 2050/2100.
# Figure 02: policy costs, LCOE, and electricity prices.
# Figure 03: power-generation and final-energy composition in 2050/2100.
# Figure 04: CDR, CCS, and cumulative geological CO2 storage.
# Figure 05: GLMDI decomposition of net CO2-emission changes, 2020-2100.

find_project_root <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  script_dir <- if (length(file_arg) > 0) {
    dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE))
  } else {
    NA_character_
  }

  start_dirs <- unique(na.omit(c(script_dir, getwd())))
  candidates <- unique(unlist(lapply(start_dirs, function(path) {
    path <- normalizePath(path, mustWork = TRUE)
    c(path, dirname(path), dirname(dirname(path)))
  })))
  is_root <- file.exists(file.path(candidates, "R", "offshore_analysis.R")) &
    file.exists(file.path(
      candidates, "data", "offshore_sensitivity", "JPN_IAMC.gdx"
    ))

  if (!any(is_root)) {
    stop("Could not locate the JMIP3 project root from the script or working directory.")
  }
  normalizePath(candidates[which(is_root)[[1]]], winslash = "/", mustWork = TRUE)
}

project_root <- find_project_root()

r_minor <- strsplit(R.version$minor, "\\.")[[1]][1]
user_library <- file.path(
  Sys.getenv("USERPROFILE"), "AppData", "Local", "R", "win-library",
  paste(R.version$major, r_minor, sep = ".")
)
.libPaths(unique(c(user_library, .libPaths())))

suppressPackageStartupMessages({
  library(gdxrrw)
  library(tidyverse)
  library(scales)
  library(grid)
})

source(file.path(project_root, "R", "others", "theme_ncc.R"))

gdx_file <- file.path(
  project_root, "data", "offshore_sensitivity", "JPN_IAMC.gdx"
)
output_dir <- file.path(project_root, "output", "Laboseminar261104")
output_file <- file.path(output_dir, "01_co2_emissions.png")
output_file_2 <- file.path(output_dir, "02_costs_and_lcoe.png")
output_file_3 <- file.path(output_dir, "03_energy_composition.png")
output_file_4 <- file.path(output_dir, "04_cdr_ccs.png")
output_file_5 <- file.path(output_dir, "05_emissions_decomposition.png")

if (!file.exists(gdx_file)) stop("GDX file not found: ", gdx_file)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Canonical scenario order for every figure in this analysis.
scenario_key <- c(
  "SSP2_BaU_NoCC_No" = "BaU",
  "SSP2_CM2_NoCC_No" = "NZE",
  "SSP2_CM2_cap2_cf10_NoCC_No" = "NZE_cap/high",
  "SSP2_CM2_cap1_cf05_NoCC_No" = "NZE_CF/low",
  "SSP2_CM2_cap2_cf05_NoCC_No" = "NZE_cap/high_CF/low",
  "SSP2_CM2_off_NoCC_No" = "NZE_wo/offshore"
)
scenario_levels <- unname(scenario_key)

# Colour-blind-safe palette, with line type and shape as redundant encodings.
scenario_colours <- c(
  "BaU" = "#222222",
  "NZE" = "#0072B2",
  "NZE_wo/offshore" = "#7A7A7A",
  "NZE_cap/high_CF/low" = "#CC79A7",
  "NZE_cap/high" = "#E69F00",
  "NZE_CF/low" = "#009E73"
)
scenario_linetypes <- c(
  "BaU" = "solid",
  "NZE" = "solid",
  "NZE_wo/offshore" = "dotted",
  "NZE_cap/high_CF/low" = "longdash",
  "NZE_cap/high" = "dashed",
  "NZE_CF/low" = "dotdash"
)
scenario_shapes <- c(
  "BaU" = 16, "NZE" = 17, "NZE_wo/offshore" = 4,
  "NZE_cap/high_CF/low" = 18, "NZE_cap/high" = 15, "NZE_CF/low" = 3
)

component_key <- c(
  "Emi_CO2_AFO" = "AFOLU",
  "Emi_CO2_Ene" = "Energy",
  "Emi_CO2_Ind_Pro" = "Industrial Processes",
  "Emi_CO2_Pro_Use" = "Product Use",
  "Emi_CO2_Was" = "Waste",
  "Emi_CO2_Cap_and_Rem" = "Capture and Removal"
)
sector_levels <- c(
  "AFOLU", "Energy", "Industrial Processes", "Other",
  "Product Use", "Waste", "Capture and Removal"
)
sector_colours <- c(
  "AFOLU" = "#009E73",
  "Energy" = "#D55E00",
  "Industrial Processes" = "#0072B2",
  "Other" = "#7A7A7A",
  "Product Use" = "#CC79A7",
  "Waste" = "#E69F00",
  "Capture and Removal" = "#56B4E9"
)

raw <- rgdx.param(gdx_file, "IAMC_template") %>%
  transmute(
    scenario_code = as.character(SCENARIO),
    region = as.character(REMF),
    variable = as.character(VEMF),
    year = as.integer(as.character(YEMF)),
    value = as.numeric(IAMC_Template)
  ) %>%
  filter(
    scenario_code %in% names(scenario_key),
    region == "JPN",
    year >= 2020,
    year <= 2100
  ) %>%
  group_by(scenario_code, variable, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    scenario = factor(unname(scenario_key[scenario_code]), levels = scenario_levels)
  )

missing_scenarios <- setdiff(names(scenario_key), unique(raw$scenario_code))
if (length(missing_scenarios) > 0) {
  stop("Scenario(s) missing from GDX: ", paste(missing_scenarios, collapse = ", "))
}

total_emissions <- raw %>%
  filter(variable == "Emi_CO2")

composition_wide <- raw %>%
  filter(
    variable %in% c("Emi_CO2", names(component_key)),
    year %in% c(2050L, 2100L)
  ) %>%
  select(scenario, year, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value, values_fill = 0) %>%
  mutate(
    Other = Emi_CO2 - Emi_CO2_AFO - Emi_CO2_Ene - Emi_CO2_Ind_Pro -
      Emi_CO2_Pro_Use - Emi_CO2_Was - Emi_CO2_Cap_and_Rem
  )

composition <- composition_wide %>%
  filter(scenario != "BaU") %>%
  select(
    scenario, year,
    AFOLU = Emi_CO2_AFO,
    Energy = Emi_CO2_Ene,
    `Industrial Processes` = Emi_CO2_Ind_Pro,
    Other,
    `Product Use` = Emi_CO2_Pro_Use,
    Waste = Emi_CO2_Was,
    `Capture and Removal` = Emi_CO2_Cap_and_Rem
  ) %>%
  pivot_longer(
    cols = all_of(sector_levels), names_to = "sector", values_to = "value"
  ) %>%
  mutate(
    sector = factor(sector, levels = sector_levels),
    scenario_plot = factor(
      as.character(scenario), levels = rev(setdiff(scenario_levels, "BaU"))
    ),
    year = factor(year, levels = c(2050L, 2100L))
  )

p_left <- ggplot(
  total_emissions,
  aes(year, value, colour = scenario, linetype = scenario, shape = scenario)
) +
  geom_line(linewidth = 0.65, lineend = "round") +
  geom_point(
    data = filter(total_emissions, year %in% c(2050L, 2100L)),
    size = 1.55, stroke = 0.45
  ) +
  scale_colour_manual(values = scenario_colours, drop = FALSE) +
  scale_linetype_manual(values = scenario_linetypes, drop = FALSE) +
  scale_shape_manual(values = scenario_shapes, drop = FALSE) +
  scale_x_continuous(breaks = c(2020, 2050, 2100), expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(
    limits = c(0, NA), labels = label_number(big.mark = ",", accuracy = 1),
    expand = expansion(mult = c(0, 0.035))
  ) +
  labs(tag = "a", x = "Year", y = expression(CO[2]~emissions~(Mt~CO[2]~yr^{-1}))) +
  guides(
    colour = guide_legend(ncol = 2, byrow = TRUE),
    linetype = guide_legend(ncol = 2, byrow = TRUE),
    shape = guide_legend(ncol = 2, byrow = TRUE)
  ) +
  theme_ncc()

p_right <- ggplot(composition, aes(value, scenario_plot, fill = sector)) +
  geom_col(width = 0.68, colour = NA) +
  geom_vline(xintercept = 0, linewidth = 0.35, colour = "black") +
  facet_wrap(~year, ncol = 1) +
  scale_fill_manual(values = sector_colours, drop = FALSE) +
  scale_x_continuous(
    labels = label_number(big.mark = ",", accuracy = 1),
    expand = expansion(mult = c(0.035, 0.035))
  ) +
  labs(tag = "b", x = expression(CO[2]~emissions~(Mt~CO[2]~yr^{-1})), y = NULL) +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
  theme_ncc() +
  theme(panel.spacing.y = unit(3, "mm"))

# Full-width Nature-style figure (approximately 183 mm wide), PNG only.
png(
  filename = output_file,
  width = 183, height = 102, units = "mm", res = 600,
  type = "cairo", bg = "white"
)
grid.newpage()
layout <- grid.layout(
  nrow = 1, ncol = 2,
  widths = unit(c(0.47, 0.53), "null")
)
pushViewport(viewport(layout = layout))
print(p_left, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p_right, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()

message("Wrote: ", normalizePath(output_file, winslash = "/", mustWork = FALSE))

# -----------------------------------------------------------------------------
# Figure 02: policy costs, technology LCOE, and system/final electricity prices

policy_spec <- tribble(
  ~variable, ~metric,
  "Prc_Car", "Carbon price\n(USD2010/t CO2)",
  "Pol_Cos_Cns_Los_rat", "Consumption loss\n(% vs BaU)",
  "Pol_Cos_GDP_Los_rat", "GDP loss\n(% vs BaU)"
)
policy_metric_levels <- policy_spec$metric

policy_metrics <- raw %>%
  inner_join(policy_spec, by = "variable") %>%
  select(scenario, year, metric, value) %>%
  complete(
    scenario,
    year = sort(unique(raw$year)),
    metric = policy_metric_levels,
    fill = list(value = 0)
  ) %>%
  mutate(metric = factor(metric, levels = policy_metric_levels))

p_policy <- ggplot(
  policy_metrics,
  aes(year, value, colour = scenario, linetype = scenario, shape = scenario)
) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey65") +
  geom_line(linewidth = 0.62, lineend = "round") +
  geom_point(
    data = filter(policy_metrics, year %in% c(2050L, 2100L)),
    size = 1.45, stroke = 0.4
  ) +
  facet_wrap(~metric, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = scenario_colours, drop = FALSE) +
  scale_linetype_manual(values = scenario_linetypes, drop = FALSE) +
  scale_shape_manual(values = scenario_shapes, drop = FALSE) +
  scale_x_continuous(
    breaks = c(2020, 2050, 2100),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = label_number(big.mark = ",", accuracy = 0.1),
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  expand_limits(y = 0) +
  labs(tag = "a", x = "Year", y = NULL) +
  guides(
    colour = guide_legend(ncol = 2, byrow = TRUE),
    linetype = guide_legend(ncol = 2, byrow = TRUE),
    shape = guide_legend(ncol = 2, byrow = TRUE)
  ) +
  theme_ncc()

lcoe_spec <- tribble(
  ~variable, ~technology,
  "LCO_Ele_Coa_w_CCS", "Coal + CCS",
  "LCO_Ele_Gas_w_CCS", "Gas + CCS",
  "LCO_Ele_Nuc", "Nuclear",
  "LCO_Ele_SolarPV_Ave", "Solar PV",
  "LCO_Ele_Win_Ons_Ave", "Onshore wind",
  "LCO_Ele_Win_Off_Ave", "Offshore wind"
)
technology_levels <- lcoe_spec$technology

lcoe_selected <- raw %>%
  inner_join(lcoe_spec, by = "variable") %>%
  filter(year %in% c(2050L, 2100L)) %>%
  mutate(
    technology = factor(technology, levels = rev(technology_levels)),
    year = factor(year, levels = c(2050L, 2100L))
  )

p_lcoe <- ggplot(
  lcoe_selected,
  aes(value, technology, colour = scenario, shape = scenario)
) +
  geom_point(
    position = position_dodge(width = 0.62), size = 1.75, stroke = 0.45,
    na.rm = TRUE
  ) +
  facet_wrap(~year, nrow = 1) +
  scale_colour_manual(values = scenario_colours, drop = FALSE) +
  scale_shape_manual(values = scenario_shapes, drop = FALSE) +
  scale_x_continuous(
    limits = c(0, NA), labels = label_number(big.mark = ",", accuracy = 1),
    expand = expansion(mult = c(0, 0.04))
  ) +
  labs(tag = "b", x = expression(LCOE~(USD[2010]~MWh^{-1})), y = NULL) +
  theme_ncc() +
  theme(legend.position = "none")

price_spec <- tribble(
  ~variable, ~price_type, ~conversion,
  "Prc_Sec_Ene_Ele_MWh", "Secondary electricity", 1,
  "Prc_Fin_Ene_Ele", "Final electricity", 3.6
)
price_type_levels <- price_spec$price_type
price_colours <- c(
  "Secondary electricity" = "#4D4D4D",
  "Final electricity" = "#56B4E9"
)

electricity_prices <- raw %>%
  inner_join(price_spec, by = "variable") %>%
  filter(year %in% c(2050L, 2100L)) %>%
  mutate(
    value = value * conversion,
    price_type = factor(price_type, levels = price_type_levels),
    scenario_plot = factor(as.character(scenario), levels = rev(scenario_levels)),
    year = factor(year, levels = c(2050L, 2100L))
  )

p_prices <- ggplot(
  electricity_prices,
  aes(value, scenario_plot, fill = price_type)
) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62) +
  facet_wrap(~year, nrow = 1) +
  scale_fill_manual(values = price_colours, drop = FALSE) +
  scale_x_continuous(
    limits = c(0, NA), labels = label_number(big.mark = ",", accuracy = 1),
    expand = expansion(mult = c(0, 0.04))
  ) +
  labs(tag = "c", x = expression(Electricity~price~(USD[2010]~MWh^{-1})), y = NULL) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  theme_ncc()

png(
  filename = output_file_2,
  width = 183, height = 145, units = "mm", res = 600,
  type = "cairo", bg = "white"
)
grid.newpage()
layout_2 <- grid.layout(
  nrow = 2, ncol = 2,
  widths = unit(c(0.43, 0.57), "null"),
  heights = unit(c(0.53, 0.47), "null")
)
pushViewport(viewport(layout = layout_2))
print(p_policy, vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1))
print(p_lcoe, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(p_prices, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
dev.off()

message("Wrote: ", normalizePath(output_file_2, winslash = "/", mustWork = FALSE))

# -----------------------------------------------------------------------------
# Figure 03: power-generation mix and final energy by sector and carrier

# The detailed CCS/non-CCS generation series are aggregated by primary power
# source. These ten mutually exclusive groups sum exactly to Sec_Ene_Ele.
generation_spec <- tribble(
  ~variable, ~component,
  "Sec_Ene_Ele_Bio_w_CCS", "Bioenergy",
  "Sec_Ene_Ele_Bio_wo_CCS", "Bioenergy",
  "Sec_Ene_Ele_Coa_w_CCS", "Coal",
  "Sec_Ene_Ele_Coa_wo_CCS", "Coal",
  "Sec_Ene_Ele_Gas_w_CCS", "Gas",
  "Sec_Ene_Ele_Gas_wo_CCS", "Gas",
  "Sec_Ene_Ele_Oil_w_CCS", "Oil",
  "Sec_Ene_Ele_Oil_wo_CCS", "Oil",
  "Sec_Ene_Ele_Geo", "Geothermal",
  "Sec_Ene_Ele_Hyp", "Hydro",
  "Sec_Ene_Ele_Nuc", "Nuclear",
  "Sec_Ene_Ele_Solar", "Solar",
  "Sec_Ene_Ele_Win_Ons", "Onshore wind",
  "Sec_Ene_Ele_Win_Off", "Offshore wind"
)
generation_levels <- c(
  "Bioenergy", "Coal", "Gas", "Oil", "Geothermal", "Hydro", "Nuclear",
  "Solar", "Onshore wind", "Offshore wind"
)

# Both final-energy mappings below are mutually exclusive and independently
# sum exactly to Fin_Ene. International bunkers are included in transport.
final_sector_spec <- tribble(
  ~variable, ~component,
  "Fin_Ene_Agr", "Agriculture",
  "Fin_Ene_Tra_w_bun", "Transport incl. bunkers",
  "Fin_Ene_Res_and_Com", "Residential & commercial",
  "Fin_Ene_Ind", "Industry",
  "Fin_Ene_NonEneUse", "Non-energy use",
  "Fin_Ene_Car_Man", "Carbon management"
)
final_sector_levels <- final_sector_spec$component

final_carrier_spec <- tribble(
  ~variable, ~component,
  "Fin_Ene_Ele", "Electricity",
  "Fin_Ene_Gas", "Gas",
  "Fin_Ene_Heat", "Heat",
  "Fin_Ene_Hyd", "Hydrogen",
  "Fin_Ene_Liq", "Liquids",
  "Fin_Ene_Solids", "Solids"
)
final_carrier_levels <- final_carrier_spec$component

selected_years <- c(2050L, 2100L)

prepare_composition <- function(spec, component_levels) {
  raw %>%
    filter(year %in% selected_years, variable %in% spec$variable) %>%
    inner_join(spec, by = "variable") %>%
    group_by(scenario, year, component) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    complete(
      scenario = factor(scenario_levels, levels = scenario_levels),
      year = selected_years,
      component = component_levels,
      fill = list(value = 0)
    ) %>%
    mutate(
      scenario_plot = factor(as.character(scenario), levels = rev(scenario_levels)),
      year = factor(year, levels = selected_years),
      component = factor(component, levels = component_levels)
    )
}

check_composition_total <- function(data, total_variable, label) {
  component_totals <- data %>%
    mutate(
      scenario = factor(as.character(scenario_plot), levels = scenario_levels),
      year = as.integer(as.character(year))
    ) %>%
    group_by(scenario, year) %>%
    summarise(component_sum = sum(value), .groups = "drop")

  reported_totals <- raw %>%
    filter(year %in% selected_years, variable == total_variable) %>%
    select(scenario, year, reported_total = value)

  comparison <- left_join(component_totals, reported_totals, by = c("scenario", "year")) %>%
    # GDX/IAMC_template omits some exact zero totals (e.g. BaU CCS).
    mutate(
      reported_total = replace_na(reported_total, 0),
      abs_difference = abs(component_sum - reported_total)
    )

  tolerance <- 1e-8
  if (any(comparison$abs_difference > tolerance)) {
    stop(label, " components do not sum to ", total_variable, ".")
  }
  invisible(comparison)
}

generation_mix <- prepare_composition(generation_spec, generation_levels)
final_sector_mix <- prepare_composition(final_sector_spec, final_sector_levels)
final_carrier_mix <- prepare_composition(final_carrier_spec, final_carrier_levels)

check_composition_total(generation_mix, "Sec_Ene_Ele", "Power generation")
check_composition_total(final_sector_mix, "Fin_Ene", "Final energy by sector")
check_composition_total(final_carrier_mix, "Fin_Ene", "Final energy by carrier")

generation_colours <- c(
  "Bioenergy" = "#66A61E",
  "Coal" = "#333333",
  "Gas" = "#E6AB02",
  "Oil" = "#D95F02",
  "Geothermal" = "#7570B3",
  "Hydro" = "#80B1D3",
  "Nuclear" = "#E7298A",
  "Solar" = "#FFD92F",
  "Onshore wind" = "#1B9E77",
  "Offshore wind" = "#377EB8"
)
final_sector_colours <- c(
  "Agriculture" = "#66A61E",
  "Transport incl. bunkers" = "#D95F02",
  "Residential & commercial" = "#1B9E77",
  "Industry" = "#7570B3",
  "Non-energy use" = "#E7298A",
  "Carbon management" = "#666666"
)
final_carrier_colours <- c(
  "Electricity" = "#377EB8",
  "Gas" = "#E6AB02",
  "Heat" = "#D95F02",
  "Hydrogen" = "#66C2A5",
  "Liquids" = "#A6761D",
  "Solids" = "#4D4D4D"
)

composition_plot <- function(data, colours, tag, x_label, legend_title, legend_nrow = 1) {
  ggplot(data, aes(value, scenario_plot, fill = component)) +
    geom_col(
      width = 0.68, colour = "white", linewidth = 0.12,
      position = position_stack(reverse = TRUE)
    ) +
    facet_wrap(~year, nrow = 1) +
    scale_fill_manual(values = colours, drop = FALSE) +
    scale_x_continuous(
      limits = c(0, NA), labels = label_number(accuracy = 1),
      expand = expansion(mult = c(0, 0.025))
    ) +
    labs(tag = tag, x = x_label, y = NULL, fill = legend_title) +
    guides(fill = guide_legend(nrow = legend_nrow, byrow = TRUE)) +
    theme_ncc() +
    theme(
      legend.position = "bottom",
      legend.justification = "left",
      legend.box.just = "left",
      panel.spacing.x = unit(4, "mm")
    )
}

p_generation_mix <- composition_plot(
  generation_mix, generation_colours, "a",
  expression(Power~generation~(EJ~yr^{-1})), "Power source", 2
)
p_final_sector <- composition_plot(
  final_sector_mix, final_sector_colours, "b",
  expression(Final~energy~(EJ~yr^{-1})), "Sector", 1
)
p_final_carrier <- composition_plot(
  final_carrier_mix, final_carrier_colours, "c",
  expression(Final~energy~(EJ~yr^{-1})), "Carrier", 1
)

png(
  filename = output_file_3,
  width = 183, height = 183, units = "mm", res = 600,
  type = "cairo", bg = "white"
)
grid.newpage()
layout_3 <- grid.layout(
  nrow = 3, ncol = 1,
  heights = unit(c(1.12, 0.94, 0.94), "null")
)
pushViewport(viewport(layout = layout_3))
print(p_generation_mix, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p_final_sector, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(p_final_carrier, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
dev.off()

message("Wrote: ", normalizePath(output_file_3, winslash = "/", mustWork = FALSE))

# -----------------------------------------------------------------------------
# Figure 04: CDR pathways, geological CCS by source, and cumulative CCS

# These pathway/source groups are mutually exclusive and sum exactly to the
# reported CDR and geological-storage totals, respectively.
cdr_spec <- tribble(
  ~variable, ~component,
  "Car_Rem_Bio_wit_CCS", "BECCS",
  "Car_Rem_Dir_Air_Cap_wit_CCS", "DACCS",
  "Car_Rem_Enh_Wea", "Enhanced weathering",
  "Car_Rem_Frs", "Afforestation",
  "Car_Rem_Soi_Car_Seq", "Soil carbon"
)
cdr_levels <- c(
  "BECCS", "DACCS", "Enhanced weathering", "Afforestation", "Soil carbon"
)

ccs_spec <- tribble(
  ~variable, ~component,
  "Car_Cap_Geo_Sto_Bio", "Bioenergy",
  "Car_Cap_Geo_Sto_Dir_Air_Cap", "Direct air capture",
  "Car_Cap_Geo_Sto_Fos", "Fossil fuels",
  "Car_Cap_Geo_Sto_Ind_Pro", "Industrial processes"
)
ccs_levels <- c(
  "Bioenergy", "Direct air capture", "Fossil fuels", "Industrial processes"
)

cdr_mix <- prepare_composition(cdr_spec, cdr_levels)
ccs_mix <- prepare_composition(ccs_spec, ccs_levels)

check_composition_total(cdr_mix, "Car_Rem", "Carbon dioxide removal")
check_composition_total(ccs_mix, "Car_Cap_Geo_Sto", "Geological CCS")

cdr_colours <- c(
  "BECCS" = "#1B9E77",
  "DACCS" = "#377EB8",
  "Enhanced weathering" = "#7570B3",
  "Afforestation" = "#66A61E",
  "Soil carbon" = "#A6761D"
)
ccs_colours <- c(
  "Bioenergy" = "#1B9E77",
  "Direct air capture" = "#377EB8",
  "Fossil fuels" = "#4D4D4D",
  "Industrial processes" = "#D95F02"
)

p_cdr <- composition_plot(
  cdr_mix, cdr_colours, "a",
  expression(CDR~(Mt~CO[2]~yr^{-1})), "Removal pathway", 1
)
p_ccs <- composition_plot(
  ccs_mix, ccs_colours, "b",
  expression(CCS~storage~(Mt~CO[2]~yr^{-1})), "Stored CO2 source", 1
)

# Use the model-reported cumulative CCS stock at 2100 rather than numerically
# integrating five-year flow outputs. GDX omits the exact zero for BaU.
cumulative_ccs <- raw %>%
  filter(variable == "Cum_Car_Seq_CCS", year == 2100L) %>%
  select(scenario, value) %>%
  complete(
    scenario = factor(scenario_levels, levels = scenario_levels),
    fill = list(value = 0)
  ) %>%
  mutate(
    value = value / 1000,
    scenario_plot = factor(as.character(scenario), levels = rev(scenario_levels)),
    value_label = label_number(accuracy = 0.1)(value)
  )

p_cumulative_ccs <- ggplot(
  cumulative_ccs,
  aes(value, scenario_plot, fill = scenario)
) +
  geom_col(width = 0.68) +
  geom_text(
    aes(label = value_label), hjust = -0.18,
    size = 2.7, fontface = "bold", colour = "black"
  ) +
  scale_fill_manual(values = scenario_colours, drop = FALSE) +
  scale_x_continuous(
    limits = c(0, NA), labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0, 0.09))
  ) +
  labs(
    tag = "c",
    x = expression(Cumulative~CCS~storage*","~2020-2100~(Gt~CO[2])),
    y = NULL
  ) +
  theme_ncc() +
  theme(legend.position = "none")

png(
  filename = output_file_4,
  width = 183, height = 166, units = "mm", res = 600,
  type = "cairo", bg = "white"
)
grid.newpage()
layout_4 <- grid.layout(
  nrow = 3, ncol = 1,
  heights = unit(c(1, 1, 0.82), "null")
)
pushViewport(viewport(layout = layout_4))
print(p_cdr, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p_ccs, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(p_cumulative_ccs, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
dev.off()

message("Wrote: ", normalizePath(output_file_4, winslash = "/", mustWork = FALSE))

# -----------------------------------------------------------------------------
# Figure 05: GLMDI decomposition of net CO2-emission changes, 2020-2100

# Identity used for gross emissions:
# Gross CO2 = GDP * (final energy / GDP) * (electricity / final energy) *
#             (gross CO2 / electricity)
# Net CO2 is gross emissions plus Gro_Rem_CO2, which is stored as a negative
# quantity in IAMC_template. The logarithmic-mean decomposition is exact.
lmdi_variables <- c(
  "Gro_Emi_CO2", "GDP_MER", "Fin_Ene", "Fin_Ene_Ele", "Gro_Rem_CO2"
)
lmdi_years <- c(2020L, 2050L, 2100L)
lmdi_periods <- tribble(
  ~period, ~start_year, ~end_year,
  "2020-2050", 2020L, 2050L,
  "2050-2100", 2050L, 2100L
)
lmdi_period_levels <- lmdi_periods$period

lmdi_raw <- raw %>%
  filter(year %in% lmdi_years, variable %in% lmdi_variables) %>%
  select(scenario, year, variable, value)

lmdi_missing <- crossing(
  scenario = factor(scenario_levels, levels = scenario_levels),
  year = lmdi_years,
  variable = lmdi_variables
) %>%
  anti_join(lmdi_raw, by = c("scenario", "year", "variable"))

if (nrow(lmdi_missing) > 0) {
  stop("Variables required for the two-period GLMDI decomposition are missing.")
}

lmdi_inputs <- lmdi_raw %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(
    removal = Gro_Rem_CO2,
    economic_activity = GDP_MER,
    energy_intensity = Fin_Ene / GDP_MER,
    electrification = Fin_Ene_Ele / Fin_Ene,
    carbon_intensity = Gro_Emi_CO2 / Fin_Ene_Ele,
    net_co2 = Gro_Emi_CO2 + removal
  ) %>%
  arrange(scenario, year)

if (any(lmdi_inputs$Gro_Rem_CO2 > 0)) {
  stop("Gro_Rem_CO2 must be non-positive for the GLMDI sign convention.")
}
if (any(
  lmdi_inputs$Gro_Emi_CO2 <= 0 |
    lmdi_inputs$economic_activity <= 0 |
    lmdi_inputs$energy_intensity <= 0 |
    lmdi_inputs$electrification <= 0 |
    lmdi_inputs$carbon_intensity <= 0
)) {
  stop("Gross emissions and all multiplicative GLMDI factors must be positive.")
}

logarithmic_mean <- function(a, b) {
  if (!is.finite(a) || !is.finite(b) || a <= 0 || b <= 0) {
    stop("Logarithmic mean requires positive finite values.")
  }
  if (isTRUE(all.equal(a, b))) return(a)
  (a - b) / (log(a) - log(b))
}

lmdi_driver_levels <- c(
  "Start", "Economic activity", "Energy intensity", "Electrification",
  "Carbon intensity", "Carbon removal", "End"
)

run_time_glmdi <- function(data_scenario, start_year, end_year) {
  start <- data_scenario %>% filter(year == start_year)
  end <- data_scenario %>% filter(year == end_year)
  if (nrow(start) != 1 || nrow(end) != 1) {
    stop("Each scenario must contain one observation at each period endpoint.")
  }

  gross_0 <- start$Gro_Emi_CO2[[1]]
  gross_1 <- end$Gro_Emi_CO2[[1]]
  removal_0 <- start$removal[[1]]
  removal_1 <- end$removal[[1]]
  factors_0 <- c(
    `Economic activity` = start$economic_activity[[1]],
    `Energy intensity` = start$energy_intensity[[1]],
    Electrification = start$electrification[[1]],
    `Carbon intensity` = start$carbon_intensity[[1]]
  )
  factors_1 <- c(
    `Economic activity` = end$economic_activity[[1]],
    `Energy intensity` = end$energy_intensity[[1]],
    Electrification = end$electrification[[1]],
    `Carbon intensity` = end$carbon_intensity[[1]]
  )

  effects <- c(
    logarithmic_mean(gross_1, gross_0) * log(factors_1 / factors_0),
    `Carbon removal` = removal_1 - removal_0
  )
  net_0 <- gross_0 + removal_0
  net_1 <- gross_1 + removal_1
  residual <- net_1 - net_0 - sum(effects)
  tolerance <- 1e-6 * max(1, abs(net_0), abs(net_1))
  if (abs(residual) > tolerance) {
    stop("GLMDI residual exceeds tolerance: ", format(residual, scientific = TRUE))
  }

  before <- net_0 + c(0, cumsum(head(effects, -1)))
  after <- before + effects

  bind_rows(
    tibble(
      driver = "Start", value = net_0,
      ymin = pmin(0, net_0), ymax = pmax(0, net_0), end_level = net_0
    ),
    tibble(
      driver = names(effects), value = as.numeric(effects),
      ymin = pmin(before, after), ymax = pmax(before, after), end_level = after
    ),
    tibble(
      driver = "End", value = net_1,
      ymin = pmin(0, net_1), ymax = pmax(0, net_1), end_level = net_1
    )
  )
}

lmdi_waterfall <- map_dfr(scenario_levels, function(scenario_name) {
  scenario_data <- lmdi_inputs %>%
    filter(as.character(scenario) == scenario_name)

  map_dfr(seq_len(nrow(lmdi_periods)), function(i) {
    period_row <- lmdi_periods[i, ]
    run_time_glmdi(
      scenario_data, period_row$start_year[[1]], period_row$end_year[[1]]
    ) %>%
      mutate(
        scenario = scenario_name,
        period = period_row$period[[1]],
        start_year = period_row$start_year[[1]],
        end_year = period_row$end_year[[1]]
      )
  })
}) %>%
  mutate(
    scenario = factor(as.character(scenario), levels = scenario_levels),
    period = factor(period, levels = lmdi_period_levels),
    driver = factor(driver, levels = lmdi_driver_levels),
    x = match(as.character(driver), lmdi_driver_levels),
    value_label = if_else(
      driver %in% c("Start", "End"),
      label_number(accuracy = 1, big.mark = ",")(value),
      sprintf("%+.0f", value)
    ),
    label_y = case_when(
      value >= 0 ~ ymax,
      driver == "Carbon removal" ~ ymax,
      TRUE ~ ymin
    ),
    label_vjust = case_when(
      value >= 0 ~ -0.35,
      driver == "Carbon removal" ~ -0.35,
      driver == "Carbon intensity" ~ -0.25,
      TRUE ~ 1.15
    ),
    facet_label = factor(
      paste(as.character(scenario), as.character(period), sep = "\n"),
      levels = unlist(map(
        scenario_levels,
        ~paste(.x, lmdi_period_levels, sep = "\n")
      ))
    )
  )

lmdi_connectors <- lmdi_waterfall %>%
  filter(driver != "End") %>%
  transmute(
    scenario, period, facet_label,
    x = x + 0.38,
    xend = x + 0.62,
    y = end_level,
    yend = end_level
  )

lmdi_colours <- c(
  "Economic activity" = "#D55E00",
  "Energy intensity" = "#0072B2",
  "Electrification" = "#E69F00",
  "Carbon intensity" = "#009E73",
  "Carbon removal" = "#CC79A7",
  "Start" = "#777777",
  "End" = "#222222"
)

p_lmdi <- ggplot(lmdi_waterfall) +
  geom_hline(yintercept = 0, linewidth = 0.35, colour = "black") +
  geom_segment(
    data = lmdi_connectors,
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE, colour = "grey55", linewidth = 0.35
  ) +
  geom_rect(
    aes(
      xmin = x - 0.38, xmax = x + 0.38,
      ymin = ymin, ymax = ymax, fill = driver
    )
  ) +
  geom_text(
    aes(x = x, y = label_y, label = value_label, vjust = label_vjust),
    size = 2.05, fontface = "bold"
  ) +
  facet_wrap(~facet_label, ncol = 2, scales = "free_y") +
  scale_x_continuous(
    breaks = c(1, 7), labels = c("Start", "End"),
    limits = c(0.5, 7.5), expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 1, big.mark = ","),
    expand = expansion(mult = c(0.08, 0.12))
  ) +
  scale_fill_manual(
    values = lmdi_colours,
    breaks = c(
      "Economic activity", "Energy intensity", "Electrification",
      "Carbon intensity", "Carbon removal"
    ),
    name = "Emission driver"
  ) +
  labs(
    x = NULL,
    y = expression(Net~CO[2]~emissions~and~contributions~(Mt~CO[2]~yr^{-1}))
  ) +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  theme_ncc() +
  theme(
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    legend.justification = "left",
    panel.spacing = unit(3, "mm")
  )

png(
  filename = output_file_5,
  width = 183, height = 232, units = "mm", res = 600,
  type = "cairo", bg = "white"
)
print(p_lmdi)
dev.off()

message("Wrote: ", normalizePath(output_file_5, winslash = "/", mustWork = FALSE))
