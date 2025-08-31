# ──────────────────────────────────────────────────────────────────────────────
# compute_oxygen_emissions.R
#
# Purpose:  Calculate baseline and projected (2030, -50% fossil, 0% fossil)
#           oxygen-supply emissions (g CO₂e per pure-L) by country & region.
#
# Inputs:
#   - lbl_regions.xlsx           (World Bank country + lending groups)
#   - targets_download.xlsx      (2030 RES targets, sheet 4)
#   - OWID CSVs: per-capita electricity mix & carbon intensity
#
# Outputs:
#   - oxemissions_bycountry.csv
#   - emissions_perliter_projections.csv
#
# Usage:
#   Rscript compute_oxygen_emissions.R
#
# Dependencies:
#   tidyverse, readxl, here
# ──────────────────────────────────────────────────────────────────────────────

# Add Libraries
library(tidyverse)
library(readxl)
library(here)


## ─────────────────────────────────────────────────────────────────────────────
## 1.  PART 1: GET EMISSIONS DATA FOR EACH COUNTRY
## ─────────────────────────────────────────────────────────────────────────────

# 1.1  Read region labels (World Bank country & lending groups)
lbl_data <- read_xlsx(
  here("lbl_regions.xlsx")
)
#  - Columns: Entity (OWID name), Code (ISO-3), Region (WB lending group)

# 1.2  Download per-capita electricity generation mix from OWID
grid_data <- read_csv(
  "https://ourworldindata.org/grapher/per-capita-electricity-fossil-nuclear-renewables.csv?v=1&csvType=full&useColumnShortNames=true"
) |>
  rename(
    fossil_pc = per_capita_fossil_generation__kwh_chart_per_capita_electricity_fossil_nuclear_renewables,
    nuclear_pc = per_capita_nuclear_generation__kwh_chart_per_capita_electricity_fossil_nuclear_renewables,
    renewable_pc = per_capita_renewable_generation__kwh_chart_per_capita_electricity_fossil_nuclear_renewables
  )
#  - Units: kWh per person per year for each source

# 1.3  Download grid carbon intensity (g CO₂e/kWh) from OWID
carbon_intensity <- read_csv(
  "https://ourworldindata.org/grapher/carbon-intensity-electricity.csv?v=1&csvType=full&useColumnShortNames=true"
)

# 1.4  Merge & select latest non-NA carbon intensity per country
grid_data_ci <- grid_data |>
  # join lending groups
  left_join(lbl_data |> select(-Entity), by = "Code") |>
  # if no region in lbl_data, fall back to OWID Entity name
  mutate(Region = if_else(is.na(Region), Entity, Region)) |>
  # now bring in carbon intensity
  left_join(
    carbon_intensity |>
      select(Entity, Year, co2_intensity = co2_intensity__gco2_kwh),
    by = c("Entity", "Year")
  ) |>
  # drop years with missing CI
  drop_na(co2_intensity) |>
  # pick the most recent year per Entity
  group_by(Entity) |>
  arrange(desc(Year), .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  # compute shares of generation mix
  mutate(
    total_generation = fossil_pc + nuclear_pc + renewable_pc,
    fossil_generation = 100 * fossil_pc / total_generation,
    nuclear_generation = 100 * nuclear_pc / total_generation,
    renewable_generation = 100 * renewable_pc / total_generation
  ) |>
  # remove the old CI (we’ll re-join it in next step)
  select(-co2_intensity) |>
  # re-attach CI to preserve Year
  left_join(
    carbon_intensity |>
      select(Entity, Year, co2_intensity = co2_intensity__gco2_kwh),
    by = c("Entity", "Year")
  ) |>
  # normalize Europe's ISO code
  mutate(Code = if_else(Entity == "Europe", "EU", Code))


## ─────────────────────────────────────────────────────────────────────────────
## 2.  PART 2: DEFINE EMISSIONS FOR EACH DELIVERY SYSTEM
## ─────────────────────────────────────────────────────────────────────────────

#' @title Compute per-component GWP for an O₂ delivery system
#' @description
#'   Given component names and their percent share of the total system GWP
#'   (at measured purity), compute the raw and 100%-purity CO₂e emissions
#'   in kg and g per pure liter of O₂.
#' @param components         Character vector of leg names, e.g. c("prod","transp")
#' @param percent_emissions  Numeric vector of % shares summing to 100
#' @param total_kg_GWP_1L    Total kg CO₂e per L at the measured purity
#' @param purity_factor      Fractional purity of measured O₂ (e.g. 0.995)
#' @return A tibble with columns:
#'   * component
#'   * percent_emissions
#'   * kg_GWP_1L        (kg CO₂e per pure-L at 100% purity)
#'   * g_GWP_1L         (g CO₂e per pure-L at 100% purity)

make_system_emissions <- function(
  components,
  percent_emissions,
  total_kg_GWP_1L,
  purity_factor
) {
  tibble(
    component = components,
    percent_emissions = percent_emissions
  ) |>
    mutate(
      # compute raw kg CO₂e per liter at measured purity
      kg_GWP_1L_raw = percent_emissions / 100 * total_kg_GWP_1L,
      # scale that to 100% purity
      kg_GWP_1L = kg_GWP_1L_raw / purity_factor,
      # convert to grams
      g_GWP_1L = kg_GWP_1L * 1000
    ) |>
    select(component, percent_emissions, kg_GWP_1L, g_GWP_1L)
}


## System 1: Bulk LOX ----------------------------------------------------------

# component names and their % shares of total GWP at 99.5% purity
components_lox <- c("total", "prod", "transp", "otro")
percent_emissions_lox <- c(100, 82, 18, 0)
# observed GWP per L at 99.5% purity
total_kg_GWP_1L_lox <- 0.0001700
purity_factor_lox <- 0.995 # LOX purity fraction

# compute per-component emissions adjusted to 100% purity
system1_lox <- make_system_emissions(
  components = components_lox,
  percent_emissions = percent_emissions_lox,
  total_kg_GWP_1L = total_kg_GWP_1L_lox,
  purity_factor = purity_factor_lox
)

# inspect breakdown
system1_lox


## System 2: Cylinder LOX ------------------------------------------------------

components_cylox <- c("total", "prod", "transp", "otro")
percent_emissions_cylox <- c(100, 34, 36, 30)
total_kg_GWP_1L_cylox <- 0.0004110
purity_factor_cylox <- 0.995 # cylinder LOX purity

system2_cylox <- make_system_emissions(
  components = components_cylox,
  percent_emissions = percent_emissions_cylox,
  total_kg_GWP_1L = total_kg_GWP_1L_cylox,
  purity_factor = purity_factor_cylox
)

system2_cylox


## System 3: PSA Local ----------------------------------------------------------

components_psal <- c("total", "prod", "transp", "otro")
percent_emissions_psal <- c(100, 99, 0, 1)
total_kg_GWP_1L_psal <- 0.0000844
purity_factor_psal <- 0.93 # PSA purity

system3_psal <- make_system_emissions(
  components = components_psal,
  percent_emissions = percent_emissions_psal,
  total_kg_GWP_1L = total_kg_GWP_1L_psal,
  purity_factor = purity_factor_psal
)

system3_psal


## System 4: Oxygen Concentrator -----------------------------------------------

components_oxconc <- c("total", "prod", "transp", "otro")
percent_emissions_oxconc <- c(100, 99.5, 0, 0.5)
total_kg_GWP_1L_oxconc <- 0.000123
purity_factor_oxconc <- 0.93 # concentrator purity

system4_oxconc <- make_system_emissions(
  components = components_oxconc,
  percent_emissions = percent_emissions_oxconc,
  total_kg_GWP_1L = total_kg_GWP_1L_oxconc,
  purity_factor = purity_factor_oxconc
)

system4_oxconc


## System 5: Composite PSA Cylinder Emissions ----------------------------------

# We combine:
#  1) PSA-local “prod” & “otro” legs at 93% purity (system3_psal),
#  2) LOX-cylinder “transp” leg adjusted from 99.5% → 93% (system2_cylox),
#  3) then rebuild a “total” row and re-normalize shares to 100%.

# purity factors
purity_cylox <- 0.995 # original cylinder LOX purity
purity_psal <- 0.93 # final PSA purity

# 1) PSA-local legs (prod & otro) already at 93% purity
psal_base <- system3_psal |>
  filter(component %in% c("prod", "otro")) |>
  select(component, kg_GWP_1L, g_GWP_1L)

# 2) Adjust the LOX-cylinder transport leg to 93% purity
cylox_transp_adj <- system2_cylox |>
  filter(component == "transp") |>
  mutate(
    # revert 99.5% scaling to raw kg
    raw_kg = kg_GWP_1L * purity_cylox,
    # scale raw to 93% purity
    kg_GWP_1L = raw_kg / purity_psal,
    # convert to grams
    g_GWP_1L = kg_GWP_1L * 1000
  ) |>
  select(component, kg_GWP_1L, g_GWP_1L)

# 3) Combine legs
components_only <- bind_rows(psal_base, cylox_transp_adj)

# 4) Compute grand totals (kg & g per pure-L)
new_total_kg <- sum(components_only$kg_GWP_1L)
new_total_g <- sum(components_only$g_GWP_1L)

# 5) Recompute % shares so they sum to 100%
components_only <- components_only |>
  mutate(
    percent_emissions = 100 * kg_GWP_1L / new_total_kg
  )

# 6) Build and bind the “total” row
total_row <- tibble(
  component = "total",
  percent_emissions = 100,
  kg_GWP_1L = new_total_kg,
  g_GWP_1L = new_total_g
)
system5_psac <- bind_rows(total_row, components_only) |>
  arrange(factor(component, levels = c("total", "prod", "transp", "otro")))

# inspect final composite breakdown
system5_psac


## ─────────────────────────────────────────────────────────────────────────────
## 3.  PART 3: CALCULATE EMISSION PER COUNTRY PER DELIVERY SYSTEM
## ─────────────────────────────────────────────────────────────────────────────

# 3.1  Combine the five per‐system tibbles into a single long table
systems <- list(
  system1_lox = system1_lox,
  system2_cylox = system2_cylox,
  system3_psal = system3_psal,
  system4_oxconc = system4_oxconc,
  system5_psac = system5_psac
)

# imap_dfr() binds them together and adds a column "system" with the list names
systems_long <- imap_dfr(
  systems,
  ~ mutate(.x, system = .y)
)
# systems_long has columns:
#  - component         (e.g. "prod", "transp", "otro")
#  - percent_emissions (% share within each system)
#  - kg_GWP_1L         (kg CO₂e per pure-L at 100% purity)
#  - g_GWP_1L          (g CO₂e per pure-L at 100% purity)
#  - system            (name of the delivery system)

# 3.2  Cross-join each country with every system component,
#      then scale the “prod” leg by country CI / Ontario CI
ONTARIO_BASELINE_CI <- 28 # g CO₂e/kWh baseline from Ontario LCA

emissions_components <- grid_data_ci |>
  select(Entity, co2_intensity) |> # only need country & its CI
  crossing(systems_long) |> # replicate every component for each country
  mutate(
    # production leg is proportional to the country’s grid CI
    g_GWP_1L = if_else(
      component == "prod",
      g_GWP_1L * (co2_intensity / ONTARIO_BASELINE_CI),
      g_GWP_1L
    )
  ) |>
  # 3.3  Recompute percent shares within each country × system so
  #      prod + transp + otro sum to 100%
  group_by(Entity, system) |>
  mutate(
    percent_emissions = 100 * g_GWP_1L / sum(g_GWP_1L[component != "total"])
  ) |>
  ungroup()

# 3.4  Summarize to get one total g-CO₂e/pure-L per country × system
emissions_totals <- emissions_components |>
  filter(component != "total") |> # drop the original “total” row
  group_by(Entity, system) |>
  summarise(
    total_g_GWP_1L = sum(g_GWP_1L), # grams CO₂e per pure-L
    .groups = "drop"
  )

# 3.5  Pivot to wide: one column per delivery system
emissions_wide <- emissions_totals |>
  mutate(system = paste0("GWP_", system)) |> # e.g. “GWP_system1_lox”
  pivot_wider(
    names_from = system,
    values_from = total_g_GWP_1L
  )

# 3.6  Attach these per-system totals back to the country CI data
country_emissions <- grid_data_ci |>
  left_join(emissions_wide, by = "Entity")
# Now country_emissions includes:
#  - Entity, Code, Year, Region, co2_intensity, generation shares…
#  - GWP_system1_lox, GWP_system2_cylox, …, GWP_system5_psac

# 3.7  Inspect result
country_emissions


## ─────────────────────────────────────────────────────────────────────────────
## 4. PART 4: CALCULATE EMISSION (g CO₂e) PER 1 LPM O₂ FOR EACH COUNTRY
## ─────────────────────────────────────────────────────────────────────────────

# 4.1  Define the typical delivery‐system mix by region (percent shares)
oxygen_distribution <- tribble(
  ~Entity,
  ~system1_lox,
  ~system2_cylox,
  ~system3_psal,
  ~system4_oxconc,
  ~system5_psac,
  "North America",
  90,
  5,
  5,
  0,
  0,
  "Europe - HIC",
  90,
  10,
  0,
  0,
  0,
  "Europe - HM/LMIC",
  65,
  10,
  15,
  1,
  9,
  "Latin America - HIC",
  85,
  10,
  5,
  0,
  0,
  "Latin America - HMIC",
  60,
  10,
  20,
  1,
  9,
  "Latin America - LMIC",
  25,
  25,
  30,
  5,
  15,
  "Asia - HIC",
  85,
  5,
  10,
  0,
  0,
  "Asia - HMIC",
  65,
  10,
  15,
  1,
  9,
  "Asia - LMIC",
  30,
  25,
  25,
  5,
  15,
  "Oceania - HIC",
  90,
  5,
  5,
  0,
  0,
  "Oceania - HMIC",
  60,
  15,
  15,
  1,
  9,
  "Oceania - LMIC",
  25,
  20,
  20,
  5,
  30,
  "SSA",
  15,
  20,
  15,
  10,
  40,
  "MENA",
  45,
  25,
  15,
  5,
  10
)
#  - Each row sums to 100%; indicates % of O₂ supplied via each system.

# 4.2  Append “_share” suffix to make clear these are weights
oxygen_mix <- oxygen_distribution |>
  rename_with(~ paste0(.x, "_share"), -Entity)

# 4.3  List the GWP columns and the corresponding share columns
system_cols <- c(
  "GWP_system1_lox",
  "GWP_system2_cylox",
  "GWP_system3_psal",
  "GWP_system4_oxconc",
  "GWP_system5_psac"
)
share_cols <- c(
  "system1_lox_share",
  "system2_cylox_share",
  "system3_psal_share",
  "system4_oxconc_share",
  "system5_psac_share"
)

# 4.4  Compute weighted average g CO₂e per pure-L for each country
country_1l <- country_emissions |>
  # join region‐specific mix shares
  left_join(oxygen_mix, by = c("Region" = "Entity")) |>
  # calculate per-row 1 L emissions: sum(GWP_i * share_i) / 100
  rowwise() |>
  mutate(
    g_CO2e_1L = sum(
      {
        gwp_vals <- c_across(all_of(system_cols))
        share_vals <- c_across(all_of(share_cols))
        gwp_vals * share_vals
      },
      na.rm = TRUE
    ) /
      100
  ) |>
  ungroup()

# 4.5  Compute continent‐level means for regions that represent wholes
continent_names <- c("Africa", "Asia", "Europe", "Oceania", "South America")
continent_avgs <- country_1l |>
  # keep only sub‐regions (exclude rows where Entity==continent)
  filter(!Entity %in% continent_names) |>
  mutate(
    continent = case_when(
      Region %in% c("SSA", "MENA") ~ "Africa",
      str_detect(Region, regex("Asia", ignore_case = TRUE)) ~ "Asia",
      str_detect(Region, regex("Europe", ignore_case = TRUE)) ~ "Europe",
      str_detect(Region, regex("Oceania", ignore_case = TRUE)) ~ "Oceania",
      str_detect(Region, regex("Latin America", ignore_case = TRUE)) ~
        "South America",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(continent)) |>
  group_by(continent) |>
  summarise(
    g_CO2e_1L = mean(g_CO2e_1L, na.rm = TRUE),
    .groups = "drop"
  )

# 4.6  Override those continent‐rows and drop helper columns
country_final <- country_1l |>
  left_join(continent_avgs, by = c("Entity" = "continent")) |>
  mutate(
    # if the row *is* a continent, use continent average; else keep country value
    g_CO2e_1L = if_else(
      Entity %in% continent_names,
      g_CO2e_1L.y,
      g_CO2e_1L.x
    )
  ) |>
  select(-g_CO2e_1L.x, -g_CO2e_1L.y) |>
  # drop any zero‐emission rows (if present)
  filter(g_CO2e_1L > 0)

# 4.7  Inspect and save results
print(
  country_final |>
    select(Entity, Region, g_CO2e_1L) |>
    arrange(g_CO2e_1L),
  n = Inf
)

write_csv(country_final, here("oxemissions_bycountry.csv"))


############## PART 5: CALCULATE GRID-INTENSITY SCENARIO METRICS ################
###############################################################################
# GOAL:
#   Compute three hypothetical grid carbon‐intensity metrics (g CO₂e/kWh)
#     • co2_intensity_2030  – if each country meets its 2030 RES target
#     • co2_intensity_half  – fossil share cut by 50% (nuclear fixed)
#     • co2_intensity_clean – fossil share eliminated (nuclear fixed)
#
# ASSUMPTIONS:
#   1. nuclear_generation remains constant
#   2. any reduction in fossil generation is back‐filled by renewables
#   3. ef_nuclear = 12 g CO₂e/kWh (IPCC AR5); ef_fossil/ef_renew = medians of >99% fossil/renewable grids

# ──────────────────────────────────────────────────────────────────────────────
# (1) READ 2030 RENEWABLE TARGETS AND MERGE INTO COUNTRY TABLE
# ──────────────────────────────────────────────────────────────────────────────
targets_renwbl <- read_xlsx(
  here("targets_download.xlsx"), # relative path via here()
  sheet = 4 # sheet containing RES targets
) |>
  select(
    Code = country_code, # OWID ISO-3 code
    target_perc_2030 = res_share_target # target RES share (%) by 2030
  ) |>
  mutate(
    # map Kosovo code from XKX → OWID_KOS
    Code = if_else(Code == "XKX", "OWID_KOS", Code)
  )

target_2030 <- country_final |> # take baseline from Part 4
  left_join(targets_renwbl, by = "Code") |> # add target_perc_2030
  mutate(
    # 1.a Fill missing Europe targets with the Europe‐row value (EU average)
    target_perc_2030 = if_else(
      is.na(target_perc_2030) & str_starts(Region, "Europe"),
      first(target_perc_2030[Entity == "Europe"], default = NA_real_),
      target_perc_2030
    ),
    # 1.b South America inherits average of Latin-America subregions
    target_perc_2030 = if_else(
      Entity == "South America",
      mean(target_perc_2030[str_starts(Region, "Latin America")], na.rm = TRUE),
      target_perc_2030
    ),
    # 1.c Fill any remaining Latin America NAs with the South America value
    target_perc_2030 = if_else(
      is.na(target_perc_2030) & str_starts(Region, "Latin America"),
      first(target_perc_2030[Entity == "South America"], default = NA_real_),
      target_perc_2030
    )
  ) |>
  filter(!is.na(Code)) # drop rows lacking a valid ISO-3 code

# ──────────────────────────────────────────────────────────────────────────────
# (2) DEFINE “PURE” EMISSION FACTORS (g CO₂e/kWh)
# ──────────────────────────────────────────────────────────────────────────────
ef_nuclear <- 12 # fixed nuclear factor

ef_fossil <- grid_data_ci |> # median CI among almost‐100% fossil grids
  filter(fossil_generation > 99) |>
  summarise(median(co2_intensity, na.rm = TRUE)) |>
  pull()

ef_renew <- grid_data_ci |> # median CI among almost‐100% renewables grids
  filter(renewable_generation > 99) |>
  summarise(median(co2_intensity, na.rm = TRUE)) |>
  pull()

# ──────────────────────────────────────────────────────────────────────────────
# (3) ADD THE THREE SCENARIO INTENSITIES INTO A SINGLE TABLE
# ──────────────────────────────────────────────────────────────────────────────
target_intensities <- target_2030 |>
  mutate(
    # ─────────────────────────────────────────────────────────
    # 2030 TARGET SCENARIO – only compute if target exists
    # ─────────────────────────────────────────────────────────
    co2_intensity_2030 = if_else(
      is.na(target_perc_2030),
      NA_real_, # leave as NA if no target
      {
        # compute renewable share capped by target & nuclear constraint
        rg <- pmin(
          100 - nuclear_generation,
          pmax(renewable_generation, target_perc_2030)
        )
        # remaining share is fossil
        fg <- 100 - nuclear_generation - rg
        # weighted sum of pure emission factors
        fg /
          100 *
          ef_fossil +
          rg / 100 * ef_renew +
          nuclear_generation / 100 * ef_nuclear
      }
    ),

    # ─────────────────────────────────────────────────────────
    # HALF SCENARIO – fossil generation halved, nuclear constant
    # ─────────────────────────────────────────────────────────
    co2_intensity_half = {
      fg <- fossil_generation * 0.5 # half the original fossil share
      rg <- 100 - nuclear_generation - fg # residual to renewables
      fg /
        100 *
        ef_fossil +
        rg / 100 * ef_renew +
        nuclear_generation / 100 * ef_nuclear
    },

    # ─────────────────────────────────────────────────────────
    # CLEAN SCENARIO – zero fossil, nuclear constant
    # ─────────────────────────────────────────────────────────
    co2_intensity_clean = {
      rg <- 100 - nuclear_generation # all non-nuclear = renewables
      rg / 100 * ef_renew + nuclear_generation / 100 * ef_nuclear
    }
  )

# target_intensities now contains:
#   Entity, Code, Region, co2_intensity (baseline),
#   target_perc_2030,
#   co2_intensity_2030, co2_intensity_half, co2_intensity_clean

######### PART 6: APPLY SCENARIO INTENSITIES & ASSEMBLE FINAL OUTPUT ###########
###############################################################################
# In this part, we re-run the delivery-system LCA (Part 3) for each scenario:
#   • 2030 TARGET – use co2_intensity_2030
#   • HALF SCENARIO (–50% fossil) – use co2_intensity_half
#   • CLEAN SCENARIO (0% fossil) – use co2_intensity_clean
# Finally, we join all results into one table and export.
###############################################################################

# ──────────────────────────────────────────────────────────────────────────────
# 2030 TARGET SCENARIO
# 0) Inject co2_intensity_2030 into a fresh grid_data_ci
# ──────────────────────────────────────────────────────────────────────────────
grid_2030 <- grid_data_ci |>
  select(-co2_intensity) |> # drop baseline CI
  distinct(Code, .keep_all = TRUE) |> # one row per country
  left_join(
    target_intensities |>
      select(Code, co2_intensity_2030), # bring in scenario CI
    by = "Code"
  ) |>
  rename(co2_intensity = co2_intensity_2030) # rename for pipe consistency

# 1) Expand to every system × component
emissions_components_2030 <- grid_2030 |>
  select(Entity = Code, co2_intensity) |> # prepare identifiers
  crossing(systems_long) |> # replicate per system tibble
  mutate(
    # scale only the production leg by change in CI relative to Ontario baseline
    g_GWP_1L = if_else(
      component == "prod",
      g_GWP_1L * (co2_intensity / 28),
      g_GWP_1L
    )
  ) |>
  group_by(Entity, system) |> # recompute shares per system
  mutate(
    percent_emissions = g_GWP_1L /
      sum(g_GWP_1L[component != "total"]) *
      100
  ) |>
  ungroup()

# 2) Sum g_CO₂e per pure-L O₂ for each country × system
emissions_totals_2030 <- emissions_components_2030 |>
  filter(component != "total") |> # drop the aggregate row
  group_by(Entity, system) |>
  summarise(
    total_g_GWP_1L = sum(g_GWP_1L), # g-CO₂e / pure-L
    .groups = "drop"
  )

# 3) Convert to wide: one column per delivery system
emissions_wide_2030 <- emissions_totals_2030 |>
  mutate(system = paste0("GWP_", system)) |> # prefix for clarity
  pivot_wider(
    names_from = system,
    values_from = total_g_GWP_1L
  )

# 4) Assemble country table for 2030 scenario
country_emissions_2030 <- grid_2030 |>
  select(Code) |> # keep only code
  left_join(emissions_wide_2030, by = c("Code" = "Entity")) |>
  filter(!is.na(Code) & Code != "") # drop any empty codes

# ──────────────────────────────────────────────────────────────────────────────
# Compute weighted 1 L/min emissions using mix shares
# ──────────────────────────────────────────────────────────────────────────────
country_1l_2030 <- country_emissions_2030 |>
  left_join(select(grid_2030, Code, Region), by = "Code") |> # add Region
  left_join(oxygen_mix, by = c("Region" = "Entity")) |> # add system mix shares
  rowwise() |> # per-row summary
  mutate(
    all_gwp_na = if_all(all_of(system_cols), is.na), # detect fully NA rows
    g_CO2e_1L_2030 = if_else(
      all_gwp_na,
      NA_real_, # preserve NA
      sum(
        c_across(all_of(system_cols)) *
          c_across(all_of(share_cols)),
        na.rm = TRUE
      ) /
        100
    )
  ) |>
  ungroup() |>
  select(Code, g_CO2e_1L_2030) |>
  filter(!is.na(g_CO2e_1L_2030)) # drop NAs

###############################################################################
# HALF SCENARIO (–50% fossil) – identical steps, using co2_intensity_half
###############################################################################
# 0) Inject half‐scenario CI
grid_half <- grid_data_ci |>
  select(-co2_intensity) |>
  distinct(Code, .keep_all = TRUE) |>
  left_join(
    target_intensities |> select(Code, co2_intensity_half),
    by = "Code"
  ) |>
  rename(co2_intensity = co2_intensity_half)

# 1) Cross with delivery-system tables & scale “prod”
emissions_components_half <- grid_half |>
  select(Entity = Code, co2_intensity) |>
  crossing(systems_long) |>
  mutate(
    g_GWP_1L = if_else(
      component == "prod",
      g_GWP_1L * (co2_intensity / 28),
      g_GWP_1L
    )
  ) |>
  group_by(Entity, system) |>
  mutate(
    percent_emissions = g_GWP_1L / sum(g_GWP_1L[component != "total"]) * 100
  ) |>
  ungroup()

# 2) Sum per system
emissions_totals_half <- emissions_components_half |>
  filter(component != "total") |>
  group_by(Entity, system) |>
  summarise(total_g_GWP_1L = sum(g_GWP_1L), .groups = "drop")

# 3) Wide layout
emissions_wide_half <- emissions_totals_half |>
  mutate(system = paste0("GWP_", system)) |>
  pivot_wider(names_from = system, values_from = total_g_GWP_1L)

# 4) Assemble country table
country_emissions_half <- grid_half |>
  select(Code) |>
  left_join(emissions_wide_half, by = c("Code" = "Entity")) |>
  filter(!is.na(Code) & Code != "")

# 5) Compute weighted 1 L/min emissions
country_1l_half <- country_emissions_half |>
  left_join(select(grid_half, Code, Region), by = "Code") |>
  left_join(oxygen_mix, by = c("Region" = "Entity")) |>
  rowwise() |>
  mutate(
    all_gwp_na = if_all(all_of(system_cols), is.na),
    g_CO2e_1L_half = if_else(
      all_gwp_na,
      NA_real_,
      sum(
        c_across(all_of(system_cols)) *
          c_across(all_of(share_cols)),
        na.rm = TRUE
      ) /
        100
    )
  ) |>
  ungroup() |>
  select(Code, g_CO2e_1L_half) |>
  filter(!is.na(g_CO2e_1L_half))

###############################################################################
# CLEAN SCENARIO (0% fossil) – identical steps, using co2_intensity_clean
###############################################################################
# 0) Inject clean‐scenario CI
grid_clean <- grid_data_ci |>
  select(-co2_intensity) |>
  distinct(Code, .keep_all = TRUE) |>
  left_join(
    target_intensities |> select(Code, co2_intensity_clean),
    by = "Code"
  ) |>
  rename(co2_intensity = co2_intensity_clean)

# 1) Cross & scale “prod”
emissions_components_clean <- grid_clean |>
  select(Entity = Code, co2_intensity) |>
  crossing(systems_long) |>
  mutate(
    g_GWP_1L = if_else(
      component == "prod",
      g_GWP_1L * (co2_intensity / 28),
      g_GWP_1L
    )
  ) |>
  group_by(Entity, system) |>
  mutate(
    percent_emissions = g_GWP_1L / sum(g_GWP_1L[component != "total"]) * 100
  ) |>
  ungroup()

# 2) Totals per system
emissions_totals_clean <- emissions_components_clean |>
  filter(component != "total") |>
  group_by(Entity, system) |>
  summarise(total_g_GWP_1L = sum(g_GWP_1L), .groups = "drop")

# 3) Wide layout
emissions_wide_clean <- emissions_totals_clean |>
  mutate(system = paste0("GWP_", system)) |>
  pivot_wider(names_from = system, values_from = total_g_GWP_1L)

# 4) Assemble country table
country_emissions_clean <- grid_clean |>
  select(Code) |>
  left_join(emissions_wide_clean, by = c("Code" = "Entity")) |>
  filter(!is.na(Code) & Code != "")

# 5) Compute weighted 1 L/min emissions
country_1l_clean <- country_emissions_clean |>
  left_join(select(grid_clean, Code, Region), by = "Code") |>
  left_join(oxygen_mix, by = c("Region" = "Entity")) |>
  rowwise() |>
  mutate(
    all_gwp_na = if_all(all_of(system_cols), is.na),
    g_CO2e_1L_clean = if_else(
      all_gwp_na,
      NA_real_,
      sum(
        c_across(all_of(system_cols)) *
          c_across(all_of(share_cols)),
        na.rm = TRUE
      ) /
        100
    )
  ) |>
  ungroup() |>
  select(Code, g_CO2e_1L_clean) |>
  filter(!is.na(g_CO2e_1L_clean))

###############################################################################
# FINAL ASSEMBLY & EXPORT
# Join baseline + all three scenarios and write to CSV
###############################################################################
final_table <- target_intensities |>
  select(
    Entity,
    Code,
    Region,
    co2_intensity, # baseline
    co2_intensity_2030,
    co2_intensity_half,
    co2_intensity_clean,
    g_CO2e_1L # baseline 1 L/min
  ) |>
  left_join(country_1l_2030, by = "Code") |>
  left_join(country_1l_half, by = "Code") |>
  left_join(country_1l_clean, by = "Code")

write_csv(
  final_table,
  file = here("emissions_perliter_projections.csv")
)
