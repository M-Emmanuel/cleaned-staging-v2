# Batch input: building study objects -------------------------------------------

# Field order of a study object, as saved by the iCLEANED app
batch_farm_fields <- c(
  "database_code", "region", "farm_name", "climate_zone_2", "soil_description",
  "cropland_system", "cropland_tillage", "cropland_orgmatter",
  "grassland_management", "grassland_implevel", "purchased_manure",
  "purchased_compost", "purchased_organic_n", "purchased_bedding",
  "waste_production_milk", "waste_production_meat", "waste_distribution_milk",
  "waste_distribution_meat", "waste_processing_milk", "waste_processing_meat",
  "waste_consume_milk", "waste_consume_meat", "annual_prec", "rain_length",
  "soil_k_value", "soil_n", "soil_c", "soil_clay", "soil_bulk", "soil_depth",
  "et", "cropland_system_ipcc", "cropland_tillage_ipcc",
  "cropland_orgmatter_ipcc", "grassland_management_ipcc",
  "grassland_implevel_ipcc", "grassland_toarable", "arable_tograssland"
)
batch_farm_text <- c("database_code", "region", "farm_name", "climate_zone_2",
                     "soil_description", "cropland_system", "cropland_tillage",
                     "cropland_orgmatter", "grassland_management", "grassland_implevel")

# IPCC factor fields: description field, lookup table, description column
batch_farm_factors <- list(
  cropland_system_ipcc = c("cropland_system", "lkp_croplandsystem", "sys_desc"),
  cropland_tillage_ipcc = c("cropland_tillage", "lkp_tillageregime", "tillage_desc"),
  cropland_orgmatter_ipcc = c("cropland_orgmatter", "lkp_organicmatter", "orgmatter_desc"),
  grassland_management_ipcc = c("grassland_management", "lkp_grasslandman", "management_desc"),
  grassland_implevel_ipcc = c("grassland_implevel", "lkp_grassinputlevel", "grassinputlevel_desc")
)

batch_livestock_fields <- c(
  "livetype_code", "livetype_desc", "species", "animal_class", "herd_composition",
  "annual_milk", "annual_wool", "annual_growth", "time_in_stable",
  "manure_in_stable", "manureman_stable", "time_in_non_roofed_enclosure",
  "manure_in_non_roofed_enclosure", "manureman_non_roofed_enclosure",
  "time_in_onfarm_grazing", "manure_in_field", "manureman_onfarm_grazing",
  "time_in_offfarm_grazing", "manureman_offfarm_grazing", "distance_to_pasture",
  "manure_onfarm_fraction", "manure_sales_fraction", "body_weight",
  "body_weight_weaning", "body_weight_year_one", "adult_weight", "work_hour",
  "litter_size", "piglets_relying_on_milk", "lactation_length",
  "proportion_growth_piglets_milk", "lw_gain_piglets", "cp_maintenance",
  "cp_lys_pregnancy", "cp_lactmilk", "cp_lys_growth", "birth_interval",
  "protein_milkcontent", "fat_milkcontent", "energy_milkcontent",
  "energy_meatcontent", "protein_meatcontent", "carcass_fraction",
  "n_manure_content", "meat_product", "milk_product", "ipcc_ef_category_t2",
  "ipcc_manure_ch4_category", "ipcc_n_loss_category", "ipcc_prp_category",
  # a second manure system for a share of the dung of a place
  "manure_share_1_stable", "manureman_2_stable", "manure_share_2_stable",
  "manure_share_1_non_roofed_enclosure", "manureman_2_non_roofed_enclosure", "manure_share_2_non_roofed_enclosure",
  "manure_share_1_onfarm_grazing", "manureman_2_onfarm_grazing", "manure_share_2_onfarm_grazing",
  "manure_share_1_offfarm_grazing", "manureman_2_offfarm_grazing", "manure_share_2_offfarm_grazing"
)
batch_livestock_text <- c("manureman_2_stable", "manureman_2_non_roofed_enclosure",
                          "manureman_2_onfarm_grazing", "manureman_2_offfarm_grazing","livetype_code", "livetype_desc", "species", "animal_class",
                          "manureman_stable", "manureman_non_roofed_enclosure",
                          "manureman_onfarm_grazing", "manureman_offfarm_grazing",
                          "meat_product", "milk_product", "ipcc_ef_category_t2",
                          "ipcc_manure_ch4_category", "ipcc_n_loss_category",
                          "ipcc_prp_category")
# Farm-level livestock fields (file > lkp_defaults > 0); the rest come from lkp_livetype
batch_livestock_farm_fields <- c(
  "annual_milk", "annual_wool", "annual_growth", "work_hour",
  "piglets_relying_on_milk", "time_in_stable", "time_in_non_roofed_enclosure",
  "time_in_onfarm_grazing", "time_in_offfarm_grazing", "manure_in_stable",
  "manure_in_non_roofed_enclosure", "manure_in_field", "manure_onfarm_fraction",
  "manure_sales_fraction", "distance_to_pasture",
  "manure_share_2_stable", "manure_share_2_non_roofed_enclosure",
  "manure_share_2_onfarm_grazing", "manure_share_2_offfarm_grazing",
  "manure_share_1_stable", "manure_share_1_non_roofed_enclosure",
  "manure_share_1_onfarm_grazing", "manure_share_1_offfarm_grazing"
)

# Fraction of the dung of a place under option 2: given, or 1 - the fraction
# under option 1 when only that one is given
batch_manure_share_2 <- function(share_1, share_2) {
  share_1 <- batch_num(share_1); share_2 <- batch_num(share_2)
  ifelse(is.na(share_2) & !is.na(share_1), 1 - share_1, share_2)
}

batch_feed_fields <- c(
  "crop_code", "feed_item_code", "crop_name", "feed_item_name", "source_type",
  "intercrop", "intercrop_fraction", "cut_carry_fraction", "land_cover_desc",
  "landcover_c_factor", "slope_desc", "slope_p_factor", "slope_length",
  "grassman_desc", "grassman_change_factor", "main_product_removal",
  "residue_removal", "residue_burnt", "dm_content", "me_content", "cp_content",
  "water_regime", "cultivation_period", "ecosystem_type", "organic_amendment",
  "dry_yield", "residue_dry_yield", "residue_n", "kc_initial", "kc_midseason",
  "kc_late", "category", "trees_ha", "trees_dhb", "trees_growth",
  "trees_removal", "trees_ha_dbh25", "average_dbh25", "increase_dbh25",
  "trees_ha_dbh2550", "average_dbh2550", "increase_dbh2550", "trees_ha_dbh50",
  "average_dbh50", "increase_dbh50", "time_horizon", "diameter_breast",
  "fraction_as_manure", "n_fertilizer", "main_n", "land_cover", "slope",
  "grassman", "fraction_as_fertilizer", "urea", "npk", "dap",
  "ammonium_nitrate", "ammonium_sulfate", "n_solutions", "ammonia"
)
batch_feed_text <- c("crop_code", "feed_item_code", "crop_name", "feed_item_name",
                     "source_type", "land_cover_desc", "slope_desc", "grassman_desc",
                     "water_regime", "ecosystem_type", "organic_amendment",
                     "category", "fraction_as_manure", "n_fertilizer",
                     "land_cover", "slope", "grassman")
# Crop management fields: feed_items sheet > feed_type sheet > lkp_crops_defaults
batch_crop_management_fields <- c(
  "main_product_removal", "residue_removal", "residue_burnt", "cultivation_period",
  "grassman_desc", "grassman_change_factor", "water_regime", "ecosystem_type",
  "organic_amendment"
)
# Standard N content (%) of mineral fertilisers, as in the iCLEANED app
batch_fertilizer_n <- c("Urea" = 46, "DAP" = 18, "Ammonium nitrate" = 34.5,
                        "Ammonium sulfate" = 21, "N solutions" = 32, "Ammonia" = 82)

# First non-missing value
batch_first <- function(...) {
  for (v in list(...)) {
    if (length(v) && !is.na(v[1]) && !identical(trimws(as.character(v[1])), "")) return(v[1])
  }
  NA
}
# Value of a column in a one-row data frame (NA when the column is absent)
batch_cell <- function(row, field) {
  if (is.null(row) || nrow(row) == 0 || !field %in% names(row)) return(NA)
  row[[field]][1]
}
batch_lookup <- function(table, key_col, key, value_col) {
  if (is.null(table) || is.na(key) || !all(c(key_col, value_col) %in% names(table))) return(NA)
  hit <- which(trimws(as.character(table[[key_col]])) == trimws(as.character(key)))
  if (length(hit) == 0) NA else table[[value_col]][hit[1]]
}
batch_as <- function(value, text) {
  if (text) {
    if (is.na(value)) "" else as.character(value)
  } else {
    v <- suppressWarnings(as.numeric(value))
    if (is.na(v)) 0 else v
  }
}

#' Find livestock types that are calves
#'
#' @param parameter_database A list from [read_parameter_database()].
#' @return The `livetype_code`s whose name contains "calf" or "calves".
#' @export
batch_calf_codes <- function(parameter_database) {
  lt <- parameter_database$lkp_livetype
  as.character(lt$livetype_code[grepl("\\bcal(f|ves)\\b", lt$livetype_desc, ignore.case = TRUE)])
}

#' Build study objects from an iCLEANED batch input file
#'
#' Creates one study object per farm of a batch file, in the format saved by
#' the iCLEANED app, ready for [feed_quality()], [energy_requirement()] and
#' the other model functions. Every value is taken, from highest to lowest
#' priority, from the farm's rows in the batch file, the defaults stored in the
#' parameter database (`lkp_defaults`, `lkp_feeditem_defaults`,
#' `lkp_crops_defaults`) or its lookup tables, and finally a built-in default
#' (see [batch_dictionary()]).
#'
#' The file is checked first with [validate_batch_input()]; building stops if
#' the check finds errors.
#'
#' @param file Path of the batch `.xlsx` file, or the result of
#'   [read_batch_input()].
#' @param parameter_database A list from [read_parameter_database()], or the
#'   path of a parameter database folder.
#' @param database_name Name of the parameter database, stored as
#'   `database_code` in each study object. Defaults to the folder name when
#'   `parameter_database` is a path.
#' @param exclude_livetypes Livestock type codes to leave out of every farm
#'   (with their feed basket rows), e.g. `batch_calf_codes(db)` to exclude
#'   calves.
#' @return A named list of study objects (names are the farm names). Attribute
#'   `farms` is a data frame with `Ids`, `farm_name` and the `group_*` and
#'   `country` columns of the farm sheet; attribute `validation` holds the
#'   validation report.
#' @export
build_study_objects <- function(file, parameter_database, database_name = NULL,
                                exclude_livetypes = character(0)) {
  if (is.character(parameter_database)) {
    if (is.null(database_name)) database_name <- basename(parameter_database)
    db <- read_parameter_database(parameter_database)
  } else {
    db <- parameter_database
  }
  input <- if (is.list(file) && all(batch_data_sheets %in% names(file))) file else read_batch_input(file)
  exclude_livetypes <- as.character(exclude_livetypes)
  if (length(exclude_livetypes)) {
    for (sh in c("livestock", "feed_basket")) {
      x <- input[[sh]]
      if (nrow(x) > 0 && "livetype_code" %in% names(x)) {
        input[[sh]] <- x[!x$livetype_code %in% exclude_livetypes, , drop = FALSE]
      }
    }
  }

  # project values (_p sheets) fill the blanks, before the database's batch defaults
  applied <- batch_apply_project(input, db)
  input <- applied$input; db <- applied$db
  report <- validate_batch_input(input, db)
  errors <- report[report$severity == "error", , drop = FALSE]
  if (nrow(errors) > 0) {
    err <- simpleError(sprintf(
      "The batch file has %d error(s); fix them before building study objects. First: [%s row %s] %s",
      nrow(errors), errors$sheet[1], errors$row[1], errors$message[1]))
    err$report <- report
    stop(err)
  }

  defaults <- batch_db_defaults(db)
  farm <- input$farm
  farm_names <- if ("farm_name" %in% names(farm)) ifelse(is.na(farm$farm_name), farm$Ids, farm$farm_name) else farm$Ids
  farm_names <- make.unique(as.character(farm_names), sep = "_")

  objects <- lapply(seq_len(nrow(farm)), function(i) {
    tryCatch(
      batch_build_farm(farm[i, , drop = FALSE], farm_names[i], input, db, defaults, database_name),
      error = function(e) stop(sprintf("Farm '%s': %s", farm$Ids[i], conditionMessage(e)), call. = FALSE)
    )
  })
  names(objects) <- farm_names

  meta_cols <- c("Ids", grep("^group_", names(farm), value = TRUE), intersect(c("country", "latitude", "longitude", "respondent_age"), names(farm)))
  meta <- farm[, meta_cols, drop = FALSE]
  # (coordinates identify the farm: kept as text so they are not averaged)
  for (f in intersect(c("latitude", "longitude"), names(meta))) meta[[f]] <- as.character(meta[[f]])
  meta$farm_name <- farm_names
  meta <- meta[, c("Ids", "farm_name", setdiff(meta_cols, "Ids")), drop = FALSE]
  rownames(meta) <- NULL
  attr(objects, "farms") <- meta
  attr(objects, "validation") <- report
  objects
}

# One study object
batch_build_farm <- function(farm_row, farm_name, input, db, defaults, database_name) {
  id <- farm_row$Ids
  of_farm <- function(sheet) {
    x <- input[[sheet]]
    if (nrow(x) == 0 || !"Ids" %in% names(x)) return(x[0, , drop = FALSE])
    x[!is.na(x$Ids) & x$Ids == id, , drop = FALSE]
  }

  # Farm -------------------------------------------------------------------
  obj <- list()
  s <- of_farm("season")
  seasons <- if (nrow(s) > 0) {
    data.frame(season_name = s$season_name, season_length = batch_num(s$season_length),
               stringsAsFactors = FALSE)
  } else {
    defaults$season
  }
  obj$seasons <- seasons
  for (f in batch_farm_fields) {
    value <- switch(f,
      database_code = database_name %||% "",
      farm_name = farm_name,
      batch_first(batch_cell(farm_row, f), defaults$farm[[f]])
    )
    if (is.na(value) && f == "soil_k_value") {
      value <- batch_lookup(db$lkp_soil, "soil_desc", obj$soil_description, "k_value")
    }
    if (is.na(value) && f %in% names(batch_farm_factors)) {
      spec <- batch_farm_factors[[f]]
      value <- batch_lookup(db[[spec[2]]], spec[3], obj[[spec[1]]], "change_factor")
    }
    obj[[f]] <- batch_as(value, f %in% batch_farm_text)
  }

  # Livestock --------------------------------------------------------------
  lv <- of_farm("livestock")
  lt <- db$lkp_livetype
  livestock <- lapply(seq_len(nrow(lv)), function(k) {
    row <- lv[k, , drop = FALSE]
    db_row <- lt[as.character(lt$livetype_code) == row$livetype_code, , drop = FALSE]
    out <- list()
    for (f in batch_livestock_fields) {
      value <- if (f == "herd_composition") {
        batch_cell(row, "number")
      } else if (f %in% c("livetype_code")) {
        row$livetype_code
      } else if (f %in% c("livetype_desc", "species", "animal_class", names(ipcc_livestock_categories))) {
        batch_cell(db_row, f)
      } else if (grepl("^manure_share_[12]_", f)) {
        # either fraction may be left blank: worked out from the other below
        batch_cell(row, f)
      } else if (f %in% batch_livestock_farm_fields || grepl("^manureman_", f)) {
        batch_first(batch_cell(row, f), defaults$livestock[[f]])
      } else {
        batch_first(batch_cell(row, f), batch_cell(db_row, f))
      }
      out[[f]] <- if (grepl("^manure_share_[12]_", f)) suppressWarnings(as.numeric(value)) else
        batch_as(value, f %in% batch_livestock_text)
    }
    out
  })
  livestock <- do.call(rbind, lapply(livestock, as.data.frame, stringsAsFactors = FALSE))
  livestock <- normalise_manure_systems(livestock)
  # Locations where the animals spend no time still need a manure system label
  # that the IPCC tables know (it is multiplied by zero)
  filler <- if ("Pasture/Range/Paddock" %in% db$lkp_manureman$manureman_desc) "Pasture/Range/Paddock" else db$lkp_manureman$manureman_desc[1]
  for (loc in c("stable", "non_roofed_enclosure", "onfarm_grazing", "offfarm_grazing")) {
    col <- paste0("manureman_", loc)
    blank <- livestock[[col]] == ""
    livestock[[col]][blank] <- filler
    # fractions under option 1 and 2 (either may be given); no second system: 0
    share <- paste0("manure_share_2_", loc)
    livestock[[share]] <- batch_manure_share_2(livestock[[paste0("manure_share_1_", loc)]], livestock[[share]])
    livestock[[share]][is.na(livestock[[share]]) | livestock[[paste0("manureman_2_", loc)]] %in% c("", NA)] <- 0
    livestock[[paste0("manure_share_1_", loc)]] <- 1 - livestock[[share]]
  }
  # the second-system fields only when the farm uses them (study objects stay
  # as the app writes them otherwise)
  shares <- grep("^manure_share_2_", names(livestock), value = TRUE)
  if (!any(vapply(shares, function(f) any(suppressWarnings(as.numeric(livestock[[f]])) > 0, na.rm = TRUE), TRUE))) {
    livestock <- livestock[, !grepl("^(manureman_2_|manure_share_[12]_)", names(livestock)), drop = FALSE]
  }
  livestock <- resolve_livestock_categories(livestock)
  obj$livestock <- livestock

  # Feeds ------------------------------------------------------------------
  fb <- of_farm("feed_basket")
  feed_cols <- grep("^[0-9]+$", names(input$feed_basket), value = TRUE)
  alloc <- sapply(feed_cols, function(f) { a <- batch_num(fb[[f]]); ifelse(is.na(a), 0, a) })
  if (!is.matrix(alloc)) alloc <- matrix(alloc, nrow = nrow(fb), dimnames = list(NULL, feed_cols))
  used <- feed_cols[colSums(alloc > 0) > 0]
  fi_file <- of_farm("feed_items")
  ft_file <- of_farm("feed_type")
  fd <- db$lkp_feeditem_defaults
  cd <- db$lkp_crops_defaults
  grass <- db$lkp_grasslandman

  feed_items <- lapply(used, function(code) {
    db_feed <- db$lkp_feeditem[as.character(db$lkp_feeditem$feed_item_code) == code, , drop = FALSE]
    file_feed <- if ("feed_item_code" %in% names(fi_file)) fi_file[!is.na(fi_file$feed_item_code) & fi_file$feed_item_code == code, , drop = FALSE] else fi_file[0, , drop = FALSE]
    def_feed <- if (is.null(fd)) NULL else fd[as.character(fd$feed_item_code) == code, , drop = FALSE]
    crop <- as.character(batch_first(batch_cell(file_feed, "crop_code"), batch_cell(db_feed, "crop_code")))
    db_crop <- db$lkp_crops[as.character(db$lkp_crops$crop_code) == crop, , drop = FALSE]
    file_crop <- if ("crop_code" %in% names(ft_file)) ft_file[!is.na(ft_file$crop_code) & ft_file$crop_code == crop, , drop = FALSE] else ft_file[0, , drop = FALSE]
    def_crop <- if (is.null(cd)) NULL else cd[as.character(cd$crop_code) == crop, , drop = FALSE]
    category <- batch_first(batch_cell(file_crop, "category"), batch_cell(db_crop, "category"))

    out <- list()
    for (f in batch_feed_fields) {
      value <- if (f == "crop_code") {
        crop
      } else if (f == "feed_item_code") {
        code
      } else if (f %in% c("feed_item_name", "dm_content", "me_content", "cp_content")) {
        batch_first(batch_cell(file_feed, f), batch_cell(db_feed, f))
      } else if (f %in% batch_crop_management_fields) {
        batch_first(batch_cell(file_feed, f), batch_cell(file_crop, f), batch_cell(def_crop, f))
      } else if (f %in% c("fraction_as_manure", "n_fertilizer")) {
        "NULL"
      } else if (f %in% c("land_cover", "slope", "grassman", "landcover_c_factor", "slope_p_factor")) {
        batch_first(batch_cell(file_feed, f), batch_cell(def_feed, f))
      } else if (f %in% c("crop_name", "category", "dry_yield", "residue_dry_yield", "main_n",
                          "residue_n", "kc_initial", "kc_midseason", "kc_late") ||
                 grepl("^trees_|dbh|^time_horizon$|^diameter_breast$", f)) {
        batch_first(batch_cell(file_crop, f), batch_cell(db_crop, f))
      } else {
        batch_first(batch_cell(file_feed, f), batch_cell(def_feed, f))
      }
      out[[f]] <- value
    }
    # Built-in defaults and codes/factors looked up from the database
    if (is.na(out$source_type)) out$source_type <- "Main"
    if (is.na(out$grassman_desc)) out$grassman_desc <- grass$management_desc[1]
    lc <- db$lkp_landcover; sl <- db$lkp_slope
    if (is.na(out$land_cover)) out$land_cover <- batch_lookup(lc, "landcover_desc", out$land_cover_desc, "landcover_code")
    if (is.na(out$landcover_c_factor)) out$landcover_c_factor <- batch_lookup(lc, "landcover_desc", out$land_cover_desc, "c_factor")
    if (is.na(out$slope)) out$slope <- batch_lookup(sl, "slope_desc", out$slope_desc, "slope_code")
    if (is.na(out$slope_p_factor)) out$slope_p_factor <- batch_lookup(sl, "slope_desc", out$slope_desc, "p_factor")
    if (is.na(out$grassman)) out$grassman <- batch_lookup(grass, "management_desc", out$grassman_desc, "management_code")
    if (is.na(out$grassman_change_factor)) out$grassman_change_factor <- batch_lookup(grass, "management_desc", out$grassman_desc, "change_factor")
    for (f in batch_feed_fields) out[[f]] <- batch_as(out[[f]], f %in% batch_feed_text)
    out
  })
  obj$feed_items <- do.call(rbind, lapply(feed_items, as.data.frame, stringsAsFactors = FALSE))

  # Fertilisers ------------------------------------------------------------
  fe <- of_farm("fertilizer")
  ofert <- db$lkp_orgfertilizer
  obj$fertilizer <- if (nrow(fe) == 0) list() else data.frame(
    fertilizer_code = vapply(seq_len(nrow(fe)), function(k) as.character(batch_first(
      batch_cell(fe[k, , drop = FALSE], "fertilizer_code"),
      batch_lookup(ofert, "fertilizer_desc", fe$fertilizer_desc[k], "fertilizer_code"))), ""),
    fertilizer_desc = fe$fertilizer_desc,
    fraction = 0,
    percentage_n = vapply(seq_len(nrow(fe)), function(k) batch_as(batch_first(
      batch_cell(fe[k, , drop = FALSE], "percentage_n"),
      unname(batch_fertilizer_n[fe$fertilizer_desc[k]])), FALSE), 0),
    stringsAsFactors = FALSE
  )

  # Feed basket ------------------------------------------------------------
  feed_crop <- stats::setNames(obj$feed_items$crop_code, obj$feed_items$feed_item_code)
  obj$feed_basket <- lapply(seasons$season_name, function(season) {
    rows <- fb$season_name == season
    list(
      feeds = lapply(used, function(code) {
        list(
          feed_item_code = code,
          crop_code = unname(feed_crop[code]),
          livestock = lapply(livestock$livetype_code, function(lcode) {
            hit <- which(rows & fb$livetype_code == lcode)
            list(livetype_code = lcode,
                 allocation = if (length(hit)) unname(alloc[hit[1], code]) else 0)
          })
        )
      }),
      season_name = season
    )
  })

  obj[c("seasons", batch_farm_fields, "livestock", "feed_items", "fertilizer", "feed_basket")]
}

#' Save study objects as JSON files
#'
#' Writes each study object from [build_study_objects()] to
#' `<dir>/<name>.json`, in the format the iCLEANED app loads.
#'
#' @param objects A named list of study objects.
#' @param dir Output folder (created if needed).
#' @return The paths of the files written, invisibly.
#' @export
write_study_objects <- function(objects, dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  files <- file.path(dir, paste0(gsub("[^A-Za-z0-9._-]+", "_", names(objects)), ".json"))
  for (k in seq_along(objects)) {
    jsonlite::write_json(objects[[k]], files[k], auto_unbox = TRUE, pretty = TRUE,
                         digits = NA, null = "null", na = "null")
  }
  invisible(files)
}
