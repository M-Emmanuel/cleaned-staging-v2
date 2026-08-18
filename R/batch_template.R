#' Batch template field dictionary
#'
#' Returns the definition of every field of the iCLEANED batch input template:
#' the sheet it belongs to, its type and unit, whether it is required, where
#' its value comes from when left blank (`precedence`) and its allowed values.
#'
#' The batch file only holds farm-level data. For each field the value is
#' taken, from highest to lowest priority, from:
#' * the farm's row in the batch file (`farm`, `season`, `livestock`,
#'   `feed_items`, `feed_type`, `fertilizer` sheets);
#' * the defaults stored in the selected parameter database
#'   (`lkp_defaults`, `lkp_feeditem_defaults`, `lkp_crops_defaults`) or its
#'   livestock, feed, crop and lookup tables, matched by code or description;
#' * a built-in default (usually 0), where the dictionary says so.
#'
#' "Required" means the value must be available from the file or the
#' database.
#'
#' @return A data frame with columns `sheet`, `field`, `applies_to`, `type`,
#'   `unit`, `required`, `precedence`, `allowed_values` and `description`.
#' @export
batch_dictionary <- function() {
  path <- system.file("extdata", "batch", "batch_dictionary.csv", package = "cleaned")
  if (!nzchar(path)) {
    path <- file.path("inst", "extdata", "batch", "batch_dictionary.csv")
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                  na.strings = character(0), encoding = "UTF-8")
}

#' Read an iCLEANED parameter database
#'
#' Reads the lookup tables (`lkp_*.csv`) of a parameter database folder, as
#' used by the iCLEANED app, into a named list. Column names are cleaned of
#' byte-order marks and surrounding spaces.
#'
#' @param path Folder containing the `lkp_*.csv` files.
#' @return A named list of data frames, e.g. `db$lkp_livetype`.
#' @export
read_parameter_database <- function(path) {
  files <- list.files(path, pattern = "^lkp_.*\\.csv$", full.names = TRUE)
  if (length(files) == 0) {
    stop("No lkp_*.csv tables found in ", path, call. = FALSE)
  }
  db <- lapply(files, function(f) {
    # fread (as in the app) reads files with mixed or Latin-1 characters
    # completely; read.csv stops at the first character it cannot decode
    x <- as.data.frame(data.table::fread(f, encoding = "UTF-8", na.strings = c("", "NA"),
                                         showProgress = FALSE),
                       stringsAsFactors = FALSE)
    names(x) <- trimws(gsub("^\ufeff", "", names(x)))
    names(x) <- gsub('^"|"$', "", names(x))
    x[] <- lapply(x, function(col) {
      if (!is.character(col)) return(col)
      bad <- !is.na(col) & !validUTF8(col)
      col[bad] <- iconv(col[bad], from = "latin1", to = "UTF-8")
      trimws(col)
    })
    x
  })
  names(db) <- tools::file_path_sans_ext(basename(files))
  db
}

# Reference lists written into the template (sheet name, table, columns)
batch_reference_tables <- list(
  ref_feeds = list("lkp_feeditem", c("feed_item_code", "feed_item_name", "crop_code", "dm_content", "me_content", "cp_content")),
  ref_crops = list("lkp_crops", c("crop_code", "crop_name", "category")),
  ref_manure = list("lkp_manureman", "manureman_desc"),
  ref_region = list("lkp_region", c("region_code", "region_desc")),
  ref_climate = list("lkp_climate", "climate_desc"),
  ref_soil = list("lkp_soil", c("soil_desc", "k_value")),
  ref_cropland_system = list("lkp_croplandsystem", c("sys_desc", "change_factor")),
  ref_cropland_tillage = list("lkp_tillageregime", c("tillage_desc", "change_factor")),
  ref_cropland_orgmatter = list("lkp_organicmatter", c("orgmatter_desc", "change_factor")),
  ref_grassland_management = list("lkp_grasslandman", c("management_desc", "change_factor")),
  ref_grassland_input = list("lkp_grassinputlevel", c("grassinputlevel_desc", "change_factor")),
  ref_landcover = list("lkp_landcover", c("landcover_desc", "c_factor")),
  ref_slope = list("lkp_slope", c("slope_desc", "p_factor")),
  ref_fertilizer = list("lkp_orgfertilizer", "fertilizer_desc")
)

# Dropdowns added to the data sheets: field -> reference sheet and column
batch_dropdowns <- list(
  region = c("ref_region", "region_code"),
  climate_zone_2 = c("ref_climate", "climate_desc"),
  soil_description = c("ref_soil", "soil_desc"),
  cropland_system = c("ref_cropland_system", "sys_desc"),
  cropland_tillage = c("ref_cropland_tillage", "tillage_desc"),
  cropland_orgmatter = c("ref_cropland_orgmatter", "orgmatter_desc"),
  grassland_management = c("ref_grassland_management", "management_desc"),
  grassland_implevel = c("ref_grassland_input", "grassinputlevel_desc"),
  livetype_code = c("livestock_p", "livetype_code"),
  species = c("ref_choices", "species"),
  animal_class = c("ref_choices", "animal_class"),
  ipcc_ef_category_t2 = c("ref_choices", "ipcc_ef_category_t2"),
  ipcc_manure_ch4_category = c("ref_choices", "ipcc_manure_ch4_category"),
  ipcc_n_loss_category = c("ref_choices", "ipcc_n_loss_category"),
  ipcc_prp_category = c("ref_choices", "ipcc_prp_category"),
  manureman_stable = c("ref_manure", "manureman_desc"),
  manureman_non_roofed_enclosure = c("ref_manure", "manureman_desc"),
  manureman_onfarm_grazing = c("ref_manure", "manureman_desc"),
  manureman_offfarm_grazing = c("ref_manure", "manureman_desc"),
  manureman_2_stable = c("ref_manure", "manureman_desc"),
  manureman_2_non_roofed_enclosure = c("ref_manure", "manureman_desc"),
  manureman_2_onfarm_grazing = c("ref_manure", "manureman_desc"),
  manureman_2_offfarm_grazing = c("ref_manure", "manureman_desc"),
  land_cover_desc = c("ref_landcover", "landcover_desc"),
  slope_desc = c("ref_slope", "slope_desc"),
  grassman_desc = c("ref_grassland_management", "management_desc"),
  fertilizer_desc = c("ref_fertilizer", "fertilizer_desc"),
  feed_item_code = c("ref_feeds", "feed_item_code"),
  crop_code = c("ref_crops", "crop_code"),
  # fixed lists (sheet ref_choices)
  source_type = c("ref_choices", "source_type"),
  intercrop = c("ref_choices", "intercrop"),
  category = c("ref_choices", "crop_category"),
  ecosystem_type = c("ref_choices", "ecosystem_type"),
  water_regime = c("ref_choices", "water_regime"),
  organic_amendment = c("ref_choices", "organic_amendment")
)

# Lists of values with no table in the parameter database
batch_choice_lists <- function(parameter_database) {
  cat <- parameter_database$lkp_crops$category
  cat <- sort(unique(trimws(as.character(cat[!is.na(cat) & nzchar(trimws(cat))]))))
  c(list(source_type = c("Main", "Residue", "Purchased"), intercrop = c("No", "Yes"),
         crop_category = cat), rice_options(),
    list(species = names(livestock_species_classes),
         animal_class = unique(unlist(livestock_species_classes, use.names = FALSE))),
    ipcc_livestock_categories)
}

#' Write an empty iCLEANED batch input template
#'
#' Creates the Excel template used to describe many farms at once. The data
#' sheets (farm-level only) get their column headers from
#' [batch_dictionary()], and each is followed by its project twin (`_p`
#' sheets, see [batch_default_project()]): the values used for the cells left
#' blank, filled with the database's batch defaults; reference sheets
#' list the allowed values of the chosen parameter database, with dropdowns in
#' the data sheets.
#'
#' @param parameter_database A list from [read_parameter_database()], or the
#'   path of a parameter database folder.
#' @param file Path of the `.xlsx` file to write.
#' @param database_name Name of the parameter database, shown in the README.
#' @param max_rows Number of rows that get dropdowns in each data sheet.
#' @return `file`, invisibly.
#' @export
write_batch_template <- function(parameter_database, file, database_name = NULL,
                                 max_rows = 2000) {
  if (is.character(parameter_database)) {
    if (is.null(database_name)) database_name <- basename(parameter_database)
    parameter_database <- read_parameter_database(parameter_database)
  }
  dict <- batch_dictionary()
  wb <- openxlsx::createWorkbook()
  header_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9E1F2",
                                        border = "bottom", wrapText = TRUE)

  sheet_data <- list()
  add_table <- function(sheet, data, widths = 18) {
    sheet_data[[sheet]] <<- data
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, data, headerStyle = header_style)
    openxlsx::freezePane(wb, sheet, firstRow = TRUE)
    openxlsx::setColWidths(wb, sheet, cols = seq_len(max(1, ncol(data))), widths = widths)
  }

  # README ---------------------------------------------------------------
  readme <- data.frame(`iCLEANED batch input template` = c(
    paste("Template version 1 - parameter database:", ifelse(is.null(database_name), "(not set)", database_name)),
    "",
    "This file holds farm-level data only: one row per farm in 'farm' (column Ids); the other sheets refer to farms by Ids.",
    "Each data sheet has a green twin whose name ends in _p ('project'), with the same columns: a cell left blank for a farm takes the value of the _p sheet, for the same livestock type, feed, crop or fertiliser. Fill a farm's cell only where it differs.",
    "The _p sheets come filled with the batch defaults of the parameter database. Change them for this project; values left blank in both use the batch defaults. You can also change them in the app, in the step 'What's missing', after uploading.",
    "farm_p: one row for all farms; add rows with a value in a group_ column (e.g. group_site = North) for the farms of that group.",
    "season_p: the seasons of farms that have none in 'season'.",
    "livestock_p: one row per livestock type: its species, class, IPCC categories and parameters, and the farms' practices (time in each place, manure systems...). Add a type with a new livetype_code (fill every column; copying a similar row is easiest): it is then offered in the livestock and feed_basket sheets.",
    "feed_items_p: every feed of the parameter database, with its dry matter, energy, protein and crop, and how it is grown (land cover, slope, fertiliser...). feed_type_p: every crop, with its yields, N content, crop coefficients and trees, and how it is managed (crop residues, rice...). A value changed here is used for all farms; practice cells left blank use the farm's answer or the built-in value.",
    "fertilizer_p: the N content of each fertiliser (standard values; give it for NPK, e.g. 17 for 17-17-17, otherwise NPK counts as 0 % N).",
    "Order of priority for any field: the farm's cell > the _p sheet > the batch defaults of the parameter database > built-in value (see 'precedence' in the dictionary).",
    "fertilizer: one row per farm and fertiliser it uses (the amounts per crop go in feed_items).",
    "livestock: one row per farm and livestock type (livetype_code from livestock_p). livetype_desc fills in by itself from the code (grey column): do not type in it.",
    "feed_basket: one row per farm, livestock type and season; one column per feed item code (ref_feeds), holding the % of the diet. Each row adds up to 100.",
    "Feeds and crops are matched by code to the parameter database. Only fill feed_items / feed_type for farms whose feed or crop values differ from feed_items_p / feed_type_p.",
    "Columns starting with group_ in 'farm' (e.g. group_site) are kept for batch summaries.",
    "Blank cells mean 'use the default'. Every categorical column has a dropdown. Its values, and the factors linked to them, are in the ref_ sheets, which are hidden (in Excel: right-click a sheet tab > Unhide); fixed lists (source type, intercrop, crop category, rice settings) are in 'ref_choices'.",
    "Full field list: see 'dictionary'."
  ), check.names = FALSE)
  add_table("README", readme, widths = 140)
  add_table("dictionary", dict, widths = c(12, 26, 22, 9, 16, 16, 40, 40, 60))

  # Data sheets ----------------------------------------------------------
  fields_of <- function(sheet, exclude = character(0)) {
    f <- dict$field[dict$sheet == sheet & !grepl("^<|\\*$", dict$field) &
                      !dict$applies_to %in% c("parameter database", "set by converter") &
                      !grepl("^alias of", dict$precedence)]
    setdiff(f, exclude)
  }
  empty <- function(cols) {
    as.data.frame(stats::setNames(replicate(length(cols), character(0), simplify = FALSE), cols),
                  check.names = FALSE)
  }

  # Each data sheet is followed by its project twin (_p, green header): same
  # columns, one row per livestock type, feed, crop or fertiliser (farm_p: for
  # all farms, or per group). A blank cell of a farm takes the _p value.
  project <- batch_default_project(parameter_database)
  project_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#E2EFDA",
                                         border = "bottom", wrapText = TRUE)
  add_project <- function(sheet, widths = 18) {
    data <- project[[sheet]]
    sheet_data[[sheet]] <<- data
    openxlsx::addWorksheet(wb, sheet, tabColour = "#70AD47")
    openxlsx::writeData(wb, sheet, data, headerStyle = project_style)
    openxlsx::freezePane(wb, sheet, firstRow = TRUE, firstCol = TRUE)
    openxlsx::setColWidths(wb, sheet, cols = seq_len(max(1, ncol(data))), widths = widths)
  }
  farm_fields <- fields_of("farm")
  add_table("farm", empty(c(farm_fields, "group_site")))
  project$farm_p <- cbind(data.frame(group_site = NA_character_, stringsAsFactors = FALSE), project$farm_p)
  add_project("farm_p")
  add_table("season", empty(fields_of("season")))
  add_project("season_p")
  add_table("livestock", empty(fields_of("livestock")))
  add_project("livestock_p")
  feed_codes <- if (!is.null(parameter_database$lkp_feeditem)) as.character(parameter_database$lkp_feeditem$feed_item_code) else character(0)
  add_table("feed_basket", empty(c("Ids", "livetype_code", "livetype_desc", "season_name", feed_codes)))
  add_table("feed_items", empty(fields_of("feed_items")))
  add_project("feed_items_p")
  add_table("feed_type", empty(fields_of("feed_type")))
  add_project("feed_type_p")
  add_table("fertilizer", empty(fields_of("fertilizer", exclude = "fertilizer_code")))
  add_project("fertilizer_p")

  # Reference sheets -----------------------------------------------------
  for (ref in names(batch_reference_tables)) {
    tbl <- parameter_database[[batch_reference_tables[[ref]][[1]]]]
    cols <- batch_reference_tables[[ref]][[2]]
    if (is.null(tbl)) next
    tbl <- tbl[, intersect(cols, names(tbl)), drop = FALSE]
    if (ref == "ref_fertilizer") {
      # the N content used when percentage_n is left blank (none for NPK: give it)
      n <- batch_fertilizer_n[as.character(tbl$fertilizer_desc)]
      tbl$standard_percentage_n <- ifelse(is.na(n), "give it in percentage_n", as.character(n))
    }
    add_table(ref, tbl, widths = 30)
  }

  lists <- batch_choice_lists(parameter_database)
  n_max <- max(lengths(lists))
  add_table("ref_choices", as.data.frame(lapply(lists, function(v) c(v, rep(NA, n_max - length(v)))),
                                         stringsAsFactors = FALSE), widths = 45)

  # Dropdowns --------------------------------------------------------------
  data_sheets <- c("farm", "season", "livestock", "feed_basket", "feed_items", "feed_type", "fertilizer",
                   batch_project_sheets)
  list_ref <- function(ref) {
    ref_tbl <- sheet_data[[ref[1]]]
    if (is.null(ref_tbl)) return(NULL)
    k <- match(ref[2], names(ref_tbl))
    if (is.na(k) || nrow(ref_tbl) == 0) return(NULL)
    n_ref <- sum(!is.na(ref_tbl[[k]]) & nzchar(as.character(ref_tbl[[k]])))
    if (n_ref == 0) return(NULL)
    # (livestock types added in the file are offered too)
    if (ref[1] == "livestock_p") n_ref <- max(n_ref, 499)
    col_letter <- openxlsx::int2col(k)
    sprintf("'%s'!$%s$2:$%s$%d", ref[1], col_letter, col_letter, n_ref + 1)
  }
  for (sheet in data_sheets) {
    cols <- names(sheet_data[[sheet]])
    for (j in which(cols %in% names(batch_dropdowns))) {
      if (batch_dropdowns[[cols[j]]][1] == sheet) next   # (not a list of the sheet itself)
      v <- list_ref(batch_dropdowns[[cols[j]]])
      if (is.null(v)) next
      # (openxlsx emits a harmless internal sprintf warning here)
      suppressWarnings(openxlsx::dataValidation(wb, sheet, cols = j, rows = 2:max_rows, type = "list", value = v))
    }
  }

  # The livestock name follows the code chosen (livestock and feed_basket
  # sheets): a formula looks it up in livestock_p, so the two cannot disagree
  lp <- names(sheet_data$livestock_p)
  kc <- match("livetype_code", lp); kd <- match("livetype_desc", lp)
  if (!is.na(kc) && !is.na(kd)) {
    lp_code <- sprintf("livestock_p!$%s$2:$%s$500", openxlsx::int2col(kc), openxlsx::int2col(kc))
    lp_desc <- sprintf("livestock_p!$%s$2:$%s$500", openxlsx::int2col(kd), openxlsx::int2col(kd))
    auto_style <- openxlsx::createStyle(fgFill = "#F2F2F2", fontColour = "#595959")
    for (sheet in c("livestock", "feed_basket")) {
      cols <- names(sheet_data[[sheet]])
      jc <- match("livetype_code", cols); jd <- match("livetype_desc", cols)
      if (is.na(jc) || is.na(jd)) next
      ref <- paste0("$", openxlsx::int2col(jc), seq(2, max_rows))
      # (the code may be typed as a number or chosen from the list as text)
      f <- sprintf('IF(%s="","",IFERROR(INDEX(%s,MATCH(%s&"",%s,0)),IFERROR(INDEX(%s,MATCH(--%s,%s,0)),"Code not in livestock_p")))',
                   ref, lp_desc, ref, lp_code, lp_desc, ref, lp_code)
      openxlsx::writeFormula(wb, sheet, x = f, startCol = jd, startRow = 2)
      openxlsx::addStyle(wb, sheet, auto_style, rows = 2:max_rows, cols = jd, gridExpand = TRUE)
    }
  }

  # Reference sheets are hidden: the dropdowns still use them (Unhide in Excel to see them)
  ref <- grepl("^ref_", names(wb))
  openxlsx::sheetVisibility(wb)[ref] <- "hidden"
  openxlsx::activeSheet(wb) <- "README"

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}
