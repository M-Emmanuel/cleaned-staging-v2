# Batch input: running many farms ------------------------------------------------

#' Run the CLEANED model for one study object
#'
#' Runs the full model chain, in the same order and with the same parameter
#' files as the iCLEANED app, and combines the results with
#' [combineOutputs()].
#'
#' @param para A study object (a list as read with
#'   `jsonlite::fromJSON(path, flatten = TRUE)`), or the path of its JSON file.
#' @param file_path Path, without extension, of the outputs written by
#'   [combineOutputs()] (`<file_path>.xlsx`). A temporary file when `NULL`.
#' @param primary_excel Passed to [combineOutputs()]: the app's read-me
#'   workbook, to produce the compact outputs used by the app.
#' @param ghg_ipcc_data,energy_parameters,stock_change_para Model parameters;
#'   by default the files shipped with the package.
#' @return The list returned by [combineOutputs()], with element
#'   `intake_warning`: livestock types whose daily dry matter intake is above
#'   4% of their body weight (as checked by the app).
#' @export
run_study_object <- function(para, file_path = NULL, primary_excel = NULL,
                             ghg_ipcc_data = NULL, energy_parameters = NULL,
                             stock_change_para = NULL) {
  if (is.character(para)) para <- jsonlite::fromJSON(para, flatten = TRUE)
  if (is.list(para$feed_basket) && !is.data.frame(para$feed_basket)) {
    # a list built in R: round-trip through JSON to get the app's layout
    para <- jsonlite::fromJSON(jsonlite::toJSON(para, auto_unbox = TRUE, digits = NA,
                                                null = "null", na = "null"),
                               flatten = TRUE)
  }
  ext <- function(f) jsonlite::fromJSON(system.file("extdata", f, package = "cleaned"), flatten = TRUE)
  if (is.null(ghg_ipcc_data)) ghg_ipcc_data <- ext("ghg_parameters.json")
  if (is.null(energy_parameters)) energy_parameters <- ext("energy_parameters.json")
  if (is.null(stock_change_para)) stock_change_para <- ext("stock_change_parameters.json")
  if (is.null(file_path)) file_path <- tempfile("cleaned_")

  # These functions read earlier results from the calling environment (as in
  # the app), so they run with this function's environment
  n_balance <- n_balance
  ghg_emission <- ghg_emission
  combineOutputs <- combineOutputs
  environment(n_balance) <- environment()
  environment(ghg_emission) <- environment()
  environment(combineOutputs) <- environment()

  feed_basket_quality <- feed_quality(para = para)
  energy_required <- energy_requirement(para = para, feed_basket_quality = feed_basket_quality,
                                        energy_parameters = energy_parameters)
  land_required <- land_requirement(feed_basket_quality = feed_basket_quality,
                                    energy_required = energy_required, para = para)
  soil_erosion <- soil_health(para = para, land_required = land_required)
  water_required <- water_requirement(para = para, land_required = land_required)
  nitrogen_balance <- n_balance(para = para, land_required = land_required,
                                soil_erosion = soil_erosion)
  livestock_productivity <- land_productivity(para = para, energy_required = energy_required)
  biomass <- biomass_calculation(para = para, land_required = land_required)
  soil_carbon <- soil_organic_carbon(para = para, stock_change_para = stock_change_para,
                                     land_required = land_required, biomass = biomass)
  ghg_emissions <- ghg_emission(para = para, energy_required = energy_required,
                                ghg_ipcc_data = ghg_ipcc_data, land_required = land_required,
                                nitrogen_balance = nitrogen_balance)

  out <- combineOutputs(
    para = para, feed_basket_quality = feed_basket_quality,
    energy_required = energy_required, land_required = land_required,
    soil_erosion = soil_erosion, water_required = water_required,
    nitrogen_balance = nitrogen_balance,
    livestock_productivity = livestock_productivity, biomass = biomass,
    soil_carbon = soil_carbon, ghg_emission = ghg_emissions,
    filePath = file_path, primary_excel = primary_excel
  )
  annual <- energy_required$annual_results
  high <- annual$dmi_tot / 365 > annual$body_weight * 0.04
  out$intake_warning <- as.character(annual$livestock_category_name[which(high)])
  out
}

#' Run the CLEANED model for every farm of a batch
#'
#' Runs each study object (one per farm) with [run_study_object()], saves its
#' outputs, and summarises the batch. A farm that fails is logged and the batch
#' goes on with the next one.
#'
#' Outputs written to `output_dir`:
#' * `farms/<farm>/`: the farm's study object (`study_object.json`), its
#'   result file (`<farm>.json`), the detailed result workbook (`<farm>.xlsx`,
#'   written by [combineOutputs()]: with `primary_excel = NULL` it holds all the
#'   batch-processing tables - land, feed quality, energy, nitrogen, water,
#'   products, manure, every greenhouse gas table, biomass, soil carbon) and
#'   `scenario_data.rds` (the tables the iCLEANED app plots);
#' * `batch_results.xlsx`: sheets `summary` (one row per farm with the main
#'   indicators, from [calculate_differences()], and the farm's `group_*`
#'   columns), `by_<group>` (mean and median of each indicator per group, and
#'   for all farms), `log` (status, error message, warnings and run time per
#'   farm) and `validation` (the validation report of the batch file);
#' * `batch_summary.json`: the summary table;
#' * `batch_detailed_results.xlsx`: every result table of the farm workbooks
#'   (land, feed, nitrogen, water, products, manure, greenhouse gases,
#'   biomass, soil carbon, waste), all farms stacked with their farm name, Ids
#'   and group columns in front, plus the reference sheets once.
#'
#' @param objects A named list of study objects from [build_study_objects()],
#'   or the path of a batch `.xlsx` file (then `parameter_database` is
#'   required and the objects are built first).
#' @param output_dir Folder for the outputs (created if needed).
#' @param parameter_database,database_name,exclude_livetypes Passed to
#'   [build_study_objects()] when `objects` is a file.
#' @param farms Data frame with `farm_name`, `Ids` and the `group_*` columns
#'   used for the group summaries. Defaults to `attr(objects, "farms")`.
#' @param primary_excel Passed to [combineOutputs()]. Leave `NULL` (default)
#'   for the full detailed batch outputs; the app's read-me workbook gives the
#'   shorter set of tables of a single app scenario instead.
#' @param progress Optional function called before each farm as
#'   `progress(i, n, farm_name)`, e.g. to update a progress bar.
#' @return Invisibly, a list with `summary`, `groups` (one data frame per
#'   group column), `log`, `detailed` (names of the detailed result sheets) and
#'   `output_dir`.
#' @export
run_batch <- function(objects, output_dir, parameter_database = NULL,
                      database_name = NULL, exclude_livetypes = character(0),
                      farms = NULL, primary_excel = NULL, progress = NULL) {
  if (is.character(objects) && length(objects) == 1) {
    if (is.null(parameter_database)) stop("parameter_database is needed to build study objects from a batch file.", call. = FALSE)
    objects <- build_study_objects(objects, parameter_database, database_name = database_name,
                                   exclude_livetypes = exclude_livetypes)
  }
  if (is.null(names(objects)) || any(names(objects) == "")) stop("objects must be a named list (one name per farm).", call. = FALSE)
  if (is.null(farms)) farms <- attr(objects, "farms")
  if (is.null(farms)) farms <- data.frame(farm_name = names(objects), stringsAsFactors = FALSE)
  validation <- attr(objects, "validation")

  dir.create(file.path(output_dir, "farms"), recursive = TRUE, showWarnings = FALSE)
  ext <- function(f) jsonlite::fromJSON(system.file("extdata", f, package = "cleaned"), flatten = TRUE)
  params <- list(ghg_ipcc_data = ext("ghg_parameters.json"),
                 energy_parameters = ext("energy_parameters.json"),
                 stock_change_para = ext("stock_change_parameters.json"))
  folder_names <- make.unique(gsub("[^A-Za-z0-9._-]+", "_", names(objects)), sep = "_")

  n <- length(objects)
  log <- data.frame(farm_name = names(objects), folder = folder_names, status = NA_character_,
                    message = NA_character_, warnings = NA_character_, seconds = NA_real_,
                    stringsAsFactors = FALSE)
  result_files <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    if (is.function(progress)) progress(i, n, names(objects)[i])
    farm_dir <- file.path(output_dir, "farms", folder_names[i])
    dir.create(farm_dir, showWarnings = FALSE)
    obj_file <- file.path(farm_dir, "study_object.json")
    jsonlite::write_json(objects[[i]], obj_file, auto_unbox = TRUE, pretty = TRUE,
                         digits = NA, null = "null", na = "null")
    warns <- character(0)
    t0 <- Sys.time()
    res <- tryCatch(
      withCallingHandlers(
        suppressMessages(do.call(run_study_object, c(list(
          para = obj_file, file_path = file.path(farm_dir, folder_names[i]),
          primary_excel = primary_excel), params))),
        warning = function(w) {
          warns <<- c(warns, conditionMessage(w))
          invokeRestart("muffleWarning")
        }),
      error = function(e) e
    )
    log$seconds[i] <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
    if (inherits(res, "error")) {
      log$status[i] <- "failed"
      log$message[i] <- conditionMessage(res)
      next
    }
    result_files[i] <- file.path(farm_dir, paste0(folder_names[i], ".json"))
    write(res$json_output, result_files[i])
    saveRDS(list(
      warning_msg = list(
        warning_check_index = if (length(res$intake_warning)) seq_along(res$intake_warning) else NULL,
        warning_text = if (length(res$intake_warning)) paste0(
          "The following livestock types have a daily dry matter intake greater than 4% of their body weight: ",
          paste(res$intake_warning, collapse = ", ")) else NULL),
      on_farm_table = res$on_farm_table, nitrogen_balance = res$nitrogen_balance,
      land_required = res$land_required, water_use_per_feed_item = res$water_use_per_feed_item
    ), file.path(farm_dir, "scenario_data.rds"))
    log$status[i] <- "ok"
    notes <- c(if (length(res$intake_warning)) paste0(
      "Dry matter intake above 4% of body weight: ", paste(res$intake_warning, collapse = ", ")),
      unique(warns))
    if (length(notes)) log$warnings[i] <- paste(notes, collapse = " | ")
  }

  # Summary --------------------------------------------------------------------
  ok <- !is.na(result_files)
  summary <- data.frame()
  if (any(ok)) {
    summary_dir <- file.path(tempfile("batch_summary"))
    dir.create(summary_dir)
    summary_file <- file.path(summary_dir, "batch_summary.json")
    suppressWarnings(do.call(calculate_differences, c(list(summary_file), as.list(result_files[ok]))))
    summary <- jsonlite::fromJSON(summary_file, flatten = TRUE)
    unlink(summary_dir, recursive = TRUE)
    summary$scenario <- NULL
    summary <- cbind(data.frame(farm_name = names(objects)[ok], stringsAsFactors = FALSE), summary)
    meta <- farms[, setdiff(names(farms), "farm_name"), drop = FALSE]
    if (ncol(meta) > 0) {
      meta <- meta[match(summary$farm_name, farms$farm_name), , drop = FALSE]
      summary <- cbind(summary["farm_name"], meta, summary[setdiff(names(summary), "farm_name")])
    }
    rownames(summary) <- NULL
  }
  indicators <- names(summary)[vapply(summary, is.numeric, TRUE)]
  group_cols <- grep("^group_", names(summary), value = TRUE)
  # (grouping columns left blank for every farm are not compared)
  group_cols <- group_cols[vapply(group_cols, function(g) any(!is.na(summary[[g]]) & summary[[g]] != ""), TRUE)]
  groups <- lapply(stats::setNames(c(group_cols, "all_farms"), c(group_cols, "all_farms")), function(g) {
    if (nrow(summary) == 0) return(data.frame())
    key <- if (g == "all_farms") rep("All farms", nrow(summary)) else ifelse(is.na(summary[[g]]), "(blank)", summary[[g]])
    out <- do.call(rbind, lapply(split(seq_len(nrow(summary)), key), function(rows) {
      x <- summary[rows, indicators, drop = FALSE]
      rbind(
        data.frame(group = key[rows[1]], statistic = "mean", farms = length(rows),
                   as.list(colMeans(x, na.rm = TRUE)), check.names = FALSE),
        data.frame(group = key[rows[1]], statistic = "median", farms = length(rows),
                   as.list(apply(x, 2, stats::median, na.rm = TRUE)), check.names = FALSE)
      )
    }))
    names(out)[1] <- if (g == "all_farms") "group" else g
    rownames(out) <- NULL
    out
  })

  # Files ----------------------------------------------------------------------------
  wb <- openxlsx::createWorkbook()
  header <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9E1F2", wrapText = TRUE)
  add <- function(sheet, x) {
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, x, headerStyle = header)
    openxlsx::freezePane(wb, sheet, firstRow = TRUE)
  }
  add("summary", summary)
  for (g in names(groups)) add(substr(if (g == "all_farms") "all_farms" else paste0("by_", sub("^group_", "", g)), 1, 31), groups[[g]])
  add("log", log)
  if (!is.null(validation)) add("validation", validation)
  openxlsx::saveWorkbook(wb, file.path(output_dir, "batch_results.xlsx"), overwrite = TRUE)
  jsonlite::write_json(summary, file.path(output_dir, "batch_summary.json"), pretty = TRUE, digits = NA)

  if (is.function(progress)) progress(n, n, "Combining the detailed results of all farms")
  # (a problem here must not lose the farm results already saved)
  detailed <- tryCatch(batch_detailed_results(output_dir, log, farms), error = function(e) {
    warning("The combined detailed results could not be written: ", conditionMessage(e), call. = FALSE)
    NULL
  })

  invisible(list(summary = summary, groups = groups, log = log, detailed = detailed,
                 output_dir = output_dir))
}

# Detailed results of all farms in one workbook: every result sheet that
# combineOutputs() writes for a farm (Land Required, Nitrogen Balance, GHG
# Balance, ...), with the farms stacked under each other and the farm's name,
# Ids and group columns in front. Reference sheets (README, benchmarks) are
# copied once, from the first farm.
batch_detailed_results <- function(output_dir, log, farms) {
  ok <- which(log$status == "ok")
  if (!length(ok)) return(invisible(NULL))
  reference <- function(sheet) grepl("^README$|benchmark", sheet, ignore.case = TRUE)
  meta_cols <- intersect(c("farm_name", "Ids", grep("^group_", names(farms), value = TRUE), "country", "latitude", "longitude", "respondent_age"), names(farms))
  tables <- list(); reference_tables <- list()
  for (i in ok) {
    folder <- log$folder[i]
    f <- file.path(output_dir, "farms", folder, paste0(folder, ".xlsx"))
    if (!file.exists(f)) next
    sheets <- openxlsx::getSheetNames(f)
    meta <- farms[match(log$farm_name[i], farms$farm_name), meta_cols, drop = FALSE]
    if (!nrow(meta) || is.na(meta$farm_name[1])) meta <- data.frame(farm_name = log$farm_name[i])
    for (sheet in sheets) {
      if (reference(sheet)) {
        if (is.null(reference_tables[[sheet]])) {
          reference_tables[[sheet]] <- openxlsx::read.xlsx(f, sheet = sheet, colNames = FALSE, skipEmptyRows = FALSE)
        }
        next
      }
      d <- tryCatch(suppressWarnings(openxlsx::read.xlsx(f, sheet = sheet, check.names = FALSE, sep.names = " ")),
                    error = function(e) NULL)
      if (is.null(d) || !nrow(d)) next
      d[] <- lapply(d, function(x) if (is.factor(x)) as.character(x) else x)
      tables[[sheet]][[length(tables[[sheet]]) + 1]] <- cbind(meta[rep(1, nrow(d)), , drop = FALSE], d)
    }
  }
  tables <- lapply(tables, function(x) {
    # a column can be read as text for one farm and as numbers for another
    cols <- unique(unlist(lapply(x, names)))
    mixed <- cols[vapply(cols, function(cl) length(unique(unlist(lapply(x, function(d)
      if (cl %in% names(d)) class(d[[cl]])[1])))) > 1, TRUE)]
    x <- lapply(x, function(d) {
      for (cl in intersect(c(mixed, names(d)[vapply(d, is.logical, TRUE)]), names(d))) d[[cl]] <- as.character(d[[cl]])
      d
    })
    out <- dplyr::bind_rows(x)
    rownames(out) <- NULL
    out
  })

  wb <- openxlsx::createWorkbook()
  header <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9E1F2", wrapText = TRUE)
  intro <- data.frame(`Detailed results of the batch` = c(
    "Each sheet holds one table of the CLEANED results (as in the result workbook of a single farm), for all farms of the batch.",
    "The first columns identify the farm (farm_name, Ids and the group_ columns of the batch file); the rest is the farm's table.",
    "Filter on farm_name to see one farm. The summary indicators per farm are in batch_results.xlsx.",
    "Reference sheets (README, benchmarks) are included once at the end."
  ), check.names = FALSE)
  openxlsx::addWorksheet(wb, "About")
  openxlsx::writeData(wb, "About", intro, headerStyle = header)
  openxlsx::setColWidths(wb, "About", 1, 120)
  for (sheet in names(tables)) {
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, tables[[sheet]], headerStyle = header)
    openxlsx::freezePane(wb, sheet, firstActiveRow = 2, firstActiveCol = length(meta_cols) + 1)
    openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(tables[[sheet]])), widths = 16)
  }
  for (sheet in names(reference_tables)) {
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, reference_tables[[sheet]], colNames = FALSE)
  }
  openxlsx::saveWorkbook(wb, file.path(output_dir, "batch_detailed_results.xlsx"), overwrite = TRUE)
  invisible(names(tables))
}

#' Compare two batch runs farm by farm
#'
#' Matches the farms of two batch summaries (e.g. a baseline and a scenario
#' built from two batch files) and computes, for every indicator, the
#' difference and the percentage change.
#'
#' @param baseline,scenario Summaries from [run_batch()] (`$summary`), or
#'   the paths of their `batch_summary.json` files.
#' @param by Column used to match farms: `"Ids"` when present, otherwise
#'   `"farm_name"`.
#' @return A long data frame: farm key, `indicator`, `baseline`, `scenario`,
#'   `difference` and `percent_change`.
#' @export
compare_batches <- function(baseline, scenario, by = NULL) {
  read <- function(x) if (is.character(x)) jsonlite::fromJSON(x, flatten = TRUE) else x
  a <- read(baseline); b <- read(scenario)
  if (is.null(by)) by <- if ("Ids" %in% names(a) && "Ids" %in% names(b)) "Ids" else "farm_name"
  ind <- intersect(names(a)[vapply(a, is.numeric, TRUE)], names(b)[vapply(b, is.numeric, TRUE)])
  keys <- intersect(as.character(a[[by]]), as.character(b[[by]]))
  if (length(keys) == 0) stop("No farms in common (matched by ", by, ").", call. = FALSE)
  ia <- match(keys, as.character(a[[by]])); ib <- match(keys, as.character(b[[by]]))
  out <- do.call(rbind, lapply(ind, function(k) {
    va <- a[[k]][ia]; vb <- b[[k]][ib]
    data.frame(key = keys, indicator = k, baseline = va, scenario = vb, difference = vb - va,
               percent_change = ifelse(is.finite(va) & va != 0, 100 * (vb - va) / abs(va), NA_real_),
               stringsAsFactors = FALSE)
  }))
  names(out)[1] <- by
  out[order(match(out[[by]], keys)), , drop = FALSE]
}
