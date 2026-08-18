# Batch input: reading and validation -----------------------------------------

batch_data_sheets <- c("farm", "season", "livestock", "feed_basket",
                       "feed_items", "feed_type", "fertilizer")

# Older column names accepted in the batch file, renamed on reading
batch_column_aliases <- list(
  feed_type = c(feed_type_code = "crop_code", feed_type_name = "crop_name")
)

#' Read an iCLEANED batch input file
#'
#' Reads the farm-level sheets of a batch file (see [write_batch_template()])
#' into a list of data frames. Text is trimmed, fully blank rows are dropped,
#' older column names are renamed, and each row keeps its spreadsheet row
#' number in column `.row` so that problems can be reported precisely.
#'
#' @param file Path of the `.xlsx` batch file, or a named list of data frames
#'   (e.g. from an ODK export) with the same sheet and column names.
#' @return A named list of data frames. Sheets that are not part of the
#'   template are listed in attribute `ignored_sheets`.
#' @export
read_batch_input <- function(file) {
  if (is.list(file) && !is.data.frame(file)) {
    raw <- file
  } else {
    sheets <- openxlsx::getSheetNames(file)
    raw <- lapply(stats::setNames(sheets, sheets), function(sh) {
      x <- openxlsx::read.xlsx(file, sheet = sh, colNames = TRUE, check.names = FALSE,
                               sep.names = " ", skipEmptyRows = FALSE,
                               skipEmptyCols = TRUE, detectDates = FALSE)
      if (is.null(x)) data.frame() else x
    })
  }
  # (older templates had information-only copies of the defaults: not used)
  ignored <- setdiff(names(raw), c(batch_data_sheets, batch_project_sheets, "README", "dictionary",
                                   "defaults", "farm_defaults", "livestock_defaults", "season_defaults",
                                   "feed_defaults", "crop_defaults", "livestock_types",
                                   grep("^ref_", names(raw), value = TRUE)))
  out <- lapply(stats::setNames(batch_data_sheets, batch_data_sheets), function(sh) {
    x <- raw[[sh]]
    if (is.null(x) || ncol(x) == 0) return(data.frame(.row = integer(0)))
    x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
    names(x) <- trimws(gsub("^\ufeff", "", names(x)))
    for (old in names(batch_column_aliases[[sh]])) {
      if (old %in% names(x) && !batch_column_aliases[[sh]][[old]] %in% names(x)) {
        names(x)[names(x) == old] <- batch_column_aliases[[sh]][[old]]
      }
    }
    x[] <- lapply(x, function(col) {
      col <- as.character(col)
      col <- trimws(col)
      col[!is.na(col) & col %in% c("", "NA", "N/A", "na", "n/a")] <- NA_character_
      col
    })
    for (cl in intersect(batch_yes_no_fields, names(x))) x[[cl]] <- batch_to_01(x[[cl]])
    x$.row <- if (".row" %in% names(raw[[sh]])) raw[[sh]]$.row else seq_len(nrow(x)) + 1L
    keep <- rowSums(!is.na(x[setdiff(names(x), ".row")])) > 0
    x[keep, , drop = FALSE]
  })
  attr(out, "ignored_sheets") <- ignored
  # project values (_p sheets)
  project <- if (is.list(file) && !is.data.frame(file)) {
    x <- if (!is.null(attr(file, "project"))) attr(file, "project") else raw[intersect(batch_project_sheets, names(raw))]
    if (length(x)) lapply(x, as_chr_df) else NULL
  } else read_batch_project(file)
  attr(out, "project") <- project
  out
}

# Helpers ----------------------------------------------------------------------

batch_num <- function(x) suppressWarnings(as.numeric(x))

# Batch defaults stored in the parameter database (lkp_defaults)
batch_db_defaults <- function(db) {
  d <- db$lkp_defaults
  empty <- list(farm = list(), livestock = list(),
                season = data.frame(season_name = character(0), season_length = numeric(0)))
  if (is.null(d) || nrow(d) == 0) return(empty)
  d$value <- trimws(as.character(d$value))
  d$value[!is.na(d$value) & d$value == ""] <- NA
  pick <- function(section) {
    x <- d[d$section == section & !is.na(d$value), , drop = FALSE]
    stats::setNames(as.list(x$value), x$field)
  }
  s <- d[d$section == "season" & !is.na(d$value), , drop = FALSE]
  list(farm = pick("farm"), livestock = pick("livestock"),
       season = data.frame(season_name = s$field, season_length = batch_num(s$value),
                           stringsAsFactors = FALSE))
}

# Allowed values of a dictionary field, from the parameter database
batch_allowed <- function(spec, db) {
  a <- spec$allowed_values
  if (grepl("^lkp_[a-z_]+\\.[a-z_]+$", a)) {
    tbl <- db[[sub("\\..*", "", a)]]
    col <- sub(".*\\.", "", a)
    if (is.null(tbl) || !col %in% names(tbl)) return(NULL)
    return(unique(trimws(as.character(tbl[[col]]))))
  }
  if (spec$field == "source_type") return(c("Main", "Residue", "Purchased"))
  if (spec$field %in% batch_yes_no_fields) return(c("Yes", "No", "1", "0"))
  if (spec$field %in% names(rice_option_fields)) return(rice_options()[[spec$field]])
  if (spec$field == "category" && !is.null(db$lkp_crops)) return(unique(trimws(as.character(db$lkp_crops$category))))
  if (spec$field == "fertilizer_desc") {
    return(c("Urea", "NPK", "DAP", "Ammonium nitrate", "Ammonium sulfate", "N solutions", "Ammonia"))
  }
  NULL
}

# Numeric range from the dictionary text (">= 0", "0-1", "0-100", "> 0", ...)
batch_range_ok <- function(x, allowed) {
  ok <- rep(TRUE, length(x))
  if (grepl("^0-1\\b", allowed)) ok <- x >= 0 & x <= 1
  else if (grepl("^0-100\\b", allowed)) ok <- x >= 0 & x <= 100
  else if (grepl("^0-12\\b", allowed)) ok <- x >= 0 & x <= 12
  else if (grepl("^> 0", allowed)) ok <- x > 0
  else if (grepl("^>= 0", allowed)) ok <- x >= 0
  ok | is.na(x)
}

#' Validate an iCLEANED batch input file
#'
#' Checks a batch file against the template dictionary ([batch_dictionary()])
#' and the selected parameter database, before any study object is built.
#' Values left blank in the file are checked using the defaults of the
#' parameter database, so the report shows what would actually be used.
#'
#' Errors must be fixed before study objects can be built; warnings point to
#' values that will be corrected or replaced automatically, or that look
#' unusual.
#'
#' @param file Path of the batch `.xlsx` file, or the result of
#'   [read_batch_input()].
#' @param parameter_database A list from [read_parameter_database()], or the
#'   path of a parameter database folder.
#' @return A data frame with one row per problem: `severity` ("error" or
#'   "warning"), `sheet`, `row` (spreadsheet row), `Ids`, `field`, `value`
#'   and `message`. Attribute `summary` gives counts.
#' @export
validate_batch_input <- function(file, parameter_database) {
  db <- if (is.character(parameter_database)) read_parameter_database(parameter_database) else parameter_database
  input <- if (is.list(file) && all(batch_data_sheets %in% names(file))) file else read_batch_input(file)
  # project values (_p sheets) fill the blanks, before the database's batch defaults
  applied <- batch_apply_project(input, db)
  input <- applied$input; db <- applied$db
  file_ignored <- attr(input, "project_problems")
  dict <- batch_dictionary()
  defaults <- batch_db_defaults(db)
  issues <- list()
  add <- function(severity, sheet, row, ids, field, value, message) {
    n <- max(length(row), length(ids), length(value), length(message), length(field), 1)
    issues[[length(issues) + 1]] <<- data.frame(
      severity = rep(severity, n), sheet = rep(sheet, n),
      row = rep_len(if (length(row)) row else NA_integer_, n),
      Ids = rep_len(if (length(ids)) as.character(ids) else NA_character_, n),
      field = rep_len(if (length(field)) as.character(field) else NA_character_, n), value = rep_len(if (length(value)) as.character(value) else NA_character_, n),
      message = rep_len(message, n), stringsAsFactors = FALSE
    )
  }
  spec_of <- function(sheet, field) {
    s <- dict[dict$sheet == sheet & dict$field == field, , drop = FALSE]
    if (nrow(s) == 0) NULL else s[1, ]
  }

  for (sh in attr(input, "ignored_sheets")) {
    add("warning", sh, NA, NA, NA, NA,
        if (grepl("_p$", sh)) "Batch-default sheets are not used: defaults come from the parameter database (lkp_defaults). This sheet is ignored."
        else "This sheet is not part of the batch template and is ignored.")
  }
  if (!is.null(file_ignored) && nrow(file_ignored)) {
    for (i in seq_len(nrow(file_ignored))) {
      add(file_ignored$severity[i], file_ignored$sheet[i], file_ignored$row[i], NA, NA, file_ignored$key[i],
          paste0(toupper(substr(file_ignored$message[i], 1, 1)), substring(file_ignored$message[i], 2), "."))
    }
  }

  # Generic column checks: unknown columns, numbers, allowed values ---------
  check_columns <- function(sheet, x, skip = character(0)) {
    if (nrow(x) == 0) return(invisible())
    known <- dict$field[dict$sheet == sheet]
    for (col in setdiff(names(x), c(".row", skip))) {
      if (sheet == "farm" && grepl("^group_", col)) next
      spec <- spec_of(sheet, col)
      if (is.null(spec)) {
        add("warning", sheet, NA, NA, col, NA, "Unknown column; it is ignored.")
        next
      }
      v <- x[[col]]
      present <- !is.na(v)
      if (spec$type == "number") {
        num <- batch_num(v)
        bad <- present & is.na(num)
        if (any(bad)) add("error", sheet, x$.row[bad], x$Ids[bad], col, v[bad], "Not a number.")
        out <- present & !bad & !batch_range_ok(num, spec$allowed_values)
        if (any(out)) add("error", sheet, x$.row[out], x$Ids[out], col, v[out],
                          paste0("Out of range (allowed: ", spec$allowed_values, ")."))
      } else if (spec$type == "choice") {
        allowed <- batch_allowed(spec, db)
        if (!is.null(allowed)) {
          fixed <- if (grepl("^manureman_", col)) {
            normalise_manure_systems(stats::setNames(data.frame(v, stringsAsFactors = FALSE), "manureman_x"))$manureman_x
          } else v
          bad <- present & !fixed %in% allowed
          if (any(bad)) add("error", sheet, x$.row[bad], x$Ids[bad], col, v[bad],
                            paste0("Not an allowed value (see ", spec$allowed_values, ")."))
          relabel <- present & fixed != v & fixed %in% allowed
          if (any(relabel)) add("warning", sheet, x$.row[relabel], x$Ids[relabel], col, v[relabel],
                                paste0("Will be replaced by '", fixed[relabel], "'."))
        }
      }
    }
  }

  farm <- input$farm
  # Farms --------------------------------------------------------------------
  if (nrow(farm) == 0 || !"Ids" %in% names(farm)) {
    add("error", "farm", NA, NA, "Ids", NA, "The farm sheet must have one row per farm, with an Ids column.")
    farm <- data.frame(Ids = character(0), .row = integer(0))
  } else {
    blank <- is.na(farm$Ids)
    if (any(blank)) add("error", "farm", farm$.row[blank], NA, "Ids", NA, "Missing farm identifier.")
    dup <- duplicated(farm$Ids) & !blank
    if (any(dup)) add("error", "farm", farm$.row[dup], farm$Ids[dup], "Ids", farm$Ids[dup], "Duplicate farm identifier.")
  }
  check_columns("farm", farm)
  farm_ids <- unique(farm$Ids[!is.na(farm$Ids)])

  # Effective farm values: file > database defaults
  farm_specs <- dict[dict$sheet == "farm" & dict$required == "yes" & !dict$field %in% c("Ids"), , drop = FALSE]
  for (i in seq_len(nrow(farm_specs))) {
    f <- farm_specs$field[i]
    from_file <- if (f %in% names(farm)) farm[[f]] else rep(NA_character_, nrow(farm))
    missing <- is.na(from_file) & is.null(defaults$farm[[f]])
    if (any(missing)) add("error", "farm", farm$.row[missing], farm$Ids[missing], f, NA,
                          "No value in the file and no default in the parameter database (lkp_defaults).")
  }
  for (f in names(defaults$farm)) {
    spec <- spec_of("farm", f)
    if (is.null(spec) || spec$type != "choice") next
    allowed <- batch_allowed(spec, db)
    if (!is.null(allowed) && !defaults$farm[[f]] %in% allowed) {
      add("error", "defaults", NA, NA, f, defaults$farm[[f]],
          paste0("Default in lkp_defaults is not an allowed value (see ", spec$allowed_values, ")."))
    }
  }
  # IPCC factor given in the file but different from the database
  for (f in c("cropland_system", "cropland_tillage", "cropland_orgmatter", "grassland_management", "grassland_implevel")) {
    fct <- paste0(f, "_ipcc")
    if (!fct %in% names(farm)) next
    spec <- spec_of("farm", f)
    tbl <- db[[sub("\\..*", "", spec$allowed_values)]]
    desc <- if (f %in% names(farm)) ifelse(is.na(farm[[f]]), defaults$farm[[f]] %||% NA, farm[[f]]) else rep(defaults$farm[[f]] %||% NA, nrow(farm))
    db_factor <- tbl$change_factor[match(desc, trimws(tbl[[sub(".*\\.", "", spec$allowed_values)]]))]
    given <- batch_num(farm[[fct]])
    diff <- !is.na(given) & !is.na(db_factor) & abs(given - db_factor) > 1e-6
    if (any(diff)) add("warning", "farm", farm$.row[diff], farm$Ids[diff], fct, farm[[fct]][diff],
                       paste0("Differs from the database factor for '", desc[diff], "' (", db_factor[diff], "); the file value is used."))
  }

  # Other sheets refer to existing farms --------------------------------------
  for (sh in setdiff(batch_data_sheets, "farm")) {
    x <- input[[sh]]
    if (nrow(x) == 0) next
    if (!"Ids" %in% names(x)) {
      add("error", sh, NA, NA, "Ids", NA, "Missing Ids column.")
      next
    }
    unknown <- !is.na(x$Ids) & !x$Ids %in% farm_ids
    if (any(unknown)) add("error", sh, x$.row[unknown], x$Ids[unknown], "Ids", x$Ids[unknown], "Farm not found in the farm sheet.")
    blank <- is.na(x$Ids)
    if (any(blank)) add("error", sh, x$.row[blank], NA, "Ids", NA, "Missing farm identifier.")
  }

  # Seasons --------------------------------------------------------------------
  season <- input$season
  check_columns("season", season)
  farm_seasons <- function(id) {
    s <- season[!is.na(season$Ids) & season$Ids == id, , drop = FALSE]
    if (nrow(s) > 0) s$season_name else defaults$season$season_name
  }
  if (nrow(season) > 0) {
    for (id in intersect(unique(season$Ids), farm_ids)) {
      s <- season[season$Ids == id, , drop = FALSE]
      total <- sum(batch_num(s$season_length), na.rm = TRUE)
      if (abs(total - 365) > 1e-6) add("error", "season", s$.row, id, "season_length", total,
                                       paste0("The farm's seasons add up to ", total, " days, not 365."))
      dup <- duplicated(s$season_name)
      if (any(dup)) add("error", "season", s$.row[dup], id, "season_name", s$season_name[dup], "Duplicate season name.")
    }
  }
  if (nrow(defaults$season) == 0 && length(setdiff(farm_ids, season$Ids)) > 0) {
    add("error", "season", NA, NA, "season_name", NA,
        "Some farms have no seasons and the parameter database has no default seasons (lkp_defaults, section season).")
  } else if (nrow(defaults$season) > 0 && abs(sum(defaults$season$season_length) - 365) > 1e-6) {
    add("error", "defaults", NA, NA, "season", sum(defaults$season$season_length), "Default seasons in lkp_defaults do not add up to 365 days.")
  }

  # Livestock ------------------------------------------------------------------
  lv <- input$livestock
  check_columns("livestock", lv)
  lt <- db$lkp_livetype
  if (nrow(lv) > 0) {
    no_lv <- setdiff(farm_ids, lv$Ids)
    if (length(no_lv)) add("error", "livestock", NA, no_lv, "Ids", NA, "Farm has no livestock rows.")
    code <- lv$livetype_code
    unknown <- !is.na(code) & !code %in% as.character(lt$livetype_code)
    if (any(unknown)) add("error", "livestock", lv$.row[unknown], lv$Ids[unknown], "livetype_code", code[unknown],
                          "Livestock type not found in the livestock_p sheet or the parameter database.")
    if (any(is.na(code))) add("error", "livestock", lv$.row[is.na(code)], lv$Ids[is.na(code)], "livetype_code", NA, "Missing livestock type.")
    dup <- duplicated(paste(lv$Ids, code)) & !is.na(code)
    if (any(dup)) add("error", "livestock", lv$.row[dup], lv$Ids[dup], "livetype_code", code[dup],
                      "The same livestock type appears twice for this farm; use separate livestock types (e.g. dry and lactating cows).")
    known <- !is.na(code) & !unknown
    if (any(known)) {
      # Livestock types used must have complete species, class and IPCC categories
      used <- lt[as.character(lt$livetype_code) %in% unique(code[known]), , drop = FALSE]
      for (k in seq_len(nrow(used))) {
        msg <- tryCatch({ resolve_livestock_categories(used[k, , drop = FALSE]); NULL },
                        error = function(e) conditionMessage(e))
        if (!is.null(msg)) add("error", "livestock", lv$.row[known & code == used$livetype_code[k]], NA,
                               "livetype_code", used$livetype_code[k], paste("Parameter database:", msg))
      }
      db_desc <- trimws(lt$livetype_desc[match(code, as.character(lt$livetype_code))])
      differ <- known & !is.na(lv$livetype_desc %||% NA) & lv$livetype_desc != db_desc
      if ("livetype_desc" %in% names(lv) && any(differ)) {
        add("warning", "livestock", lv$.row[differ], lv$Ids[differ], "livetype_desc", lv$livetype_desc[differ],
            paste0("Name differs from the parameter database ('", db_desc[differ], "'); the database name is used."))
      }
    }
    # (non-numeric and zero values are reported by the column checks above)
    if (!"number" %in% names(lv)) {
      add("error", "livestock", NA, NA, "number", NA, "Missing column: number of animals.")
    } else if (any(is.na(lv$number))) {
      add("error", "livestock", lv$.row[is.na(lv$number)], lv$Ids[is.na(lv$number)], "number", NA, "Missing number of animals.")
    }

    # Effective time budget and manure systems (file > database defaults > 0)
    eff <- function(f) {
      v <- if (f %in% names(lv)) lv[[f]] else rep(NA_character_, nrow(lv))
      d <- defaults$livestock[[f]]
      ifelse(is.na(v), if (is.null(d)) NA_character_ else d, v)
    }
    locs <- c("stable", "non_roofed_enclosure", "onfarm_grazing", "offfarm_grazing")
    times <- sapply(locs, function(l) { t <- batch_num(eff(paste0("time_in_", l))); ifelse(is.na(t), 0, t) })
    if (!is.matrix(times)) times <- matrix(times, nrow = nrow(lv), dimnames = list(NULL, locs))
    tot <- rowSums(times)
    bad <- abs(tot - 1) > 0.01
    if (any(bad)) add("error", "livestock", lv$.row[bad], lv$Ids[bad], "time_in_*", round(tot[bad], 3),
                      "Time in stable, enclosure, on-farm and off-farm grazing must add up to 1.")
    allowed_mm <- trimws(db$lkp_manureman$manureman_desc)
    for (l in locs) {
      mm <- normalise_manure_systems(data.frame(manureman_x = eff(paste0("manureman_", l)), stringsAsFactors = FALSE))$manureman_x
      need <- times[, l] > 0 & (is.na(mm) | !mm %in% allowed_mm)
      if (any(need)) add("error", "livestock", lv$.row[need], lv$Ids[need], paste0("manureman_", l), eff(paste0("manureman_", l))[need],
                         paste0("A valid manure system is needed because time_in_", l, " > 0."))
      # second system for a share of the dung of the place
      share_1 <- batch_num(eff(paste0("manure_share_1_", l))); share_2 <- batch_num(eff(paste0("manure_share_2_", l)))
      both <- !is.na(share_1) & !is.na(share_2) & abs(share_1 + share_2 - 1) > 0.01
      if (any(both)) add("error", "livestock", lv$.row[both], lv$Ids[both], paste0("manure_share_1_", l),
                         paste(share_1[both], "+", share_2[both]),
                         paste0("The fractions under option 1 and option 2 must add up to 1 (manure_share_1_", l, " + manure_share_2_", l, ")."))
      share <- batch_manure_share_2(share_1, share_2)
      mm2 <- normalise_manure_systems(data.frame(manureman_x = eff(paste0("manureman_2_", l)), stringsAsFactors = FALSE))$manureman_x
      bad2 <- (share > 0) %in% TRUE & (is.na(mm2) | !mm2 %in% allowed_mm)
      if (any(bad2)) add("error", "livestock", lv$.row[bad2], lv$Ids[bad2], paste0("manureman_2_", l), eff(paste0("manureman_2_", l))[bad2],
                         paste0("A valid manure system for option 2 is needed: part of the dung (manure_share_2_", l, " > 0, or manure_share_1_", l, " < 1) is under option 2."))
      orphan <- !(share > 0) %in% TRUE & !is.na(mm2) & mm2 != ""
      if (any(orphan)) add("warning", "livestock", lv$.row[orphan], lv$Ids[orphan], paste0("manure_share_2_", l), eff(paste0("manure_share_2_", l))[orphan],
                           paste0("A second manure system is given but its share is 0 or blank: it is not used."))
    }
  }

  # Feed basket ------------------------------------------------------------------
  fb <- input$feed_basket
  feed_cols <- grep("^[0-9]+$", names(fb), value = TRUE)
  check_columns("feed_basket", fb, skip = feed_cols)
  fi_db <- db$lkp_feeditem
  if (nrow(fb) == 0) {
    add("error", "feed_basket", NA, NA, NA, NA, "The feed_basket sheet is empty.")
  } else {
    unknown_feed <- setdiff(feed_cols, as.character(fi_db$feed_item_code))
    if (length(unknown_feed)) add("error", "feed_basket", 1, NA, unknown_feed, NA,
                                  "Feed item code not found in the parameter database (lkp_feeditem).")
    alloc <- sapply(feed_cols, function(f) batch_num(fb[[f]]))
    if (!is.matrix(alloc)) alloc <- matrix(alloc, nrow = nrow(fb), dimnames = list(NULL, feed_cols))
    for (f in feed_cols) {
      bad <- !is.na(fb[[f]]) & (is.na(alloc[, f]) | alloc[, f] < 0)
      if (any(bad)) add("error", "feed_basket", fb$.row[bad], fb$Ids[bad], f, fb[[f]][bad], "Allocation must be a number >= 0.")
    }
    tot <- rowSums(alloc, na.rm = TRUE)
    bad <- abs(tot - 100) > 0.01
    if (any(bad)) add("error", "feed_basket", fb$.row[bad], fb$Ids[bad], "allocation", round(tot[bad], 2),
                      "Feed allocations of a livestock type in a season must add up to 100.")
    for (id in intersect(unique(fb$Ids), farm_ids)) {
      rows <- fb$Ids == id
      ok_season <- fb$season_name[rows] %in% farm_seasons(id)
      if (any(!ok_season)) add("error", "feed_basket", fb$.row[rows][!ok_season], id, "season_name", fb$season_name[rows][!ok_season],
                               "Season not defined for this farm (season sheet or lkp_defaults).")
    }
    key_fb <- paste(fb$Ids, fb$livetype_code)
    if (nrow(lv) > 0) {
      key_lv <- paste(lv$Ids, lv$livetype_code)
      orphan <- !key_fb %in% key_lv
      if (any(orphan)) add("error", "feed_basket", fb$.row[orphan], fb$Ids[orphan], "livetype_code", fb$livetype_code[orphan],
                           "No matching livestock row for this farm and livestock type.")
      for (k in which(!duplicated(key_lv))) {
        id <- lv$Ids[k]
        have <- fb$season_name[key_fb == key_lv[k]]
        missing <- setdiff(farm_seasons(id), have)
        if (length(missing)) add("error", "livestock", lv$.row[k], id, "livetype_code", lv$livetype_code[k],
                                 paste0("No feed basket row for season(s): ", paste(missing, collapse = ", "), "."))
      }
    }

    # Feeds used must have their field settings (file > database defaults)
    used_feeds <- feed_cols[colSums(alloc > 0, na.rm = TRUE) > 0]
    fd <- db$lkp_feeditem_defaults
    fi_file <- input$feed_items
    for (f in intersect(used_feeds, as.character(fi_db$feed_item_code))) {
      name <- fi_db$feed_item_name[as.character(fi_db$feed_item_code) == f][1]
      # Bought feeds use no land of the farm: their field settings are not needed
      farms_using <- unique(fb$Ids[!is.na(alloc[, f]) & alloc[, f] > 0])
      bought <- if (nrow(fi_file) > 0 && all(c("source_type", "feed_item_code") %in% names(fi_file))) {
        unique(fi_file$Ids[fi_file$feed_item_code == f & fi_file$source_type %in% "Purchased"])
      } else character(0)
      if (length(farms_using) && all(farms_using %in% bought)) next
      for (fld in c("land_cover_desc", "slope_desc", "source_type")) {
        in_db <- !is.null(fd) && fld %in% names(fd) &&
          any(!is.na(fd[[fld]][as.character(fd$feed_item_code) == f]) & fd[[fld]][as.character(fd$feed_item_code) == f] != "")
        in_file <- nrow(fi_file) > 0 && fld %in% names(fi_file) &&
          any(!is.na(fi_file[[fld]][fi_file$feed_item_code == f]))
        if (!in_db && !in_file) {
          if (fld == "source_type") {
            add("warning", "feed_basket", 1, NA, f, name, "source_type not set (lkp_feeditem_defaults or feed_items); 'Main' is assumed.")
          } else {
            add("error", "feed_basket", 1, NA, f, name,
                paste0("No ", fld, " for this feed in lkp_feeditem_defaults or the feed_items sheet."))
          }
        }
      }
    }
  }

  # Farm-specific feed, crop and fertiliser values ------------------------------
  check_columns("feed_items", input$feed_items)
  if (nrow(input$feed_items) > 0 && "feed_item_code" %in% names(input$feed_items)) {
    x <- input$feed_items
    bad <- !x$feed_item_code %in% as.character(fi_db$feed_item_code)
    if (any(bad)) add("error", "feed_items", x$.row[bad], x$Ids[bad], "feed_item_code", x$feed_item_code[bad],
                      "Feed item code not found in the parameter database (lkp_feeditem).")
  }
  check_columns("feed_type", input$feed_type)
  if (nrow(input$feed_type) > 0 && "crop_code" %in% names(input$feed_type)) {
    x <- input$feed_type
    bad <- !x$crop_code %in% as.character(db$lkp_crops$crop_code)
    if (any(bad)) add("error", "feed_type", x$.row[bad], x$Ids[bad], "crop_code", x$crop_code[bad],
                      "Crop code not found in the parameter database (lkp_crops).")
  }
  check_columns("fertilizer", input$fertilizer)

  # Fertiliser applied to crops (feed_items or feed defaults) needs its row in the
  # fertilizer sheet, which gives its N content; NPK has no standard N content.
  # Without them the model counts 0 N: flagged.
  fert_cols <- c(urea = "Urea", npk = "NPK", dap = "DAP", ammonium_nitrate = "Ammonium nitrate",
                 ammonium_sulfate = "Ammonium sulfate", n_solutions = "N solutions", ammonia = "Ammonia")
  if (nrow(fb) > 0 && length(feed_cols)) {
    fi_in <- input$feed_items; fe_in <- input$fertilizer; fdef <- db$lkp_feeditem_defaults
    for (id in unique(fb$Ids[!is.na(fb$Ids)])) {
      rows <- which(fb$Ids == id)
      feeds <- feed_cols[colSums(alloc[rows, , drop = FALSE] > 0, na.rm = TRUE) > 0]
      fe_farm <- if (nrow(fe_in) && "fertilizer_desc" %in% names(fe_in)) fe_in[fe_in$Ids %in% id, , drop = FALSE] else fe_in[0, , drop = FALSE]
      for (col in names(fert_cols)) {
        amount <- vapply(feeds, function(f) {
          v <- if (nrow(fi_in) && col %in% names(fi_in)) fi_in[[col]][fi_in$Ids %in% id & fi_in$feed_item_code %in% f] else NULL
          v <- batch_num(v[!batch_blank(v)])
          if (!length(v) && !is.null(fdef) && col %in% names(fdef)) v <- batch_num(fdef[[col]][as.character(fdef$feed_item_code) == f])
          if (length(v) && !is.na(v[1])) v[1] else 0
        }, 0)
        if (!any(amount > 0)) next
        used_on <- paste(feeds[amount > 0], collapse = ", ")
        r <- fe_farm[fe_farm$fertilizer_desc %in% fert_cols[[col]], , drop = FALSE]
        if (!nrow(r)) {
          add("warning", "fertilizer", NA, id, "fertilizer_desc", fert_cols[[col]],
              sprintf("%s is applied (feeds %s) but the farm has no %s row in the fertilizer sheet: its nitrogen is counted as 0%s.",
                      fert_cols[[col]], used_on, fert_cols[[col]],
                      if (!nrow(fe_farm)) ", and with no fertiliser rows at all, the emissions of producing fertiliser too" else ""))
        } else if (col == "npk" && (!"percentage_n" %in% names(r) || all(is.na(batch_num(r$percentage_n))))) {
          add("warning", "fertilizer", r$.row[1], id, "percentage_n", NA,
              sprintf("NPK is applied (feeds %s) but has no N content (percentage_n): its nitrogen is counted as 0. NPK blends differ (e.g. 17-17-17 has 17%% N).", used_on))
        }
      }
    }
  }

  report <- if (length(issues)) do.call(rbind, issues) else
    data.frame(severity = character(0), sheet = character(0), row = integer(0), Ids = character(0),
               field = character(0), value = character(0), message = character(0))
  report <- report[order(match(report$severity, c("error", "warning")), report$sheet, report$row), , drop = FALSE]
  rownames(report) <- NULL
  attr(report, "summary") <- c(farms = length(farm_ids), errors = sum(report$severity == "error"),
                               warnings = sum(report$severity == "warning"))
  report
}

`%||%` <- function(a, b) if (is.null(a)) b else a
