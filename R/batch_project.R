# Project values: the `_p` sheets of a batch file ----------------------------------
#
# Each data sheet of the batch file has a twin with the same columns whose name
# ends in `_p` ("project"): a blank cell in a farm's row takes the value of the
# same column in the `_p` sheet, for the same livestock type, feed, crop or
# fertiliser (farm_p: for all farms, or the farms of a group). Values left blank
# in both come from the parameter database ("Batch defaults").
#
#   farm       -> farm_p        one row for all farms, or one per group (group_* columns)
#   season     -> season_p      the default seasons
#   livestock  -> livestock_p   one row per livestock type: its species, class, IPCC
#                               categories and parameters, and the practices of the
#                               farms (time in each place, manure systems, ...).
#                               New types can be added with a new livetype_code.
#   feed_items -> feed_items_p  one row per feed of the database: its parameters (dry matter,
#                               energy, protein, crop) and how it is grown (land cover, slope, fertiliser, ...)
#   feed_type  -> feed_type_p   one row per crop of the database: its parameters (yields, N,
#                               crop coefficients, trees) and how it is managed (residues, rice, ...)
#   fertilizer -> fertilizer_p  one row per fertiliser: its N content
#
# The `_p` sheets are filled in by the user in Excel, or in the app's "What's
# missing" step; for a survey they start from the database's batch defaults.

batch_project_sheets <- c("farm_p", "season_p", "livestock_p", "feed_items_p", "feed_type_p", "fertilizer_p")

batch_blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))

# Yes/No in the template and the app, 1/0 in the model (intercrop)
batch_yes_no_fields <- "intercrop"
batch_to_01 <- function(x) {
  v <- tolower(trimws(as.character(x)))
  ifelse(v %in% c("yes", "y", "true"), "1", ifelse(v %in% c("no", "n", "false"), "0", as.character(x)))
}
batch_to_yes_no <- function(x) {
  v <- trimws(as.character(x))
  ifelse(v %in% c("1", "1.0"), "Yes", ifelse(v %in% c("0", "0.0"), "No", as.character(x)))
}

batch_same_value <- function(a, b) {
  a <- trimws(as.character(a)); b <- trimws(as.character(b))
  a[is.na(a)] <- ""; b[is.na(b)] <- ""
  na <- suppressWarnings(as.numeric(a)); nb <- suppressWarnings(as.numeric(b))
  ifelse(!is.na(na) & !is.na(nb), abs(na - nb) < 1e-9, a == b)
}

# Fields that take a project value, from the dictionary
batch_project_fields <- function() {
  d <- batch_dictionary()
  has <- function(p) grepl(p, d$precedence, fixed = TRUE)
  derived <- grepl("parameter database \\(lkp_[a-z]+[.]", d$precedence) | grepl("factor$", d$field)
  list(
    farm = setdiff(d$field[d$sheet == "farm" & has("lkp_defaults (section farm)") & !derived], "country"),
    livestock = unique(c(d$field[d$sheet == "livestock" & has("lkp_defaults (section livestock)")],
                         grep("^manureman_2_|^manure_share_2_", d$field[d$sheet == "livestock"], value = TRUE))),
    feed = d$field[d$sheet == "feed_items" & has("lkp_feeditem_defaults") & !derived],
    crop = d$field[d$sheet == "feed_type" & has("lkp_crops_defaults") & !derived],
    # the feed's and the crop's own parameters (from lkp_feeditem and lkp_crops)
    feed_db = d$field[d$sheet == "feed_items" & has("parameter database (lkp_feeditem, by feed_item_code)")],
    crop_db = d$field[d$sheet == "feed_type" & has("parameter database (lkp_crops, by crop_code)")]
  )
}

# Built-in value of a field when neither the farm, the project nor the database
# gives one ("default (0)" in the dictionary), or NA
batch_builtin_default <- function(field) {
  d <- batch_dictionary()
  p <- d$precedence[d$field == field][1]
  if (is.na(p)) return(NA_character_)
  m <- regmatches(p, regexpr("default \\(([^)]*)\\)$|> 0$", p))
  if (!length(m)) return(NA_character_)
  if (m == "> 0") "0" else sub("^default \\((.*)\\)$", "\\1", m)
}

as_chr_df <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  x[] <- lapply(x, function(v) { v <- trimws(as.character(v)); v[!is.na(v) & v %in% c("", "NA")] <- NA; v })
  x
}

#' Project values from the batch defaults of a parameter database
#'
#' The `_p` sheets as the database's batch defaults fill them: what the
#' template and a survey start from.
#'
#' @param parameter_database A list from [read_parameter_database()], or the
#'   path of a parameter database folder.
#' @return A named list of data frames (`farm_p`, `season_p`, `livestock_p`,
#'   `feed_items_p`, `feed_type_p`, `fertilizer_p`).
#' @export
batch_default_project <- function(parameter_database) {
  db <- if (is.character(parameter_database)) read_parameter_database(parameter_database) else parameter_database
  f <- batch_project_fields()
  d <- db$lkp_defaults
  d <- if (is.null(d)) data.frame(section = character(0), field = character(0), value = character(0)) else as_chr_df(d)
  val <- function(section, field) { v <- d$value[d$section %in% section & d$field %in% field]; if (length(v)) v[1] else NA_character_ }
  farm_p <- as.data.frame(stats::setNames(lapply(f$farm, function(x) val("farm", x)), f$farm),
                          stringsAsFactors = FALSE, check.names = FALSE)
  s <- d[d$section %in% "season", , drop = FALSE]
  season_p <- data.frame(season_name = s$field, season_length = s$value, stringsAsFactors = FALSE)
  lt <- if (is.null(db$lkp_livetype)) data.frame(livetype_code = character(0)) else as_chr_df(db$lkp_livetype)
  mgmt <- as.data.frame(stats::setNames(lapply(f$livestock, function(x) rep(val("livestock", x), nrow(lt))), f$livestock),
                        stringsAsFactors = FALSE, check.names = FALSE)
  livestock_p <- cbind(lt, mgmt[, setdiff(names(mgmt), names(lt)), drop = FALSE])
  # (types no longer offered are not listed; files that use them are still read)
  if (nrow(livestock_p) && "livetype_desc" %in% names(livestock_p)) livestock_p <- livestock_p[!livestock_type_retired(livestock_p), , drop = FALSE]
  # every feed and crop of the database: its own parameters (lkp_feeditem,
  # lkp_crops), then how it is grown or managed (batch defaults, blank where the
  # database has none: the farms' answers or the built-in values are used)
  every_item <- function(defaults, key, items, name_col, db_cols, cols) {
    tbl <- if (is.null(defaults)) data.frame(stats::setNames(list(character(0)), key), check.names = FALSE) else as_chr_df(defaults)
    items <- if (is.null(items)) data.frame(stats::setNames(list(character(0)), key), check.names = FALSE) else as_chr_df(items)
    codes <- unique(c(as.character(items[[key]]), as.character(tbl[[key]])))
    codes <- codes[!is.na(codes)]
    out <- data.frame(stats::setNames(list(codes), key), stringsAsFactors = FALSE, check.names = FALSE)
    i <- match(codes, as.character(items[[key]]))
    for (cl in unique(c(name_col, db_cols))) out[[cl]] <- if (cl %in% names(items)) items[[cl]][i] else rep(NA_character_, length(codes))
    k <- match(codes, as.character(tbl[[key]]))
    for (cl in cols) out[[cl]] <- if (cl %in% names(tbl)) tbl[[cl]][k] else rep(NA_character_, length(codes))
    out
  }
  feed_items_p <- every_item(db$lkp_feeditem_defaults, "feed_item_code", db$lkp_feeditem, "feed_item_name", f$feed_db, f$feed)
  for (cl in intersect(batch_yes_no_fields, names(feed_items_p))) feed_items_p[[cl]] <- batch_to_yes_no(feed_items_p[[cl]])
  feed_type_p <- every_item(db$lkp_crops_defaults, "crop_code", db$lkp_crops, "crop_name", f$crop_db, f$crop)
  fert <- if (!is.null(db$lkp_orgfertilizer)) as.character(db$lkp_orgfertilizer$fertilizer_desc) else names(batch_fertilizer_n)
  fert <- unique(c(fert, "NPK"))
  fertilizer_p <- data.frame(fertilizer_desc = fert, percentage_n = unname(as.character(batch_fertilizer_n[fert])),
                             stringsAsFactors = FALSE)
  list(farm_p = farm_p, season_p = season_p, livestock_p = livestock_p, feed_items_p = feed_items_p,
       feed_type_p = feed_type_p, fertilizer_p = fertilizer_p)
}

#' Read the project values (`_p` sheets) of a batch file
#'
#' @param file Path of a batch `.xlsx` file.
#' @return A named list of data frames for the `_p` sheets the file has (with
#'   a `.row` column), or `NULL` when it has none.
#' @export
read_batch_project <- function(file) {
  sheets <- intersect(batch_project_sheets, openxlsx::getSheetNames(file))
  if (!length(sheets)) return(NULL)
  lapply(stats::setNames(sheets, sheets), function(sh) {
    x <- openxlsx::read.xlsx(file, sheet = sh, colNames = TRUE, check.names = FALSE, sep.names = " ",
                             skipEmptyRows = FALSE, detectDates = FALSE)
    if (is.null(x) || !ncol(x)) return(data.frame(.row = integer(0)))
    x <- as_chr_df(x)
    x$.row <- seq_len(nrow(x)) + 1L
    x[rowSums(!is.na(x[setdiff(names(x), ".row")])) > 0, , drop = FALSE]
  })
}

#' Write project values (`_p` sheets) into a batch file
#'
#' Replaces the `_p` sheets given (other sheets are kept).
#'
#' @param file Path of a batch `.xlsx` file (changed in place).
#' @param project A named list of data frames, as from [read_batch_project()].
#' @return `file`, invisibly.
#' @export
write_batch_project <- function(file, project) {
  wb <- openxlsx::loadWorkbook(file)
  style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#E2EFDA", border = "bottom", wrapText = TRUE)
  for (sh in intersect(names(project), batch_project_sheets)) {
    x <- project[[sh]]
    x <- x[, setdiff(names(x), ".row"), drop = FALSE]
    if (sh %in% names(wb)) {
      old <- openxlsx::read.xlsx(wb, sh, colNames = FALSE, skipEmptyRows = FALSE)
      if (!is.null(old)) openxlsx::deleteData(wb, sh, cols = seq_len(max(ncol(old), ncol(x))),
                                              rows = seq_len(max(nrow(old), nrow(x)) + 1), gridExpand = TRUE)
    } else {
      openxlsx::addWorksheet(wb, sh)
    }
    openxlsx::writeData(wb, sh, x, headerStyle = style)
  }
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}

# Apply the project values to the input and to the database (in memory).
# Returns list(input, db); the problems found are in attr(input, "project_problems").
batch_apply_project <- function(input, db) {
  if (isTRUE(attr(input, "project_applied"))) return(list(input = input, db = db))
  project <- attr(input, "project")
  problems <- list()
  problem <- function(severity, sheet, row, key, message) problems[[length(problems) + 1]] <<-
    data.frame(severity = severity, sheet = sheet, row = row, key = as.character(key), message = message, stringsAsFactors = FALSE)
  f <- batch_project_fields()
  rows_of <- function(x) if (".row" %in% names(x)) x$.row else seq_len(nrow(x)) + 1L
  fill_blank <- function(target, where, field, value) {
    if (batch_blank(value)) return(target)
    if (!field %in% names(target)) target[[field]] <- rep(NA_character_, nrow(target))
    hit <- where & batch_blank(target[[field]])
    target[[field]][hit] <- value
    target
  }

  if (!is.null(project)) {
    d <- if (is.null(db$lkp_defaults)) data.frame(section = character(0), field = character(0), value = character(0)) else as_chr_df(db$lkp_defaults)
    set_default <- function(section, field, value) {
      j <- which(d$section %in% section & d$field %in% field)
      if (length(j)) d$value[j[1]] <<- value else {
        row <- d[0, , drop = FALSE][1, , drop = FALSE]
        row$section <- section; row$field <- field; row$value <- value
        d <<- rbind(d, row)
      }
    }

    # farm_p: a row without group applies to all farms (database level); a row
    # with group values fills the blank cells of the farms of that group
    x <- project$farm_p
    if (!is.null(x) && nrow(x)) {
      gcols <- grep("^group_", names(x), value = TRUE)
      fields <- setdiff(names(x), c(gcols, ".row", "Ids", "farm_name"))
      unknown <- setdiff(fields, f$farm)
      if (length(unknown)) problem("warning", "farm_p", 1, paste(unknown, collapse = ", "), "these columns are not farm values with a default and are not used")
      fields <- intersect(fields, f$farm)
      all_rows <- if (length(gcols)) rowSums(!is.na(x[gcols])) == 0 else rep(TRUE, nrow(x))
      for (i in which(all_rows)) for (fl in fields) if (!batch_blank(x[[fl]][i])) set_default("farm", fl, x[[fl]][i])
      farm <- input$farm
      for (i in which(!all_rows)) {
        g <- gcols[!is.na(unlist(x[i, gcols]))]
        miss <- setdiff(g, names(farm))
        if (length(miss)) { problem("warning", "farm_p", rows_of(x)[i], paste(miss, collapse = ", "), "the farms have no such group column: row not used"); next }
        where <- Reduce(`&`, lapply(g, function(gc) !is.na(farm[[gc]]) & farm[[gc]] == x[[gc]][i]))
        if (!any(where)) problem("warning", "farm_p", rows_of(x)[i], paste0(g, " = ", unlist(x[i, g]), collapse = ", "), "no farm is in this group")
        for (fl in fields) farm <- fill_blank(farm, where, fl, x[[fl]][i])
      }
      input$farm <- farm
    }

    # season_p: the default seasons (replaced as a whole)
    x <- project$season_p
    if (!is.null(x) && nrow(x) && all(c("season_name", "season_length") %in% names(x))) {
      x <- x[!batch_blank(x$season_name) & !batch_blank(x$season_length), , drop = FALSE]
      if (nrow(x)) {
        new <- d[0, , drop = FALSE][seq_len(nrow(x)), , drop = FALSE]
        new$section <- "season"; new$field <- x$season_name; new$value <- x$season_length
        d <- rbind(d[!d$section %in% "season", , drop = FALSE], new)
      }
    }

    # livestock_p: the types (species, class, IPCC categories, parameters) and the practices
    x <- project$livestock_p
    if (!is.null(x) && nrow(x) && "livetype_code" %in% names(x)) {
      lt <- if (is.null(db$lkp_livetype)) data.frame(livetype_code = character(0)) else as_chr_df(db$lkp_livetype)
      type_cols <- intersect(names(x), names(lt))
      lv <- input$livestock
      for (i in seq_len(nrow(x))) {
        code <- x$livetype_code[i]
        if (batch_blank(code)) next
        j <- which(lt$livetype_code == code)
        merged <- if (length(j)) lt[j[1], , drop = FALSE] else lt[0, , drop = FALSE][1, , drop = FALSE]
        for (cl in type_cols) if (!batch_blank(x[[cl]][i])) merged[[cl]] <- x[[cl]][i]
        merged$livetype_code <- code
        # meat and milk products follow from the species
        k <- match(merged$species, livestock_species_products$species)
        if (!is.na(k)) for (cl in intersect(c("meat_product", "milk_product"), names(merged))) {
          merged[[cl]] <- livestock_species_products[[cl]][k]
        }
        bad <- batch_livestock_category_problem(merged)
        if (!is.null(bad)) { problem("error", "livestock_p", rows_of(x)[i], code, bad); next }
        if (!length(j)) {
          missing <- setdiff(names(lt)[batch_blank(unlist(merged[1, ]))], "livetype_code")
          if (length(missing)) {
            problem("error", "livestock_p", rows_of(x)[i], code,
                    paste("a new livestock type needs every value; missing:", paste(missing, collapse = ", ")))
            next
          }
          lt <- rbind(lt, merged[, names(lt), drop = FALSE])
        } else {
          lt[j[1], ] <- merged[1, names(lt)]
        }
        if (nrow(lv) && "livetype_code" %in% names(lv)) {
          where <- !is.na(lv$livetype_code) & lv$livetype_code == code
          for (fl in intersect(f$livestock, names(x))) lv <- fill_blank(lv, where, fl, x[[fl]][i])
        }
      }
      db$lkp_livetype <- lt
      input$livestock <- lv
    }

    # feed_items_p and feed_type_p: per feed and per crop (database level)
    per_item <- list(feed_items_p = c("lkp_feeditem_defaults", "feed_item_code", "lkp_feeditem", "feed"),
                     feed_type_p = c("lkp_crops_defaults", "crop_code", "lkp_crops", "crop"))
    for (sh in names(per_item)) {
      p <- per_item[[sh]]; key <- p[2]
      x <- project[[sh]]
      if (is.null(x) || !nrow(x) || !key %in% names(x)) next
      cols <- intersect(names(x), f[[p[4]]])
      for (cl in intersect(batch_yes_no_fields, names(x))) x[[cl]] <- batch_to_01(x[[cl]])
      items <- db[[p[3]]]
      # the feed's or crop's own parameters change the database's values
      own_cols <- if (is.null(items)) character(0) else setdiff(intersect(names(x), intersect(f[[paste0(p[4], "_db")]], names(items))), key)
      tbl <- if (is.null(db[[p[1]]])) data.frame(stats::setNames(list(character(0)), key), check.names = FALSE) else as_chr_df(db[[p[1]]])
      for (cl in setdiff(cols, names(tbl))) tbl[[cl]] <- rep(NA_character_, nrow(tbl))
      for (i in seq_len(nrow(x))) {
        code <- x[[key]][i]
        if (batch_blank(code)) next
        if (!is.null(items) && !code %in% as.character(items[[key]])) {
          problem("warning", sh, rows_of(x)[i], code, paste("no", p[4], "with this code in the database: row not used"))
          next
        }
        if (length(own_cols)) {
          r <- which(as.character(items[[key]]) == code)[1]
          for (cl in own_cols) {
            v <- x[[cl]][i]
            if (batch_blank(v) || isTRUE(batch_same_value(v, items[[cl]][r]))) next
            items[[cl]][r] <- if (is.numeric(items[[cl]])) suppressWarnings(as.numeric(v)) else v
          }
        }
        given <- cols[!vapply(cols, function(cl) batch_blank(x[[cl]][i]), TRUE)]
        if (!length(given)) next
        j <- which(as.character(tbl[[key]]) == code)
        if (!length(j)) {
          tbl <- rbind(tbl, tbl[0, , drop = FALSE][1, , drop = FALSE])
          j <- nrow(tbl); tbl[[key]][j] <- code
        }
        for (cl in given) tbl[[cl]][j[1]] <- x[[cl]][i]
      }
      if (!is.null(items)) db[[p[3]]] <- items
      db[[p[1]]] <- tbl
    }

    # fertilizer_p: N content of each fertiliser (blank cells of the farms' rows)
    x <- project$fertilizer_p
    fe <- input$fertilizer
    if (!is.null(x) && nrow(x) && nrow(fe) && all(c("fertilizer_desc", "percentage_n") %in% names(x)) && "fertilizer_desc" %in% names(fe)) {
      for (i in seq_len(nrow(x))) {
        fe <- fill_blank(fe, !is.na(fe$fertilizer_desc) & fe$fertilizer_desc == x$fertilizer_desc[i], "percentage_n", x$percentage_n[i])
      }
      input$fertilizer <- fe
    }
    db$lkp_defaults <- d
  }
  attr(input, "project_applied") <- TRUE
  attr(input, "project_problems") <- if (length(problems)) do.call(rbind, problems) else NULL
  list(input = input, db = db)
}

batch_livestock_category_problem <- function(r) {
  r <- as.list(r)
  sp <- r[["species"]]; cl <- r[["animal_class"]]
  if (!is.null(sp) && !batch_blank(sp) && !sp %in% names(livestock_species_classes))
    return(paste0("species '", sp, "' is not one of ", paste(names(livestock_species_classes), collapse = ", ")))
  if (!is.null(sp) && !is.null(cl) && !batch_blank(sp) && !batch_blank(cl) && !cl %in% livestock_species_classes[[sp]])
    return(paste0("class '", cl, "' is not a class of ", sp, " (", paste(livestock_species_classes[[sp]], collapse = ", "), ")"))
  for (f in names(ipcc_livestock_categories)) {
    v <- r[[f]]
    if (!is.null(v) && !batch_blank(v) && !v %in% ipcc_livestock_categories[[f]]) return(paste0(f, " '", v, "' is not an allowed value"))
  }
  NULL
}

#' What's missing in a batch file, and the values that will be used
#'
#' Lists the values the farms left blank and that are filled from the project
#' values (`_p` sheets) or, when these are blank too, from the parameter
#' database's batch defaults (or the built-in value). One row per field and
#' livestock type, feed, crop, fertiliser or group, with the number of farms.
#'
#' @param file Path of a batch `.xlsx` file, or a list from [read_batch_input()].
#' @param parameter_database A list from [read_parameter_database()], or the
#'   path of a parameter database folder.
#' @return A data frame: `topic`, `sheet` (the `_p` sheet holding the value),
#'   `key` (livestock type, feed, crop or fertiliser code, or the group as
#'   `group_x=value`, or "" for all farms), `item` (its name), `field`, `label`,
#'   `farms` (number of farms concerned), `value` (what will be used) and
#'   `batch_default` (the database's or built-in value).
#' @export
batch_missing_values <- function(file, parameter_database) {
  db0 <- if (is.character(parameter_database)) read_parameter_database(parameter_database) else parameter_database
  raw <- if (is.list(file) && all(batch_data_sheets %in% names(file))) file else read_batch_input(file)
  applied <- batch_apply_project(raw, db0)
  inp <- applied$input; db <- applied$db
  f <- batch_project_fields()
  dict <- batch_dictionary()
  short <- function(x) sub("[.;:(].*$", "", x)
  label_of <- function(field, sheet) {
    x <- dict$description[dict$field == field & dict$sheet == sheet][1]
    if (is.na(x)) return(field)
    same <- sum(short(dict$description[dict$sheet == sheet]) == short(x), na.rm = TRUE)
    if (same > 1) {   # (several fields share the label: name the field)
      nm <- gsub("_", " ", sub("_desc$", "", field))
      return(paste0(toupper(substr(nm, 1, 1)), substring(nm, 2), " (", tolower(short(x)), ")"))
    }
    short(x)
  }
  out <- list()
  add <- function(topic, sheet, key, item, field, farms, value, default, label_sheet) {
    if (!length(farms) || farms == 0) return()
    value <- as.character(value %||% NA); default <- as.character(default %||% NA)
    if (field %in% batch_yes_no_fields) { value <- batch_to_yes_no(value); default <- batch_to_yes_no(default) }
    out[[length(out) + 1]] <<- data.frame(topic = topic, sheet = sheet, key = as.character(key), item = as.character(item %||% ""),
                                          field = field, label = label_of(field, label_sheet), farms = farms,
                                          value = value, batch_default = default, stringsAsFactors = FALSE)
  }
  def0 <- batch_db_defaults(db0); def <- batch_db_defaults(db)
  first_of <- function(...) { for (v in list(...)) if (length(v) && !batch_blank(v[1])) return(as.character(v[1])); NA_character_ }
  ids <- raw$farm$Ids

  # Farm --------------------------------------------------------------------------
  farm0 <- raw$farm; farm1 <- inp$farm
  for (fl in f$farm) {
    blank <- if (fl %in% names(farm0)) batch_blank(farm0[[fl]]) else rep(TRUE, nrow(farm0))
    if (!any(blank)) next
    v1 <- if (fl %in% names(farm1)) farm1[[fl]] else rep(NA, nrow(farm1))
    by_group <- blank & !batch_blank(v1)
    pg <- attr(raw, "project")$farm_p
    gcols <- if (!is.null(pg)) grep("^group_", names(pg), value = TRUE) else character(0)
    if (any(by_group) && length(gcols)) {
      keys <- vapply(which(by_group), function(i) {
        g <- gcols[gcols %in% names(farm0) & !is.na(unlist(farm0[i, intersect(gcols, names(farm0))]))]
        paste0(g, "=", unlist(farm0[i, g]), collapse = "&")
      }, "")
      for (k in unique(keys)) {
        w <- which(by_group)[keys == k]
        add("Farm", "farm_p", k, sub("^group_", "", gsub("&", ", ", gsub("=", ": ", k))), fl, length(w), v1[w[1]],
            first_of(def0$farm[[fl]], batch_builtin_default(fl)), "farm")
      }
    }
    rest <- blank & !by_group
    add("Farm", "farm_p", "", "All farms", fl, sum(rest), first_of(def$farm[[fl]], batch_builtin_default(fl)),
        first_of(def0$farm[[fl]], batch_builtin_default(fl)), "farm")
  }

  # Seasons -----------------------------------------------------------------------
  no_season <- setdiff(ids, raw$season$Ids)
  describe <- function(s) if (nrow(s)) paste0(s$season_name, " (", s$season_length, " days)", collapse = ", ") else NA
  add("Seasons", "season_p", "", "Default seasons", "season_name", length(no_season), describe(def$season), describe(def0$season), "season")

  # Livestock: practices and livestock type values -------------------------------
  lv0 <- raw$livestock; lv1 <- inp$livestock
  lt <- as_chr_df(db$lkp_livetype); lt0 <- as_chr_df(db0$lkp_livetype)
  props <- intersect(dict$field[dict$sheet == "livestock" & grepl("lkp_livetype, by livetype_code", dict$precedence)], names(lt))
  if (nrow(lv0) && "livetype_code" %in% names(lv0)) {
    for (code in unique(lv0$livetype_code[!is.na(lv0$livetype_code)])) {
      w <- which(lv0$livetype_code == code)
      name <- first_of(lt$livetype_desc[lt$livetype_code == code], code)
      num1 <- function(fl, rows) {
        v <- if (fl %in% names(lv1)) suppressWarnings(as.numeric(lv1[[fl]][rows])) else rep(NA_real_, length(rows))
        d <- suppressWarnings(as.numeric(def$livestock[[fl]] %||% NA))
        ifelse(is.na(v), ifelse(is.na(d), 0, d), v)
      }
      for (fl in f$livestock) {
        blank <- w[if (fl %in% names(lv0)) batch_blank(lv0[[fl]][w]) else rep(TRUE, length(w))]
        # a manure system only matters for a place the animals use (time spent there),
        # and a second system only when part of the manure goes to it
        place <- regmatches(fl, regexpr("(stable|non_roofed_enclosure|onfarm_grazing|offfarm_grazing)$", fl))
        if (length(place) && grepl("^manureman_|^manure_share_2_", fl)) blank <- blank[num1(paste0("time_in_", place), blank) > 0]
        if (length(place) && grepl("^manureman_2_", fl)) blank <- blank[num1(paste0("manure_share_2_", place), blank) > 0]
        if (!length(blank)) next
        v <- first_of(if (fl %in% names(lv1)) lv1[[fl]][blank] else NULL, def$livestock[[fl]], batch_builtin_default(fl))
        add("Livestock", "livestock_p", code, name, fl, length(unique(lv0$Ids[blank])), v,
            first_of(def0$livestock[[fl]], batch_builtin_default(fl)), "livestock")
      }
      for (fl in props) {
        blank <- w[if (fl %in% names(lv0)) batch_blank(lv0[[fl]][w]) else rep(TRUE, length(w))]
        if (!length(blank)) next
        add("Livestock types", "livestock_p", code, name, fl, length(unique(lv0$Ids[blank])),
            lt[[fl]][lt$livetype_code == code][1], if (code %in% lt0$livetype_code) lt0[[fl]][lt0$livetype_code == code][1] else NA, "livestock")
      }
    }
  }

  # Feeds and crops ------------------------------------------------------------------
  fb <- raw$feed_basket
  feed_cols <- grep("^[0-9]+$", names(fb), value = TRUE)
  fi0 <- raw$feed_items; ft0 <- raw$feed_type
  fdef <- as_chr_df(db$lkp_feeditem_defaults); fdef0 <- as_chr_df(db0$lkp_feeditem_defaults)
  cdef <- as_chr_df(db$lkp_crops_defaults); cdef0 <- as_chr_df(db0$lkp_crops_defaults)
  items <- as_chr_df(db$lkp_feeditem); crops <- as_chr_df(db$lkp_crops)
  cell <- function(tbl, id, key, code, fl) {
    if (!nrow(tbl) || !fl %in% names(tbl) || !key %in% names(tbl)) return(NA)
    v <- tbl[[fl]][tbl$Ids %in% id & tbl[[key]] %in% code]
    if (length(v)) v[1] else NA
  }
  dval <- function(tbl, key, code, fl) if (nrow(tbl) && fl %in% names(tbl)) tbl[[fl]][as.character(tbl[[key]]) == code][1] else NA
  if (nrow(fb) && length(feed_cols)) {
    use <- list(); crop_use <- list()
    for (r in seq_len(nrow(fb))) for (fc in feed_cols) {
      a <- suppressWarnings(as.numeric(fb[[fc]][r]))
      if (!is.na(a) && a > 0) {
        use[[length(use) + 1]] <- c(fb$Ids[r], fc)
        cc <- first_of(cell(fi0, fb$Ids[r], "feed_item_code", fc, "crop_code"), items$crop_code[items$feed_item_code == fc])
        if (!is.na(cc)) crop_use[[length(crop_use) + 1]] <- c(fb$Ids[r], fc, cc)
      }
    }
    use <- unique(do.call(rbind, use)); crop_use <- if (length(crop_use)) unique(do.call(rbind, crop_use)) else NULL
    for (fc in unique(use[, 2])) {
      farms <- unique(use[use[, 2] == fc, 1])
      for (fl in f$feed) {
        n <- sum(vapply(farms, function(id) batch_blank(cell(fi0, id, "feed_item_code", fc, fl)), TRUE))
        add("Feeds", "feed_items_p", fc, first_of(items$feed_item_name[items$feed_item_code == fc], fc), fl, n,
            first_of(dval(fdef, "feed_item_code", fc, fl), batch_builtin_default(fl)),
            first_of(dval(fdef0, "feed_item_code", fc, fl), batch_builtin_default(fl)), "feed_items")
      }
    }
    if (!is.null(crop_use)) for (cc in unique(crop_use[, 3])) {
      pairs <- crop_use[crop_use[, 3] == cc, , drop = FALSE]
      farms <- unique(pairs[, 1])
      for (fl in f$crop) {
        n <- sum(vapply(farms, function(id) {
          feeds <- pairs[pairs[, 1] == id, 2]
          all(vapply(feeds, function(fc) batch_blank(cell(fi0, id, "feed_item_code", fc, fl)), TRUE)) &&
            batch_blank(cell(ft0, id, "crop_code", cc, fl))
        }, TRUE))
        add("Crops", "feed_type_p", cc, first_of(crops$crop_name[crops$crop_code == cc], cc), fl, n,
            first_of(dval(cdef, "crop_code", cc, fl), batch_builtin_default(fl)),
            first_of(dval(cdef0, "crop_code", cc, fl), batch_builtin_default(fl)), "feed_type")
      }
    }
  }

  # Fertiliser N content ----------------------------------------------------------------
  fe0 <- raw$fertilizer; fe1 <- inp$fertilizer
  if (nrow(fe0) && "fertilizer_desc" %in% names(fe0)) {
    blank <- if ("percentage_n" %in% names(fe0)) batch_blank(fe0$percentage_n) else rep(TRUE, nrow(fe0))
    for (fd in unique(fe0$fertilizer_desc[blank & !is.na(fe0$fertilizer_desc)])) {
      w <- which(blank & fe0$fertilizer_desc %in% fd)
      std <- unname(batch_fertilizer_n[fd])
      v <- first_of(if ("percentage_n" %in% names(fe1)) fe1$percentage_n[w] else NULL, std, "0")
      add("Fertiliser", "fertilizer_p", fd, fd, "percentage_n", length(unique(fe0$Ids[w])), v,
          if (is.na(std)) "0" else std, "fertilizer")
    }
  }

  if (!length(out)) return(data.frame(topic = character(0), sheet = character(0), key = character(0), item = character(0),
                                      field = character(0), label = character(0), farms = integer(0),
                                      value = character(0), batch_default = character(0)))
  do.call(rbind, out)
}
