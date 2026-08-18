# One parameter database for batch processing ------------------------------------------
#
# Batch processing uses one long list of livestock types, feeds and crops, made
# by combining the parameter databases shipped with iCLEANED, rather than a
# regional database.

batch_database_marker <- "batch_database.json"

#' Build the batch parameter database (one long list)
#'
#' Combines several parameter databases into one: every livestock type, feed
#' and crop appears once (names are compared without case, spaces or
#' punctuation). When the databases give different values for the same name,
#' the values most of them use are kept (ties: the first database in
#' alphabetical order); every such case is written to `review_conflicts.csv`
#' with the value of each database, for review.
#'
#' Codes: livestock types keep their code when all databases use the same one;
#' feeds and crops are numbered in alphabetical order, and each feed is linked
#' to the new code of its crop. Feed and crop defaults
#' (`lkp_feeditem_defaults`, `lkp_crops_defaults`) are carried over by name.
#' Other lookup tables (climate, soil, land cover, manure systems, ...) are
#' combined by description. Batch defaults (`lkp_defaults`) are copied from
#' `defaults_from`. A file `batch_database.json` marks the folder (and copies
#' of it) as a batch database.
#'
#' @param paths Folders of the parameter databases to combine.
#' @param out_dir Folder to write (created; existing `lkp_*.csv` files in it
#'   are replaced).
#' @param defaults_from Optional folder whose `lkp_defaults.csv` is copied.
#' @return Invisibly, a list with the number of entries per table and the
#'   conflicts data frame.
#' @export
build_batch_database <- function(paths, out_dir, defaults_from = NULL) {
  paths <- sort(paths)
  dbs <- stats::setNames(basename(paths), paths)
  rd <- function(p, t) {
    x <- batch_read_raw(p, t)
    if (is.null(x)) return(NULL)
    names(x) <- trimws(gsub('^"|"$', "", gsub("^\ufeff", "", names(x))))
    x[] <- lapply(x, function(v) {
      bad <- !is.na(v) & !validUTF8(v)
      v[bad] <- iconv(v[bad], from = "latin1", to = "UTF-8")
      trimws(v)
    })
    x$.db <- rep(basename(p), nrow(x))
    x
  }
  stack <- function(t) {
    x <- lapply(paths, rd, t = t)
    x <- x[!vapply(x, is.null, TRUE)]
    if (!length(x)) return(NULL)
    dplyr::bind_rows(x)
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  unlink(list.files(out_dir, pattern = "^lkp_.*\\.csv$", full.names = TRUE))
  conflicts <- list()

  # One row per name: the values most databases use
  pick <- function(x, name_col, code_col, table, extra_values = NULL, exact = FALSE) {
    # reference lists (manure systems, soils, ...) are matched on their exact
    # wording: "deep bedding < 1 month" and "> 1 month" must stay apart
    x$.key <- if (exact) tolower(trimws(x[[name_col]])) else batch_normalise_text(x[[name_col]])
    x <- x[!is.na(x$.key) & nzchar(x$.key), , drop = FALSE]
    vals <- setdiff(names(x), c(name_col, code_col, ".db", ".key", "crop_code"))
    vals <- unique(c(vals, extra_values))
    sig <- do.call(paste, c(lapply(vals, function(v) ifelse(is.na(x[[v]]), "", x[[v]])), sep = "\r"))
    if (!length(vals)) sig <- rep("", nrow(x))
    out <- lapply(split(seq_len(nrow(x)), factor(x$.key, unique(x$.key))), function(i) {
      counts <- table(sig[i])
      best <- names(counts)[counts == max(counts)]
      j <- i[sig[i] %in% best][1]
      row <- x[j, , drop = FALSE]
      # the spelling most databases use
      spell <- table(x[[name_col]][i]); row[[name_col]] <- names(spell)[spell == max(spell)][1]
      if (length(unique(sig[i])) > 1) {
        for (v in vals) {
          vv <- ifelse(is.na(x[[v]][i]), "", x[[v]][i])
          if (length(unique(vv)) > 1) {
            conflicts[[length(conflicts) + 1]] <<- data.frame(
              table = table, name = row[[name_col]], field = if (v == ".crop") "crop" else v, kept = ifelse(is.na(row[[v]]), "", row[[v]]),
              values_by_database = paste0(x$.db[i], ": ", vv, collapse = " | "), stringsAsFactors = FALSE)
          }
        }
      }
      row$.dbs <- paste(x$.db[i], collapse = "; ")
      row
    })
    dplyr::bind_rows(out)
  }

  # Crops ----------------------------------------------------------------------
  crops_all <- stack("lkp_crops")
  crops <- pick(crops_all, "crop_name", "crop_code", "lkp_crops")
  crops <- crops[order(tolower(crops$crop_name)), , drop = FALSE]
  crops$new_code <- as.character(seq_len(nrow(crops)))
  crop_new <- function(db, code) {
    nm <- crops_all$crop_name[match(paste(db, code), paste(crops_all$.db, crops_all$crop_code))]
    crops$new_code[match(batch_normalise_text(nm), crops$.key)]
  }

  # Feeds (the crop is part of a feed's values: compared by crop name) ------------
  feeds_all <- stack("lkp_feeditem")
  feeds_all$.crop <- batch_normalise_text(crops_all$crop_name[match(paste(feeds_all$.db, feeds_all$crop_code),
                                                                     paste(crops_all$.db, crops_all$crop_code))])
  feeds <- pick(feeds_all, "feed_item_name", "feed_item_code", "lkp_feeditem", extra_values = ".crop")
  feeds <- feeds[order(tolower(feeds$feed_item_name)), , drop = FALSE]
  feeds$new_code <- as.character(seq_len(nrow(feeds)))
  feeds$new_crop <- crops$new_code[match(feeds$.crop, crops$.key)]
  feed_new <- function(db, code) {
    nm <- feeds_all$feed_item_name[match(paste(db, code), paste(feeds_all$.db, feeds_all$feed_item_code))]
    feeds$new_code[match(batch_normalise_text(nm), feeds$.key)]
  }

  # Livestock types: codes kept when the databases agree ------------------------------
  lt_all <- stack("lkp_livetype")
  lt <- pick(lt_all, "livetype_desc", "livetype_code", "lkp_livetype")
  lt_all$.key <- batch_normalise_text(lt_all$livetype_desc)
  same_code <- vapply(lt$.key, function(k) length(unique(lt_all$livetype_code[lt_all$.key == k])) == 1, TRUE)
  lt$new_code <- ifelse(same_code, lt$livetype_code, NA)
  lt$new_code[duplicated(lt$new_code) & !is.na(lt$new_code)] <- NA
  if (any(is.na(lt$new_code))) lt$new_code[is.na(lt$new_code)] <- batch_next_code(lt$new_code[!is.na(lt$new_code)], sum(is.na(lt$new_code)))
  lt <- lt[order(as.numeric(lt$new_code)), , drop = FALSE]

  # Write the three lists, in the column order of the first database --------------
  write_tab <- function(x, table, code_col, new_col, template) {
    cols <- setdiff(names(template), ".db")
    x[[code_col]] <- x[[new_col]]
    out <- x[, intersect(cols, names(x)), drop = FALSE]
    out[is.na(out)] <- ""
    data.table::fwrite(out, file.path(out_dir, paste0(table, ".csv")))
  }
  crops_tpl <- crops_all[0, , drop = FALSE]
  write_tab(crops, "lkp_crops", "crop_code", "new_code", crops_tpl)
  feeds$crop_code <- ifelse(is.na(feeds$new_crop), "", feeds$new_crop)
  write_tab(feeds, "lkp_feeditem", "feed_item_code", "new_code", feeds_all[0, setdiff(names(feeds_all), ".crop"), drop = FALSE])
  write_tab(lt, "lkp_livetype", "livetype_code", "new_code", lt_all[0, setdiff(names(lt_all), ".key"), drop = FALSE])

  # Feed and crop defaults, carried over by name -------------------------------------
  defaults_by_name <- function(table, code_col, new_of) {
    x <- stack(table)
    if (is.null(x)) return(0L)
    x[[code_col]] <- vapply(seq_len(nrow(x)), function(i) new_of(x$.db[i], x[[code_col]][i]) %||% NA_character_, "")
    x <- x[!is.na(x[[code_col]]), , drop = FALSE]
    x <- x[!duplicated(x[[code_col]]), setdiff(names(x), ".db"), drop = FALSE]
    x <- x[order(as.numeric(x[[code_col]])), , drop = FALSE]
    x[is.na(x)] <- ""
    data.table::fwrite(x, file.path(out_dir, paste0(table, ".csv")))
    nrow(x)
  }
  n_fd <- defaults_by_name("lkp_feeditem_defaults", "feed_item_code", feed_new)
  n_cd <- defaults_by_name("lkp_crops_defaults", "crop_code", crop_new)

  # Other lookup tables: combined by description ---------------------------------------
  tables <- unique(sub("\\.csv$", "", unlist(lapply(paths, list.files, pattern = "^lkp_.*\\.csv$"))))
  other <- setdiff(tables, c("lkp_crops", "lkp_feeditem", "lkp_livetype", "lkp_feeditem_defaults",
                             "lkp_crops_defaults", "lkp_defaults", "lkp_answer_matches"))
  for (t in other) {
    x <- stack(t)
    desc <- grep("_desc$|_name$", names(x), value = TRUE)[1]
    if (is.na(desc)) desc <- names(x)[min(2, ncol(x) - 1)]
    code <- setdiff(names(x), c(desc, ".db"))[1]
    y <- pick(x, desc, if (identical(code, desc)) NULL else code, t, exact = TRUE)
    y <- y[, setdiff(names(x), ".db"), drop = FALSE]
    y[is.na(y)] <- ""
    data.table::fwrite(y, file.path(out_dir, paste0(t, ".csv")))
  }
  if (!is.null(defaults_from) && file.exists(file.path(defaults_from, "lkp_defaults.csv"))) {
    file.copy(file.path(defaults_from, "lkp_defaults.csv"), file.path(out_dir, "lkp_defaults.csv"), overwrite = TRUE)
  }

  conflicts <- if (length(conflicts)) do.call(rbind, conflicts) else
    data.frame(table = character(0), name = character(0), field = character(0), kept = character(0), values_by_database = character(0))
  utils::write.csv(conflicts, file.path(out_dir, "review_conflicts.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  counts <- c(livestock_types = nrow(lt), feeds = nrow(feeds), crops = nrow(crops),
              feed_defaults = n_fd, crop_defaults = n_cd)
  jsonlite::write_json(list(type = "batch long list", built = format(Sys.time(), "%Y-%m-%d"),
                            from = unname(dbs), counts = as.list(counts),
                            conflicts = length(unique(paste(conflicts$table, conflicts$name)))),
                       file.path(out_dir, batch_database_marker), auto_unbox = TRUE, pretty = TRUE)
  invisible(list(counts = counts, conflicts = conflicts))
}

#' Is a folder a batch parameter database?
#'
#' @param path Folder of a parameter database.
#' @return `TRUE` when the folder was made by [build_batch_database()] (or is
#'   a copy of one).
#' @export
is_batch_database <- function(path) file.exists(file.path(path, batch_database_marker))
