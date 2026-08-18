# Feed and crop library -----------------------------------------------------------
#
# Farmers name feeds that are often missing from the parameter database used
# for a batch, although another database has them with their values. The
# library combines the feeds and crops of several parameter databases so that
# answers can be matched against all of them; a feed chosen from the library is
# copied, with its crop and values, into the user's own database under new,
# unique codes.

#' Build a library of feeds and crops from several parameter databases
#'
#' @param paths Folders of parameter databases (e.g. the default databases of
#'   iCLEANED).
#' @return A list with `feeds` (one row per distinct feed name: `lib_id`,
#'   `feed_item_name`, `dm_content`, `me_content`, `cp_content`, `lib_crop_id`,
#'   `crop_name`, `source_db`) and `crops` (one row per distinct crop name:
#'   `lib_crop_id`, `source_db` and the columns of `lkp_crops`), plus the
#'   database defaults of each feed and crop when the source has them
#'   (`feed_defaults`, `crop_defaults`).
#' @export
build_feed_library <- function(paths) {
  feeds <- list(); crops <- list(); feed_def <- list(); crop_def <- list()
  for (p in paths) {
    db <- tryCatch(read_parameter_database(p), error = function(e) NULL)
    if (is.null(db) || is.null(db$lkp_feeditem) || is.null(db$lkp_crops)) next
    src <- basename(p)
    cr <- db$lkp_crops
    cr$source_db <- src
    cr$crop_code <- as.character(cr$crop_code)
    crops[[length(crops) + 1]] <- cr
    fi <- db$lkp_feeditem
    fi$source_db <- src
    fi$feed_item_code <- as.character(fi$feed_item_code)
    fi$crop_code <- as.character(fi$crop_code)
    fi$crop_name <- cr$crop_name[match(fi$crop_code, cr$crop_code)]
    feeds[[length(feeds) + 1]] <- fi
    if (!is.null(db$lkp_feeditem_defaults)) {
      d <- db$lkp_feeditem_defaults; d$source_db <- src; d$feed_item_code <- as.character(d$feed_item_code)
      feed_def[[length(feed_def) + 1]] <- d
    }
    if (!is.null(db$lkp_crops_defaults)) {
      d <- db$lkp_crops_defaults; d$source_db <- src; d$crop_code <- as.character(d$crop_code)
      crop_def[[length(crop_def) + 1]] <- d
    }
  }
  if (!length(feeds)) stop("No feed tables found in the databases given.", call. = FALSE)
  bind <- function(x) {
    x <- lapply(x, function(d) { d[] <- lapply(d, function(v) if (is.factor(v)) as.character(v) else v); d })
    cols <- unique(unlist(lapply(x, names)))
    x <- lapply(x, function(d) { for (cl in setdiff(cols, names(d))) d[[cl]] <- NA; d[, cols] })
    do.call(rbind, x)
  }
  crops <- bind(crops); feeds <- bind(feeds)
  # one entry per name (the first database that has it)
  crops$key <- batch_normalise_text(crops$crop_name)
  crops <- crops[nzchar(crops$key) & !duplicated(crops$key), , drop = FALSE]
  crops$lib_crop_id <- sprintf("C%03d", seq_len(nrow(crops)))
  feeds$key <- batch_normalise_text(feeds$feed_item_name)
  feeds <- feeds[nzchar(feeds$key) & !duplicated(feeds$key), , drop = FALSE]
  feeds$lib_id <- sprintf("F%03d", seq_len(nrow(feeds)))
  feeds$lib_crop_id <- crops$lib_crop_id[match(batch_normalise_text(feeds$crop_name), crops$key)]
  lib <- list(
    feeds = feeds[, c("lib_id", "feed_item_name", "dm_content", "me_content", "cp_content",
                      "lib_crop_id", "crop_name", "source_db", "feed_item_code", "key")],
    crops = crops[, c("lib_crop_id", "source_db", setdiff(names(crops), c("lib_crop_id", "source_db")))],
    feed_defaults = if (length(feed_def)) bind(feed_def) else NULL,
    crop_defaults = if (length(crop_def)) bind(crop_def) else NULL
  )
  class(lib) <- c("feed_library", "list")
  lib
}

#' @export
print.feed_library <- function(x, ...) {
  cat(sprintf("Feed library: %d feeds and %d crops from %d databases\n",
              nrow(x$feeds), nrow(x$crops), length(unique(x$feeds$source_db))))
  invisible(x)
}

# Next free code of a table: one more than the largest number used, never a
# code that is already taken
batch_next_code <- function(existing, n = 1) {
  existing <- trimws(as.character(existing))
  start <- suppressWarnings(max(c(0, as.numeric(existing)), na.rm = TRUE))
  out <- character(0); k <- start
  while (length(out) < n) {
    k <- k + 1
    if (!as.character(k) %in% existing) out <- c(out, as.character(k))
  }
  out
}

# Read a table of a database folder as text, keeping its own column names
batch_read_raw <- function(path, table) {
  f <- file.path(path, paste0(table, ".csv"))
  if (!file.exists(f)) return(NULL)
  x <- as.data.frame(data.table::fread(f, colClasses = "character", encoding = "UTF-8", na.strings = NULL),
                     stringsAsFactors = FALSE)
  x
}
batch_append_raw <- function(path, table, rows) {
  f <- file.path(path, paste0(table, ".csv"))
  old <- batch_read_raw(path, table)
  clean <- function(n) trimws(gsub("^\ufeff", "", n))
  if (is.null(old)) {
    out <- rows
  } else {
    names_old <- names(old)
    keys <- clean(names_old)
    add <- as.data.frame(lapply(keys, function(k) {
      v <- if (k %in% names(rows)) rows[[k]] else rep("", nrow(rows))
      v <- as.character(v); v[is.na(v)] <- ""; v
    }), stringsAsFactors = FALSE, col.names = names_old, check.names = FALSE)
    out <- rbind(old, add)
  }
  data.table::fwrite(out, f)
  invisible(f)
}

#' Add feeds and crops to a parameter database
#'
#' Turns confirmed matches that point to the feed library, or to a new feed
#' copied from another one, into real feeds and crops of the database folder,
#' with new codes that are not used yet. Matches that already point to a
#' feed, crop or manure system of the database are left as they are.
#'
#' Values of a match:
#' * a code of the database: kept;
#' * `"lib:<lib_id>"` (feed) or `"libcrop:<lib_crop_id>"` (crop): the library
#'   entry is copied, with its crop for a feed (a crop of the same name in the
#'   database is reused);
#' * `"new:<code>"` or `"new:lib:<lib_id>"` (feed), `"newcrop:<code>"` or
#'   `"newcrop:libcrop:<lib_crop_id>"` (crop): a new entry named as the
#'   farmer's answer, with the values of the feed or crop given.
#'
#' Feeds and crops stay linked: a new feed gets its own crop, a copy of the
#' crop of the feed it copies, named as the new feed; and a crop that is added
#' gets a feed (its main product) when no feed of the database uses it, with
#' the values of a feed of the same crop. Database defaults of the copied feed
#' or crop (`lkp_feeditem_defaults`, `lkp_crops_defaults`) are copied as well.
#' Names are kept unique: a feed or crop whose name already exists in the
#' database is not added twice.
#'
#' @param matches Data frame with `kind`, `answer` and `value` (see
#'   [read_odk_export()]).
#' @param path Folder of the user's parameter database (written to).
#' @param library A library from [build_feed_library()].
#' @return `matches` with `value` replaced by the codes in the database, and
#'   attribute `added` (data frame of what was added: `kind`, `code`, `name`,
#'   `copied_from`).
#' @export
add_matches_to_database <- function(matches, path, library = NULL) {
  added <- list()
  db <- read_parameter_database(path)
  feed_codes <- as.character(db$lkp_feeditem$feed_item_code)
  crop_codes <- as.character(db$lkp_crops$crop_code)
  feed_names <- batch_normalise_text(db$lkp_feeditem$feed_item_name)
  crop_names <- batch_normalise_text(db$lkp_crops$crop_name)
  lib_feed <- function(id) library$feeds[library$feeds$lib_id == id, , drop = FALSE]
  lib_crop <- function(id) library$crops[library$crops$lib_crop_id == id, , drop = FALSE]

  crop_source <- list()   # code of each crop added -> where its values came from
  # Add a crop (row with lkp_crops columns); returns its code
  add_crop <- function(row, name, from, defaults = NULL, source = NULL) {
    key <- batch_normalise_text(name)
    if (key %in% crop_names) return(crop_codes[match(key, crop_names)])
    code <- batch_next_code(crop_codes)
    row <- row[, setdiff(names(row), c("lib_crop_id", "source_db", "key")), drop = FALSE]
    row$crop_code <- code; row$crop_name <- name
    batch_append_raw(path, "lkp_crops", row)
    if (!is.null(defaults) && nrow(defaults)) {
      d <- defaults[1, setdiff(names(defaults), "source_db"), drop = FALSE]; d$crop_code <- code
      batch_append_raw(path, "lkp_crops_defaults", d)
    }
    crop_codes <<- c(crop_codes, code); crop_names <<- c(crop_names, key)
    crop_source[[code]] <<- source
    added[[length(added) + 1]] <<- data.frame(kind = "crop", code = code, name = name, copied_from = from)
    code
  }
  # Add a feed; returns its code
  add_feed <- function(name, dm, me, cp, crop_code, from, defaults = NULL) {
    key <- batch_normalise_text(name)
    if (key %in% feed_names) return(feed_codes[match(key, feed_names)])
    code <- batch_next_code(feed_codes)
    batch_append_raw(path, "lkp_feeditem", data.frame(
      feed_item_code = code, crop_code = crop_code, feed_item_name = name,
      dm_content = dm, me_content = me, cp_content = cp, stringsAsFactors = FALSE))
    if (!is.null(defaults) && nrow(defaults)) {
      d <- defaults[1, setdiff(names(defaults), "source_db"), drop = FALSE]; d$feed_item_code <- code
      batch_append_raw(path, "lkp_feeditem_defaults", d)
    }
    feed_codes <<- c(feed_codes, code); feed_names <<- c(feed_names, key)
    added[[length(added) + 1]] <<- data.frame(kind = "feed", code = code, name = name, copied_from = from)
    code
  }
  from_library_feed <- function(id, name = NULL) {
    f <- lib_feed(id)
    if (!nrow(f)) stop("Library feed ", id, " not found.", call. = FALSE)
    c0 <- lib_crop(f$lib_crop_id)
    cdef <- if (!is.null(library$crop_defaults) && nrow(c0)) library$crop_defaults[library$crop_defaults$source_db == c0$source_db & library$crop_defaults$crop_code == c0$crop_code, , drop = FALSE]
    # a feed under a new name gets its own crop of the same name (so the
    # farmer's answers about that crop reach this feed)
    crop <- if (nrow(c0)) add_crop(c0, name %||% c0$crop_name, paste0(c0$crop_name, " (", c0$source_db, ")"), cdef,
                                   source = list(lib_feed = id)) else ""
    fdef <- if (!is.null(library$feed_defaults)) library$feed_defaults[library$feed_defaults$source_db == f$source_db & library$feed_defaults$feed_item_code == f$feed_item_code, , drop = FALSE]
    add_feed(name %||% f$feed_item_name, f$dm_content, f$me_content, f$cp_content, crop,
             paste0(f$feed_item_name, " (", f$source_db, ")"), fdef)
  }
  from_db_feed <- function(code, name) {
    f <- db$lkp_feeditem[as.character(db$lkp_feeditem$feed_item_code) == code, , drop = FALSE]
    if (!nrow(f)) stop("Feed ", code, " not found in the database.", call. = FALSE)
    fdef <- if (!is.null(db$lkp_feeditem_defaults)) db$lkp_feeditem_defaults[as.character(db$lkp_feeditem_defaults$feed_item_code) == code, , drop = FALSE]
    # its own crop, a copy of the crop of the feed it copies
    c0 <- db$lkp_crops[as.character(db$lkp_crops$crop_code) == as.character(f$crop_code), , drop = FALSE]
    cdef <- if (!is.null(db$lkp_crops_defaults)) db$lkp_crops_defaults[as.character(db$lkp_crops_defaults$crop_code) == as.character(f$crop_code), , drop = FALSE]
    crop <- if (nrow(c0)) add_crop(c0, name, c0$crop_name, cdef, source = list(db_feed = code)) else as.character(f$crop_code)
    add_feed(name, f$dm_content, f$me_content, f$cp_content, crop, f$feed_item_name, fdef)
  }
  copy_crop <- function(source, name) {
    if (startsWith(source, "libcrop:")) {
      c0 <- lib_crop(sub("^libcrop:", "", source))
      cdef <- if (!is.null(library$crop_defaults)) library$crop_defaults[library$crop_defaults$source_db == c0$source_db & library$crop_defaults$crop_code == c0$crop_code, , drop = FALSE]
      return(add_crop(c0, name %||% c0$crop_name, paste0(c0$crop_name, " (", c0$source_db, ")"), cdef,
                      source = list(lib_crop = c0$lib_crop_id)))
    }
    c0 <- db$lkp_crops[as.character(db$lkp_crops$crop_code) == source, , drop = FALSE]
    cdef <- if (!is.null(db$lkp_crops_defaults)) db$lkp_crops_defaults[as.character(db$lkp_crops_defaults$crop_code) == source, , drop = FALSE]
    add_crop(c0, name, c0$crop_name, cdef, source = list(db_crop = source))
  }
  nice <- function(x) { x <- trimws(x); paste0(toupper(substr(x, 1, 1)), substring(x, 2)) }

  # feeds first: a crop answer with the same name then reuses the feed's crop
  for (i in order(matches$kind != "feed")) {
    v <- as.character(matches$value[i])
    if (is.na(v) || !grepl("^(lib|libcrop|new|newcrop):", v)) next
    name <- nice(matches$answer[i])
    matches$value[i] <- if (startsWith(v, "lib:")) {
      from_library_feed(sub("^lib:", "", v))
    } else if (startsWith(v, "libcrop:")) {
      copy_crop(v, NULL)
    } else if (startsWith(v, "new:lib:")) {
      from_library_feed(sub("^new:lib:", "", v), name)
    } else if (startsWith(v, "new:")) {
      from_db_feed(sub("^new:", "", v), name)
    } else {
      copy_crop(sub("^newcrop:", "", v), name)
    }
  }
  # Every crop added must be the crop of at least one feed, otherwise nothing
  # in the diets can use it: add its main feed, with the values of a feed of
  # the same crop in the library or the database
  feeds_now <- batch_read_raw(path, "lkp_feeditem")
  names(feeds_now) <- trimws(gsub("^\ufeff", "", names(feeds_now)))
  for (code in names(crop_source)) {
    if (code %in% trimws(feeds_now$crop_code)) next
    src <- crop_source[[code]]
    crop_name <- batch_read_raw(path, "lkp_crops")
    names(crop_name) <- trimws(gsub("^\ufeff", "", names(crop_name)))
    crop_name <- crop_name$crop_name[trimws(crop_name$crop_code) == code][1]
    val <- NULL; from <- NULL
    if (!is.null(src$lib_crop) && !is.null(library)) {
      f <- library$feeds[library$feeds$lib_crop_id %in% src$lib_crop, , drop = FALSE]
    } else if (!is.null(src$db_crop)) {
      f <- db$lkp_feeditem[as.character(db$lkp_feeditem$crop_code) == src$db_crop, , drop = FALSE]
      f$source_db <- NA
    } else f <- NULL
    if (!is.null(f) && nrow(f)) {
      # the main product rather than a residue when the crop has both
      main <- !grepl("residue|stover|straw|haulm|husk|bran|peel", f$feed_item_name, ignore.case = TRUE)
      j <- if (any(main)) which(main)[1] else 1
      val <- f[j, ]
      from <- if (is.na(f$source_db[j])) f$feed_item_name[j] else paste0(f$feed_item_name[j], " (", f$source_db[j], ")")
    }
    if (is.null(val)) next
    name <- if (batch_normalise_text(crop_name) %in% feed_names) paste(crop_name, "(main product)") else crop_name
    add_feed(name, val$dm_content, val$me_content, val$cp_content, code, from)
  }
  attr(matches, "added") <- if (length(added)) do.call(rbind, added) else NULL
  matches
}
