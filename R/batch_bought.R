# Feeds bought by the farms ----------------------------------------------------------
#
# CLEANED counts the land, fertiliser, N2O and water of producing a feed off
# the farm when the feed's name carries a tag:
#   OFR  roughage bought (off-farm roughage)
#   OFC  concentrate bought, produced in the country (off-farm concentrate)
#   IP   concentrate imported from another country
# The part of a feed that a farm buys is therefore given to a copy of the feed
# with the tag (and its own crop, which takes the database's default yields:
# the farm's own yields do not describe the supplier's fields).

odk_bought_tags <- c(OFR = "Bought roughage (OFR)", OFC = "Bought concentrate, produced in the country (OFC)",
                     IP = "Imported concentrate (IP)")
odk_tag_pattern <- "\\s+(OFR|OFC|IP)$"

# Name of a feed without its tag, and with a tag
odk_untagged_name <- function(name) trimws(sub(odk_tag_pattern, "", trimws(name)))
odk_feed_tag <- function(name) { m <- regmatches(name, regexpr(odk_tag_pattern, name)); if (length(m)) trimws(m) else NA_character_ }

# Kind of a bought feed when the survey did not ask (older forms)
odk_guess_bought_kind <- function(name) {
  ifelse(grepl("grain|bran|cake|meal|concentrate|pellet|seed|molasses|mineral|germ|pollard|mash|ration|dairy|layer|pig|broiler",
               name, ignore.case = TRUE), "concentrate", "roughage")
}

# Splits the diets into the part grown or collected and the part bought, and
# gives the bought part to the tagged copy of the feed
odk_bought_feeds <- function(diet, db, conv, livestock = NULL, lv_keys = NULL) {
  out <- list(diet = diet, variant_codes = character(0), needed = NULL, assumed = NULL)
  if (!nrow(diet) || !"bought_share" %in% names(diet)) return(out)
  fi <- db$lkp_feeditem
  codes <- as.character(fi$feed_item_code)
  names_db <- as.character(fi$feed_item_name)
  keys_db <- batch_normalise_text(names_db)
  ids <- if (!is.null(livestock)) livestock$Ids[match(diet$lv_key, lv_keys)] else rep(NA_character_, nrow(diet))
  residue_pattern <- as.character(conv$bought_residue_pattern %||% "straw|stover|haulm|residue|husk|hull|chaff|peel|vine")
  unknown_origin <- as.character(conv$bought_origin_unknown %||% "local")
  assumed <- list(); needed <- list(); variants <- character(0)
  extra <- list()
  for (i in which((diet$bought_share > 0) %in% TRUE)) {
    name <- names_db[match(diet$code[i], codes)]
    if (is.na(name)) next
    kind <- diet$bought_kind[i]
    if (is.na(kind) || !kind %in% c("roughage", "concentrate")) {
      kind <- odk_guess_bought_kind(name)
      assumed[[length(assumed) + 1]] <- data.frame(Ids = ids[i], feed_item_code = diet$code[i], field = "bought feed is a",
                                                   value = paste(kind, "(not asked: guessed from the feed name)"), stringsAsFactors = FALSE)
    }
    origin <- diet$bought_origin[i]
    if (kind == "concentrate" && (is.na(origin) || !origin %in% c("local", "imported"))) {
      origin <- unknown_origin
      assumed[[length(assumed) + 1]] <- data.frame(Ids = ids[i], feed_item_code = diet$code[i], field = "where it was produced",
                                                   value = paste(if (origin == "imported") "imported" else "in the country", "(not known)"),
                                                   stringsAsFactors = FALSE)
    }
    tag <- if (kind == "roughage") "OFR" else if (origin %in% "imported") "IP" else "OFC"
    # a feed of the database that already carries the tag is used as it is
    own_tag <- odk_feed_tag(name)
    variant <- if (!is.na(own_tag) && own_tag == tag) diet$code[i] else
      codes[match(batch_normalise_text(paste(odk_untagged_name(name), tag)), keys_db)]
    residue <- diet$source[i] %in% "leftover" || grepl(residue_pattern, name, ignore.case = TRUE)
    src <- if (residue) "bought_residue" else "bought_main"
    if (is.na(variant)) {
      needed[[length(needed) + 1]] <- data.frame(feed_item_code = diet$code[i], feed = name, tag = tag,
                                                 name = paste(odk_untagged_name(name), tag), stringsAsFactors = FALSE)
      # until the tagged feed exists, the bought part is counted without its
      # off-farm land and emissions
      if (diet$bought_share[i] >= 1) diet$source[i] <- "bought"
      next
    }
    variants <- c(variants, variant)
    if (diet$bought_share[i] >= 1) {
      diet$code[i] <- variant; diet$source[i] <- src
    } else {
      row <- diet[i, , drop = FALSE]
      row$code <- variant; row$source <- src; row$amount <- diet$amount[i] * diet$bought_share[i]
      diet$amount[i] <- diet$amount[i] * (1 - diet$bought_share[i])
      extra[[length(extra) + 1]] <- row
    }
  }
  if (length(extra)) diet <- rbind(diet, do.call(rbind, extra))
  out$diet <- diet
  out$variant_codes <- unique(variants)
  if (length(needed)) {
    n <- do.call(rbind, needed)
    out$needed <- n[!duplicated(paste(n$feed_item_code, n$tag)), , drop = FALSE]
    rownames(out$needed) <- NULL
  }
  if (length(assumed)) {
    a <- do.call(rbind, assumed)
    out$assumed <- a[!duplicated(paste(a$Ids, a$feed_item_code, a$field)), , drop = FALSE]
  }
  out
}

# One row per feed a farm buys: how much, of what kind, and how that compares
# with what the diets suggest
odk_bought_table <- function(fl, key_to_id, diet, bought, livestock, lv_keys, season, db, conv, num, col) {
  if (is.null(fl) || !nrow(fl)) return(NULL)
  src <- col(fl, "feed_source"); also <- col(fl, "also_bought") %in% "yes"
  sel <- src %in% "bought" | also
  if (!any(sel)) return(NULL)
  fl <- fl[sel, , drop = FALSE]; src <- src[sel]; also <- also[sel]
  fi <- db$lkp_feeditem
  ids <- unname(key_to_id[col(fl, "PARENT_KEY")])
  feed_key <- batch_normalise_text(col(fl, "feed_name"))
  amount <- num(col(fl, "bought_amount")); per <- col(fl, "bought_per"); months <- num(col(fl, "bought_months"))
  kg_year <- ifelse(per %in% "year", amount, ifelse(per %in% "month", amount * months,
                    ifelse(per %in% "week", amount * 52 / 12 * months, NA)))
  share <- ifelse(src %in% "bought", 1, num(col(fl, "bought_pct")) / 100)

  # Estimated intake of the bought part, from the diets: animals x body weight x
  # intake per kg of body weight x days x share of the feed in the diet
  dmi <- as.numeric(conv$dmi_share_body_weight %||% 0.025)
  d_ids <- livestock$Ids[match(diet$lv_key, lv_keys)]
  number <- num(livestock$number[match(diet$lv_key, lv_keys)])
  bw <- num(livestock$body_weight[match(diet$lv_key, lv_keys)])
  total <- stats::ave(diet$amount, paste(diet$lv_key, diet$season_name), FUN = sum)
  n_seasons <- stats::ave(seq_len(nrow(diet)), diet$lv_key, FUN = function(x) length(unique(diet$season_name[x])))
  days <- num(season$season_length[match(paste(d_ids, diet$season_name), paste(season$Ids, season$season_name))])
  days[is.na(days)] <- 365 / n_seasons[is.na(days)]
  intake <- number * bw * dmi * days * diet$amount / total
  is_bought <- diet$source %in% c("bought", "bought_main", "bought_residue")
  ratio_flag <- as.numeric(conv$bought_check_ratio %||% 2)

  code_of <- function(i) {
    j <- which(d_ids == ids[i] & batch_normalise_text(diet$text) == feed_key[i] & is_bought)
    if (length(j)) diet$code[j[1]] else NA_character_
  }
  code <- vapply(seq_along(ids), code_of, "")
  name <- fi$feed_item_name[match(code, as.character(fi$feed_item_code))]
  tag <- vapply(name, function(x) if (is.na(x)) NA_character_ else odk_feed_tag(x), "", USE.NAMES = FALSE)
  dm <- num(fi$dm_content[match(code, as.character(fi$feed_item_code))]) / 100
  est <- vapply(seq_along(ids), function(i) {
    j <- which(d_ids == ids[i] & batch_normalise_text(diet$text) == feed_key[i] & is_bought)
    if (!length(j) || all(is.na(intake[j]))) NA_real_ else sum(intake[j], na.rm = TRUE)
  }, 0)
  bought_dm <- kg_year * dm
  crop <- as.character(fi$crop_code[match(code, as.character(fi$feed_item_code))])
  yield <- num(db$lkp_crops$dry_yield[match(crop, as.character(db$lkp_crops$crop_code))])
  residue_yield <- num(db$lkp_crops$residue_dry_yield[match(crop, as.character(db$lkp_crops$crop_code))])
  no_yield <- !is.na(code) & !is.na(tag) & !((yield > 0) %in% TRUE | (residue_yield > 0) %in% TRUE)
  note <- ifelse(is.na(code), "Not in any diet: not used.",
          ifelse(is.na(tag), "Counted without the land and emissions of producing it: the tagged feed is not in the database yet.",
          ifelse(no_yield, "The database gives no yield for the crop of this feed, so no off-farm land can be counted: match the answer to a feed with a real crop (e.g. maize grain, wheat bran).",
          ifelse(is.na(kg_year), "Amount bought not known.",
          ifelse(!is.na(est) & est > 0 & (bought_dm / est > ratio_flag | est / bought_dm > ratio_flag),
                 sprintf("Check: the diets suggest about %s kg DM a year, %s than the amount bought.",
                         format(round(est), big.mark = ","), ifelse(est > bought_dm, "more", "less")), "")))))
  out <- data.frame(
    Ids = ids, feed = col(fl, "feed_name"), matched_feed = name,
    counted_as = ifelse(is.na(tag), "Purchased (no off-farm footprint)", unname(odk_bought_tags[tag])),
    origin = unname(c(local = "In the country", imported = "Imported", unknown = "Not known")[col(fl, "bought_origin")]),
    share_bought = share, kg_bought_year = round(kg_year), kg_dm_year = round(bought_dm),
    kg_dm_diets = round(est), note = note, stringsAsFactors = FALSE)
  out$origin[is.na(out$origin)] <- ""
  rownames(out) <- NULL
  out
}

#' Add tagged copies of bought feeds to a parameter database
#'
#' CLEANED counts the land, fertiliser, nitrous oxide and water of producing a
#' feed off the farm when the feed's name ends in a tag: `OFR` (roughage
#' bought), `OFC` (concentrate bought, produced in the country) or `IP`
#' (concentrate imported). [read_odk_export()] lists in `variants` the tagged
#' feeds its farms need that the database does not have yet; this function
#' adds them, each a copy of the feed (same nutritive values and defaults)
#' named with the tag, with its own crop (a copy of the feed's crop named
#' "<crop> (off-farm)") so that the database's default yields are used for it
#' rather than the farm's own. Run [read_odk_export()] again afterwards.
#'
#' @param variants Data frame with `feed_item_code` and `tag` (the `variants`
#'   element returned by [read_odk_export()]).
#' @param path Folder of the user's parameter database (written to).
#' @return Invisibly, a data frame of what was added: `kind`, `code`, `name`,
#'   `copied_from`.
#' @export
add_feed_variants <- function(variants, path) {
  if (is.null(variants) || !nrow(variants)) return(invisible(NULL))
  clean_names <- function(x) { names(x) <- trimws(gsub("^\ufeff", "", names(x))); x }
  feeds <- clean_names(batch_read_raw(path, "lkp_feeditem"))
  crops <- clean_names(batch_read_raw(path, "lkp_crops"))
  feed_defaults <- batch_read_raw(path, "lkp_feeditem_defaults")
  if (!is.null(feed_defaults)) feed_defaults <- clean_names(feed_defaults)
  crop_defaults <- batch_read_raw(path, "lkp_crops_defaults")
  if (!is.null(crop_defaults)) crop_defaults <- clean_names(crop_defaults)
  feed_codes <- trimws(feeds$feed_item_code); crop_codes <- trimws(crops$crop_code)
  feed_keys <- batch_normalise_text(feeds$feed_item_name); crop_keys <- batch_normalise_text(crops$crop_name)
  added <- list()
  for (i in seq_len(nrow(variants))) {
    f <- feeds[feed_codes == as.character(variants$feed_item_code[i]), , drop = FALSE]
    if (!nrow(f)) next
    tag <- variants$tag[i]
    name <- paste(odk_untagged_name(f$feed_item_name[1]), tag)
    if (batch_normalise_text(name) %in% feed_keys) next
    # its own crop, shared by all tagged copies of feeds of that crop
    c0 <- crops[crop_codes == trimws(f$crop_code[1]), , drop = FALSE]
    crop <- trimws(f$crop_code[1])
    if (nrow(c0)) {
      crop_name <- paste(c0$crop_name[1], "(off-farm)")
      k <- match(batch_normalise_text(crop_name), crop_keys)
      if (!is.na(k)) {
        crop <- crop_codes[k]
      } else {
        crop <- batch_next_code(crop_codes)
        row <- c0[1, , drop = FALSE]; row$crop_code <- crop; row$crop_name <- crop_name
        batch_append_raw(path, "lkp_crops", row)
        if (!is.null(crop_defaults)) {
          d <- crop_defaults[trimws(crop_defaults$crop_code) == trimws(c0$crop_code[1]), , drop = FALSE]
          if (nrow(d)) { d <- d[1, , drop = FALSE]; d$crop_code <- crop; batch_append_raw(path, "lkp_crops_defaults", d) }
        }
        crop_codes <- c(crop_codes, crop); crop_keys <- c(crop_keys, batch_normalise_text(crop_name))
        added[[length(added) + 1]] <- data.frame(kind = "crop", code = crop, name = crop_name, copied_from = c0$crop_name[1])
      }
    }
    code <- batch_next_code(feed_codes)
    row <- f[1, , drop = FALSE]; row$feed_item_code <- code; row$crop_code <- crop; row$feed_item_name <- name
    batch_append_raw(path, "lkp_feeditem", row)
    if (!is.null(feed_defaults)) {
      d <- feed_defaults[trimws(feed_defaults$feed_item_code) == trimws(f$feed_item_code[1]), , drop = FALSE]
      if (nrow(d)) {
        d <- d[1, , drop = FALSE]; d$feed_item_code <- code
        if ("source_type" %in% names(d)) d$source_type <- "Main"
        batch_append_raw(path, "lkp_feeditem_defaults", d)
      }
    }
    feed_codes <- c(feed_codes, code); feed_keys <- c(feed_keys, batch_normalise_text(name))
    added[[length(added) + 1]] <- data.frame(kind = "feed", code = code, name = name, copied_from = f$feed_item_name[1])
  }
  invisible(if (length(added)) do.call(rbind, added) else NULL)
}

# Removal shares of the off-farm crops of bought feeds, when the database's
# crop defaults have none (without them the model counts no land)
odk_offfarm_feed_type <- function(feed_items, variant_codes, db, conv) {
  out <- list(feed_type = NULL, assumed = NULL)
  if (!length(variant_codes) || !nrow(feed_items)) return(out)
  fi <- db$lkp_feeditem
  rows <- feed_items[feed_items$feed_item_code %in% variant_codes, , drop = FALSE]
  if (!nrow(rows)) return(out)
  crop <- as.character(fi$crop_code[match(rows$feed_item_code, as.character(fi$feed_item_code))])
  cd <- db$lkp_crops_defaults
  default_of <- function(code, field) {
    if (is.null(cd) || !field %in% names(cd)) return(NA_real_)
    v <- suppressWarnings(as.numeric(cd[[field]][as.character(cd$crop_code) == code]))
    if (length(v) && !is.na(v[1]) && v[1] > 0) v[1] else NA_real_
  }
  removal <- as.numeric(conv$offfarm_removal %||% 0.9)
  ft <- list(); assumed <- list()
  for (i in seq_len(nrow(rows))) {
    residue <- rows$source_type[i] %in% "Residue"
    field <- if (residue) "residue_removal" else "main_product_removal"
    if (!is.na(default_of(crop[i], field))) next
    r <- data.frame(Ids = rows$Ids[i], crop_code = crop[i], stringsAsFactors = FALSE)
    r[[field]] <- as.character(removal)
    if (residue) r$main_product_removal <- as.character(default_of(crop[i], "main_product_removal") %|na|% removal)
    ft[[length(ft) + 1]] <- r
    assumed[[length(assumed) + 1]] <- data.frame(Ids = rows$Ids[i], feed_item_code = rows$feed_item_code[i],
      field = if (residue) "share of crop residues taken (off-farm)" else "share of the crop taken (off-farm)",
      value = as.character(removal), stringsAsFactors = FALSE)
  }
  if (length(ft)) {
    f <- dplyr::bind_rows(ft)
    out$feed_type <- f[!duplicated(paste(f$Ids, f$crop_code)), , drop = FALSE]
  }
  if (length(assumed)) out$assumed <- do.call(rbind, assumed)
  out
}
`%|na|%` <- function(a, b) if (is.na(a)) b else a
