#' Livestock species and animal classes
#'
#' Allowed `animal_class` values for each `species`. Together they select the
#' IPCC Tier 2 energy-requirement equations (maintenance, activity, growth,
#' lactation, pregnancy) and the nitrogen intake and retention equations, so a
#' livestock category is calculated from these fields rather than from its
#' name (`livetype_desc`).
#'
#' \describe{
#'   \item{Cattle, Buffalo}{"Cows" (adult females, lactating or dry), "Adult
#'     males", "Young stock" (steers, heifers and calves).}
#'   \item{Sheep}{"Ewes", "Breeding rams", "Fattening rams", "Lambs".}
#'   \item{Goats}{"Does", "Breeding bucks", "Fattening bucks", "Kids".}
#'   \item{Pigs}{"Lactating/pregnant sows", "Dry sows/boars", "Growers".}
#' }
#'
#' @format A named list of character vectors.
#' @export
livestock_species_classes <- list(
  Cattle = c("Cows", "Adult males", "Young stock"),
  Buffalo = c("Cows", "Adult males", "Young stock"),
  Sheep = c("Ewes", "Breeding rams", "Fattening rams", "Lambs"),
  Goats = c("Does", "Breeding bucks", "Fattening bucks", "Kids"),
  Pigs = c("Lactating/pregnant sows", "Dry sows/boars", "Growers")
)

#' Meat and milk products of each species
#'
#' Labels of the products of a livestock type, set from its species (the
#' quantities come from growth, carcass fraction and milk; results are grouped
#' by species).
#'
#' @format A data frame with columns `species`, `meat_product`, `milk_product`.
#' @export
livestock_species_products <- data.frame(
  species = c("Cattle", "Buffalo", "Sheep", "Goats", "Pigs"),
  meat_product = c("beef", "buffalo", "mutton", "goat", "pork"),
  milk_product = c("cow milk", "buffalo milk", "sheep milk", "goat milk", "N/A"),
  stringsAsFactors = FALSE
)

#' Livestock types no longer offered
#'
#' Types kept in parameter databases so that data collected with them are read,
#' but not offered in new survey forms and batch templates: the pregnant-only
#' cow types (pregnant cows are counted with the dry or the lactating cows;
#' CLEANED counts pregnancy for every cow through the birth interval).
#'
#' @param lt A livestock types table (`lkp_livetype`).
#' @return A logical vector: `TRUE` for the types not offered.
#' @export
livestock_type_retired <- function(lt) {
  grepl("^Cattle - Cows pregnant \\(", as.character(lt$livetype_desc))
}

#' IPCC Tier 2 livestock categories
#'
#' Controlled vocabularies for the livestock fields that select IPCC Tier 2
#' emission factors. Each field feeds exactly one IPCC table, so a livestock
#' category's emissions depend on these fields rather than on its name
#' (`livetype_desc`).
#'
#' \describe{
#'   \item{ipcc_ef_category_t2}{Methane conversion factor (Ym) for enteric
#'     fermentation, IPCC Tables 10.12 and 10.13.}
#'   \item{ipcc_manure_ch4_category}{Maximum methane producing capacity (Bo)
#'     for manure, IPCC Table 10.16 (combined with region and productivity).}
#'   \item{ipcc_n_loss_category}{Nitrogen lost through volatilisation and
#'     leaching from manure management, IPCC Table 10.22.}
#'   \item{ipcc_prp_category}{Emission factor for urine and dung deposited on
#'     pasture, range and paddock (EF3PRP), IPCC Table 11.1.}
#' }
#'
#' @format A named list of character vectors.
#' @export
ipcc_livestock_categories <- list(
  ipcc_ef_category_t2 = c("Dairy cows", "Non-dairy", "Sheep", "Goats", "N/A"),
  ipcc_manure_ch4_category = c("Dairy cattle", "Other cattle", "Buffalo",
                               "Sheep", "Goats", "Swine"),
  ipcc_n_loss_category = c("Dairy Cow", "Other Cattle", "Swine", "Poultry",
                           "Other animals"),
  ipcc_prp_category = c("Cattle, pigs and poultry", "Sheep and other animals")
)

# Default categories of the livestock types shipped with CLEANED. Used only to
# fill in categories for study objects created before these fields existed,
# so that those objects give the same results as before.
legacy_livestock_categories <- data.frame(
  livetype_desc = c(
    "Cattle - Cows (local)", "Cattle - Cows (improved)",
    "Cattle - Cows (high productive)", "Cattle - Adult male",
    "Cattle - Steers/heifers", "Cattle - Steers/heifers (improved)",
    "Cattle - Calves", "Cattle - Calves (improved)",
    "Buffalo - Cows", "Buffalo - Adult male", "Buffalo - Steers/heifers",
    "Buffalo - Calves",
    "Sheep - Ewes", "Sheep - Breeding Rams", "Sheep - Fattening Rams",
    "Sheep - Lambs",
    "Goats - Does", "Goats - Breeding Bucks", "Goats - Fattening Bucks",
    "Goats - Kids",
    "Pigs - lactating/pregnant sows", "Pigs - dry sows/boars", "Pigs - growers"
  ),
  species = c(rep("Cattle", 8), rep("Buffalo", 4), rep("Sheep", 4),
              rep("Goats", 4), rep("Pigs", 3)),
  animal_class = c(
    rep("Cows", 3), "Adult males", rep("Young stock", 4),
    "Cows", "Adult males", rep("Young stock", 2),
    "Ewes", "Breeding rams", "Fattening rams", "Lambs",
    "Does", "Breeding bucks", "Fattening bucks", "Kids",
    "Lactating/pregnant sows", "Dry sows/boars", "Growers"
  ),
  ipcc_manure_ch4_category = c(
    rep("Dairy cattle", 3), rep("Other cattle", 5),
    rep("Buffalo", 4), rep("Sheep", 4), rep("Goats", 4), rep("Swine", 3)
  ),
  ipcc_n_loss_category = c(
    rep("Dairy Cow", 3), rep("Other Cattle", 5),
    rep("Other animals", 12), rep("Swine", 3)
  ),
  ipcc_prp_category = c(
    rep("Cattle, pigs and poultry", 9),
    # "Buffalo - Adult male" was in no pasture group before, so its pasture N
    # was not counted. Kept as NA to reproduce earlier results.
    NA,
    rep("Cattle, pigs and poultry", 2),
    rep("Sheep and other animals", 8),
    rep("Cattle, pigs and poultry", 3)
  ),
  stringsAsFactors = FALSE
)

#' Resolve the species, class and IPCC Tier 2 categories of livestock
#'
#' Makes sure every livestock row has a valid `species` and `animal_class`
#' (see [livestock_species_classes]) and the IPCC Tier 2 category fields
#' listed in [ipcc_livestock_categories]. Rows from older study objects that lack a
#' field get it from the default categories of their `livetype_desc`. Values
#' outside the allowed vocabularies, or missing values that cannot be filled
#' in, stop with an informative error instead of producing wrong emissions.
#'
#' @param livestock A data frame of livestock, as in `para[["livestock"]]`.
#'
#' @return `livestock` with the category fields filled in.
#' @export
resolve_livestock_categories <- function(livestock) {
  livestock <- as.data.frame(livestock, stringsAsFactors = FALSE)
  vocab <- c(list(species = names(livestock_species_classes),
                  animal_class = unique(unlist(livestock_species_classes))),
             ipcc_livestock_categories)
  fields <- names(vocab)
  legacy_idx <- match(livestock$livetype_desc,
                      legacy_livestock_categories$livetype_desc)

  for (field in fields) {
    if (!field %in% names(livestock)) {
      livestock[[field]] <- NA_character_
    }
    values <- as.character(livestock[[field]])
    values[!is.na(values) & trimws(values) == ""] <- NA_character_

    # Fill missing values from the defaults of the shipped livestock types
    if (field %in% names(legacy_livestock_categories)) {
      fill <- is.na(values) & !is.na(legacy_idx)
      values[fill] <- legacy_livestock_categories[[field]][legacy_idx[fill]]
    }

    # Pasture group may legitimately stay NA only for legacy rows
    allow_na <- field == "ipcc_prp_category" & !is.na(legacy_idx)
    missing <- is.na(values) & !allow_na
    if (any(missing)) {
      stop(sprintf(
        "Livestock '%s' has no '%s'. Choose one of: %s.",
        paste(livestock$livetype_desc[missing], collapse = "', '"),
        field, paste(vocab[[field]], collapse = ", ")
      ), call. = FALSE)
    }

    invalid <- !is.na(values) & !values %in% vocab[[field]]
    if (any(invalid)) {
      stop(sprintf(
        "Livestock '%s' has an invalid '%s' ('%s'). Choose one of: %s.",
        paste(livestock$livetype_desc[invalid], collapse = "', '"), field,
        paste(unique(values[invalid]), collapse = "', '"),
        paste(vocab[[field]], collapse = ", ")
      ), call. = FALSE)
    }
    livestock[[field]] <- values
  }

  # The class must be one of the classes of the species
  wrong_class <- !mapply(function(sp, cl) cl %in% livestock_species_classes[[sp]],
                         livestock$species, livestock$animal_class)
  if (any(wrong_class)) {
    stop(sprintf(
      "Livestock '%s': animal_class '%s' does not belong to species '%s'.",
      livestock$livetype_desc[wrong_class], livestock$animal_class[wrong_class],
      livestock$species[wrong_class]
    )[1], call. = FALSE)
  }
  livestock
}

# Manure management labels used by older iCLEANED databases that do not match
# the IPCC tables (Table 10.17 MCF, Table 10.21 EF3), with their IPCC label
manure_system_label_fixes <- c(
  "Pasture / range / paddock" = "Pasture/Range/Paddock",
  "Anaer digester, Low leak, HQ stor, HQ tec." = "Anaer digester, Low leak, HQ stor, HQ tec"
)

#' Replace manure management labels that do not match the IPCC tables
#'
#' @param livestock A data frame of livestock, as in `para[["livestock"]]`.
#' @return `livestock` with the `manureman_*` labels corrected.
#' @export
normalise_manure_systems <- function(livestock) {
  for (column in grep("^manureman_", names(livestock), value = TRUE)) {
    values <- as.character(livestock[[column]])
    fix <- values %in% names(manure_system_label_fixes)
    values[fix] <- unname(manure_system_label_fixes[values[fix]])
    livestock[[column]] <- values
  }
  livestock
}
