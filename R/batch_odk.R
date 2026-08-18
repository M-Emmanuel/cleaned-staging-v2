# Batch input: ODK form and import ------------------------------------------------
#
# The survey only asks what farmers can answer (herd, milk, where the animals
# spend the day, what happens to the manure, what they are fed). Farm conditions
# (soil, climate, rainfall, land management) are not asked: they come from the
# parameter database and are set in the app before study objects are built.
# Feeds and unusual manure practices are written in the farmer's own words and
# matched to the database afterwards (see batch_match_suggestions()).

# Places where animals spend the day: form name, batch field suffix, label
odk_places <- data.frame(
  key = c("stable", "enclosure", "onfarm", "offfarm"),
  field = c("stable", "non_roofed_enclosure", "onfarm_grazing", "offfarm_grazing"),
  label = c("in a stable or shed (with a roof)", "in an open enclosure, boma or kraal (no roof)",
            "grazing on the farm's own land", "grazing outside the farm (communal land, forest, riverbanks or roadsides)"),
  short = c("stable", "enclosure", "on-farm grazing", "off-farm grazing"),
  grazing = c(FALSE, FALSE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

# Litres of milk to kg, and days per month, used to turn the milk answers into
# kg per year
odk_milk_kg_per_litre <- 1.03
# Age groups of the UN Provisional Guidelines on Standard International Age
# Classifications (Statistical Papers, Series M, No. 74), broad groups for adults
# Labels of the age brackets of earlier forms (the age is now asked in years)
odk_age_brackets <- c(`15_24` = "18-24 years", `25_44` = "25-44 years", `45_64` = "45-64 years",
                      `65_plus` = "65 years or over", prefer_not = "Not given")
odk_gender_labels <- c(male = "Male", female = "Female", prefer_not = "Prefer not to say")
odk_consent_text <- paste(
  "Read to the respondent: We are collecting information on how livestock are kept and fed on farms in this area,",
  "to estimate the environmental effects of livestock production and how they could be reduced.",
  "The interview takes about 30 to 45 minutes. Taking part is voluntary: you may refuse, skip any question",
  "or stop at any time, without any consequence for you. Your answers are kept confidential and are only",
  "reported in summaries in which no farm or person can be identified.",
  "Do you have any questions? Do you agree to take part?")
odk_days_per_month <- 365 / 12

# Stable choice names for descriptions (ODK names cannot hold spaces or commas)
odk_choice_names <- function(x) {
  n <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  n <- gsub("^_|_$", "", n)
  n[!grepl("^[a-z]", n)] <- paste0("x", n[!grepl("^[a-z]", n)])
  make.unique(n, sep = "_")
}

#' Manure practices described in plain words
#'
#' The answers the survey offers for "what happens to the manure", each linked to
#' an IPCC manure management system of the parameter database. A database can
#' hold its own list in `lkp_manure_practices`; otherwise the list shipped with
#' the package is used. Practices whose IPCC system is not in the database's
#' `lkp_manureman` are left out.
#'
#' @param parameter_database A list from [read_parameter_database()].
#' @return A data frame with `practice_code`, `label` (the words shown to the
#'   enumerator), `manureman_desc` (IPCC system) and `keywords` (used to
#'   suggest a practice for answers written in free text).
#' @export
batch_manure_practices <- function(parameter_database) {
  p <- parameter_database$lkp_manure_practices
  if (is.null(p)) {
    path <- system.file("extdata", "batch", "manure_practices.csv", package = "cleaned")
    if (!nzchar(path)) path <- file.path("inst", "extdata", "batch", "manure_practices.csv")
    p <- utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
  }
  systems <- trimws(as.character(parameter_database$lkp_manureman$manureman_desc))
  p$manureman_desc <- trimws(p$manureman_desc)
  if (!"part" %in% names(p)) p$part <- ifelse(p$practice_code %in% c("floor", "bedding", "bedding_short"), "stays", "out")
  p[p$manureman_desc %in% systems, , drop = FALSE]
}

#' Write an ODK (XLSForm) questionnaire for batch data collection
#'
#' Creates an XLSForm, to be published on ODK Central (or KoboToolbox), that
#' asks farmers what they can answer, in their own terms, in this order:
#' * the interviewer's name, a consent statement, the respondent's agreement
#'   and signature (the interview ends if the respondent does not agree);
#' * the farm ID and location (GPS point, country, region, district, village)
#'   and the respondent's gender and age bracket;
#' * whether the household keeps livestock (the interview ends if not);
#' * for each livestock type: number of animals, milk (litres per day and
#'   months milked), hours per day in each place, and what happens to the manure
#'   taken out and the manure that stays in each place (plain-language practices
#'   or the farmer's own words), with the percentages collected, used on the
#'   farm and sold;
#' * feeding: the seasons (start and end dates), the feeds used on the farm
#'   (where each comes from; for bought feeds what kind, where it was produced
#'   and how much), then what each livestock type eats in each season;
#' * crops and residue management for the feeds grown on the farm, and manure,
#'   compost and bedding bought.
#'
#' The interviewer, region, district, village, gender and age bracket are
#' stored as `group_*` columns of the batch file, so the results can be
#' compared by each of them.
#'
#' Totals are checked while the enumerator types: a message shows the running
#' total and the form does not let them finish while the hours of a day are
#' not 24 or the seasons do not cover the 365 days of the year. Diet amounts do not need to
#' add up: they are turned into shares of the diet on import.
#'
#' Soil, climate, rainfall and land management are not asked; they come from
#' the parameter database and are set in the app for the batch.
#'
#' The data are imported with [read_odk_export()].
#'
#' @param parameter_database A list from [read_parameter_database()], or the
#'   path of a parameter database folder.
#' @param file Path of the XLSForm (`.xlsx`) to write.
#' @param database_name Name of the parameter database (not shown in the form).
#' @param groups Names of extra farm grouping questions (stored as
#'   `group_<name>`), e.g. `c("site", "system")`. The interviewer, region,
#'   district, village, gender and age bracket are always asked.
#' @param consent_text Consent statement read to the respondent.
#' @param form_id,form_title,version Form settings. `version` defaults to the
#'   date and time.
#' @return `file`, invisibly.
#' @export
write_odk_form <- function(parameter_database, file, database_name = NULL,
                           groups = character(0), form_id = "icleaned_batch",
                           consent_text = odk_consent_text,
                           form_title = NULL, version = format(Sys.time(), "%Y%m%d%H%M")) {
  if (is.character(parameter_database)) {
    if (is.null(database_name)) database_name <- basename(parameter_database)
    db <- read_parameter_database(parameter_database)
  } else {
    db <- parameter_database
  }
  defaults <- batch_db_defaults(db)
  if (is.null(form_title)) form_title <- "iCLEANED farmer survey"

  survey <- list()
  q <- function(type, name = "", label = "", hint = "", required = "", relevant = "",
                constraint = "", constraint_message = "", calculation = "",
                appearance = "", repeat_count = "", default = "") {
    survey[[length(survey) + 1]] <<- data.frame(
      type = type, name = name, label = label, hint = hint, required = required,
      relevant = relevant, constraint = constraint, constraint_message = constraint_message,
      calculation = calculation, appearance = appearance, repeat_count = repeat_count,
      default = default, stringsAsFactors = FALSE)
  }
  choices <- list()
  ch <- function(list_name, name, label, ...) {
    extra <- list(...)
    choices[[length(choices) + 1]] <<- do.call(data.frame, c(
      list(list_name = list_name, name = name, label = label), extra,
      list(stringsAsFactors = FALSE, check.names = FALSE)))
  }
  # A warning shown (no answer box) while a total is wrong. The form itself is
  # stopped by the constraint on the last answer of the total.
  warn <- function(name, relevant, message) {
    q("note", name, paste0("<span style='color:#b3261e'>\u26a0 ", message, "</span>"), relevant = relevant)
  }

  ch("yes_no", c("yes", "no"), c("Yes", "No"))

  # Metadata ------------------------------------------------------------------------
  q("start", "start"); q("end", "end"); q("today", "today")

  # Interviewer and consent ------------------------------------------------------
  ch("gender", c("male", "female", "prefer_not"), c("Male", "Female", "Prefer not to say"))
  q("begin_group", "consent_section", "Consent")
  q("text", "group_interviewer", "Name of the interviewer", required = "yes")
  q("note", "consent_note", consent_text)
  q("note", "adult_note", "The respondent must be an adult (18 or older) to give consent.")
  q("select_one yes_no", "consent", "Does the respondent agree to take part in the survey?", required = "yes")
  q("image", "signature", "Signature of the respondent",
    "Ask the respondent to sign on the screen (or to make a mark).", required = "yes",
    appearance = "signature", relevant = "${consent} = 'yes'")
  q("note", "no_consent_note", "The respondent did not agree: thank them and end the interview here (save the form).",
    relevant = "${consent} = 'no'")
  q("end_group")

  q("begin_group", "survey", "Survey", relevant = "${consent} = 'yes'")
  # Farm and its location ------------------------------------------------------------
  q("begin_group", "farm", "Farm")
  q("text", "Ids", "Farm ID",
    "A code unique to this farm across all interviewers, e.g. the district code and household number (KIS-0123).",
    required = "yes", constraint = "string-length(.) > 0 and not(contains(., ' '))",
    constraint_message = "Use a code without spaces.")
  q("geopoint", "farm_location", "Location of the farm (GPS)",
    "Stand at the homestead and record the location. Leave it if no GPS signal can be found.")
  q("text", "country", "Country", required = "yes")
  q("text", "group_region", "Region, county or province", required = "yes")
  q("text", "group_district", "District or sub-county", required = "yes")
  q("text", "group_village", "Village", required = "yes")
  for (g in setdiff(groups, c("region", "district", "village", "interviewer", "gender", "age_bracket"))) {
    q("text", paste0("group_", g), tools::toTitleCase(gsub("_", " ", g)),
      "Used to group farms in the results.", required = "yes")
  }
  q("end_group")

  # Respondent -------------------------------------------------------------------------
  q("begin_group", "respondent", "Respondent")
  q("select_one gender", "group_gender", "Gender of the respondent", required = "yes")
  q("integer", "respondent_age", "Age of the respondent (years)",
    "Leave empty if the respondent prefers not to say.",
    constraint = ". >= 18 and . <= 120", constraint_message = "The respondent must be 18 or older to take part.")
  q("end_group")

  q("select_one yes_no", "keeps_livestock", "Does the household keep any livestock?", required = "yes")
  q("note", "no_livestock_note", "This survey is about livestock: thank the respondent and end the interview here (save the form).",
    relevant = "${keeps_livestock} = 'no'")
  q("begin_group", "livestock_section", "Livestock", relevant = "${keeps_livestock} = 'yes'")

  # Livestock ----------------------------------------------------------------------------
  lt <- db$lkp_livetype
  lt <- lt[!livestock_type_retired(lt), , drop = FALSE]   # (types no longer offered)
  ch("livetype", as.character(lt$livetype_code), lt$livetype_desc,
     species = lt$species %||% "", animal_class = lt$animal_class %||% "",
     body_weight = lt$body_weight %||% "")
  practices <- batch_manure_practices(db)
  out_p <- practices[practices$part == "out", , drop = FALSE]
  stay_p <- practices[practices$part == "stays", , drop = FALSE]
  ch("practice", c(out_p$practice_code, "other"), c(out_p$label, "Something else (describe what the farmer does)"))
  ch("practice_stay", c(stay_p$practice_code, "other"), c(stay_p$label, "Something else (describe what happens)"))

  q("begin_repeat", "livestock", "Livestock type")
  q("select_one livetype", "livetype_code", "Which animals?",
    "Add one entry for each type kept on the farm (for example milking cows and dry cows separately).",
    required = "yes", appearance = "minimal")
  q("calculate", "lt_name", calculation = "jr:choice-name(${livetype_code}, '${livetype_code}')")
  q("calculate", "lt_species", calculation = "instance('livetype')/root/item[name = ${livetype_code}]/species")
  q("calculate", "lt_class", calculation = "instance('livetype')/root/item[name = ${livetype_code}]/animal_class")
  q("calculate", "lt_weight", calculation = "instance('livetype')/root/item[name = ${livetype_code}]/body_weight")
  q("integer", "number", "How many ${lt_name} does the farmer keep?", required = "yes",
    constraint = ". > 0", constraint_message = "Enter at least 1 animal.")
  q("decimal", "body_weight", "About how heavy is one of the ${lt_name} (kg)?",
    "Only if known. Leave blank to use ${lt_weight} kg.", constraint = ". > 0")

  female <- "${lt_class} = 'Cows' or ${lt_class} = 'Ewes' or ${lt_class} = 'Does' or ${lt_class} = 'Lactating/pregnant sows'"
  sow <- "${lt_class} = 'Lactating/pregnant sows'"
  small_rum <- "${lt_species} = 'Sheep' or ${lt_species} = 'Goats'"
  cattle <- "${lt_species} = 'Cattle' or ${lt_species} = 'Buffalo'"
  ewe_doe <- "${lt_class} = 'Ewes' or ${lt_class} = 'Does'"
  # work (ploughing, carrying): adult males, steers and buffalo (not calves)
  draught <- paste0("${lt_class} = 'Adult males' or contains(${lt_name}, 'Steers') or ",
                    "(${lt_species} = 'Buffalo' and not(contains(${lt_name}, 'Calves')))")
  q("calculate", "young", calculation = paste0(
    "if(${lt_species} = 'Sheep', 'lambs', if(${lt_species} = 'Goats', 'kids', if(${lt_species} = 'Pigs', 'piglets', 'calves')))"))
  # Milk taken by people (annual_milk): cows, ewes and does that are milked
  q("decimal", "milk_litres_day", "When she is milked, how many litres of milk does one animal give per day?",
    "Only the milk taken for the family or for sale, not what the ${young} suckle. 0 if not milked.",
    relevant = sprintf("(%s) and ${lt_species} != 'Pigs'", female),
    constraint = ". >= 0 and . <= 60", constraint_message = "Enter 0 to 60 litres.")
  q("decimal", "milk_months", "For how many of the last 12 months was she milked (for the family or for sale)?",
    relevant = "${milk_litres_day} > 0", required = "yes",
    constraint = ". > 0 and . <= 12", constraint_message = "Enter 1 to 12 months.")
  q("decimal", "annual_growth", "If known: about how many kg did one animal gain in the last 12 months?",
    "Leave blank if not known.", relevant = sprintf("not(%s)", female), constraint = ". >= 0")
  q("decimal", "annual_wool", "Wool or hair per animal in the last 12 months (kg)", "Leave blank if none.",
    relevant = small_rum, constraint = ". >= 0")
  q("decimal", "work_hour", "How many hours per day are they used for ploughing or carrying?",
    "0 if they do not work.", relevant = draught, constraint = ". >= 0 and . <= 24")
  q("decimal", "piglets_relying_on_milk", "What percentage of the piglets live on the sow's milk only (no creep feed)?",
    "100 if all piglets only suckle; less if some also get feed.", relevant = sow,
    constraint = ". >= 0 and . <= 100", constraint_message = "Enter a percentage from 0 to 100.")

  # More details: only what the model uses for these animals
  #   adult weight         - growth of cattle and buffalo
  #   months between births - pregnancy of breeding females
  #   suckling days        - milk for the young of ewes, does and sows
  #   weaning weight, weight at one year - growth of sheep and goats (and suckling of ewes and does)
  q("select_one yes_no", "more_details", "Does the farmer know more about the ${lt_name}, such as the adult weight or the time between births?",
    "Answer 'No' to use the usual values for these animals.", required = "yes", default = "no",
    relevant = sprintf("(%s) or (%s) or (%s)", cattle, small_rum, female))
  q("begin_group", "details", "More about the ${lt_name}", relevant = "${more_details} = 'yes'")
  q("note", "details_note", "Fill in only what the farmer knows.")
  q("decimal", "adult_weight", "Weight of a fully grown animal of this breed (kg)",
    "Used for the energy needed to grow.", relevant = cattle, constraint = ". > 0")
  q("decimal", "birth_interval_months", "Months between two births", relevant = female,
    constraint = ". > 0 and . <= 60", constraint_message = "Enter 1 to 60 months.")
  q("decimal", "lactation_length", "For how many days do the ${young} suckle after birth (until weaning)?",
    "The time the mother feeds her ${young} with milk.", relevant = sprintf("(%s) or (%s)", ewe_doe, sow),
    constraint = ". >= 0 and . <= 730")
  q("decimal", "body_weight_weaning", "Weight of the ${young} at weaning (kg)", relevant = small_rum, constraint = ". >= 0")
  q("decimal", "body_weight_year_one", "Weight at one year of age, or at sale if sold earlier (kg)",
    relevant = small_rum, constraint = ". >= 0")
  q("end_group")

  # Day ------------------------------------------------------------------
  q("begin_group", "time", "Where do the ${lt_name} spend the day and night?")
  q("note", "time_note", "Give the hours in each place over a full day and night (24 hours).")
  for (k in seq_len(nrow(odk_places))) {
    last <- k == nrow(odk_places)
    q("decimal", paste0("hours_", odk_places$key[k]), paste("Hours", odk_places$label[k]),
      required = "yes", default = "0",
      constraint = if (last) ". >= 0 and ${hours_total} = 24" else ". >= 0 and ${hours_total} <= 24",
      constraint_message = if (last) "The hours of all places must add up to 24 (a full day and night)." else
        "The hours cannot add up to more than 24.")
  }
  q("calculate", "hours_total", calculation = paste0("${hours_", odk_places$key, "}", collapse = " + "))
  q("note", "hours_note", "So far: ${hours_total} of 24 hours.")
  warn("hours_fix", "${hours_total} != 24", "The hours must add up to 24.")
  q("end_group")

  # Manure ------------------------------------------------------------------
  q("begin_group", "manure", "Manure of the ${lt_name}")
  q("note", "manure_note", paste(
    "For each place, say how much of the manure is taken out and what is done with it, and what happens to the manure that stays.",
    "Manure dropped while grazing and left on the land is counted as manure on pasture."))
  # Two IPCC systems per place: one for the manure taken out, one for the manure
  # that stays, each with its share
  for (k in which(!odk_places$grazing)) {
    key <- odk_places$key[k]; place <- odk_places$short[k]
    rel <- sprintf("${hours_%s} > 0", key)
    q("decimal", paste0("collected_", key),
      sprintf("Of the manure dropped in the %s, what percentage is taken out (%%)?", place),
      sprintf("0 if it all stays in the %s, 100 if it is all taken out.", place), required = "yes", relevant = rel,
      constraint = ". >= 0 and . <= 100", constraint_message = "Enter a percentage from 0 to 100.")
    q("select_one practice", paste0("practice_", key),
      sprintf("What is done with the manure taken out of the %s?", place),
      required = "yes", relevant = sprintf("%s and ${collected_%s} > 0", rel, key))
    q("text", paste0("practice_", key, "_other"), "Describe in the farmer's words what is done with it",
      required = "yes", relevant = sprintf("selected(${practice_%s}, 'other')", key))
    q("select_one practice_stay", paste0("stay_", key),
      sprintf("What happens to the manure that stays in the %s?", place),
      required = "yes", relevant = sprintf("%s and ${collected_%s} < 100", rel, key))
    q("text", paste0("stay_", key, "_other"), "Describe in the farmer's words what happens to it",
      required = "yes", relevant = sprintf("selected(${stay_%s}, 'other')", key))
  }
  q("decimal", "collected_field", "Of the manure dropped while grazing on the farm's own land, what percentage is collected (%)?",
    "0 if it is all left on the land.", required = "yes", relevant = "${hours_onfarm} > 0",
    constraint = ". >= 0 and . <= 100", constraint_message = "Enter a percentage from 0 to 100.")
  grazing <- "${hours_onfarm} > 0 or ${hours_offfarm} > 0"
  any_collected <- paste0("${collected_", c("stable", "enclosure", "field"), "} > 0", collapse = " or ")
  q("decimal", "used_onfarm", "Of all the manure taken out or collected, what percentage goes to the farmer's own fields or garden (%)?",
    required = "yes", relevant = any_collected,
    constraint = ". >= 0 and . <= 100", constraint_message = "Enter a percentage from 0 to 100.")
  q("decimal", "sold", "And what percentage is sold or given away (%)?", required = "yes",
    relevant = any_collected, constraint = ". >= 0 and . + ${used_onfarm} <= 100",
    constraint_message = "Used on the farm and sold cannot add up to more than 100%.")
  q("decimal", "distance_to_pasture", "How far do the animals walk to the grazing land (km)?",
    relevant = grazing, constraint = ". >= 0")
  q("end_group")

  q("end_repeat")

  # Feeding: seasons, feeds used, then what each livestock type eats -----------------
  q("begin_group", "feeding_section", "Feeding")
  # Seasons ----------------------------------------------------------------------------
  ds <- defaults$season
  default_text <- if (nrow(ds)) paste0(ds$season_name, collapse = ", ") else "one season"
  default_name_expr <- if (nrow(ds)) {
    expr <- "''"
    for (k in rev(seq_len(nrow(ds)))) expr <- sprintf("if(position(..) = %d, '%s', %s)", k, gsub("'", "", ds$season_name[k]), expr)
    expr
  } else "''"
  q("begin_group", "seasons", "Seasons")
  q("select_one yes_no", "own_seasons",
    "Does the farmer feed the animals differently in different seasons of the year (for example in the wet and the dry season)?",
    paste0("If 'No', the feeding is asked once for the whole year (", default_text, ")."),
    required = "yes", default = "no")
  q("integer", "season_count", "How many seasons does the farmer feed differently in?", "For example 2: a wet and a dry season.",
    relevant = "${own_seasons} = 'yes'", required = "yes",
    constraint = ". >= 2 and . <= 6", constraint_message = "Enter 2 to 6 seasons.")
  q("begin_repeat", "season", "Season", relevant = "${own_seasons} = 'yes'", repeat_count = "${season_count}")
  q("calculate", "season_no", calculation = "position(..)")
  q("text", "season_name", "Name of season ${season_no}", "As the farmer calls it, for example: rainy season, dry season.",
    required = "yes")
  # Days between two dates, whatever the year chosen: seasons repeat every year,
  # and a season may run over the new year (November to February)
  # (both dates are moved to the same year without 29 February first)
  day_of <- function(x) sprintf("decimal-date-time(date(concat('2025-', format-date(%s, '%%m-%%d'))))", x)
  days_between <- function(from, to) {
    sprintf("((round(%s - %s) mod 365) + 365) mod 365", day_of(to), day_of(from))
  }
  no_29feb <- "format-date(., '%m-%d') != '02-29'"
  q("calculate", "prev_end", calculation = "if(position(..) > 1, indexed-repeat(${season_end}, ${season}, position(..) - 1), '')")
  q("calculate", "prev_end_label", calculation = "if(${prev_end} != '', format-date(${prev_end}, '%e %b'), '')")
  q("date", "season_start", "When does the ${season_name} start?",
    "Any year: only the day and the month count. Enter the seasons in the order of the year.",
    required = "yes", appearance = "no-calendar",
    constraint = paste0(no_29feb, " and (position(..) = 1 or ", days_between("${prev_end}", "."), " = 1)"),
    constraint_message = "Each season starts the day after the previous one ends (${prev_end_label}). Do not use 29 February.")
  q("date", "season_end", "When does the ${season_name} end?", "The last day of the season (any year).",
    required = "yes", appearance = "no-calendar",
    # the last season closes the year: all seasons together cover 365 days
    constraint = paste0(no_29feb, " and (position(..) < ${season_count} or ${days_total} = 365)"),
    constraint_message = paste("Do not use 29 February. The last season must end the day before the first one starts,",
                               "so that the seasons cover the whole year (365 days)."))
  q("calculate", "season_days", calculation = sprintf(
    "if(${season_start} != '' and ${season_end} != '', %s + 1, 0)", days_between("${season_start}", "${season_end}")))
  q("note", "season_days_note", "${season_name}: ${season_days} days.",
    relevant = "${season_start} != '' and ${season_end} != ''")
  q("end_repeat")
  q("calculate", "days_total", calculation = "sum(${season_days})")
  q("note", "days_note", "The seasons cover ${days_total} of the 365 days of the year.",
    relevant = "${own_seasons} = 'yes'")
  warn("days_fix", "${own_seasons} = 'yes' and count(${season}) = ${season_count} and ${days_total} != 365",
       "The seasons must cover the whole year (365 days), each starting the day after the previous one ends.")
  q("calculate", "n_seasons", calculation = sprintf("if(${own_seasons} = 'yes', ${season_count}, %d)", max(1, nrow(ds))))
  q("end_group")

  # Feeds used on the farm (each named once; the diets pick from this list) ----------
  q("begin_group", "feeds", "Feeds used on the farm")
  q("note", "feeds_note", paste(
    "List every feed the animals ate in the last 12 months, once each, including grazing.",
    "For each feed, say where it comes from and, for feeds that are bought, what kind of feed it is, where it was",
    "produced and how much is bought. Next you will say which animals eat it and how much; then, for feeds grown",
    "on the farm, how the crop is grown."))
  q("begin_repeat", "feed_list", "Feed")
  q("text", "feed_name", "Feed (as the farmer names it)",
    "For example: grazing, maize stover, napier grass, dairy meal.", required = "yes",
    constraint = "count(/data/feeds/feed_list[feed_name = current()]) <= 1",
    constraint_message = "This feed is already in the list.")
  q("select_one feed_source", "feed_source", "Where does the ${feed_name} come from?", required = "yes")
  q("text", "crop_of", "Which crop grown on the farm is it a residue of?", "For example: maize (for maize stover).",
    relevant = "${feed_source} = 'leftover'", required = "yes")
  q("select_one yes_no", "also_bought", "Does the farmer also buy some ${feed_name}?",
    relevant = "${feed_source} = 'grown' or ${feed_source} = 'leftover'", required = "yes")
  q("integer", "bought_pct", "About what share of the ${feed_name} the animals eat is bought? (%)",
    "Best estimate. 50 = half of it is bought.", relevant = "${also_bought} = 'yes'", required = "yes",
    constraint = ". >= 1 and . <= 99", constraint_message = "Enter a share between 1 and 99%.")
  # Bought feeds: what kind, from where, how much (off-farm land and emissions)
  bought_rel <- "${feed_source} = 'bought' or ${also_bought} = 'yes'"
  q("begin_group", "bought_feed", "Buying ${feed_name}", relevant = bought_rel)
  q("select_one bought_kind", "bought_kind", "What kind of feed is the ${feed_name} that is bought?", required = "yes")
  q("select_one bought_origin", "bought_origin", "Where was it produced?",
    "Ask the seller or look at the bag. Imported feeds are counted separately in the results.", required = "yes")
  q("decimal", "bought_amount", "How many kg of ${feed_name} does the farmer buy at a time?",
    "In kg, the way it is bought (fresh or dry). Leave empty if the farmer does not know.",
    constraint = ". > 0", constraint_message = "Enter an amount above 0.")
  q("select_one bought_per", "bought_per", "How often?", relevant = "${bought_amount} > 0", required = "yes")
  q("integer", "bought_months", "In how many of the last 12 months did the farmer buy it?",
    relevant = "${bought_per} = 'week' or ${bought_per} = 'month'", required = "yes",
    constraint = ". >= 1 and . <= 12", constraint_message = "Enter 1 to 12 months.")
  q("calculate", "bought_kg_year", calculation = paste0(
    "if(${bought_per} = 'year', ${bought_amount}, if(${bought_per} = 'month', ${bought_amount} * ${bought_months}, ",
    "if(${bought_per} = 'week', round(${bought_amount} * 4.345 * ${bought_months}, 0), '')))"))
  q("note", "bought_kg_note", "That is about ${bought_kg_year} kg of ${feed_name} bought in the last 12 months.",
    relevant = "${bought_kg_year} != ''")
  q("end_group")
  q("end_repeat")
  q("end_group")

  q("begin_repeat", "feeding", "Feeding of a livestock type", repeat_count = "count(${livestock})")
  q("calculate", "feed_pos", calculation = "position(..)")
  q("calculate", "feed_lt_code", calculation = "indexed-repeat(${livetype_code}, ${livestock}, position(..))")
  q("calculate", "feed_lt_name", calculation = "indexed-repeat(${lt_name}, ${livestock}, position(..))")
  # Diet ------------------------------------------------------------------
  q("begin_group", "diet", "What the ${feed_lt_name} eat")
  q("select_one yes_no", "same_diet", "Do the ${feed_lt_name} eat the same as the animals entered just before?",
    "Answer 'Yes' to copy their feeds and amounts.", relevant = "${feed_pos} > 1", required = "yes", default = "no")
  q("begin_repeat", "diet_season", "Feeding in a season", repeat_count = "${n_seasons}",
    relevant = "${same_diet} != 'yes'")
  q("calculate", "diet_season_name",
    calculation = sprintf("if(${own_seasons} = 'yes', indexed-repeat(${season_name}, ${season}, position(..)), %s)", default_name_expr))
  q("note", "diet_note", "Feeding of the ${feed_lt_name} during: ${diet_season_name}. For each feed of the farm, say how much they get (0 or blank if they do not eat it in this season).")
  q("select_one diet_unit", "diet_unit", "How will the amounts for ${diet_season_name} be given?",
    "Use the same way for every feed of this season.", required = "yes", default = "percent")
  # every feed the farmer reported, in turn (one entry per feed of the list)
  q("begin_repeat", "diet_feed", "Feed", repeat_count = "count(${feed_list})")
  q("calculate", "feed_pick", calculation = "indexed-repeat(${feed_name}, ${feed_list}, position(..))")
  q("decimal", "feed_amount",
    "${feed_pick}: how much do the ${feed_lt_name} get in ${diet_season_name}? (${diet_unit_label})",
    "0 or blank if they do not eat it in this season.",
    constraint = ". >= 0", constraint_message = "Enter 0 or more.")
  q("calculate", "feed_amount_n", calculation = "coalesce(${feed_amount}, 0)")
  q("end_repeat")
  q("calculate", "diet_unit_label", calculation = "jr:choice-name(${diet_unit}, '${diet_unit}')")
  # (summing a number that is never empty keeps the total readable in web forms)
  q("calculate", "diet_total", calculation = "sum(${feed_amount_n})")
  q("note", "diet_total_note", "Total entered for ${diet_season_name}: ${diet_total} (${diet_unit_label}). The application turns the amounts into shares of the diet.")
  warn("diet_pct_fix", "${diet_unit} = 'percent' and ${diet_total} != 100",
       "The percentages for ${diet_season_name} add up to ${diet_total}, not 100: check them with the farmer.")
  warn("diet_empty", "${diet_total} = 0", "No feed has an amount for ${diet_season_name}.")
  q("end_repeat")
  q("end_group")
  q("end_repeat")
  q("end_group")

  # Crops and forages grown ---------------------------------------------------------
  ch("diet_unit", c("percent", "kg_as_fed", "kg_dm"),
     c("% of what they eat", "kg of fresh feed per animal per day (only if measured)",
       "kg of dry matter per animal per day (only if measured)"))
  ch("feed_source", c("grown", "leftover", "bought", "communal"),
     c("Grown on the farm and fed as the crop itself (grass, fodder, grain)",
       "Crop residues of a crop grown on the farm (stover, straw, haulms, peels)",
       "Bought", "Grazed or collected on communal or public land"))
  ch("bought_kind", c("roughage", "concentrate"),
     c("Roughage: grass, hay, straw, stover, silage, fodder, crop residues",
       "Concentrate: grain, bran, oil cake, dairy meal, pellets, other processed feed"))
  ch("bought_origin", c("local", "imported", "unknown"),
     c("Produced in this country", "Imported from another country", "Don't know"))
  ch("bought_per", c("week", "month", "year"), c("Every week", "Every month", "In the whole year"))
  ch("area_unit", c("acre", "hectare", "sqm"), c("acres", "hectares", "square metres"))
  ch("yield_method", c("per_cut", "per_harvest", "per_area", "daily", "unknown"),
     c("It is cut several times a year: the amount of one cut is known (e.g. napier, lucerne)",
       "It is harvested once or a few times a year: the amount of one harvest is known (e.g. maize, beans)",
       "The usual harvest per acre or hectare is known",
       "It is cut and fed every day: the daily amount is known",
       "The amount is not known"))
  ch("state", c("fresh", "dried"), c("Fresh or green, as cut", "Dried (hay, grain, dry stover)"))
  ch("crop_use", c("cut_carry", "grazed", "both"),
     c("Cut and carried to the animals", "Grazed in the field", "Both"))
  ch("fertiliser", c("urea", "dap", "npk", "can", "sulfate"),
     c("Urea", "DAP", "NPK (compound)", "CAN (calcium ammonium nitrate)", "Ammonium sulphate"))
  ch("slope", c("flat", "gentle", "steep", "very_steep"),
     c("Flat", "Gently sloping", "Steep", "Very steep"))
  ch("pasture", c("improved", "normal", "degraded", "severely_degraded"),
     c("Improved: planted, fertilised or well managed", "Normal: natural grass in fair condition",
       "Degraded: thin grass, bare patches", "Severely degraded: mostly bare, eroded"))

  q("begin_group", "crops_intro", "Crops grown for these feeds")
  q("note", "crops_note", paste(
    "Now the questions about how each feed grown on the farm (or the crop its residues come from) is grown.",
    "Bought and communal feeds are skipped. If an answer is not known, leave it blank: the usual values for the area are used."))
  q("end_group")
  # one entry per feed of the list; the questions are asked only for feeds grown on the farm
  q("begin_repeat", "crop", "Crop", repeat_count = "count(${feed_list})")
  q("calculate", "crop_feed", calculation = "indexed-repeat(${feed_name}, ${feed_list}, position(..))")
  q("calculate", "crop_src", calculation = "indexed-repeat(${feed_source}, ${feed_list}, position(..))")
  q("calculate", "crop_of_feed", calculation = "indexed-repeat(${crop_of}, ${feed_list}, position(..))")
  q("calculate", "crop_text", calculation = "if(${crop_src} = 'leftover', ${crop_of_feed}, ${crop_feed})")
  q("begin_group", "crop_q", "${crop_text}", relevant = "${crop_src} = 'grown' or ${crop_src} = 'leftover'")
  q("note", "crop_q_note", "How the ${crop_text} is grown on the farm (fed as: ${crop_feed}).")
  q("decimal", "area", "How much land is under ${crop_text}?", "The whole area, even if mixed with another crop.",
    required = "yes", constraint = ". > 0")
  q("select_one area_unit", "area_unit", "Unit of the area", required = "yes", default = "acre")
  q("calculate", "area_unit_label", calculation = "jr:choice-name(${area_unit}, '${area_unit}')")
  q("select_one crop_use", "crop_use", "How is the ${crop_text} used?", required = "yes")
  q("decimal", "pct_cut", "What percentage is cut and carried (the rest is grazed)?", relevant = "${crop_use} = 'both'",
    required = "yes", constraint = ". >= 0 and . <= 100")

  # Share of what grows that leaves the field (main_product_removal)
  conv_form <- odk_conversions(db)
  q("decimal", "pct_cut_removed", "When the ${crop_text} is cut or harvested, about what percentage of what grows is taken away from the field?",
    sprintf("The rest stays on the field (stubble, losses). Leave blank if not known: %d%% is used.", round(100 * conv_form$removal_cut_carry)),
    relevant = "${crop_use} = 'cut_carry' or ${crop_use} = 'both'",
    constraint = ". >= 0 and . <= 100", constraint_message = "Enter a percentage from 0 to 100.")
  q("decimal", "pct_grazed_eaten", "When the animals graze the ${crop_text}, about what percentage of the grass do they eat?",
    sprintf("The rest is trampled or left standing. Leave blank if not known: %d%% is used.", round(100 * conv_form$removal_grazed)),
    relevant = "${crop_use} = 'grazed' or ${crop_use} = 'both'",
    constraint = ". >= 0 and . <= 100", constraint_message = "Enter a percentage from 0 to 100.")
  q("begin_group", "harvest", "How much ${crop_text} is cut or harvested?", relevant = "${crop_use} != 'grazed'")
  q("note", "harvest_both_note", "Give the amounts of the part that is cut and carried only (not what is grazed).",
    relevant = "${crop_use} = 'both'")
  q("select_one yield_method", "yield_method", "Which does the farmer know best?", required = "yes")
  known <- "${yield_method} != 'unknown' and ${yield_method} != ''"
  q("calculate", "amount_label", calculation = paste0(
    "if(${yield_method} = 'per_cut', 'How much is harvested in one cut', ",
    "if(${yield_method} = 'per_harvest', 'How much is harvested in one harvest', ",
    "if(${yield_method} = 'per_area', concat('How much is harvested per ', substr(${area_unit_label}, 0, string-length(${area_unit_label}) - 1), ' in one harvest'), ",
    "'How much is cut and fed per day')))"))
  q("decimal", "amount", "${amount_label} (kg, whole area)?", relevant = sprintf("%s and ${yield_method} != 'per_area'", known),
    required = "yes", constraint = ". > 0")
  q("decimal", "amount_per_area", "${amount_label} (kg)?", relevant = "${yield_method} = 'per_area'",
    required = "yes", constraint = ". > 0")
  q("select_one state", "amount_state", "Is that amount fresh or dried?", relevant = known, required = "yes")
  q("integer", "times_per_year", "How many times was it ${yield_word} in the last 12 months?",
    relevant = "${yield_method} = 'per_cut' or ${yield_method} = 'per_harvest' or ${yield_method} = 'per_area'",
    required = "yes", constraint = ". > 0 and . <= 24", constraint_message = "Enter 1 to 24 times.")
  q("calculate", "yield_word", calculation = "if(${yield_method} = 'per_cut', 'cut', 'harvested')")
  q("integer", "days_fed", "For how many days in the last 12 months was it cut and fed?", relevant = "${yield_method} = 'daily'",
    required = "yes", constraint = ". > 0 and . <= 366")
  q("calculate", "kg_year", calculation = paste0(
    "round(if(${yield_method} = 'per_cut' or ${yield_method} = 'per_harvest', ${amount} * ${times_per_year}, ",
    "if(${yield_method} = 'per_area', ${amount_per_area} * ${times_per_year} * ${area}, ",
    "if(${yield_method} = 'daily', ${amount} * ${days_fed}, 0))), 0)"))
  q("note", "kg_year_note", "That is about ${kg_year} kg (${amount_state}) of ${crop_text} in the last 12 months from ${area} ${area_unit_label}. Check this with the farmer.",
    relevant = known)
  q("end_group")

  q("begin_group", "residues", "Crop residues after harvest")
  q("select_one yes_no", "has_residues", "Does the ${crop_text} leave stover, straw or other crop residues after harvest?",
    required = "yes", default = "no")
  res <- "${has_residues} = 'yes'"
  q("note", "res_note", "What happens to the crop residues? Give percentages that add up to 100.", relevant = res)
  q("decimal", "pct_res_removed", "Taken away or fed to the animals (%)", relevant = res, required = "yes", default = "0",
    constraint = ". >= 0 and ${res_total} <= 100", constraint_message = "The percentages cannot add up to more than 100.")
  q("decimal", "pct_res_grazed", "Grazed in the field (%)", relevant = res, required = "yes", default = "0",
    constraint = ". >= 0 and ${res_total} <= 100", constraint_message = "The percentages cannot add up to more than 100.")
  q("decimal", "pct_res_burned", "Burned in the field (%)", relevant = res, required = "yes", default = "0",
    constraint = ". >= 0 and ${res_total} <= 100", constraint_message = "The percentages cannot add up to more than 100.")
  q("decimal", "pct_res_left", "Left on the field as mulch, or ploughed in (%)", relevant = res, required = "yes", default = "0",
    constraint = ". >= 0 and ${res_total} = 100", constraint_message = "The four percentages must add up to 100.")
  q("calculate", "res_total", calculation = "${pct_res_removed} + ${pct_res_grazed} + ${pct_res_burned} + ${pct_res_left}")
  q("note", "res_total_note", "So far: ${res_total} of 100%.", relevant = res)
  warn("res_fix", sprintf("%s and ${res_total} != 100", res), "The percentages of the crop residues must add up to 100.")
  q("decimal", "res_amount", "If known: how many kg of stover or straw are collected in one harvest (whole area)?",
    "Leave blank if not known.", relevant = res, constraint = ". > 0")
  q("end_group")

  q("begin_group", "inputs", "Inputs and field")
  q("select_one yes_no", "intercrop", "Is the ${crop_text} grown mixed with another crop?", required = "yes", default = "no")
  q("text", "intercrop_with", "Which crop?", relevant = "${intercrop} = 'yes'")
  q("decimal", "intercrop_pct", "What share of the field is taken by the other crop (%)?",
    relevant = "${intercrop} = 'yes'", required = "yes", constraint = ". > 0 and . < 100",
    constraint_message = "Enter a percentage between 0 and 100.")
  q("select_one yes_no", "manure_applied", "Is manure put on the ${crop_text}?", required = "yes", default = "no")
  q("decimal", "manure_pct", "What percentage of the manure used on the farm goes to the ${crop_text}?",
    relevant = "${manure_applied} = 'yes'", required = "yes", constraint = ". > 0 and . <= 100")
  q("select_multiple fertiliser", "fertilisers", "Which fertilisers are put on the ${crop_text}?",
    "Leave empty if none.")
  fert <- data.frame(key = c("urea", "dap", "npk", "can", "sulfate"),
                     label = c("Urea", "DAP", "NPK", "CAN", "ammonium sulphate"), stringsAsFactors = FALSE)
  for (k in seq_len(nrow(fert))) {
    rel <- sprintf("selected(${fertilisers}, '%s')", fert$key[k])
    q("decimal", paste0("fert_", fert$key[k]), sprintf("How many kg of %s were put on the ${crop_text} in the last 12 months (whole area)?", fert$label[k]),
      relevant = rel, required = "yes", constraint = ". > 0")
  }
  q("text", "npk_grade", "What is written on the NPK bag (for example 17:17:17 or 23-23-0)?",
    "Leave blank if not known.", relevant = "selected(${fertilisers}, 'npk')")
  q("select_one slope", "slope", "Is the field flat or sloping?", "Look at the field if you can.")
  # Pasture condition: only for grass (grassland management class)
  q("select_one yes_no", "is_grass", "Is the ${crop_text} a grass or pasture?",
    "For example napier, rhodes, brachiaria or natural pasture. 'No' for food and fodder crops such as maize or beans.",
    required = "yes")
  q("select_one pasture", "pasture_condition", "How does the ${crop_text} look?",
    "Look at the field if you can.", relevant = "${is_grass} = 'yes'", required = "yes")
  q("end_group")
  q("end_group")
  q("end_repeat")

  # Inputs bought for the whole farm -------------------------------------------------------
  q("begin_group", "bought", "Manure, compost and bedding bought")
  for (b in list(c("manure", "manure"), c("compost", "compost"), c("bedding", "bedding (straw, sawdust)"))) {
    q("select_one yes_no", paste0("bought_", b[1]), sprintf("Did the farmer buy %s in the last 12 months?", b[2]),
      required = "yes", default = "no")
    rel <- sprintf("${bought_%s} = 'yes'", b[1])
    q("decimal", paste0("bought_", b[1], "_amount"), sprintf("How many kg of %s were bought in the last 12 months?", b[2]),
      relevant = rel, required = "yes", constraint = ". > 0")
  }
  q("end_group")
  q("end_group")  # livestock_section
  q("end_group")  # survey

  survey <- do.call(rbind, survey)
  choices <- do.call(rbind, lapply(choices, function(x) {
    for (col in c("species", "animal_class", "body_weight")) if (!col %in% names(x)) x[[col]] <- ""
    x[, c("list_name", "name", "label", "species", "animal_class", "body_weight")]
  }))
  settings <- data.frame(form_title = form_title, form_id = form_id, version = version,
                         instance_name = "concat(${Ids}, ' - ', ${group_village})", stringsAsFactors = FALSE)

  wb <- openxlsx::createWorkbook()
  header <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9E1F2")
  for (sh in list(list("survey", survey), list("choices", choices), list("settings", settings))) {
    openxlsx::addWorksheet(wb, sh[[1]])
    openxlsx::writeData(wb, sh[[1]], sh[[2]], headerStyle = header)
    openxlsx::freezePane(wb, sh[[1]], firstRow = TRUE)
    openxlsx::setColWidths(wb, sh[[1]], cols = seq_len(ncol(sh[[2]])), widths = "auto")
  }
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  xlsx_drop_dangling_parts(file)
  invisible(file)
}

# Text matching -------------------------------------------------------------------

#' Normalise farmers' answers for matching
#' @param x Character vector.
#' @return Lower-case words without punctuation, single-spaced.
#' @export
batch_normalise_text <- function(x) {
  x <- tolower(iconv(as.character(x), to = "ASCII//TRANSLIT", sub = ""))
  x <- gsub("[^a-z0-9 ]+", " ", x)
  trimws(gsub(" +", " ", x))
}

# Similarity of two normalised texts (0-1): spelling distance and shared words
batch_text_similarity <- function(a, b) {
  if (!nzchar(a) || !nzchar(b)) return(0)
  spell <- 1 - as.numeric(utils::adist(a, b)) / max(nchar(a), nchar(b))
  wa <- strsplit(a, " ")[[1]]; wb <- strsplit(b, " ")[[1]]
  word_hit <- function(w, ws) any(vapply(ws, function(v) w == v ||
    (nchar(w) >= 4 && nchar(v) >= 4 && (startsWith(v, w) || startsWith(w, v) ||
      as.numeric(utils::adist(w, v)) <= 1)), TRUE))
  shared <- mean(vapply(wa, word_hit, TRUE, ws = wb))
  max(spell, 0.9 * shared * min(1, length(wb) / max(1, length(wa))) + 0.1 * shared, 0.95 * shared)
}

#' Suggest database matches for farmers' answers
#'
#' For feeds and crops named in the farmer's words, and for manure practices
#' described in free text, proposes the closest feed (`lkp_feeditem`), crop
#' (`lkp_crops`) or IPCC manure system (`lkp_manureman`). Answers confirmed before are stored in the
#' database table `lkp_answer_matches` and are matched again automatically.
#'
#' @param answers Data frame with `kind` ("feed", "crop" or "manure") and `answer`.
#' @param parameter_database A list from [read_parameter_database()].
#' @param library Optional library from [build_feed_library()]: feeds and
#'   crops that are not in the database are looked for there too, and
#'   suggested as `"lib:<lib_id>"` or `"libcrop:<lib_crop_id>"` (source
#'   "library"; add them with [add_matches_to_database()]).
#' @return `answers` with `key` (normalised answer), `value` (feed code, crop
#'   code or manure system; `NA` when nothing fits), `value_label`, `score` (0-1) and
#'   `source` ("remembered", "same name", "suggested", "library" or "no match").
#' @export
batch_match_suggestions <- function(answers, parameter_database, library = NULL) {
  db <- parameter_database
  answers$key <- batch_normalise_text(answers$answer)
  saved <- db$lkp_answer_matches
  if (!is.null(saved) && nrow(saved)) saved$key <- batch_normalise_text(saved$answer)
  fi <- db$lkp_feeditem
  feed_keys <- batch_normalise_text(fi$feed_item_name)
  practices <- batch_manure_practices(db)
  systems <- trimws(as.character(db$lkp_manureman$manureman_desc))
  out <- lapply(seq_len(nrow(answers)), function(i) {
    kind <- answers$kind[i]; key <- answers$key[i]
    hit <- if (!is.null(saved) && nrow(saved)) which(saved$kind == kind & saved$key == key) else integer(0)
    if (length(hit)) return(list(value = as.character(saved$value[hit[1]]), score = 1, source = "remembered"))
    if (kind == "crop") {
      crops <- db$lkp_crops
      cand <- c(batch_normalise_text(crops$crop_name), feed_keys,
                if (!is.null(saved) && nrow(saved)) saved$key[saved$kind == "crop"])
      code <- c(as.character(crops$crop_code), as.character(fi$crop_code),
                if (!is.null(saved) && nrow(saved)) as.character(saved$value[saved$kind == "crop"]))
      same <- which(cand == key)
      if (length(same)) return(list(value = code[same[1]], score = 1, source = "same name"))
      s <- vapply(cand, batch_text_similarity, 0, a = key)
      if (!length(s) || max(s) < 0.35) return(list(value = NA_character_, score = if (length(s)) max(s) else 0, source = "no match"))
      return(list(value = code[which.max(s)], score = max(s), source = "suggested"))
    }
    if (kind == "feed") {
      same <- which(feed_keys == key)
      if (length(same)) return(list(value = as.character(fi$feed_item_code[same[1]]), score = 1, source = "same name"))
      cand <- c(feed_keys, if (!is.null(saved) && nrow(saved)) saved$key[saved$kind == "feed"])
      code <- c(as.character(fi$feed_item_code), if (!is.null(saved) && nrow(saved)) as.character(saved$value[saved$kind == "feed"]))
      s <- vapply(cand, batch_text_similarity, 0, a = key)
      if (!length(s) || max(s) < 0.35) return(list(value = NA_character_, score = if (length(s)) max(s) else 0, source = "no match"))
      return(list(value = code[which.max(s)], score = max(s), source = "suggested"))
    }
    # manure: the farmer's description against the practice keywords
    words <- strsplit(key, " ")[[1]]
    hits <- vapply(seq_len(nrow(practices)), function(k) {
      kw <- batch_normalise_text(strsplit(practices$keywords[k], ";", fixed = TRUE)[[1]])
      sum(vapply(kw, function(w) grepl(paste0("\\b", w), key) || any(startsWith(words, w)), TRUE))
    }, 0)
    exact <- which(batch_normalise_text(systems) == key)
    if (length(exact)) return(list(value = systems[exact[1]], score = 1, source = "same name"))
    if (!length(hits) || max(hits) == 0) return(list(value = NA_character_, score = 0, source = "no match"))
    list(value = practices$manureman_desc[which.max(hits)], score = min(0.9, 0.5 + 0.2 * max(hits)), source = "suggested")
  })
  # Feeds and crops not in this database: look in the library of all databases
  if (!is.null(library)) {
    lib_f <- library$feeds[!library$feeds$key %in% feed_keys, , drop = FALSE]
    crop_keys <- batch_normalise_text(db$lkp_crops$crop_name)
    lib_c <- library$crops[!library$crops$key %in% crop_keys, , drop = FALSE]
    for (i in seq_along(out)) {
      kind <- answers$kind[i]
      if (!kind %in% c("feed", "crop") || out[[i]]$source %in% c("remembered", "same name")) next
      cand <- if (kind == "feed") lib_f else lib_c
      if (!nrow(cand)) next
      sc <- vapply(cand$key, batch_text_similarity, 0, a = answers$key[i])
      best <- which.max(sc)
      if (sc[best] >= 0.35 && sc[best] > out[[i]]$score + 0.05) {
        out[[i]] <- list(
          value = if (kind == "feed") paste0("lib:", cand$lib_id[best]) else paste0("libcrop:", cand$lib_crop_id[best]),
          score = sc[best], source = "library",
          label = paste0(if (kind == "feed") cand$feed_item_name[best] else cand$crop_name[best], " (from ", cand$source_db[best], ")"))
      }
    }
  }
  answers$value <- vapply(out, function(o) o$value %||% NA_character_, "")
  answers$score <- round(vapply(out, function(o) o$score, 0), 2)
  answers$source <- vapply(out, function(o) o$source, "")
  lib_label <- vapply(out, function(o) o$label %||% NA_character_, "")
  answers$value_label <- ifelse(
    answers$kind == "feed",
    fi$feed_item_name[match(answers$value, as.character(fi$feed_item_code))],
    ifelse(answers$kind == "crop",
           db$lkp_crops$crop_name[match(answers$value, as.character(db$lkp_crops$crop_code))],
           answers$value))
  answers$value_label <- ifelse(is.na(lib_label), answers$value_label, lib_label)
  answers
}

#' Remember confirmed matches of farmers' answers
#'
#' Adds the confirmed matches to the `lkp_answer_matches.csv` table of a
#' parameter database folder, so the same answers are matched automatically in
#' later surveys.
#'
#' @param matches Data frame with `kind`, `answer` and `value` (feed code or
#'   manure system; blank means "leave out").
#' @param path Folder of the parameter database.
#' @return The updated table, invisibly.
#' @export
remember_answer_matches <- function(matches, path) {
  f <- file.path(path, "lkp_answer_matches.csv")
  old <- if (file.exists(f)) utils::read.csv(f, stringsAsFactors = FALSE, colClasses = "character") else
    data.frame(kind = character(0), answer = character(0), value = character(0))
  new <- matches[, c("kind", "answer", "value")]
  new$value <- ifelse(is.na(new$value), "", as.character(new$value))
  key <- function(d) paste(d$kind, batch_normalise_text(d$answer))
  all <- rbind(old[!key(old) %in% key(new), , drop = FALSE], new)
  utils::write.csv(all, f, row.names = FALSE)
  invisible(all)
}

#' Turn ODK data into batch input
#'
#' Reads the data collected with the form from [write_odk_form()] and turns
#' them into the sheets of the batch template, ready for
#' [validate_batch_input()], [build_study_objects()] and [run_batch()], or to be
#' saved as an Excel batch file with [write_batch_file()].
#'
#' Answers are converted to what the model needs: hours per day become
#' fractions of the day, seasons become the number of days between their start and end dates (365 in total), milk in
#' litres per day and months milked becomes kg per year, percentages of manure
#' collected, used and sold become 0 to 1, manure practices become IPCC manure
#' systems (time spent grazing is "Pasture/Range/Paddock"), and the feed
#' amounts of each season become shares of the diet that add up to 100.
#'
#' Feeds and manure practices written in the farmer's words must be matched
#' to the database first: the result lists them in `to_match` with
#' suggestions (see [batch_match_suggestions()]). Call the function again with
#' the confirmed table as `matches` to get the batch input.
#'
#' @param export The collected data: the `.zip` file downloaded from ODK
#'   Central (CSV files), the `.xlsx` file exported from KoboToolbox (type
#'   "XLS", with "XML values and headers"), a folder with ODK Central CSV
#'   files, or a named list of data frames (main table and one per repeat,
#'   with ODK's `KEY` and `PARENT_KEY` columns).
#' @param form The XLSForm used for the survey, or the parameter database it
#'   was made from (a list from [read_parameter_database()] or a folder path).
#' @param parameter_database A list from [read_parameter_database()], or the
#'   path of a parameter database folder (for feed and manure matching).
#' @param matches Confirmed matches: a data frame with `kind`, `answer` and
#'   `value`; a blank or missing `value` for a feed leaves that feed out.
#' @param include_rejected Keep submissions whose review state is "rejected".
#' @param library Optional feed library from [build_feed_library()], used to
#'   suggest feeds and crops that the database does not have.
#' @return A list with `to_match` (every distinct free-text answer with its
#'   suggested or confirmed match and the number of farms that gave it),
#'   `input` (the batch input; `NULL` while some answers are not matched) and
#'   `conversions` (for each crop of each farm, how the farmer's quantities
#'   were turned into a dry-matter yield, with a flag for unusual values) and
#'   `assumed` (values chosen when neither the answers nor the database give
#'   one: land cover, slope, the share of a bought crop taken from the
#'   supplier's field, the kind or origin of a bought feed; see
#'   [batch_survey_conversions()]), `bought` (one row per feed a farm buys:
#'   how it is counted, share bought, kg bought per year, kg dry matter bought
#'   against what the diets suggest, and a note to check) and `variants`
#'   (tagged feeds the farms need that the database does not have; add them
#'   with [add_feed_variants()] and read again).
#'
#' Feeds bought: the part of a feed that a farm buys is given to a copy of the
#' feed whose name ends in `OFR` (roughage), `OFC` (concentrate produced in
#' the country) or `IP` (imported concentrate), so CLEANED counts the land,
#' fertiliser, N2O and water of producing it off the farm, apart from the
#' farm's own. Until that copy exists the bought part is counted as
#' `Purchased` (no off-farm footprint).
#' @export
read_odk_export <- function(export, form, parameter_database, matches = NULL,
                            include_rejected = FALSE, library = NULL) {
  db <- if (is.character(parameter_database)) read_parameter_database(parameter_database) else parameter_database
  if (is.list(form) || (is.character(form) && dir.exists(form))) {
    tmp <- tempfile(fileext = ".xlsx")
    write_odk_form(form, tmp, database_name = "database")
    form <- tmp
  }
  tables <- odk_read_tables(export)
  settings <- openxlsx::read.xlsx(form, sheet = "settings")
  find <- function(suffix) {
    hit <- names(tables)[names(tables) == suffix | endsWith(names(tables), paste0("-", suffix))]
    if (length(hit)) tables[[hit[1]]] else NULL
  }
  main <- tables[[odk_main_table(names(tables), settings$form_id)]]
  if (is.null(main)) stop("No main submissions table found in the ODK export.", call. = FALSE)
  if (!include_rejected && "ReviewState" %in% names(main)) {
    main <- main[is.na(main$ReviewState) | main$ReviewState != "rejected", , drop = FALSE]
  }
  num <- function(x) suppressWarnings(as.numeric(x))
  col <- function(df, name) if (!is.null(df) && name %in% names(df)) as.character(df[[name]]) else rep(NA_character_, NROW(df))
  if (any(col(main, "own_seasons") %in% c("Yes", "No"))) {
    stop("The export contains the answer labels ('Yes', 'No') instead of their values. ",
         "Export again with 'XML values and headers' (KoboToolbox) or the default CSV download (ODK Central).",
         call. = FALSE)
  }

  # Interviews without consent, and households without livestock, are left out
  reason <- ifelse(col(main, "consent") %in% "no", "The respondent did not agree to take part.",
            ifelse(col(main, "keeps_livestock") %in% "no", "The household keeps no livestock.", NA))
  excluded <- data.frame(Ids = col(main, "Ids")[!is.na(reason)], interviewer = col(main, "group_interviewer")[!is.na(reason)],
                         reason = reason[!is.na(reason)], stringsAsFactors = FALSE)
  main <- main[is.na(reason), , drop = FALSE]

  # Farms ------------------------------------------------------------------
  # (farms are named by their ID; forms before asked a farm name)
  farm_name <- col(main, "farm_name")
  farm <- data.frame(Ids = col(main, "Ids"), farm_name = ifelse(is.na(farm_name) | farm_name == "", col(main, "Ids"), farm_name),
                     stringsAsFactors = FALSE)
  # Each interview is its own farm: interviews given the same farm ID (e.g. by
  # two enumerators) are told apart by the interviewer, then by a number, so
  # that their answers are never merged; the changes are reported
  renamed <- NULL
  ids <- farm$Ids
  ids[is.na(ids) | !nzchar(trimws(ids))] <- "farm"
  if (anyDuplicated(ids)) {
    shared <- ids %in% ids[duplicated(ids)]
    who <- gsub("[^A-Za-z0-9]+", "", col(main, "group_interviewer") %||% rep(NA, nrow(main)))
    new <- ifelse(shared & !is.na(who) & nzchar(who), paste0(ids, "-", who), ids)
    again <- new %in% new[duplicated(new)]
    new[again] <- paste0(new[again], "-", stats::ave(seq_along(new), new, FUN = seq_along)[again])
    renamed <- data.frame(original_id = farm$Ids[shared], new_id = new[shared],
                          interviewer = col(main, "group_interviewer")[shared] %||% NA, stringsAsFactors = FALSE)
    farm$Ids <- new
  }
  for (g in grep("^group_", names(main), value = TRUE)) farm[[g]] <- col(main, g)
  if ("group_gender" %in% names(farm)) farm$group_gender <- odk_label(farm$group_gender, odk_gender_labels)
  if ("group_age_bracket" %in% names(farm)) farm$group_age_bracket <- odk_label(farm$group_age_bracket, odk_age_brackets)
  # age in years (forms since November 2026): kept as it is; the age ranges used
  # to compare results are chosen when the results are looked at (they differ
  # by region and study)
  if ("respondent_age" %in% names(main)) {
    farm$respondent_age <- suppressWarnings(as.numeric(col(main, "respondent_age")))
  }
  if ("country" %in% names(main)) farm$country <- col(main, "country")
  gps <- odk_geopoint(main)
  if (any(!is.na(gps$latitude))) { farm$latitude <- gps$latitude; farm$longitude <- gps$longitude }
  key_to_id <- stats::setNames(farm$Ids, col(main, "KEY"))
  own <- col(main, "own_seasons") %in% "yes"

  # Seasons: dates -> days that add up to 365 ------------------------------
  st <- find("season")
  season <- data.frame(Ids = character(0), season_name = character(0), season_length = character(0))
  if (!is.null(st) && nrow(st)) {
    ids <- unname(key_to_id[col(st, "PARENT_KEY")])
    # start and end dates: days counted between them (any year; a season may
    # run over the new year); forms before asked the months of each season
    same_year <- function(x) {
      # text dates ("2026-04-01", ODK Central) or Excel date numbers (Kobo)
      serial <- suppressWarnings(as.numeric(x))
      d <- ifelse(!is.na(serial), format(as.Date(serial, origin = "1899-12-30")), substr(x, 1, 10))
      as.Date(paste0("2025-", substr(d, 6, 10)), optional = TRUE)
    }
    start <- same_year(col(st, "season_start")); end <- same_year(col(st, "season_end"))
    by_dates <- (as.numeric(end - start) %% 365) + 1
    months <- num(col(st, "season_months"))
    st <- data.frame(Ids = ids, season_name = col(st, "season_name"),
                     share = ifelse(is.na(by_dates), months * 365 / 12, by_dates), stringsAsFactors = FALSE)
    st <- st[!is.na(st$Ids) & st$Ids %in% farm$Ids[own], , drop = FALSE]
    season <- do.call(rbind, c(list(season), lapply(split(st, factor(st$Ids, unique(st$Ids))), function(s) {
      # the form makes the dates cover the 365 days; otherwise shares of 365
      days <- if (isTRUE(sum(s$share) == 365)) s$share else round(s$share / sum(s$share) * 365)
      days[length(days)] <- 365 - sum(days[-length(days)])
      data.frame(Ids = s$Ids, season_name = s$season_name, season_length = as.character(days), stringsAsFactors = FALSE)
    })))
  }
  default_seasons <- batch_db_defaults(db)$season$season_name

  # Livestock --------------------------------------------------------------
  lv <- find("livestock")
  if (is.null(lv)) lv <- data.frame(KEY = character(0), PARENT_KEY = character(0))
  ids <- unname(key_to_id[col(lv, "PARENT_KEY")])
  practices <- batch_manure_practices(db)
  prp <- trimws(as.character(db$lkp_manureman$manureman_desc))
  prp <- if ("Pasture/Range/Paddock" %in% prp) "Pasture/Range/Paddock" else NA_character_

  # Feeding entries (one per livestock entry, after the livestock) ---------
  feeding <- find("feeding")
  feeding_to_lv <- character(0)
  if (!is.null(feeding) && nrow(feeding)) {
    lv_key_of <- vapply(seq_len(nrow(feeding)), function(i) {
      same_farm <- which(col(lv, "PARENT_KEY") == col(feeding, "PARENT_KEY")[i])
      hit <- same_farm[col(lv, "livetype_code")[same_farm] %in% col(feeding, "feed_lt_code")[i]]
      if (!length(hit)) {
        # by position within the farm
        pos <- sum(col(feeding, "PARENT_KEY")[seq_len(i)] == col(feeding, "PARENT_KEY")[i])
        hit <- same_farm[pos]
      }
      if (length(hit) && !is.na(hit[1])) col(lv, "KEY")[hit[1]] else NA_character_
    }, "")
    feeding_to_lv <- stats::setNames(lv_key_of, col(feeding, "KEY"))
  } else feeding <- NULL

  # Feeds per livestock entry and season -----------------------------------
  ds <- find("diet_season"); df <- find("diet_feed")
  diet <- data.frame(lv_key = character(0), season_name = character(0), unit = character(0), text = character(0),
                     source = character(0), amount = numeric(0))
  if (!is.null(ds) && nrow(ds) && !is.null(df) && nrow(df)) {
    si <- match(col(df, "PARENT_KEY"), col(ds, "KEY"))
    # feeds are picked from the farm's feed list (forms before: typed in the diet)
    picked <- if ("feed_pick" %in% names(df)) col(df, "feed_pick") else col(df, "feed_text")
    ds_parent <- col(ds, "PARENT_KEY")
    # forms where the feeding of each livestock type follows the livestock:
    # the feeding entry points to the livestock entry of the same farm and type
    if (!is.null(feeding)) ds_parent <- unname(feeding_to_lv[ds_parent])
    diet <- data.frame(lv_key = ds_parent[si], season_name = col(ds, "diet_season_name")[si],
                       unit = col(ds, "diet_unit")[si], text = picked,
                       source = col(df, "feed_source"), amount = num(col(df, "feed_amount")), stringsAsFactors = FALSE)
    diet <- diet[!is.na(diet$lv_key) & !is.na(diet$text) & nzchar(trimws(diet$text)), , drop = FALSE]
  }
  # Where each feed comes from: the farm's feed list
  fl <- find("feed_list")
  diet$bought_share <- ifelse(diet$source %in% "bought", 1, 0)
  diet$bought_kind <- rep(NA_character_, nrow(diet)); diet$bought_origin <- rep(NA_character_, nrow(diet))
  if (!is.null(fl) && nrow(fl) && nrow(diet)) {
    fl_ids <- unname(key_to_id[col(fl, "PARENT_KEY")])
    diet_ids <- unname(key_to_id[col(lv, "PARENT_KEY")[match(diet$lv_key, col(lv, "KEY"))]])
    hit <- match(paste(diet_ids, batch_normalise_text(diet$text)),
                 paste(fl_ids, batch_normalise_text(col(fl, "feed_name"))))
    diet$source <- ifelse(is.na(hit), diet$source, col(fl, "feed_source")[hit])
    # share of the feed that is bought: all of it, or the share given when the
    # farmer also grows it
    also <- col(fl, "also_bought")[hit] %in% "yes"
    pct <- num(col(fl, "bought_pct"))[hit]
    diet$bought_share <- ifelse(diet$source %in% "bought", 1, ifelse(also & !is.na(pct), pct / 100, 0))
    diet$bought_kind <- col(fl, "bought_kind")[hit]
    diet$bought_origin <- col(fl, "bought_origin")[hit]
  }
  # "Same as the animals entered just before": copy that entry's diet
  same <- if (!is.null(feeding)) col(lv, "KEY") %in% feeding_to_lv[col(feeding, "same_diet") %in% "yes"] else
    col(lv, "same_diet") %in% "yes"
  for (k in which(same)) {
    prev <- k - 1
    while (prev >= 1 && same[prev]) prev <- prev - 1
    if (prev >= 1 && identical(ids[prev], ids[k])) {
      copy <- diet[diet$lv_key == col(lv, "KEY")[prev], , drop = FALSE]
      if (nrow(copy)) { copy$lv_key <- col(lv, "KEY")[k]; diet <- rbind(diet, copy) }
    }
  }

  # Free-text answers to match ---------------------------------------------
  other <- do.call(rbind, lapply(odk_places$key[!odk_places$grazing], function(key) {
    do.call(rbind, lapply(c("practice_", "stay_"), function(pre) {
      sel <- col(lv, paste0(pre, key)) %in% "other"
      data.frame(kind = rep("manure", sum(sel)), answer = col(lv, paste0(pre, key, "_other"))[sel],
                 Ids = ids[sel], stringsAsFactors = FALSE)
    }))
  }))
  # (every reported feed is listed in each diet: only those eaten need a match)
  eaten <- (diet$amount > 0) %in% TRUE
  feed_ans <- data.frame(kind = rep("feed", sum(eaten)), answer = diet$text[eaten],
                         Ids = unname(key_to_id[col(lv, "PARENT_KEY")[match(diet$lv_key[eaten], col(lv, "KEY"))]]),
                         stringsAsFactors = FALSE)
  cr <- find("crop")
  if (is.null(cr)) cr <- data.frame(KEY = character(0), PARENT_KEY = character(0))
  # crop questions are asked for the feeds grown on the farm (one entry per feed
  # of the list); entries of other feeds are empty
  if ("crop_src" %in% names(cr)) cr <- cr[col(cr, "crop_src") %in% c("grown", "leftover") & !is.na(col(cr, "crop_text")), , drop = FALSE]
  crop_ids <- unname(key_to_id[col(cr, "PARENT_KEY")])
  crop_ans <- data.frame(kind = rep("crop", nrow(cr)), answer = col(cr, "crop_text"), Ids = crop_ids,
                         stringsAsFactors = FALSE)
  all_ans <- rbind(crop_ans, feed_ans, other)
  all_ans <- all_ans[!is.na(all_ans$answer) & nzchar(trimws(all_ans$answer)), , drop = FALSE]
  all_ans$key <- batch_normalise_text(all_ans$answer)
  first <- !duplicated(paste(all_ans$kind, all_ans$key))
  to_match <- all_ans[first, c("kind", "answer"), drop = FALSE]
  to_match$farms <- vapply(seq_len(nrow(to_match)), function(i) {
    length(unique(all_ans$Ids[all_ans$kind == to_match$kind[i] & all_ans$key == batch_normalise_text(to_match$answer[i])]))
  }, 0L)
  to_match <- if (nrow(to_match)) batch_match_suggestions(to_match, db, library) else
    data.frame(kind = character(0), answer = character(0), farms = integer(0), key = character(0),
               value = character(0), score = numeric(0), source = character(0), value_label = character(0))
  if (!is.null(matches) && nrow(matches)) {
    mk <- paste(matches$kind, batch_normalise_text(matches$answer))
    hit <- match(paste(to_match$kind, to_match$key), mk)
    has <- !is.na(hit)
    to_match$value[has] <- ifelse(is.na(matches$value[hit[has]]), "", as.character(matches$value[hit[has]]))
    to_match$source[has] <- "confirmed"
  }
  to_match <- to_match[order(match(to_match$kind, c("crop", "feed", "manure")), -to_match$farms), , drop = FALSE]
  rownames(to_match) <- NULL
  confirmed <- to_match$source %in% c("confirmed", "remembered", "same name") &
    !grepl("^(lib|libcrop|new|newcrop):", to_match$value %||% "")
  if (!all(confirmed) || any(to_match$kind == "manure" & (is.na(to_match$value) | to_match$value == ""))) {
    return(list(to_match = to_match, input = NULL, conversions = NULL, excluded = if (nrow(excluded)) excluded, renamed = renamed))
  }
  lookup <- function(kind, text) {
    to_match$value[match(paste(kind, batch_normalise_text(text)), paste(to_match$kind, to_match$key))]
  }

  # Livestock sheet ------------------------------------------------------------
  amount <- function(name) num(col(lv, name))
  litres <- num(col(lv, "milk_litres_day"))
  milk <- ifelse(is.na(litres), NA, ifelse(litres == 0, 0,
                 litres * odk_milk_kg_per_litre * num(col(lv, "milk_months")) * odk_days_per_month))
  livestock <- data.frame(
    Ids = ids, livetype_code = col(lv, "livetype_code"), livetype_desc = col(lv, "lt_name"),
    number = col(lv, "number"), body_weight = col(lv, "body_weight"), annual_milk = as.character(milk),
    annual_growth = col(lv, "annual_growth"), annual_wool = col(lv, "annual_wool"),
    work_hour = col(lv, "work_hour"), piglets_relying_on_milk = col(lv, "piglets_relying_on_milk"),
    adult_weight = col(lv, "adult_weight"),
    birth_interval = as.character(num(col(lv, "birth_interval_months")) / 12),
    lactation_length = col(lv, "lactation_length"), body_weight_weaning = col(lv, "body_weight_weaning"),
    body_weight_year_one = col(lv, "body_weight_year_one"), stringsAsFactors = FALSE)
  no_details <- !col(lv, "more_details") %in% "yes"
  for (f in c("adult_weight", "birth_interval", "lactation_length", "body_weight_weaning", "body_weight_year_one")) {
    livestock[[f]][no_details] <- NA
  }
  for (k in seq_len(nrow(odk_places))) {
    p <- odk_places[k, ]
    hours <- num(col(lv, paste0("hours_", p$key)))
    livestock[[paste0("time_in_", p$field)]] <- as.character(hours / 24)
    system <- if (p$grazing) rep(prp, nrow(lv)) else {
      sys_of <- function(pre) {
        code <- col(lv, paste0(pre, p$key))
        out <- practices$manureman_desc[match(code, practices$practice_code)]
        is_other <- code %in% "other"
        out[is_other] <- lookup("manure", col(lv, paste0(pre, p$key, "_other"))[is_other])
        out
      }
      taken <- sys_of("practice_"); stays <- sys_of("stay_")
      out_share <- num(col(lv, paste0("collected_", p$key))) / 100
      # two systems in the place: the larger part is the place's main system,
      # the smaller part its second system with its share (forms before asked
      # one practice for all the manure of the place)
      use_out <- is.na(stays) | (!is.na(taken) & (out_share >= 0.5) %in% TRUE)
      sys <- ifelse(use_out, taken, stays)
      second <- ifelse(use_out, stays, taken)
      share2 <- ifelse(use_out, 1 - out_share, out_share)
      two <- (hours > 0) %in% TRUE & !is.na(taken) & !is.na(stays) & (share2 > 0) %in% TRUE
      livestock[[paste0("manureman_2_", p$field)]] <- ifelse(two, second, NA)
      livestock[[paste0("manure_share_2_", p$field)]] <- ifelse(two, as.character(round(share2, 4)), NA)
      livestock[[paste0("manure_share_1_", p$field)]] <- ifelse(two, as.character(round(1 - share2, 4)), NA)
      sys
    }
    livestock[[paste0("manureman_", p$field)]] <- ifelse((hours > 0) %in% TRUE, system, NA)
  }
  pct <- function(name) as.character(num(col(lv, name)) / 100)
  livestock$manure_in_stable <- pct("collected_stable")
  livestock$manure_in_non_roofed_enclosure <- pct("collected_enclosure")
  livestock$manure_in_field <- pct("collected_field")
  livestock$manure_onfarm_fraction <- pct("used_onfarm")
  livestock$manure_sales_fraction <- pct("sold")
  livestock$distance_to_pasture <- col(lv, "distance_to_pasture")
  keep <- !is.na(livestock$Ids)
  lv_keys <- col(lv, "KEY")[keep]
  livestock <- livestock[keep, , drop = FALSE]

  # Feed basket: feeds -> codes, amounts -> shares of 100 ----------------------
  diet$code <- lookup("feed", diet$text)
  # "kg as fed" -> kg dry matter with the feed's dry-matter content
  dm <- num(db$lkp_feeditem$dm_content[match(diet$code, as.character(db$lkp_feeditem$feed_item_code))]) / 100
  as_fed <- diet$unit %in% "kg_as_fed" & !is.na(dm)
  diet$amount[as_fed] <- diet$amount[as_fed] * dm[as_fed]
  diet <- diet[!is.na(diet$code) & diet$code != "" & diet$lv_key %in% lv_keys & (diet$amount > 0) %in% TRUE, , drop = FALSE]
  # Bought feeds: the part bought becomes a feed tagged OFR / OFC / IP, so the
  # land and emissions of producing it off the farm are counted separately
  conv <- odk_conversions(db)
  bought <- odk_bought_feeds(diet, db, conv, livestock, lv_keys)
  diet <- bought$diet
  variant_codes <- bought$variant_codes
  li <- match(diet$lv_key, lv_keys)
  rows <- unique(data.frame(lv_key = diet$lv_key, season_name = diet$season_name, stringsAsFactors = FALSE))
  codes <- unique(diet$code); codes <- codes[order(num(codes), codes)]
  feed_basket <- data.frame(Ids = livestock$Ids[match(rows$lv_key, lv_keys)],
                            livetype_code = livestock$livetype_code[match(rows$lv_key, lv_keys)],
                            livetype_desc = livestock$livetype_desc[match(rows$lv_key, lv_keys)],
                            season_name = rows$season_name, stringsAsFactors = FALSE)
  if (nrow(rows)) {
    shares <- matrix(0, nrow(rows), length(codes), dimnames = list(NULL, codes))
    r <- match(paste(diet$lv_key, diet$season_name), paste(rows$lv_key, rows$season_name))
    for (j in seq_len(nrow(diet))) shares[r[j], diet$code[j]] <- shares[r[j], diet$code[j]] + diet$amount[j]
    shares <- shares / rowSums(shares) * 100
    for (code in codes) feed_basket[[code]] <- as.character(round(shares[, code], 4))
  }
  # Crops grown, sources of feeds, inputs bought ---------------------------------
  crops <- odk_crop_values(cr, crop_ids, lookup, db, num, col)
  conversions <- crops$conversions
  feed_items <- odk_feed_items(diet, lv_keys, livestock, crops$values, db, variant_codes)
  links <- attr(feed_items, "links")
  feed_type <- crops$feed_type
  if (nrow(crops$values)) {
    entry_rows <- seq_len(nrow(crops$values))
    used <- if (is.null(links)) integer(0) else unique(links$entry)
    # the crop of a feed linked by name gets the yields of the crop entry
    if (!is.null(links) && nrow(links)) {
      extra <- links[links$feed_crop != crops$values$crop_code[links$entry], , drop = FALSE]
      extra <- extra[!duplicated(paste(extra$Ids, extra$feed_crop)), , drop = FALSE]
      if (nrow(extra)) {
        rows <- feed_type[extra$entry, , drop = FALSE]
        rows$crop_code <- extra$feed_crop
        feed_type <- rbind(feed_type, rows)
        feed_type <- feed_type[!duplicated(paste(feed_type$Ids, feed_type$crop_code)), , drop = FALSE]
      }
    }
    # tell the user when the answers about a crop cannot be used
    unused <- setdiff(entry_rows, used)
    if (length(unused) && nrow(conversions)) {
      ci <- match(paste(crops$values$Ids[unused], crops$values$crop_key[unused]),
                  paste(conversions$Ids, batch_normalise_text(conversions$crop)))
      ci <- ci[!is.na(ci)]
      conversions$note[ci] <- trimws(paste(conversions$note[ci],
        "No feed in this farm's diets comes from this crop: its answers are not used."))
    }
  }
  # Off-farm crops of bought feeds: the share taken from the supplier's field
  off <- odk_offfarm_feed_type(feed_items, variant_codes, db, conv)
  if (!is.null(off$feed_type)) feed_type <- dplyr::bind_rows(feed_type, off$feed_type)
  assumed <- rbind(attr(feed_items, "assumed"), bought$assumed, off$assumed)
  if (!is.null(assumed)) {
    assumed$feed <- db$lkp_feeditem$feed_item_name[match(assumed$feed_item_code, as.character(db$lkp_feeditem$feed_item_code))]
    assumed$feed[is.na(assumed$feed)] <- ""
  }
  fertilizer <- crops$fertilizer
  bought_table <- odk_bought_table(fl, key_to_id, diet, bought, livestock, lv_keys, season, db, conv, num, col)
  for (b in c("manure", "compost", "bedding")) {
    amount <- num(col(main, paste0("bought_", b, "_amount")))
    unit <- col(main, paste0("bought_", b, "_unit"))
    kg <- amount * odk_unit_kg(unit, num(col(main, paste0("bought_", b, "_unit_kg"))), conv)
    kg[!col(main, paste0("bought_", b)) %in% "yes"] <- NA
    farm[[paste0("purchased_", if (b == "bedding") "bedding" else b)]] <- ifelse(
      is.na(kg), NA, as.character(round(kg * conv[[paste0("n_content_", b)]], 3)))
  }

  empty <- function(cols) as.data.frame(stats::setNames(replicate(length(cols), character(0), simplify = FALSE), cols))
  input <- list(farm = farm, season = season, livestock = livestock, feed_basket = feed_basket,
                feed_items = if (nrow(feed_items)) feed_items else empty(c("Ids", "feed_item_code")),
                feed_type = if (nrow(feed_type)) feed_type else empty(c("Ids", "crop_code")),
                fertilizer = if (nrow(fertilizer)) fertilizer else empty(c("Ids", "fertilizer_desc")))
  input <- lapply(input, function(x) { x[] <- lapply(x, function(v) { v <- as.character(v); v[v %in% c("", "NA")] <- NA; v }); x })
  list(to_match = to_match, input = input, conversions = conversions, assumed = assumed,
       bought = bought_table, variants = bought$needed, excluded = if (nrow(excluded)) excluded, renamed = renamed)
}

odk_read_tables <- function(export) {
  if (is.list(export) && !is.data.frame(export)) return(export)
  if (is.character(export) && grepl("\\.xlsx?$", export, ignore.case = TRUE)) return(odk_read_kobo(export))
  if (is.character(export) && grepl("\\.zip$", export, ignore.case = TRUE)) {
    dir <- tempfile("odk_"); dir.create(dir)
    utils::unzip(export, exdir = dir)
    export <- dir
  }
  files <- list.files(export, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  if (!length(files)) stop("No CSV files found in the ODK export.", call. = FALSE)
  tables <- lapply(files, function(f) {
    x <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character",
                         na.strings = c("", "NA"), encoding = "UTF-8")
    # ODK Central prefixes columns with their group names ("farm-Ids")
    names(x) <- sub("^.*-", "", names(x))
    x
  })
  names(tables) <- tools::file_path_sans_ext(basename(files))
  tables
}

#' Is a file an ODK or KoboToolbox export?
#'
#' @param file Path of an uploaded file.
#' @return `TRUE` for an ODK Central `.zip` export or a KoboToolbox `.xlsx`
#'   export (sheets of repeated sections with `_parent_index`), `FALSE` for
#'   other files such as a batch template.
#' @export
is_odk_export <- function(file) {
  if (grepl("\\.zip$", file, ignore.case = TRUE)) return(TRUE)
  if (!grepl("\\.xlsx$", file, ignore.case = TRUE)) return(FALSE)
  sheets <- tryCatch(openxlsx::getSheetNames(file), error = function(e) character(0))
  if ("farm" %in% sheets || !"livestock" %in% sheets) return(FALSE)
  head <- tryCatch(names(openxlsx::read.xlsx(file, sheet = "livestock", rows = 1:2, check.names = FALSE)),
                   error = function(e) character(0))
  "_parent_index" %in% head
}

# KoboToolbox "XLS" export: one sheet for the submissions and one per repeated
# section, linked by _index / _parent_index / _parent_table_name. Turned into
# the layout of an ODK Central export (KEY / PARENT_KEY).
odk_read_kobo <- function(file) {
  sheets <- openxlsx::getSheetNames(file)
  raw <- lapply(stats::setNames(sheets, sheets), function(sh) {
    x <- openxlsx::read.xlsx(file, sheet = sh, check.names = FALSE, sep.names = " ", detectDates = FALSE)
    if (is.null(x)) return(data.frame())
    x[] <- lapply(x, function(v) { v <- as.character(v); v[v %in% c("", "NA")] <- NA; v })
    x
  })
  is_repeat <- vapply(raw, function(x) "_parent_index" %in% names(x), TRUE)
  main_name <- sheets[!is_repeat][1]
  if (is.na(main_name)) stop("No submissions sheet found in the KoboToolbox export.", call. = FALSE)
  # Kobo shortens the submissions sheet name ("... - Ethio...") but writes the
  # full form title in _parent_table_name: any parent that is not a repeat
  # sheet is the submissions table
  repeat_sheets <- sheets[is_repeat]
  key <- function(table, index) {
    table <- rep_len(table, length(index))
    ifelse(table %in% repeat_sheets, paste0(table, ":", index), paste0("main:", index))
  }
  out <- list()
  for (sh in main_name) {
    x <- raw[[sh]]
    x$KEY <- key(sh, x[["_index"]])
    if ("_validation_status" %in% names(x)) {
      x$ReviewState <- ifelse(grepl("not_approved", x[["_validation_status"]]), "rejected", NA)
    }
    names(x) <- sub("^.*/", "", names(x))
    out[["kobo_main"]] <- x
  }
  for (sh in sheets[is_repeat]) {
    x <- raw[[sh]]
    x$KEY <- key(sh, x[["_index"]])
    x$PARENT_KEY <- key(x[["_parent_table_name"]], x[["_parent_index"]])
    names(x) <- sub("^.*/", "", names(x))
    out[[paste0("kobo_main-", sh)]] <- x
  }
  out
}

odk_main_table <- function(names, form_id) {
  if (!is.null(form_id) && form_id %in% names) return(form_id)
  if ("kobo_main" %in% names) return("kobo_main")
  # the main table is the one whose name is a prefix of the repeat tables
  cand <- names[!grepl("-", names)]
  if (length(cand)) cand[1] else names[1]
}

#' Save batch input as an Excel batch file
#'
#' Writes batch input (for example from [read_odk_export()]) into the batch
#' template of the parameter database, so it can be checked, edited and
#' uploaded like a file filled in by hand.
#'
#' @param input A named list of data frames with the batch sheets.
#' @param parameter_database,database_name As in [write_batch_template()].
#' @param file Path of the `.xlsx` file to write.
#' @return `file`, invisibly.
#' @export
write_batch_file <- function(input, parameter_database, file, database_name = NULL) {
  tmp <- tempfile(fileext = ".xlsx")
  write_batch_template(parameter_database, tmp, database_name = database_name)
  wb <- openxlsx::loadWorkbook(tmp)
  text_cols <- c("Ids", "farm_name", "country", "livetype_desc", "season_name", "livetype_code",
                 "feed_item_code", "crop_code", "fertilizer_desc")
  for (sheet in batch_data_sheets) {
    data <- input[[sheet]]
    if (is.null(data) || !nrow(data)) next
    data$.row <- NULL
    cols <- names(openxlsx::read.xlsx(wb, sheet = sheet, rows = 1, colNames = TRUE, check.names = FALSE, sep.names = " "))
    cols <- c(cols, setdiff(names(data), cols))
    full <- as.data.frame(stats::setNames(lapply(cols, function(c) if (c %in% names(data)) data[[c]] else rep(NA, nrow(data))), cols),
                          check.names = FALSE, stringsAsFactors = FALSE)
    num <- setdiff(names(full), c(text_cols, grep("^manureman_|^group_|_desc$|_name$|^region$|^climate|^soil_description$|^cropland_system$|^cropland_tillage$|^cropland_orgmatter$|^grassland_management$|^grassland_implevel$|^source", names(full), value = TRUE)))
    full[num] <- lapply(full[num], function(x) { v <- suppressWarnings(as.numeric(x)); if (all(is.na(v) == is.na(x))) v else x })
    openxlsx::writeData(wb, sheet, full, colNames = TRUE)
  }
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}

# Some openxlsx versions write links to drawing parts that are not in the file
# and a wrong sheet size. Excel ignores both, but ODK's form converter
# (pyxform/openpyxl) rejects the file or reads only one cell, so both are fixed.
xlsx_drop_dangling_parts <- function(file) {
  dir <- tempfile("xlsx_"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  utils::unzip(file, exdir = dir)
  changed <- FALSE
  for (rels in list.files(file.path(dir, "xl", "worksheets", "_rels"), full.names = TRUE)) {
    xml <- paste(readLines(rels, warn = FALSE, encoding = "UTF-8"), collapse = "")
    items <- regmatches(xml, gregexpr("<Relationship [^>]*/>", xml))[[1]]
    for (item in items) {
      target <- sub('.*Target="([^"]+)".*', "\\1", item)
      if (!file.exists(file.path(dir, "xl", "worksheets", target))) {
        xml <- sub(item, "", xml, fixed = TRUE)
        changed <- TRUE
      }
    }
    writeLines(xml, rels, useBytes = TRUE)
  }
  for (sheet in list.files(file.path(dir, "xl", "worksheets"), pattern = "\\.xml$", full.names = TRUE)) {
    xml <- paste(readLines(sheet, warn = FALSE, encoding = "UTF-8"), collapse = "")
    rels <- file.path(dir, "xl", "worksheets", "_rels", paste0(basename(sheet), ".rels"))
    ids <- if (file.exists(rels)) regmatches(readLines(rels, warn = FALSE), gregexpr('Id="[^"]+"', readLines(rels, warn = FALSE)))[[1]] else character(0)
    # a wrong <dimension> makes read-only readers (as used by pyxform) see one cell
    if (grepl("<dimension ref=\"[^\"]*\"/>", xml)) {
      xml <- sub("<dimension ref=\"[^\"]*\"/>", "", xml)
      changed <- TRUE
    }
    for (tag in c("drawing", "legacyDrawing")) {
      hit <- regmatches(xml, gregexpr(sprintf("<%s r:id=\"[^\"]+\"/>", tag), xml))[[1]]
      for (h in hit) {
        id <- sub('.*r:id="([^"]+)".*', 'Id="\\1"', h)
        if (!id %in% ids) { xml <- sub(h, "", xml, fixed = TRUE); changed <- TRUE }
      }
    }
    writeLines(xml, sheet, useBytes = TRUE)
  }
  if (changed) {
    unlink(file)
    zip::zip(file, files = list.files(dir, recursive = TRUE, all.files = TRUE), root = dir)
  }
  invisible(file)
}

# Survey conversions ----------------------------------------------------------------

#' Conversion values used for the farmer survey
#'
#' Values that turn farmers' answers into model inputs: removal of the crop
#' when cut and carried or grazed, dry-matter content of fresh or dried feed
#' when the feed's own value does not fit, N content of bought
#' manure, compost and bedding, N content of CAN, yield range that is flagged,
#' and the grassland management classes used for "improved", "normal" and
#' "degraded" pasture. The values shipped with the package can be replaced per
#' parameter database with rows of section "survey" in `lkp_defaults`.
#'
#' @param parameter_database A list from [read_parameter_database()].
#' @return A named list.
#' @export
batch_survey_conversions <- function(parameter_database) {
  path <- system.file("extdata", "batch", "survey_conversions.csv", package = "cleaned")
  if (!nzchar(path)) path <- file.path("inst", "extdata", "batch", "survey_conversions.csv")
  x <- utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
  out <- stats::setNames(as.list(x$value), x$key)
  d <- parameter_database$lkp_defaults
  if (!is.null(d) && nrow(d)) {
    d <- d[d$section == "survey" & !is.na(d$value) & trimws(as.character(d$value)) != "", , drop = FALSE]
    out[d$field] <- as.list(trimws(as.character(d$value)))
  }
  lapply(out, function(v) { n <- suppressWarnings(as.numeric(v)); if (is.na(n)) v else n })
}
odk_conversions <- function(db) batch_survey_conversions(db)

# All amounts are asked in kg
odk_unit_kg <- function(unit, other_kg, conv) rep(1, length(unit))

odk_fert_columns <- c(urea = "urea", dap = "dap", npk = "npk", can = "ammonium_nitrate", sulfate = "ammonium_sulfate")
odk_fert_names <- c(urea = "Urea", dap = "DAP", npk = "NPK", can = "Ammonium nitrate", sulfate = "Ammonium sulfate")

# Crops grown: yields, removal, residues, inputs ----------------------------------------
odk_crop_values <- function(cr, crop_ids, lookup, db, num, col) {
  conv <- odk_conversions(db)
  n <- NROW(cr)
  empty <- list(values = data.frame(), feed_type = data.frame(), fertilizer = data.frame(),
                conversions = data.frame())
  if (!n) return(empty)
  fi <- db$lkp_feeditem
  code <- lookup("crop", col(cr, "crop_text"))
  area_unit <- col(cr, "area_unit")
  area_ha <- num(col(cr, "area")) * ifelse(area_unit %in% "acre", 0.4046856, ifelse(area_unit %in% "sqm", 1e-4, 1))
  method <- col(cr, "yield_method")
  ukg <- odk_unit_kg(col(cr, "amount_unit"), num(col(cr, "unit_kg")), conv)
  times <- num(col(cr, "times_per_year"))
  kg <- ifelse(method %in% c("per_cut", "per_harvest"), num(col(cr, "amount")) * ukg * times,
        ifelse(method %in% "per_area", num(col(cr, "amount_per_area")) * ukg * times * num(col(cr, "area")),
        ifelse(method %in% "daily", num(col(cr, "amount")) * ukg * num(col(cr, "days_fed")), NA)))
  state <- col(cr, "amount_state")
  dm_of <- function(crop, st) {
    d <- num(fi$dm_content[as.character(fi$crop_code) == crop]) / 100
    d <- d[!is.na(d)]
    if (identical(st, "fresh")) { d <- d[d < 0.5]; if (length(d)) min(d) else conv$dm_fresh_default }
    else { d <- d[d >= 0.5]; if (length(d)) max(d) else conv$dm_dried_default }
  }
  dm <- vapply(seq_len(n), function(i) if (is.na(code[i]) || code[i] == "" || is.na(kg[i])) NA_real_ else dm_of(code[i], state[i]), 0)
  use <- col(cr, "crop_use")
  pct_cut <- num(col(cr, "pct_cut")) / 100
  cut_frac <- ifelse(use %in% "cut_carry", 1, ifelse(use %in% "grazed", 0, ifelse(use %in% "both", pct_cut, NA)))
  # Grazed crops: no harvest is asked, the database yield is used. Partly cut,
  # partly grazed: the amount cut covers the cut share of the field only
  grazed_only <- use %in% "grazed"
  kg[grazed_only] <- NA
  dm[grazed_only] <- NA
  cut_share <- ifelse(use %in% "both" & (cut_frac > 0) %in% TRUE, cut_frac, 1)
  dry_yield <- kg * dm / area_ha / 1000 / cut_share
  # share of what grows that leaves the field: the farmer's answers, or the
  # usual values when not known
  cut_removed <- num(col(cr, "pct_cut_removed")) / 100
  grazed_eaten <- num(col(cr, "pct_grazed_eaten")) / 100
  cut_known <- !is.na(cut_removed); grazed_known <- !is.na(grazed_eaten)
  cut_removed[!cut_known] <- conv$removal_cut_carry; grazed_eaten[!grazed_known] <- conv$removal_grazed
  removal <- cut_frac * cut_removed + (1 - cut_frac) * grazed_eaten
  removal_text <- ifelse(is.na(removal), "", paste0(round(100 * removal), "%",
    ifelse((cut_frac > 0 & !cut_known) | (cut_frac < 1 & !grazed_known), " (usual value)", " (farmer)")))

  has_res <- col(cr, "has_residues") %in% "yes"
  res_removal <- ifelse(has_res, (num(col(cr, "pct_res_removed")) + num(col(cr, "pct_res_grazed"))) / 100, NA)
  res_burnt <- ifelse(has_res, num(col(cr, "pct_res_burned")) / 100, NA)
  res_kg <- num(col(cr, "res_amount")) * odk_unit_kg(col(cr, "res_unit"), num(col(cr, "res_unit_kg")), conv) *
    ifelse(is.na(times), 1, times)
  res_yield <- ifelse(has_res, res_kg * conv$dm_dried_default / area_ha / 1000, NA)

  inter <- col(cr, "intercrop") %in% "yes"
  # an entry belongs to the feed it was asked for (forms before: the crop's own name)
  link <- if ("crop_feed" %in% names(cr)) col(cr, "crop_feed") else col(cr, "crop_text")
  values <- data.frame(Ids = crop_ids, crop_code = code, crop_key = batch_normalise_text(link),
                       stringsAsFactors = FALSE)
  values$intercrop <- ifelse(inter, "1", ifelse(col(cr, "intercrop") %in% "no", "0", NA))
  values$intercrop_fraction <- ifelse(inter, as.character(num(col(cr, "intercrop_pct")) / 100),
                                      ifelse(col(cr, "intercrop") %in% "no", "0", NA))
  values$fraction_as_fertilizer <- ifelse(col(cr, "manure_applied") %in% "yes", as.character(num(col(cr, "manure_pct")) / 100),
                                          ifelse(col(cr, "manure_applied") %in% "no", "0", NA))
  chosen <- col(cr, "fertilisers")
  for (k in names(odk_fert_columns)) {
    used <- !is.na(chosen) & grepl(paste0("\\b", k, "\\b"), chosen)
    rate <- num(col(cr, paste0("fert_", k))) / area_ha
    values[[odk_fert_columns[[k]]]] <- ifelse(used, as.character(round(rate, 3)), ifelse(is.na(chosen) & !"fertilisers" %in% names(cr), NA, "0"))
  }
  slope_words <- c(flat = "^Flat", gentle = "^Hilly", steep = "^Steep", very_steep = "^Extremely")
  slopes <- trimws(as.character(db$lkp_slope$slope_desc))
  values$slope_desc <- vapply(col(cr, "slope"), function(x) {
    if (is.na(x) || !x %in% names(slope_words)) return(NA_character_)
    hit <- grep(slope_words[[x]], slopes, value = TRUE); if (length(hit)) hit[1] else NA_character_
  }, "", USE.NAMES = FALSE)
  values$cut_carry_fraction <- as.character(cut_frac)
  grass <- trimws(as.character(db$lkp_grasslandman$management_desc))
  pasture <- col(cr, "pasture_condition")
  # (forms that ask whether the crop is a grass: the condition only for grass)
  if ("is_grass" %in% names(cr)) pasture[!col(cr, "is_grass") %in% "yes"] <- NA
  grassman <- vapply(pasture, function(x) {
    if (is.na(x)) return(NA_character_)
    prefix <- conv[[paste0("pasture_", x)]]
    if (is.null(prefix)) return(NA_character_)
    hit <- grass[startsWith(grass, prefix)]
    if (!length(hit)) return(NA_character_)
    pick <- hit[grepl("tropical", hit) & !grepl("montane", hit)]
    if (length(pick)) pick[1] else hit[1]
  }, "", USE.NAMES = FALSE)

  feed_type <- data.frame(
    Ids = crop_ids, crop_code = code,
    dry_yield = ifelse(is.finite(dry_yield), as.character(round(dry_yield, 4)), NA),
    residue_dry_yield = ifelse(is.finite(res_yield), as.character(round(res_yield, 4)), NA),
    main_product_removal = ifelse(is.na(removal), NA, as.character(round(removal, 4))),
    residue_removal = ifelse(is.na(res_removal), NA, as.character(res_removal)),
    residue_burnt = ifelse(is.na(res_burnt), NA, as.character(res_burnt)),
    grassman_desc = grassman, stringsAsFactors = FALSE)

  # One entry per farm and crop: a crop entered twice keeps the first entry
  dup <- duplicated(paste(crop_ids, code)) & !is.na(code) & code != ""
  ok <- !is.na(crop_ids) & !is.na(code) & code != "" & !dup
  per_unit <- c(acre = "acre", hectare = "hectare", sqm = "square metre")[area_unit]
  method_text <- ifelse(method %in% "per_cut", "per cut", ifelse(method %in% "per_harvest", "per harvest",
                 ifelse(method %in% "per_area", paste0("per ", per_unit, " and harvest"), ifelse(method %in% "daily", "per day", ""))))
  area_words <- c(acre = "acres", hectare = "hectares", sqm = "square metres")
  flag <- ifelse(is.na(dry_yield), ifelse(grazed_only, "Not asked (grazed): database yield used",
                 ifelse(method %in% "unknown" | is.na(method), "Not known: database yield used", "Incomplete answer: database yield used")),
          ifelse(dry_yield < conv$yield_warning_low, "Very low: check the amounts and the area",
          ifelse(dry_yield > conv$yield_warning_high, "Very high: check the amounts and the area", "")))
  flag[dup] <- "Crop entered twice for this farm: the first entry is used"
  conversions <- data.frame(
    Ids = crop_ids, crop = col(cr, "crop_text"),
    matched_crop = db$lkp_crops$crop_name[match(code, as.character(db$lkp_crops$crop_code))],
    answer = ifelse(grazed_only, "grazed (no harvest)", ifelse(is.na(method) | method %in% "unknown", "amount not known", paste0(
      ifelse(method %in% "per_area", col(cr, "amount_per_area"), col(cr, "amount")), " ",
      "kg ", method_text,
      ifelse(method %in% c("per_cut", "per_harvest", "per_area"), paste0(", ", times, " times a year"), ""),
      ifelse(method %in% "daily", paste0(", ", col(cr, "days_fed"), " days"), ""),
      ", ", state, ", from ", col(cr, "area"), " ", unname(area_words[area_unit]),
      ifelse(use %in% "both", paste0(" (cut part, ", round(100 * cut_frac), "% of the field)"), "")))),
    kg_per_year = round(kg), dm_fraction = round(dm, 3), area_ha = round(area_ha, 4),
    dry_yield_t_dm_ha_year = round(dry_yield, 2), removed = removal_text,
    residues = ifelse(has_res, paste0(col(cr, "pct_res_removed"), "% taken, ", col(cr, "pct_res_grazed"), "% grazed, ",
                                      col(cr, "pct_res_burned"), "% burned, ", col(cr, "pct_res_left"), "% mulch/ploughed in"), ""),
    note = flag, stringsAsFactors = FALSE)

  # Fertilisers used on the farm, with the N content of the NPK grade if given
  npk_n <- suppressWarnings(as.numeric(sub("^\\s*([0-9.]+).*$", "\\1", col(cr, "npk_grade"))))
  fert_rows <- do.call(rbind, lapply(names(odk_fert_columns), function(k) {
    used <- ok & !is.na(chosen) & grepl(paste0("\\b", k, "\\b"), chosen)
    if (!any(used)) return(NULL)
    d <- data.frame(Ids = crop_ids[used], fertilizer_desc = odk_fert_names[[k]],
                    percentage_n = if (k == "npk") as.character(npk_n[used]) else if (k == "can") as.character(conv$n_content_can) else NA_character_,
                    stringsAsFactors = FALSE)
    d[!duplicated(d$Ids), , drop = FALSE]
  }))
  if (is.null(fert_rows)) fert_rows <- data.frame()
  list(values = values[ok, , drop = FALSE], feed_type = feed_type[ok, , drop = FALSE],
       fertilizer = fert_rows, conversions = conversions[!is.na(crop_ids), , drop = FALSE])
}

# Farm-specific feed values: where the feed comes from, and the inputs of the
# crop it comes from
odk_feed_items <- function(diet, lv_keys, livestock, crop_values, db, variant_codes = character(0)) {
  if (!nrow(diet)) return(data.frame())
  fi <- db$lkp_feeditem
  d <- data.frame(Ids = livestock$Ids[match(diet$lv_key, lv_keys)], feed_item_code = diet$code,
                  source = c(grown = "Main", communal = "Main", leftover = "Residue", bought = "Purchased",
                             bought_main = "Main", bought_residue = "Residue")[diet$source],
                  stringsAsFactors = FALSE)
  d <- d[!is.na(d$Ids), , drop = FALSE]
  keys <- unique(d[, c("Ids", "feed_item_code")])
  out <- keys
  out$source_type <- vapply(seq_len(nrow(keys)), function(i) {
    s <- d$source[d$Ids == keys$Ids[i] & d$feed_item_code == keys$feed_item_code[i]]
    s <- s[!is.na(s)]
    if (!length(s)) NA_character_ else names(sort(table(s), decreasing = TRUE))[1]
  }, "")
  links <- NULL
  if (nrow(crop_values)) {
    crop <- as.character(fi$crop_code[match(out$feed_item_code, as.character(fi$feed_item_code))])
    # A crop entry belongs to a feed of the same farm when the feed comes from
    # that crop, or when the farmer gave the same name to the crop and the feed
    hit <- match(paste(out$Ids, crop), paste(crop_values$Ids, crop_values$crop_code))
    d$key <- batch_normalise_text(diet$text[!is.na(livestock$Ids[match(diet$lv_key, lv_keys)])])
    by_name <- vapply(seq_len(nrow(out)), function(i) {
      names_used <- unique(d$key[d$Ids == out$Ids[i] & d$feed_item_code == out$feed_item_code[i]])
      j <- which(crop_values$Ids == out$Ids[i] & crop_values$crop_key %in% names_used)
      if (length(j)) j[1] else NA_integer_
    }, 0L)
    hit[is.na(hit)] <- by_name[is.na(hit)]
    # the part of a feed that is bought comes from someone else's field
    hit[out$feed_item_code %in% variant_codes] <- NA
    links <- data.frame(Ids = out$Ids, feed_item_code = out$feed_item_code, feed_crop = crop,
                        entry = hit, stringsAsFactors = FALSE)
    links <- links[!is.na(links$entry) & !out$source_type %in% "Purchased", , drop = FALSE]
    for (f in setdiff(names(crop_values), c("Ids", "crop_code", "crop_key"))) {
      v <- crop_values[[f]][hit]
      # inputs of the field do not apply to bought feeds
      v[out$source_type %in% "Purchased"] <- NA
      if (any(!is.na(v))) out[[f]] <- v
    }
  }
  # Feeds grown on the farm need a land cover and a slope for the erosion
  # estimate. When neither the farmer's answers nor the database's feed
  # defaults give one, a value is chosen from the crop category.
  conv <- odk_conversions(db)
  fd <- db$lkp_feeditem_defaults
  has_default <- function(code, field) !is.null(fd) && field %in% names(fd) &&
    any(!is.na(fd[[field]][as.character(fd$feed_item_code) == code]) & fd[[field]][as.character(fd$feed_item_code) == code] != "")
  grown <- !out$source_type %in% "Purchased"
  crop_code <- as.character(fi$crop_code[match(out$feed_item_code, as.character(fi$feed_item_code))])
  category <- tolower(as.character(db$lkp_crops$category[match(crop_code, as.character(db$lkp_crops$crop_code))]))
  degraded <- if ("grassman_desc" %in% names(crop_values)) {
    g <- crop_values$grassman_desc[match(paste(out$Ids, crop_code), paste(crop_values$Ids, crop_values$crop_code))]
    grepl("degraded", g, ignore.case = TRUE)
  } else rep(FALSE, nrow(out))
  cover <- ifelse(grepl("grass", category), ifelse(degraded, conv$landcover_grass_degraded, conv$landcover_grass),
           ifelse(grepl("cereal", category), conv$landcover_cereal,
           ifelse(grepl("legume", category), conv$landcover_legume, conv$landcover_other)))
  assumed <- list()
  if (!"land_cover_desc" %in% names(out)) out$land_cover_desc <- NA_character_
  if (!"slope_desc" %in% names(out)) out$slope_desc <- NA_character_
  for (i in which(grown)) {
    if (is.na(out$land_cover_desc[i]) && !has_default(out$feed_item_code[i], "land_cover_desc")) {
      out$land_cover_desc[i] <- cover[i]
      assumed[[length(assumed) + 1]] <- data.frame(Ids = out$Ids[i], feed_item_code = out$feed_item_code[i],
                                                   field = "land cover", value = cover[i], stringsAsFactors = FALSE)
    }
    if (is.na(out$slope_desc[i]) && !has_default(out$feed_item_code[i], "slope_desc")) {
      out$slope_desc[i] <- conv$slope_default
      assumed[[length(assumed) + 1]] <- data.frame(Ids = out$Ids[i], feed_item_code = out$feed_item_code[i],
                                                   field = "slope", value = conv$slope_default, stringsAsFactors = FALSE)
    }
  }
  if (all(is.na(out$land_cover_desc))) out$land_cover_desc <- NULL
  if (all(is.na(out$slope_desc))) out$slope_desc <- NULL
  rownames(out) <- NULL
  attr(out, "assumed") <- if (length(assumed)) do.call(rbind, assumed) else NULL
  attr(out, "links") <- links
  out
}


# Label of a choice value (values kept when no label is known)
odk_label <- function(x, labels) ifelse(x %in% names(labels), unname(labels[x]), x)

# Latitude and longitude of the farm's GPS point: ODK Central splits it into
# columns (farm_location-Latitude, ...); KoboToolbox gives "lat lon alt acc"
# and _farm_location_latitude / _longitude
odk_geopoint <- function(main) {
  n <- nrow(main)
  num <- function(x) suppressWarnings(as.numeric(x))
  pick <- function(cands) { hit <- intersect(cands, names(main)); if (length(hit)) num(main[[hit[1]]]) else rep(NA_real_, n) }
  lat <- pick(c("Latitude", "farm_location-Latitude", "_farm_location_latitude", "farm_location_latitude"))
  lon <- pick(c("Longitude", "farm_location-Longitude", "_farm_location_longitude", "farm_location_longitude"))
  if (all(is.na(lat)) && "farm_location" %in% names(main)) {
    parts <- strsplit(trimws(as.character(main$farm_location)), "\\s+")
    lat <- num(vapply(parts, function(p) if (length(p) >= 2) p[1] else NA_character_, ""))
    lon <- num(vapply(parts, function(p) if (length(p) >= 2) p[2] else NA_character_, ""))
  }
  list(latitude = lat, longitude = lon)
}
