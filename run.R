library(magrittr)

orgs_govuk_raw <-
  readr::read_csv("inputs/2022-03-25_11-34-22_GMT Organisations on gov UK.csv")

orgs_audit_2021 <-
  readxl::read_excel("inputs/2021-09-17 Return tracker OFFICIAL.xlsx")

orgs_2021_lookup <- 
  orgs_audit_2021 %>% 
  dplyr::select(
    entity_2021 = "entity",
    entity_code_2021 = "entity_code",
    department_2021 = "department",
    longname_2021 = "entity_long_name"
  )

orgs_govuk_clean <-
  orgs_govuk_raw %>%
  dplyr::rename("longname_2022" = ".") %>%
  ## We add the row number to 99; this creates a 3-digit orgcode starting at
  ## 100. We start at 100 because we want it to be easy to quality assure the
  ## submitted data and minimise possible mistakes in filling out the data
  ## template.
  dplyr::mutate(orgcode_2022 = 99 + dplyr::row_number()) %>%
  ## Add the type of organisation as defined on gov uk. These are hardcoded and
  ## may require changes if the page changes.
  dplyr::mutate(
    orgtype_govuk_2022 = dplyr::case_when(
      orgcode_2022 == 100 ~ "Prime Minister's Office, 10 Downing Street",
      dplyr::between(orgcode_2022, 101, 123) ~ "Ministerial departments",
      dplyr::between(orgcode_2022, 124, 143) ~ "Non ministerial departments",
      dplyr::between(orgcode_2022, 144, 553) ~ "Agencies and other public bodies",
      dplyr::between(orgcode_2022, 554, 660) ~ "High profile groups",
      dplyr::between(orgcode_2022, 661, 673) ~ "Public corporations",
      dplyr::between(orgcode_2022, 674, 676) ~ "Devolved administrations"
    )
  )

## Attempt to match the organsiations on gov uk with the entity codes used in 2021

orgs_matched <- 
  orgs_govuk_clean %>% 
  ## Make an attempt to convert into entity codes used in 2021.
  dplyr::rowwise() %>% 
  dplyr::mutate(
    ## Remove all text that appears after an open parentheses.
    entity_2021_guess = stringr::str_split_fixed(longname_2022, pattern = "\\(", 2)[[1]],
    ## Use only the capital letters and numbers to make the cleaned department column.
    entity_2021_guess = stringr::str_extract_all(entity_2021_guess, "[A-Z0-9]+"),
    ## Convert the character vector above into a single character variable.
    entity_2021_guess = glue::glue_collapse(entity_2021_guess)
  ) %>% 
  dplyr::left_join(
    orgs_2021_lookup,
    by = c("entity_2021_guess" = "entity_2021")
  )

sysdatetime <- Sys.time() %>% 
  format("%Y-%m-%d_%H-%M-%S_%Z")

readr::write_excel_csv(orgs_matched, paste0("outputs/", sysdatetime, " orgs matched with gaps OFFICIAL.csv"))

## Find the organisations that did not match up and save a csv so manual entries can be provided.

orgs_matched_gaps <- 
  orgs_matched %>% 
  dplyr::filter(is.na(entity_code_2021))

sysdatetime <- Sys.time() %>% 
  format("%Y-%m-%d_%H-%M-%S_%Z")

readr::write_excel_csv(orgs_matched_gaps, paste0("outputs/", sysdatetime, " entity code gaps OFFICIAL.csv"))

## Use a lookup to correctly map the gaps.

lookup_2022_to_2021_gaps <- readr::read_csv("inputs/lookup_2022_to_2021_gaps.csv", na = c(""))

orgs_matched_corrected <-
  orgs_matched %>% 
  dplyr::left_join(lookup_2022_to_2021_gaps) %>% 
  dplyr::mutate(
    entity_2021_final = dplyr::case_when(
      is.na(entity_code_2021) ~ as.character(entity_2021_manual),
      T ~ as.character(entity_2021_guess)
    )
  ) %>% 
  ## Deselect these cols as they will be re-added by the next join.
  dplyr::select(
    -c(entity_code_2021, department_2021, longname_2021)
  ) %>% 
  dplyr::left_join(
    orgs_2021_lookup,
    by = c("entity_2021_final" = "entity_2021")
  )

## Make a file to allow a manual lookup to sort out duplicate entity values

orgs_matched_duplicates <- 
  orgs_matched_corrected %>% 
  dplyr::group_by(entity_2021_final) %>% 
  dplyr::mutate(group_size = dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(group_size > 1)

sysdatetime <- Sys.time() %>% 
  format("%Y-%m-%d_%H-%M-%S_%Z")

readr::write_excel_csv(orgs_matched_duplicates, paste0("outputs/", sysdatetime, " duplicate entities OFFICIAL.csv"))

## Use manual lookup to remove duplicate entries

lookup_2022_to_2021_duplicates <- readr::read_csv("inputs/lookup_2022_to_2021_duplicates.csv") %>% 
  dplyr::select(orgcode_2022, entity_code_2021_duplicates_to_keep = "entity_code_2021")

orgs_matched_duplicates_removed <- 
  orgs_matched_corrected %>% 
  dplyr::left_join(lookup_2022_to_2021_duplicates) %>% 
  dplyr::mutate(
    keep = dplyr::case_when(
      is.na(entity_code_2021_duplicates_to_keep) ~ T,
      entity_code_2021_duplicates_to_keep == entity_code_2021 ~ T,
      T ~ F
    )
  ) %>% 
  dplyr::filter(keep) %>% 
  dplyr::distinct()

## Make a final lookup table

org_lookup_2021_2022 <- 
  orgs_matched_duplicates_removed %>% 
  dplyr::select(
    orgcode_2022,
    entity_code_2021,
    longname_2022,
    longname_2021,
    entity_2021 = "entity_2021_final",
    department_2021,
    orgtype_govuk_2022
  )

sysdatetime <- Sys.time() %>% 
  format("%Y-%m-%d_%H-%M-%S_%Z")

readr::write_excel_csv(
  org_lookup_2021_2022,
  paste0("outputs/", sysdatetime, " Organisation lookup 2021 to 2022 OFFICIAL.csv")
  )
