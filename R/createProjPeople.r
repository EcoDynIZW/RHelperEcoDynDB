#' enter new project participants to the EcoDyn database
#' 
#' createProjPeople 
#' 
#' Function to add people to the EcoDyn database that participate in projects.
#' 
#' @export

createProjPeople = function (proj_name) 
{
  if (RHelperEcoDynDB::isEcoDynConnected() == FALSE) {
    conn_test = FALSE
    RHelperEcoDynDB::EcoDynConnect()
  }
  if (exists("db_con", envir = .GlobalEnv) == T) {
    db_con <- get("db_con", envir = .GlobalEnv)
  }
  if (exists("projects", envir = .GlobalEnv) == T) {
    projects <<- get0("projects", envir = .GlobalEnv)
  }
  else {
    projects <- DBI::dbGetQuery(db_con, "SELECT * FROM projects.proj_info ORDER BY proj_name, proj_year")
  }
  if ((exists("proj_name", envir = .GlobalEnv) == T) & (exists("proj_id", 
                                                               envir = .GlobalEnv) == T)) {
    proj_name <- get0("proj_name", envir = .GlobalEnv)
    proj_id <- get0("proj_id", envir = .GlobalEnv)
    type = "new project"
  }
  else {
    writeLines("\nWelcome, you want to link or enter persons to an existing project.\nStep 1: Select project:")
    project <<- utils::select.list(paste(projects$proj_name, 
                                         projects$proj_year), title = "\nSelect project by choosing it's \naccording number or '0' for exiting:", 
                                   graphics = FALSE)
    project <<- projects[paste(projects$proj_name, projects$proj_year) == 
                           project, ]
    proj_name = project$proj_name
    proj_id = project$proj_id
    proj_year = project$proj_year
    type = "existing project"
    if (length(project) == 1) {
      rm(project)
      stop("You decided to exit. Bye!")
    }
  }
  add_person = function() {
    people = DBI::dbReadTable(db_con, DBI::Id(schema = "people", 
                                              table = "people"))
    if (type == "new project") {
      writeLines("\n\nStep 3: Project persons\nTo link a person, it's affiliation and role to this project please follow the instructions.")
    }
    else {
      writeLines("\n\nStep 2: Project persons\nTo link a person, it's affiliation and role to this project please follow the instructions.")
    }
    if (exists("type")) {
      writeLines("\nStep 3.1: Select person")
    }
    else {
      writeLines("\nStep 2.1: Select person")
    }
    p_filter = readline("Enter family name (partly or first letter) to restrict search: ")
    f_people = people[grepl(tolower(p_filter), tolower(paste(people$family_name, 
                                                             people$first_name, sep = ", "))), ]
    person = utils::select.list(c(sort(paste(f_people$family_name, 
                                             f_people$first_name, sep = ", ")), "other"), graphics = F)
    if (person == "other") {
      if (type == "new project") {
        writeLines("\nStep 3.1.1: New person")
      }
      else {
        writeLines("\nStep 2.1.1: New person")
      }
      writeLines("\nYou want to add a new person to the database, please fill out the following:")
      family_name <- readline("1. Enter family name: ")
      first_name <- readline("2. Enter first name: ")
      people = DBI::dbReadTable(db_con, DBI::Id(schema = "people", 
                                                table = "people"))
      p_check = paste(people$family_name, people$first_name, 
                      sep = ", ")
      if (paste(family_name, first_name, sep = ", ") %in% 
          p_check) {
        stop(paste0("'", paste(family_name, first_name, 
                               sep = ", "), "' already exists in the database. Please check!"))
      }
      else {
        DBI::dbWriteTable(db_con, DBI::Id(schema = "people", 
                                          table = "people"), data.frame(first_name, 
                                                                        family_name), append = T)
        people = DBI::dbReadTable(db_con, DBI::Id(schema = "people", 
                                                  table = "people"))
      }
    }
    if (person != "other") {
      family_name = strsplit(person, ", ")[[1]][1]
      first_name = strsplit(person, ", ")[[1]][2]
    }
    people_id = people$people_id[paste(people$family_name, 
                                       people$first_name, sep = ", ") == paste(family_name, 
                                                                               first_name, sep = ", ")]
    person = paste(family_name, first_name, sep = ", ")
    if (type == "new project") {
      writeLines("\nStep 3.2: Person's affiliation")
    }
    else {
      writeLines("\nStep 2.2: Person's affiliation")
    }
    add_aff = function() {
      orgs = DBI::dbReadTable(db_con, DBI::Id(schema = "people", 
                                              table = "organisations"))
      affs = DBI::dbReadTable(db_con, DBI::Id(schema = "people", 
                                              table = "affiliation"))
      writeLines("\nPlease enter the person's affiliation.")
      o_filter = readline("Enter part of organisation name: ")
      f_org = orgs[grepl(tolower(o_filter), tolower(paste(orgs$organisation, 
                                                          orgs$abbreviation, sep = ", "))), ]
      organisation = utils::select.list(c(sort(paste(f_org$organisation, 
                                                     f_org$abbreviation, sep = ", ")), "other"))
      if (type == "new project") {
        writeLines("\nStep 3.2.1: Person's affiliation")
      }
      else {
        writeLines("\nStep 2.2.1: Person's affiliation")
      }
      if (organisation == "other") {
        writeLines("\nYou want to add a unknown organisation to the database, please fill out the following:")
        org = readline("1. Full name of Organisation: ")
        abbr = readline("2. Abbreviation of Organisation: ")
        countries = DBI::dbReadTable(db_con, DBI::Id(schema = "people", 
                                                     table = "countries"))
        c_filter = readline("To select the country the organisaton is resident, enter part of the country name: ")
        f_country = countries[grepl(tolower(c_filter), 
                                    tolower(countries$country)), ]
        country = utils::select.list(f_country, graphics = F)
        address = readline("Please enter full address: ")
        if (org %in% orgs$organisation) {
          stop(paste0("'", org, "' already exists in the database. Please check!"))
        }
        else {
          DBI::dbWriteTable(db_con, DBI::Id(schema = "people", 
                                            table = "organisations"), data.frame(organisation = org, 
                                                                                 abbreviation = abbr, address, country), 
                            append = T)
        }
      }
      if (organisation != "other") {
        org = strsplit(organisation, ",")[[1]][1]
      }
      year_of_aff = readline("Enter relevant year of affiliation: ")
      orgs = DBI::dbReadTable(db_con, DBI::Id(schema = "people", 
                                              table = "organisations"))
      org_id = orgs$org_id[orgs$organisation == org]
      if ((paste(people_id, org_id, year_of_aff) %in% 
           paste(affs$people_id, affs$org_id, affs$year_of_aff)) == 
          F) {
        DBI::dbWriteTable(db_con, DBI::Id(schema = "people", 
                                          table = "affiliation"), data.frame(people_id, 
                                                                             org_id, year_of_aff), append = T)
        writeLines(paste0("\n'", org, "' was added as affiliation for '", 
                          year_of_aff, "' for '", person, "."))
      }
      else {
        message(paste0("\nThe affiliation '", org, " ", 
                       year_of_aff, "' for '", person, " already exists in the database."))
      }
      writeLines("\nDoes the person have an additional affiliation relevant for this project?")
      sec_aff <<- utils::select.list(c("NO", "YES"), title = "Please chose by typing '1' or '2' and press 'Enter':", 
                                     graphics = F)
    }
    add_aff()
    while (sec_aff == "YES") {
      add_aff()
    }
    if (sec_aff == "NO") {
      writeLines(paste0("\nYou entered all relevant affiliations of person '", 
                        person, "' and linked it to the project '", 
                        proj_name, "'.\nYou can always add additional affiliations by running the createProjPeople() function."))
    }
    if (type == "new project") {
      writeLines("\nStep 3.3: Person's role")
    }
    else {
      writeLines("\nStep 2.3: Person's role")
    }
    add_role = function() {
      roles <- DBI::dbReadTable(db_con, DBI::Id(schema = "projects", 
                                                table = "proj_roles"))
      writeLines("\nPlease enter the person's project role.")
      proj_role = utils::select.list(c(sort(roles$proj_people_role), 
                                       "other"), graphics = F)
      if (proj_role == "other") {
        if (type == "new project") {
          writeLines("\nStep 3.3.1: New role")
        }
        else {
          writeLines("\nStep 2.3.1: New role")
        }
        writeLines("\nYou could not find an appropiate role and want to add a new role.")
        proj_role = readline("Enter new role title: ")
        if (proj_role == "") {
          while (proj_role == "") {
            message("When the option 'other' was chosen you must define new role or deselect 'other'.")
            rechoice = utils::select.list(c("Deselect other", 
                                            "Define new role"), title = "Please chose by typing '1' or '2':", 
                                          graphics = F)
            if (rechoice == "Define new role") {
              proj_role = readline("Enter new role title: ")
            }
            else {
              proj_role = utils::select.list(sort(roles$proj_people_role), 
                                             graphics = F)
            }
          }
        }
        if ((proj_role %in% roles$proj_people_role) == 
            F) {
          DBI::dbWriteTable(db_con, DBI::Id(schema = "projects", 
                                            table = "proj_roles"), data.frame(proj_people_role = proj_role), 
                            append = T)
        }
      }
      DBI::dbWriteTable(db_con, DBI::Id(schema = "projects", 
                                        table = "proj_people"), data.frame(proj_id, 
                                                                           people_id, proj_role), append = T)
      writeLines(paste0("\n'", person, "' was added to project '", 
                        proj_name, " ", proj_year, "' as ", proj_role, 
                        "."))
      writeLines("\nDoes the person have an additional role in this project?")
      sec_role <<- utils::select.list(c("NO", "YES"), 
                                      title = "Please chose by typing '1' or '2' and press 'Enter':", 
                                      graphics = F)
    }
    add_role()
    while (sec_role == "YES") {
      add_role()
    }
    if (sec_role == "NO") {
      writeLines(paste0("\nYou entered all relevant data of person '", 
                        person, "' and linked it to the project '", 
                        proj_name, "'.\nYou can always add additional roles by running the createProjPeople() function."))
    }
    writeLines("\nDo you want to add another person to the same project?")
    sec_person <<- utils::select.list(c("NO", "YES"), title = "Please chose by typing '1' or '2' and press 'Enter':", 
                                      graphics = F)
  }
  add_person()
  while (sec_person == "YES") {
    add_person()
  }
  if (sec_person == "NO") {
    writeLines(paste0("\nYou entered all relevant persons to the project '", 
                      proj_name, "'.\nYou can always add additional people, by running the createProjPeople() function."))
    if (type == "new project") {
      rm(list = ls())
    }
  }
}