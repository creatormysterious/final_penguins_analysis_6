
#this r script contains the code for my cleaning function. The function cleans column names, shortens species names, removes empty rows and columns, deletes columns with names starting with 'delta' and removes the 'comments' column.


#shorten_species() is not a base function in r, so it must first also be defined 
shorten_species <- function(penguins_data) {
  penguins_data %>%
    mutate(species = case_when(
      species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
      species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
      species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo"
    ))
}

cleaning <- function(penguins_raw){
  penguins_raw %>%
    clean_names() %>%
    shorten_species() %>%
    remove_empty(c("rows", "cols")) %>%
    dplyr::select(-starts_with("delta")) %>%
    dplyr::select(-comments)
}


# A function to remove rows which contain NA values
remove_NA <- function(penguins_data) {
  penguins_data %>%
    na.omit()
}

