#' Interpolates genind by pop then spp. and converts to matrix
#'
#' @param genind genind object
#'
#' @return returns matrix
#' @export
#'
#' @examples
#' print("test")
na_interpolator_pop <- function(genind) {

  tempframe <- as_tibble(gen_tabber(genind)) %>% add_column(Pop = genind@strata$pop, Indv = rownames(genind@tab),
                                                            Species = genind@strata$species) %>%
    select(Indv, Pop, Species, everything())
  popx <- unique(tempframe$Pop %>% as.character)
  spex <- unique(tempframe$Species %>% as.character)
  newframe <- tempframe %>% slice(n = 0)

  for (po in popx) {
    popframe <- tempframe %>% filter(Pop == po)
    print(po)
    if (nrow(popframe) < 2){
      print("Skipping interpolation as only one idv in pop")
      newframe <- newframe %>% add_row(popframe)
      next
    }
    popframe <- popframe %>% select(-Indv, -Pop, -Species) %>% as.matrix %>% apply(2, f3) %>% as_tibble()
    popframe2 <- tempframe %>% filter(Pop == po) %>% select(Indv, Pop, Species) %>% add_column(popframe)
    newframe <- newframe %>% add_row(popframe2)
  }
  tempframe <- newframe
  newframe <- tempframe %>% slice(n = 0)

  for (spe in spex) {

    popframe <- tempframe %>% filter(Species == spe)
    print(spe)
    if (nrow(popframe) < 2){
      print("skipping assignment as only one individual")
      newframe <- newframe %>% add_row(popframe)
      next
    }
    popframe <- popframe %>% select(-Indv, -Pop, -Species) %>% as.matrix %>% apply(2, f3) %>% as_tibble()
    popframe2 <- tempframe %>% filter(Species == spe) %>% select(Indv, Pop, Species) %>% add_column(popframe)
    newframe <- newframe %>% add_row(popframe2)
  }

  newframe <- newframe %>% arrange(Species, Pop, Indv)
  finalframe <- newframe %>% select(-Indv, -Pop, -Species) %>% as.matrix %>% apply(2, f3)
  return(finalframe)
}
