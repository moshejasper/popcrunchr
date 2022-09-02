#' Generate master template from file...
#'
#' @param filename path to reference file
#' @param ... vector of labels to target
#'
#' @return returns template object for future functions
#' @export
#'
#' @examples
#' print("test")
load_template_ref <- function(filename, ...){
  initi <- read_tsv(filename) %>% arrange(.data$targetid)
  flevs <- list(...)
  levs <- list()
  frms <- list()
  nms <- flevs %>% unlist %>% as.vector
  for (level in flevs){
    t1 <- initi[level] %>% unlist %>% as.vector
    levs[level] <- list(t1)
    frm <- list(tibble(Lev = unique(t1), Col = palletlevs[1:length(unique(t1))]))
    frms[level] <- frm
  }

  t2 <- new("RefTemplate", name = "refdefault", levnames = nms, levs = levs, levframes = frms)
  return(t2)
}


#' Title
#'
#' @param filename path to reference file
#' @param template  object of class TempRef
#'
#' @return returns object of class DataRef
#' @export
#'
#' @examples
#' print("test")
load_data_ref <- function(filename, template){
  initi <- read_tsv(filename) %>% arrange(.data$targetid) #%>%  ref_levlr_std(template)
  templevs <- ref_levs(template)
  tempframes <- ref_levframes(template)
  ref1 <- list()
  ref2 <- list()
  idvs1 <- list()
  labs1 <- list()
  clrs1 <- list()
  tlevs <- ref_levnames(template)
  initi <- initi %>% arrange(across(all_of(tlevs)))
  for (tlev in ref_levnames(template)){
    ref1[tlev] <- list(initi %>% select("targetid", "Lev"=tlev) %>% group_by(.data$Lev) %>%
      summarise(Num = n()) %>% arrange(.data$Num, .data$Lev) %>%
      select(.data$Lev) %>% unlist %>% as.character())
    #print(tempframes[tlev])
    r2 <- initi %>% select("targetid", "Lev"=tlev) %>% group_by(.data$Lev) %>%
      summarise(Num = n()) %>% arrange(.data$Num, .data$Lev) #%>%
      r3 <- inner_join(r2, tempframes[[tlev]], by = "Lev")
      ref2[tlev] <- list(r3)
    idvs1[tlev] <- list(initi[[tlev]])
    labs1[tlev] <- list(r3 %>% select(.data$Lev) %>% unlist %>% as.character())
    clrs1[tlev] <- list(r3 %>% select(.data$Col) %>% unlist %>% as.character())
  }
  outpt <- new("RefData",
               name = "Data",
               levnames = tlevs,
               reftab = initi,
               ref1 = ref1,
               ref2 = ref2,
               idvs = idvs1,
               labs = labs1,
               clrs = clrs1
               )
  return(outpt)
}








#' Convert ref frame to ref template
#'
#' @param reftab reference tibble
#'
#' @return returns template object for future functions
#' @export
#'
#' @examples
#' print("test")
make_template_ref <- function(reftab){
  print(3)
}
