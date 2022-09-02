ref_levlr <- function(datavect, templatevect){
  tempfact <- factor(datavect)
  levels(tempfact) <- unique(templatevect)
  return(tempfact)
}


ref_levlr_all <- function(datframe, template){
  levsall <- ref_levnames(template)
  for (rff in levsall){
    datavect <- datframe[[rff]]
    templatevect <- get_levs(template, rff)
    newfact <- ref_levlr(datavect, templatevect)
    datframe[rff] <- newfact
  }
  return(datframe)
}

ref_levlr_std <- function(datframe, template){
  levsall <- ref_levnames(template)
  for (rff in levsall){
    datavect <- factor(datframe[[rff]])
    newfact <- fct_relevel(datavect, sort(unique(as.character(datavect))))
    datframe[rff] <- newfact
  }
}
