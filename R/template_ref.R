new_template_ref <- function(test){
  structure(test, class = "template_ref")
}


setClass("RefTemplate",
         slots = c(
           name = "character",
           levnames = "character",
           levs = "list",
           levframes = "list"
         ),
         prototype = list(
           name = NA_character_,
           levnames = NA_character_,
           levs = list(),
           levframes = list()
         )
        )


setGeneric("ref_levs", function(x) standardGeneric("ref_levs"))
setGeneric("ref_levnames", function(x) standardGeneric("ref_levnames"))
setGeneric("ref_levframes", function(x) standardGeneric("ref_levframes"))

setGeneric("get_levs", function(x, lev) standardGeneric("get_levs"))
setGeneric("get_levframes", function(x, lev) standardGeneric("get_levframes"))

setMethod("ref_levs", "RefTemplate", function(x) x@levs)
setMethod("ref_levnames", "RefTemplate", function(x) x@levnames)
setMethod("ref_levframes", "RefTemplate", function(x) x@levframes)

setMethod("get_levs", "RefTemplate", function(x, lev) x@levs[[lev]])
setMethod("get_levframes", "RefTemplate", function(x, lev) x@levframes[[lev]])

setMethod("show", "RefTemplate", function(object){
  cat(object@name, "\n", object@levnames)
})

setClass("RefData",
         slots = c(
           name = "character",
           levnames = "character",
           reftab = "tbl_df",
           ref1 = "list",
           ref2 = "list",
           idvs = "list",
           labs = "list",
           clrs = "list"
         )
         )

setGeneric("ref_tab", function(x) standardGeneric("ref_tab"))
setGeneric("ref_ref1", function(x) standardGeneric("ref_ref1"))
setGeneric("ref_ref2", function(x) standardGeneric("ref_ref2"))
setGeneric("ref_idvs", function(x) standardGeneric("ref_idvs"))
setGeneric("ref_labs", function(x) standardGeneric("ref_labs"))
setGeneric("ref_cols", function(x) standardGeneric("ref_cols"))

setGeneric("get_idvs", function(x, lev) standardGeneric("get_idvs"))
setGeneric("get_labs", function(x, lev) standardGeneric("get_labs"))
setGeneric("get_cols", function(x, lev) standardGeneric("get_cols"))

setMethod("ref_tab", "RefData", function(x) x@reftab)
setMethod("ref_ref1", "RefData", function(x) x@ref1)
setMethod("ref_ref2", "RefData", function(x) x@ref2)
setMethod("ref_idvs", "RefData", function(x) x@idvs)
setMethod("ref_labs", "RefData", function(x) x@labs)
setMethod("ref_cols", "RefData", function(x) x@clrs)

setMethod("get_idvs", "RefData", function(x, lev) x@idvs[[lev]])
setMethod("get_labs", "RefData", function(x, lev) x@labs[[lev]])
setMethod("get_cols", "RefData", function(x, lev) x@clrs[[lev]])

setMethod("ref_levnames", "RefData", function(x) x@levnames)





