# references
# Open reference papers/reports used in this project.
new_reflist <- function(){
  new.ref <- file.choose()
  refs <- list(new.ref)
  return(refs)
}
  
add_refs <- function(reflist = ref_list){
  new.ref <- file.choose()
  refs <- list(new.ref, reflist)
  return(refs)
}

open_refs <- function(reflist = ref_list){
  reflist %>% purrr::walk(~system(paste0('open "', refs, '"')))
}

ref_list <- new_reflist()

add_refs()

open_refs()

