load_obj_rdata <- function(file, obj){
    load(file)
    return(get(obj))
}

