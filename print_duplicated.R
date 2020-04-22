print_duplicated <- function(data_frame, variable){
  data_frame[data_frame[,variable] %in% data_frame[duplicated(data_frame[,variable]),variable],]
}
