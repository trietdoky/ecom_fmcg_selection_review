ClearDriveFolder <- function(folder_id) {
  if (nrow(drive_ls(as_id(folder_id))) == 0) {
    print("FOLDER EMPTY")
  } else {
    file_list <- drive_ls(as_id(folder_id))
    for (i in 1:nrow(file_list)) {
      drive_rm(file_list[[i,2]])
    }
    print("FOLDER CLEARED")
  }
}
