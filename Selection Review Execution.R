source("ini.R")
Authorization()
Authorization(NewLogin = TRUE)

source("query.R")
source("get_data.R")

RMarkdownFileName <- "Selection-Review-V7.Rmd"

FreshCategory <- CleanSelection %>%
  select(Cate2Id, Cate2, TikingonCateReport) %>%
  distinct(Cate2Id, .keep_all = TRUE) %>%
  filter(TikingonCateReport == 'FRESH')

FmcgCategory <- CleanSelection %>%
  select(Cate2Id, Cate2, TikingonCateReport) %>%
  distinct(Cate2Id, .keep_all = TRUE) %>%
  filter(TikingonCateReport %in% c('FMCG', 'NON-FOOD'))


# FreshReviewCategory <- FreshCategory$Cate2Id
# FmcgReviewCategory <- FmcgCategory$Cate2Id

# OVERVIEW

# FileList <- data.frame(
#   FileName = c(as.character(NA)),
#   LocalAddress = c(as.character(NA)),
#   DriveFolderPathId = c(as.character(NA))) %>%
#   slice(-1)


for (i in 1:length(Regions)) {
  FolderName <- as.character(Sys.Date())
  ReviewRegion <- Regions[[i]]
  ReviewCateReport <- c("FRESH")
  rmarkdown::render(RMarkdownFileName,
                    output_format = "html_document",
                    output_file = paste0("0 - ", ReviewCateReport[[1]]," ", ReviewRegion, " - Selection Review - Overview"),
                    output_dir = paste0(getwd(),"/OUTPUT/", FolderName))

  ReviewCateReport <- c("FMCG", "NON-FOOD")
  rmarkdown::render(RMarkdownFileName,
                    output_format = "html_document",
                    output_file = paste0("0 - ", ReviewCateReport[[1]]," ", ReviewRegion, " - Selection Review - Overview"),
                    output_dir = paste0(getwd(),"/OUTPUT/", FolderName))
}

dir <- paste0(getwd(),"/OUTPUT/", FolderName)
FileList <- list.files(path = dir, full.names = TRUE)

ClearDriveFolder(DriveFolderId)
for (i in 1:length(FileList)) {
  drive_upload(FileList[[i]], path = DriveFolderId)
}

