# see "C:/Users/Natalia/OneDrive/PhD Psychology/01 - R Project/file-extensions.R"
# usethis::use_data_raw("file_extensions")

# all_zip_extensions ####
## code to prepare `all_zip_extensions` dataset goes here
all_zip_extensions <- c("7z", "7zip", "bzip2", "cpio", "gzip", "iso",
  "jar", "lzma", "lzop", "mtree", "tar", "tar.bz2", "tar.gz", "tar.lzma",
  "tar.xz", "tar.zst", "taz", "tbz", "tbz2", "tgz", "tlz", "txz", "tZ",
  "tz2", "tzo", "warc", "xz", "zip", "zstd")
#"compress", #"iso9660", #"taZ",

usethis::use_data(all_zip_extensions, internal = TRUE, overwrite = TRUE)

# all_r_extensions ####
# "*.r, *.R, *.rnw, *.Rnw, *.rmd, *.Rmd, *.rmarkdown, *.Rmarkdown, *.qmd, *.Qmd, *.md, *.rhtml, *.Rhtml, *.h, *.hpp, *.c, *.cpp"


# information ####
# ?usethis::use_data
#
# use_data() makes it easy to save package data in the correct format. I
# recommend you save scripts that generate package data in data-raw: use
# use_data_raw() to set it up. You also need to document exported datasets.
#
###
# internal: If FALSE, saves each object in its own .rda file in the data/ directory.
# These data files bypass the usual export mechanism and are available
# whenever the package is loaded (or via data() if LazyData is not true).
# If TRUE, stores all objects in a single R/sysdata.rda file.
# Objects in this file follow the usual export rules.
# Note that this means they will be exported if you are using the common
# exportPattern() rule which exports all objects except for those that start with ..
#
# compress: Choose the type of compression used by save(). Should be one of
# "gzip", "bzip2", or "xz".
#
# version: The serialization format version to use. The default, 2, was the
# default format from R 1.4.0 to 3.5.3. Version 3 became the default from R 3.6.0
# and can only be read by R versions 3.5.0 and higher.
#
# name: Name of the dataset to be prepared for inclusion in the package.
#
# open: Open the newly created file for editing? Happens in RStudio, if
# applicable, or via utils::file.edit() otherwise.
#
###
# See Also.
# The data chapter of R Packages.
# https://r-pkgs.org/data.html
#
