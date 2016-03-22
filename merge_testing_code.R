library(easimple)
ea_start()
library(data.table)
  
in_stacked_ohc <- fread("X:/LFS-Education Outcomes/data/lfs_data/stacked_acad_yr_set.csv", colClasses = "character")
in_stacked_dpi <- fread("X:/LFS-Education Outcomes/data/raw_data/DCFmatchedSample03012016.csv")
in_id_xwalk <- fread("X:/LFS-Education Outcomes/data/raw_data/xwalk_child_id.csv")


format_xwalk <- copy(in_id_xwalk)
format_dpi <- copy(in_stacked_dpi)

setnames(format_xwalk, colnames(format_xwalk), tolower(colnames(format_xwalk)))
setnames(format_dpi, colnames(format_dpi), tolower(colnames(format_dpi)))

ohc_child_id <- subset(in_stacked_ohc, select = "child_id")
xwalk_child_id <- subset(format_xwalk, select = "child_id")
xwalk_new_id <- subset(format_xwalk, select = "new_id")
dpi_lds_id <- subset(format_dpi, select = "lds_student_key")
dpi_child_id <- subset(format_dpi, select = "child_id")


ohc_child_id <- ea_no_dups(ohc_child_id, opt_key_all = 1)
xwalk_child_id <- ea_no_dups(xwalk_child_id, opt_key_all = 1)
xwalk_new_id <- ea_no_dups(xwalk_new_id, opt_key_all = 1)
dpi_lds_id <- ea_no_dups(dpi_lds_id, opt_key_all = 1)
dpi_child_id <- ea_no_dups(dpi_child_id, opt_key_all = 1)

setnames(ohc_child_id, "child_id", "id_test")
setnames(xwalk_child_id, "child_id", "id_test")
setnames(xwalk_new_id, "new_id", "id_test")
setnames(dpi_lds_id, "lds_student_key", "id_test")
setnames(dpi_child_id, "child_id", "id_test")

ohc_child_id[, id_test := as.character(id_test)]
xwalk_child_id[, id_test := as.character(id_test)]
xwalk_new_id[, id_test := as.character(id_test)]
dpi_lds_id[, id_test := as.character(id_test)]
dpi_child_id[, id_test := as.character(id_test)]


ztest1 <- ea_merge(ohc_child_id, xwalk_child_id, "id_test", "x")
ztest2 <- ea_merge(ohc_child_id, xwalk_new_id, "id_test", "y")
ztest3 <- ea_merge(ohc_child_id, dpi_lds_id, "id_test", "x")
ztest4 <- ea_merge(ohc_child_id, dpi_child_id, "id_test", "x")
ztest5 <- ea_merge(dpi_lds_id, xwalk_child_id, "id_test", "x")
ztest6 <- ea_merge(dpi_lds_id, xwalk_new_id, "id_test", "x")
ztest7 <- ea_merge(dpi_child_id, xwalk_child_id, "id_test", "x")
ztest8 <- ea_merge(dpi_child_id, xwalk_new_id, "id_test", "x")


dpi_ids <- subset(format_dpi, select = c("lds_student_key", "child_id"))
dpi_ids <- ea_no_dups(dpi_ids, "child_id")
format_xwalk <- ea_no_dups(format_xwalk, "new_id")


setnames(format_xwalk, c("new_id", "child_id"), c("ohc_key", "dpi_key"))
setnames(ohc_child_id, "id_test", "ohc_key")
setnames(dpi_ids, "child_id", "dpi_key")

dpi_ids[, dpi_key := as.character(dpi_key)]
format_xwalk[, ":="(ohc_key = as.character(ohc_key), dpi_key = as.character(dpi_key))]


ohc_xwalk <- ea_merge(ohc_child_id, format_xwalk, "ohc_key", "x")
dpi_xwalk <- ea_merge(dpi_ids, format_xwalk, "dpi_key", "x")

dpi_xwalk <- subset(dpi_xwalk, !is.na(ohc_key))


ohc_dpi <- ea_merge(ohc_xwalk, dpi_xwalk, "ohc_key", "x")



