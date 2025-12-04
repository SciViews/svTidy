# svTidy 0.1.1

*   A bug in `separate_()` is corrected: when `into=` contains `NA` and the data frame contains other columns, insertion of new columns failed.

*   On the contrary to `tidyr::separate()`, `separate_()` does not allow any more all `NA` values for the `into=` argument (meaning we want to keep none of the separated columns). It raises an error with a tip to use `select_()` instead to eliminate a column from the data frame.

# svTidy 0.1.0

*   Initial version with code that was initially in svBase.
