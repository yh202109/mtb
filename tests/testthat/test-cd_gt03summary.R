#  Copyright (C) 2025 Y Hsu <yh202109@gmail.com>
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public license as published by
#  the Free software Foundation, either version 3 of the License, or
#  any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#  GNU General Public License for more details
#
#  You should have received a copy of the GNU General Public license
#  along with this program. If not, see <https://www.gnu.org/license/>
#
############################################################

df <- data.frame(
    A = c('foo','foo', 'foo', 'foo', 'foo', 'foo', 'bar', 'bar', 'baz', 'baz', 'baz'),
    B = c('one','one','one','one', 'one', 'two', 'two', 'one', 'one', 'two', 'two'),
    C = c('y','x','x','x', 'y', 'x', 'y', 'x', 'y', 'x', 'y'),
    D = c('apple','apple','apple','apple', 'banana', 'apple', 'banana', 'apple', 'banana', 'apple', 'banana'),
    E = c('red','blue','red','red', 'red', 'blue', 'blue', 'red', 'blue', 'red', 'blue'),
    value = c(0,1,1,1, 2, 3, 4, 5, 6, 7, 8)
)

test_that("crosstab_from_list input validation", {
    expect_error(crosstab_from_list(list(), c("A"), c("D")),          "'df' must be a data.frame",       fixed=TRUE)
    expect_error(crosstab_from_list(df[0, ], c("A"), c("D")),         "'df' must not be empty.",         fixed=TRUE)
    expect_error(crosstab_from_list(df, c(),  c("D")),                 "'rows' and 'cols' must be non-empty vectors", fixed=TRUE)
    expect_error(crosstab_from_list(df, c("A"), c()),                  "'rows' and 'cols' must be non-empty vectors", fixed=TRUE)
    expect_error(crosstab_from_list(df, c("A", "Z"), c("D")),          "All elements in 'rows' must be column names of df", fixed=TRUE)
    expect_error(crosstab_from_list(df, c("A"), c("D", "Z")),          "All elements in 'cols' must be column names of df", fixed=TRUE)
    expect_error(crosstab_from_list(df, c("A", "D"), c("D")),          "The intersection of 'rows' and 'cols' must be empty", fixed=TRUE)
    expect_error(crosstab_from_list(df, c("A"), c("D"), perct_within_index="Z"), "'Z' must be in either 'rows' or 'cols'", fixed=TRUE)
    expect_error(crosstab_from_list(df, c("A"), c("D"), report_type=3),           "'report_type' must be either 1 or 2", fixed=TRUE)
    expect_error(crosstab_from_list(df, c("A"), c("D"), col_margin_perct="yes"),   "'col_margin_perct' must be logical", fixed=TRUE)
    expect_error(crosstab_from_list(df, c("A"), c("D"), row_margin_perct="yes"),   "'row_margin_perct' must be logical", fixed=TRUE)
})

test_that("crosstab_from_list returns correct structure", {
    result <- crosstab_from_list(df, rows=c("A"), cols=c("D"))
    expect_type(result, "list")
    expect_named(result, c("count", "percent", "report", "total"))
    expect_s3_class(result$count, "data.frame")
    expect_null(result$percent)
    expect_null(result$report)
    expect_null(result$total)
})

test_that("crosstab_from_list count totals are correct", {
    result <- crosstab_from_list(df, rows=c("A"), cols=c("D"))
    ct <- result$count
    row_cols  <- "A"                                   # row-key columns
    count_cols <- setdiff(names(ct), c(row_cols, "All"))  # data count columns

    # All row should equal colSums of data rows (numeric columns only)
    data_rows <- ct[ct[[row_cols]] != "All", count_cols]
    all_row   <- ct[ct[[row_cols]] == "All",  count_cols]
    expect_equal(unname(unlist(all_row)), unname(colSums(data_rows)))

    # All column should equal rowSums of count columns
    expect_equal(ct$All, rowSums(ct[, count_cols]))

    # Grand total
    expect_equal(ct$All[ct[[row_cols]] == "All"], nrow(df))
})

test_that("crosstab_from_list multi-column row and col keys", {
    result <- crosstab_from_list(df, rows=c("A","B","C"), cols=c("D","E"))
    ct <- result$count
    row_cols <- c("A","B","C")

    # Grand total in All row / All column
    expect_equal(ct$All[apply(ct[, row_cols], 1, function(r) all(r == "All"))], nrow(df))

    # Row-key columns are present as separate columns
    expect_true(all(row_cols %in% names(ct)))

    # Multi-col col names should contain " | " separator
    non_all_cols <- setdiff(names(ct), c(row_cols, "All"))
    expect_true(all(grepl(" | ", non_all_cols, fixed=TRUE)))
})

test_that("crosstab_from_list percent within row index sums to 100", {
    result <- crosstab_from_list(df, rows=c("A","B","C"), cols=c("D","E"),
                                 perct_within_index="A")
    pct <- result$percent
    expect_false(is.null(pct))
    # Each non-All row's "All" column should be 100
    non_all_rows <- which(pct[["A"]] != "All")
    expect_true(all(pct$All[non_all_rows] == 100))
})

test_that("crosstab_from_list percent within col index sums to 100", {
    result <- crosstab_from_list(df, rows=c("A"), cols=c("D","E"),
                                 perct_within_index="D")
    pct <- result$percent
    expect_false(is.null(pct))
    # Each non-All column's All-row value should be 100
    non_all_cols <- setdiff(names(pct), c("A", "All"))
    all_row <- pct[pct[["A"]] == "All", non_all_cols]
    expect_true(all(unlist(all_row) == 100))
})

test_that("crosstab_from_list report_type=1 formats correctly", {
    result <- crosstab_from_list(df, rows=c("A"), cols=c("D"),
                                 perct_within_index="A", report_type=1)
    rpt <- result$report
    expect_s3_class(rpt, "data.frame")
    expect_equal(dim(rpt), dim(result$count))
    # Count columns (not the row-key "A" column) should match "N (P%)" pattern
    count_cols <- setdiff(names(rpt), "A")
    expect_true(all(grepl("^[0-9]+ \\([0-9.]+%\\)$", as.matrix(rpt[, count_cols]))))
})

test_that("crosstab_from_list report_type=2 formats correctly", {
    result <- crosstab_from_list(df, rows=c("A"), cols=c("D"),
                                 perct_within_index="A", report_type=2)
    rpt <- result$report
    expect_s3_class(rpt, "data.frame")
    # Count columns should match "N/T (P%)" pattern
    count_cols <- setdiff(names(rpt), "A")
    expect_true(all(grepl("^[0-9]+/[0-9]+ \\([0-9.]+%\\)$", as.matrix(rpt[, count_cols]))))
})

test_that("crosstab_from_list rows are sorted alphabetically", {
    result <- crosstab_from_list(df, rows=c("A","B"), cols=c("D"))
    ct <- result$count
    data_rows <- ct[ct$A != "All", ]
    # A values should be sorted; within each A, B values should be sorted
    expect_equal(data_rows$A, sort(data_rows$A))
    for (a_val in unique(data_rows$A)) {
        b_vals <- data_rows$B[data_rows$A == a_val]
        expect_equal(b_vals, sort(b_vals))
    }
    # All row is always last
    expect_equal(tail(ct$A, 1), "All")
})

test_that("crosstab_from_list single-col keys have no separator in col names", {
    result <- crosstab_from_list(df, rows=c("A"), cols=c("D"))
    ct <- result$count
    # Single rows col: "A" is a column, not collapsed into row names
    expect_true("A" %in% names(ct))
    # Single cols: column names should not contain " | "
    data_cols <- setdiff(names(ct), c("A", "All"))
    expect_false(any(grepl(" | ", data_cols, fixed=TRUE)))
})

test_that("crosstab_from_list perct_within_index spanning rows and cols gives grand total", {
    result <- crosstab_from_list(df, rows=c("A"), cols=c("D"),
                                 perct_within_index=c("A","D"), report_type=1)
    pct <- result$percent
    expect_false(is.null(pct))
    # All cells (including margins) should sum to 100 relative to grand total
    # The All/All cell should equal 100
    expect_equal(pct$All[pct$A == "All"], 100)
})

test_that("crosstab_from_list flag columns are ordered N then Y", {
    df_fl <- data.frame(
        grpfl = c("Y","N","Y","N","Y"),
        col   = c("a","a","b","b","a")
    )
    result <- crosstab_from_list(df_fl, rows=c("grpfl"), cols=c("col"))
    ct <- result$count
    data_rows <- ct[ct$grpfl != "All", ]
    expect_equal(data_rows$grpfl[1], "N")
    expect_equal(data_rows$grpfl[2], "Y")
})

test_that("crosstab_from_list percent and report contain row-key columns", {
    result <- crosstab_from_list(df, rows=c("A","B"), cols=c("D"),
                                 perct_within_index="A", report_type=1)
    # percent and report should both carry the row-key columns
    expect_true(all(c("A","B") %in% names(result$percent)))
    expect_true(all(c("A","B") %in% names(result$report)))
    expect_true(all(c("A","B") %in% names(result$total)))
    # Row-key columns in percent should match those in count
    expect_equal(result$percent[, c("A","B")], result$count[, c("A","B")])
})
