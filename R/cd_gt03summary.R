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

############################################################
#' Create a crosstab from a data frame with optional percentage reporting
#'
#' @description
#' \code{crosstab_from_list()} produces a crosstab (contingency table) from a
#' data frame, supporting multi-column row and column keys and optional
#' within-group percentage calculations.
#'
#' @param df A \code{data.frame}.
#' @param rows A character vector of column names in \code{df} to use as row keys.
#' @param cols A character vector of column names in \code{df} to use as column keys.
#' @param perct_within_index A character vector of column names (subset of \code{rows}
#'   or \code{cols}) defining the grouping within which percentages are computed.
#'   \code{NULL} (default) skips percentage calculation.
#' @param col_margin_perct Logical. Reserved for future use. Default \code{FALSE}.
#' @param row_margin_perct Logical. Reserved for future use. Default \code{FALSE}.
#' @param report_type An integer, either \code{1} (count and percent) or
#'   \code{2} (count/total and percent). Default \code{1}.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{count}{A data frame of raw counts with an \code{All} margin row and column.}
#'   \item{percent}{A data frame of percentages, or \code{NULL} if \code{perct_within_index} is \code{NULL}.}
#'   \item{report}{A data frame of formatted strings combining counts and percentages,
#'     or \code{NULL} if \code{perct_within_index} is \code{NULL}.}
#'   \item{total}{A data frame of group totals used for percentage calculation,
#'     or \code{NULL} if \code{perct_within_index} is \code{NULL}.}
#' }
#'
#' @examples
#' df <- data.frame(
#'     A = c('foo','foo', 'foo', 'foo', 'foo', 'foo', 'bar', 'bar', 'baz', 'baz', 'baz'),
#'     B = c('one','one','one','one', 'one', 'two', 'two', 'one', 'one', 'two', 'two'),
#'     C = c('y','x','x','x', 'y', 'x', 'y', 'x', 'y', 'x', 'y'),
#'     D = c('apple','apple','apple','apple', 'banana', 'apple', 
#'     'banana', 'apple', 'banana', 'apple', 'banana'),
#'     E = c('red','blue','red','red', 'red', 'blue', 'blue', 'red', 'blue', 'red', 'blue'),
#'     value = c(0,1,1,1, 2, 3, 4, 5, 6, 7, 8)
#' )
#' result <- crosstab_from_list(
#'     df = df,
#'     rows = c("A", "B", "C"),
#'     cols = c("D", "E"),
#'     perct_within_index = "A",
#'     col_margin_perct = TRUE,
#'     row_margin_perct = TRUE,
#'     report_type = 1
#' )
#' print(result$report)
#'
#' @importFrom stats na.omit
#' @export


crosstab_from_list <- function(df, rows, cols, perct_within_index=NULL, col_margin_perct=FALSE, row_margin_perct=FALSE, report_type=1) {
    if (!is.data.frame(df)) stop("'df' must be a data.frame.")
    if (nrow(df) == 0) stop("'df' must not be empty.")
    if (length(rows) == 0 || length(cols) == 0) stop("'rows' and 'cols' must be non-empty vectors.")
    if (!all(rows %in% names(df))) stop("All elements in 'rows' must be column names of df.")
    if (!all(cols %in% names(df))) stop("All elements in 'cols' must be column names of df.")
    if (length(intersect(rows, cols)) > 0) stop("The intersection of 'rows' and 'cols' must be empty.")
    if (!is.null(perct_within_index)) {
        if (!is.vector(perct_within_index)) stop("'perct_within_index' must be a vector of column names.")
        for (idx in perct_within_index) {
            if (!(idx %in% rows || idx %in% cols)) stop(paste0("'", idx, "' must be in either 'rows' or 'cols'."))
        }
    }
    if (!(report_type %in% c(1,2))) stop("'report_type' must be either 1 or 2.")
    if (!is.logical(col_margin_perct)) stop("'col_margin_perct' must be logical.")
    if (!is.logical(row_margin_perct)) stop("'row_margin_perct' must be logical.")

    subdf_cols <- unique(c(rows, cols, perct_within_index))
    subdf <- df[, subdf_cols, drop=FALSE]

    # Convert flag columns to factors with ordered levels if needed
    flag_cols <- grep("fl$", names(subdf), ignore.case=TRUE, value=TRUE)
    for (col in flag_cols) {
        vals <- sort(unique(na.omit(subdf[[col]])))
        if ("Y" %in% vals || "N" %in% vals) {
            vals <- c("Y", setdiff(vals, "Y"))
            vals <- c("N", setdiff(vals, "N"))
        }
        subdf[[col]] <- factor(subdf[[col]], levels=vals, ordered=TRUE)
    }

    # Build row and col keys via interaction (for counting), keeping originals for output
    row_key <- if (length(rows) == 1) subdf[[rows]] else interaction(subdf[rows], sep="\t", drop=TRUE)
    col_key <- if (length(cols) == 1) subdf[[cols]] else interaction(subdf[cols], sep="\t", drop=TRUE)

    # Raw count matrix
    ct_mat <- as.data.frame.matrix(table(row_key, col_key, useNA="ifany"))

    # Rename columns: recover distinct col-key combinations, build readable names
    col_levels <- colnames(ct_mat)
    if (length(cols) > 1) {
        col_parts <- do.call(rbind, strsplit(col_levels, "\t", fixed=TRUE))
        colnames(ct_mat) <- apply(col_parts, 1, paste, collapse=" | ")
    }
    ct_mat$All <- rowSums(ct_mat)

    # Recover distinct row-key combinations from ct_mat row names
    row_levels <- rownames(ct_mat)
    if (length(rows) == 1) {
        row_index <- data.frame(setNames(list(row_levels), rows), stringsAsFactors=FALSE)
    } else {
        row_index <- as.data.frame(
            do.call(rbind, strsplit(row_levels, "\t", fixed=TRUE)),
            stringsAsFactors=FALSE
        )
        names(row_index) <- rows
    }

    # Sort rows alphabetically by each rows variable (left to right)
    row_ord <- do.call(order, lapply(rows, function(v) row_index[[v]]))
    row_index <- row_index[row_ord, , drop=FALSE]
    ct_mat    <- ct_mat[row_ord, , drop=FALSE]

    # All-margin row (row index filled with "All")
    all_row_index <- as.data.frame(
        setNames(replicate(length(rows), "All", simplify=FALSE), rows),
        stringsAsFactors=FALSE
    )
    all_counts <- as.data.frame(t(colSums(ct_mat)), stringsAsFactors=FALSE)

    # Assemble count table: row-key columns + count columns
    ct1 <- cbind(row_index, ct_mat, stringsAsFactors=FALSE)
    ct1 <- rbind(ct1, cbind(all_row_index, all_counts))
    rownames(ct1) <- NULL

    # Helper to build a parallel numeric-only data frame (same shape as ct_mat columns)
    count_cols <- setdiff(names(ct1), rows)  # col-key columns + "All"

    ct_perc  <- NULL
    ct_total <- NULL
    report   <- NULL

    if (!is.null(perct_within_index) && length(perct_within_index) > 0) {
        ct_perc  <- ct1
        ct_total <- ct1
        idx_names <- intersect(rows, perct_within_index)
        col_names <- intersect(cols, perct_within_index)

        n_data_rows <- nrow(ct1) - 1L  # exclude All row

        data_idx <- seq_len(n_data_rows)  # row indices excluding the All margin row

        if (length(idx_names) > 0 && length(col_names) == 0) {
            # Percent within groups defined by the unique combination of idx_names columns.
            # All rows sharing the same idx_names values share the same denominator.
            grp_keys <- do.call(paste, c(ct1[data_idx, idx_names, drop=FALSE], sep="\t"))
            for (i in seq_len(nrow(ct1))) {
                if (i > n_data_rows) {
                    # All margin row: denominator is grand total of data counts
                    total <- sum(ct1[data_idx, setdiff(count_cols, "All")])
                } else {
                    key_i <- paste(ct1[i, idx_names], collapse="\t")
                    grp_rows <- data_idx[grp_keys == key_i]
                    total <- sum(ct1[grp_rows, setdiff(count_cols, "All")])
                }
                ct_total[i, count_cols] <- total
                ct_perc[i, count_cols]  <- round(100 * ct1[i, count_cols] / total, 1)
            }
        } else if (length(col_names) > 0 && length(idx_names) == 0) {
            # Percent within each column group
            for (j in count_cols) {
                total <- sum(ct1[data_idx, j])
                ct_total[, j] <- total
                ct_perc[, j]  <- round(100 * ct1[, j] / total, 1)
            }
        } else if (length(idx_names) > 0 && length(col_names) > 0) {
            # Grand total
            total <- sum(ct1[data_idx, setdiff(count_cols, "All")])
            ct_total[, count_cols] <- total
            ct_perc[, count_cols]  <- round(100 * ct1[, count_cols] / total, 1)
        }

        fmt_cells <- function(ct, ct_ref, sep) {
            out <- ct1
            for (j in count_cols) {
                out[[j]] <- paste0(ct[[j]], sep, ct_ref[[j]], "%)")
            }
            out
        }

        if (report_type == 1) {
            report <- fmt_cells(ct1, ct_perc, " (")
        } else if (report_type == 2) {
            for (j in count_cols) {
                ct_total[[j]] <- paste0(ct1[[j]], "/", ct_total[[j]])
            }
            report <- fmt_cells(ct_total, ct_perc, " (")
        }
    }

    return(list(count=ct1, percent=ct_perc, report=report, total=ct_total))
}


