#  Copyright (C) 2021 Y Hsu <yh202109@gmail.com>
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

ldt2 <- list(data.frame(col1=c('a','b'), col2=c(3,4)),
             data.frame(col1=c('b','c'), col3=c(3,4)))

test_that("cross_summary1 - input validation", {
  ldt1 <- list(data.frame(col1=c(), col2=c()), data.frame(col1=c('b','c'), col3=c(3,4)), NA, NULL)
  expect_error(bill_cross_count(ldt1, 'col1'), 'ldt should be a list of tables', fixed=TRUE)
  expect_error(bill_cross_count(ldt2, 'col'),  'id should be a column name',     fixed=TRUE)
  expect_error(bill_cross_count(ldt2, 'col1', gp='col2'), 'gp should be a column name', fixed=TRUE)
})

test_that("cross_summary1 - type=count basic output", {
  out <- bill_cross_count(ldt2, 'col1')
  expect_s3_class(out, "data.frame")
  # First row: item 'a', present in table 1 only
  expect_equal(as.vector(as.matrix(out)[1, ]), c('a', ' 1', NA))
})

test_that("cross_summary1 - type=count with gp grouping", {
  ldt_gp <- list(data.frame(col1=c('a','b'), gp=c('x','x')),
                 data.frame(col1=c('b','c'), gp=c('x','y')))
  out <- bill_cross_count(ldt_gp, id='col1', gp='gp')
  expect_s3_class(out, "data.frame")
  expect_true("gp" %in% names(out))
  expect_true("col1" %in% names(out))
})

test_that("cross_summary1 - type=cond with empty condstr returns NULL", {
  expect_null(bill_cross_count(ldt2, 'col1', type='cond'))
})

test_that("cross_summary1 - type=cond with valid condstr", {
  ldt_num <- list(data.frame(col1=c('a','b'), val=c(1,5)),
                  data.frame(col1=c('a','b'), val=c(3,2)))
  out <- bill_cross_count(ldt_num, id='col1', type='cond', condstr='val>2')
  expect_s3_class(out, "data.frame")
  expect_false(is.null(out))
})

test_that("cross_summary1 - type=condwt with empty condstr returns NULL", {
  expect_null(bill_cross_count(ldt2, 'col1', type='condwt'))
})

test_that("cross_summary1 - type=condwt with valid condstr", {
  ldt_num <- list(data.frame(col1=c('a','b'), val=c(1,5)),
                  data.frame(col1=c('a','b'), val=c(3,2)))
  out <- bill_cross_count(ldt_num, id='col1', type='condwt', condstr='val>2')
  expect_s3_class(out, "data.frame")
  # condwt cells should contain "N(T)" formatted strings
  data_cols <- setdiff(names(out), 'col1')
  data_cells <- unlist(as.data.frame(out)[, data_cols, drop=FALSE])
  data_cells <- data_cells[!is.na(data_cells)]
  expect_true(all(grepl("^[0-9]+\\([0-9]+\\)$", data_cells)))
})

test_that("cross_summary1 - unknown type returns NULL", {
  expect_null(bill_cross_count(ldt2, 'col1', type='unknown'))
})

test_that("cross_summary2 - input validation", {
  dt1 <- data.frame(col1=c(1,2,3,3), col2=c('a','b','c','c'), col3=c('-','=','+','-'))
  dt2 <- data.frame(col1=c(1,2,3),   col2=c('a','b','c'),     col3=c('-','=','+'))
  expect_error(bill_cross_check(NULL, dt2, id='col1'),          'dt1 and dt2 should be tables', fixed=TRUE)
  expect_error(bill_cross_check(dt1,  NULL, id='col1'),         'dt1 and dt2 should be tables', fixed=TRUE)
  expect_error(bill_cross_check(dt1,  dt2, id=c('col1','col2'), chk='a'),  'chk should be a column name', fixed=TRUE)
  expect_error(bill_cross_check(dt1,  dt2, id=c('col1','col2'), chk=NA),   'chk should be a column name', fixed=TRUE)
})

test_that("cross_summary2 - same column with matching values", {
  dt1 <- data.frame(col1=c(1,2,3,3), col2=c('a','b','c','c'), col3=c('-','=','+','-'))
  dt2 <- data.frame(col1=c(1,2,3),   col2=c('a','b','c'),     col3=c('-','=','+'))
  out <- bill_cross_check(dt1, dt2, id=c('col1','col2'), chk='col3')
  expect_s3_class(out, "data.frame")
  expect_true("same" %in% names(out))
  expect_equal(as.numeric(out[1, 'same']), 1)
})

test_that("cross_summary2 - same column detects mismatch", {
  dt1 <- data.frame(col1=c(1), col2=c('a'), col3=c('X'))
  dt2 <- data.frame(col1=c(1), col2=c('a'), col3=c('Y'))
  out <- bill_cross_check(dt1, dt2, id=c('col1','col2'), chk='col3')
  expect_equal(as.numeric(out[1, 'same']), 0)
})

test_that("cross_summary2 - chk=NULL counts rows", {
  dt1 <- data.frame(col1=c(1,2,3,3), col2=c('a','b','c','c'), col3=c('-','=','+','-'))
  dt2 <- data.frame(col1=c(1,2,3),   col2=c('a','b','c'),     col3=c('-','=','+'))
  out <- bill_cross_check(dt1, dt2, id=c('col1','col2'), chk=NULL)
  expect_true("same" %in% names(out))
  expect_equal(as.numeric(out[1, 'same']), 1)
})

test_that("cross_summary2 - chk with multiple values silently treated as NULL", {
  dt1 <- data.frame(col1=c(1,2), col2=c('a','b'), col3=c('-','='))
  dt2 <- data.frame(col1=c(1,2), col2=c('a','b'), col3=c('-','='))
  # chk with length > 1 is silently coerced to NULL (row-count mode)
  expect_no_error(bill_cross_check(dt1, dt2, id=c('col1'), chk=c('col2','col3')))
})
