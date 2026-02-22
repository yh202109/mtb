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

test_that("function_color_set_palette_1", {
  expect_equal(
    suppressWarnings(color_set_palette(c('a','b','d','c','c'), c(1,2,4,3,3),cols=c('blue','orange'),black='a')),
    setNames( c("#000000","#0000FF","#7F527F","#FFA500"), c('a','b','c','d') )
  )
})

test_that("function_color_set_palette_2", {
  expect_equal(
    suppressWarnings(color_set_palette(c('a','b','d','c','c'), c(1,2,3,3,4),cols=c('blue','orange'),black='a')),
    setNames( c("#000000","#0000FF","#7F527F","#FFA500"), c('a','b','c','d') )
  )
})

test_that("function_color_set_palette_3", {
  expect_equal(
    suppressWarnings(color_set_palette(c('a','b','d','c','','e','d'), c(1,2,4,3,5,6,7),cols=c('blue','orange'),black='a')),
    setNames( c("#000000","#0000FF","#5537AA","#AA6E55","#FFA500"), c('a','b','c','d','e') )
  )
})

test_that("function_color_set_palette - gray9 assignment", {
  out <- suppressWarnings(color_set_palette(c('a','b','c'), c(1,2,3),
                                            cols=c('blue'), black='a', gray9='b'))
  expect_equal(unname(out["a"]), "#000000")
  expect_equal(unname(out["b"]), "#999999")
})

test_that("function_color_set_palette - color ramp when more groups than cols", {
  # 5 groups but only 2 cols after reserving black/gray9: triggers colorRamp
  out <- suppressWarnings(color_set_palette(c('a','b','c','d','e','f'), NULL,
                                            cols=c('blue','orange'), black='a', gray9='b'))
  expect_equal(length(out), 6)
  expect_type(out, "character")
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", out)))
})

test_that("function_color_set_palette - empty vect returns empty", {
  expect_equal(suppressWarnings(color_set_palette(c())), c())
})

test_that("function_color_set_palette - returns named vector", {
  out <- suppressWarnings(color_set_palette(c('x','y'), c(1,2), cols=c('red','blue')))
  expect_named(out)
  expect_true(all(c('x','y') %in% names(out)))
})

############################################################

test_that("function_color_test_palette_2", {
  expect_equal(
    suppressWarnings(color_test_palette(NULL, type='bar')),
    NULL
  )
})

test_that("function_color_test_palette - type=bar does not error", {
  colvect <- setNames(c("#000000","#0000FF","#FFA500"), c('a','b','c'))
  expect_no_error(suppressWarnings(color_test_palette(colvect, type='bar')))
})

test_that("function_color_test_palette - type=line does not error", {
  colvect <- setNames(c("#000000","#0000FF","#FFA500"), c('a','b','c'))
  expect_no_error(suppressWarnings(color_test_palette(colvect, type='line')))
})

test_that("function_color_test_palette - unknown type returns NULL with warning", {
  colvect <- setNames(c("#FF0000"), c('a'))
  expect_warning(out <- color_test_palette(colvect, type='unknown'))
  expect_null(out)
})
