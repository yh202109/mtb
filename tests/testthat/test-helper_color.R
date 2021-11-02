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

############################################################

test_that("function_color_test_palette_1", {
  colvect = setNames( c("#000000","#0000FF","#5537AA","#AA6E55","#FFA500"), c('a','b','c','d','e') )
  expect_equal(
    suppressWarnings(color_test_palette(colvect, type='bar')),
    barplot(rep(1,5), axes=FALSE, space=0, col=colvect, names.arg = names(colvect))
  )
})

test_that("function_color_test_palette_2", {
  expect_equal(
    suppressWarnings(color_test_palette(NULL, type='bar')),
    NULL
  )
})

test_that("function_color_test_palette_3", {
  colvect = setNames( c("#000000","#0000FF","#FFA500"), c('a','b','c') )
  x = seq(-3,3,0.1)
  expect_equal(
    suppressWarnings(color_test_palette(colvect, type='line')),
    {
      plot( x, y=dnorm(x, 0, 1), type='b', frame=FALSE, col=colvect[1], xlab='x', ylab='y')
      lines( x, dnorm(x, 0, 1.5), type='b', lty=2, col=colvect[2])
      lines( x, dnorm(x, 0, 2), type='b', lty=3, col=colvect[3])
      legend('topleft', legend=names(colvect), col=colvect, lty=c(1,2,3))
    }
  )
})
