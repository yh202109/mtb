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

test_that("function_trans_composition_1", {
  pdt=data.frame(x=rep(c(0.5, 1, 10,11,12, 100, 1000), each=5))
  pdt$y=pdt$x+rnorm(length(pdt$x))
  t=trans_composition(pdt$x,brk=30, dab=3)
  outbr=t$breaks(c(1,2))
  outtr=t$transform(c(-1, 1,15,100,1005))
  outtr2=t$transform(NA)
  outinv=t$inverse(c(-1,1,15,100,1005))
  outinv2=t$inverse(NA)

  expect_equal(t$name, 'composition')
  expect_equal(outbr, c(1, 10,11,12, 100,1000))
  expect_equal(outtr, c(-1, 1,15, 18, 21))
  expect_equal(outtr2, (NA))
  expect_equal(outinv, c(-1, 1,15, 1000,1000))
  expect_equal(outinv2, (NA))
})

test_that("function_trans_composition_2", {
  pdt=data.frame(x=rep(c(0.5, 1, 10,11,12, 100, 1000), each=5))
  pdt$y=pdt$x+rnorm(length(pdt$x))
  t=trans_composition(pdt$x,brk=1001)
  outbr=t$breaks(c(1,2))
  outtr=t$transform(c(-1,1,15,100,1005))
  outtr2=t$transform(NA)
  outinv=t$inverse(c(-1,1,15,100,1005))
  outinv2=t$inverse(NA)

  expect_equal(t$name, 'composition')
  expect_equal(outbr, c(1,100,1000))
  expect_equal(outtr, c(-1,1,15,100,1005))
  expect_equal(outtr2, NA)
  expect_equal(outinv, c(-1,1,15,100,1005))
  expect_equal(outinv2, NA)
})

test_that("function_trans_composition_3", {
  pdt=data.frame(x=rep(c(0.5, 1, 10,11,12, 100, 1000), each=5))
  pdt$y=pdt$x+rnorm(length(pdt$x))
  t=trans_composition(pdt$x,brk=-1)
  outbr=t$breaks(c(1,2))
  outtr=t$transform(c(-1,1,15,100,1005))
  outtr2=t$transform(NA)
  outinv=t$inverse(c(-1,1,15,100,1005))
  outinv2=t$inverse(NA)

  expect_equal(t$name, 'composition')
  expect_equal(outbr, c(0.5,1,10,11,12,100,1000))
  expect_equal(round(outtr,2), c(0.5,1.5,4.53,5.5,6.5))
  expect_equal(outtr2, (NA) )
  expect_equal(round(outinv,2), c(0.5,0.75,1000,1000,1000))
  expect_equal(outinv2, (NA))
})

