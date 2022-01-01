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
  pdt=data.frame(x=rep(c(0.5, 1, 10,11,12, 100, 1000), each=5), x2=rep(c(NA, Inf, 10,11,12, 100, 1000), each=5))
  pdt$y=pdt$x+rnorm(length(pdt$x))
  pdt$y2=pdt$x2+rnorm(length(pdt$x))
  t=trans_composition(pdt$x,brk=30, dab=3)
  t_2=trans_composition(pdt$x,brk=1001)

  outbr=t$breaks(c(1,2))
  outtr=t$transform(c(-1, 1,15,100,1005))
  outtr2=t$transform(NA)
  outinv=t$inverse(c(-1,1,15,100,1005))
  outinv2=t$inverse(NA)
  outmb1=t$minor_breaks(NA)
  outmb2=t$minor_breaks(1)
  outmb3=t$minor_breaks(c(1,2),c(1,2),1)

  expect_equal(t$name, 'composition')
  expect_equal(outbr, c(1, 10,12, 100,1000))
  expect_equal(outtr, c(-1, 1,15, 18, 21))
  expect_equal(outtr2, (NA))
  expect_equal(outinv, c(-1, 1,15, 1000,1000))
  expect_equal(outinv2, (NA))
  expect_equal(outmb1, numeric())
  expect_equal(outmb2, numeric())
  expect_equal(outmb3, seq(0,15,by=0.5))

  outbr_2=t_2$breaks(c(NA,2))
  outtr_2=t_2$transform(c(NA,1,Inf,100,1005))
  outinv_2=t_2$inverse(c(NA,1,Inf,100,1005))

  expect_equal(outbr_2, c(1,100,1000))
  expect_equal(outtr_2, c(NA,1,Inf,100,1005))
  expect_equal(outinv_2, c(NA, 1,Inf, 100,1005))
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


test_that("function_trans_loglinear_1", {
  pdt=data.frame(x=rep(c(0.5, 1, 10,11,12, 100, 1000), each=5), x2=rep(c(NA, Inf, 10,11,12, 100, 1000), each=5))
  pdt$y=pdt$x+rnorm(length(pdt$x))
  pdt$y2=pdt$x2+rnorm(length(pdt$x))
  t=trans_loglinear(pdt$x)
  t_2=trans_loglinear(pdt$x)

  outbr=t$breaks(c(1,2))
  outtr=suppressWarnings(t$transform(c(-1, 1,15,100,1005)))
  outtr2=t$transform(NA)
  outinv=t$inverse(c(-1,1,15,100,1005))
  outinv2=t$inverse(NA)
  outmb1=t$minor_breaks(NA)
  outmb2=t$minor_breaks(1)
  outmb3=t$minor_breaks(c(1,2),c(1,2),1)

  expect_equal(t$name, 'loglinear')
  expect_equal(outbr, c(0.5, 1, 10, 100,1000))
  expect_equal(round(outtr,2), c(NaN, 1.39, 3.69, 5.55, 7.85))
  #expect_equal(outtr2, (NA))
  expect_equal(round(outinv,2)[1:2], c(-0.42, 0.50))
  #expect_equal(outinv2, (NA))
  expect_equal(outmb1, numeric())
  expect_equal(outmb2, numeric())
  expect_equal(round(outmb3)[1:10], c(1,1,2,2,2,2,2,2,3,3))

  outbr_2=t_2$breaks(c(NA,2))
  outtr_2=t_2$transform(c(NA,1,Inf,100,1005))
  outinv_2=t_2$inverse(c(NA,1,Inf,100,1005))

  expect_equal(outbr_2, c(0.5, 1, 10, 100,1000))
  expect_equal(round(outtr_2,2), c(NA,1.39,Inf,5.55,7.85))
  expect_equal(outinv_2[1:3], c(NA, 0.5,Inf))
})
