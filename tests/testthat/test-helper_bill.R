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

test_that("cross_summary1", {

  ldt1=list(data.frame(col1=c(), col2=c()), data.frame(col1=c('b','c'), col3=c(3,4)), NA, NULL )
  ldt2=list(data.frame(col1=c('a','b'), col2=c(3,4)), data.frame(col1=c('b','c'), col3=c(3,4)))
  expect_error(bill_cross_count(ldt1, 'col1') , 'ldt should be a list of tables', fixed=TRUE)
  expect_error(bill_cross_count(ldt2, 'col') , 'id should be a column name', fixed=TRUE)
  expect_error(bill_cross_count(ldt2, 'col1', gp='col2') , 'gp should be a column name', fixed=TRUE)
  expect_equal(as.vector(as.matrix(bill_cross_count(ldt2, 'col1'))[1,]) , c('a',' 1',NA) )
  expect_equal(bill_cross_count(ldt2, 'col1', type='cond') , NULL )
  expect_equal(bill_cross_count(ldt2, 'col1', type='condwt') , NULL )

})
