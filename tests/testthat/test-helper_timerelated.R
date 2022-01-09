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

test_that("function_dt_plot_period_1", {
  suppressWarnings(library(ggplot2))
  dt = data.frame( id=c('ID01','ID02','ID03'), idn=1, start=160947720+10800*c(0,1,2), end=160947720+10800*c(2,-1,3), label=c('A','B','C') )
  p=time_plot_interval( dt, xlab='Time', ylab='ID', legend_title='Group', arrow_wt=3, arrow_color='gray')

  expect_equal( names(p$data), c('id','start','end','label','idn','adj','idl') )
  expect_equal( names(p$labels), c('fill','y','x','xend','yend','label') )
})

test_that("function_dt_plot_event_1", {
  suppressWarnings(library(ggplot2))
  dt = data.frame( id=paste('member',c(rep(c(1,2,3),each=3),3),sep=""), idn=c(rep(1,3),rep(-1,3), rep(2,4)), start=1800*c(0,1,2, 0.5, 1.2, 3, 1,2,3,4), end=1800*c(2,NA,3, 2, 6, NA, 2,3.5,3, 3.5),
                   label=c(paste('event-',seq(1,10),sep='')), labelend=c(paste('end-',seq(1,10),sep='')), color=rep('', 10), type=c('b', 'p', 'i','i','p','p','p','b','i','i' ) )
  p=time_plot_event( dt ) + xlab('Time')

  expect_equal( names(p$data), c('id','start','end','label','labelend','color','type','idn','yloc','yloc2') )
  expect_equal( names(p$labels), c('x','colour','yintercept','y','xend','yend','xmin','xmax','ymin','ymax','label','hjust','vjust') )
})


