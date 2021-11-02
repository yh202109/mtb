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

test_that("function_add_colored_str_1", {
  expect_equal(
    suppressWarnings(add_colored_str('one example with @#$%^', color=c(NA,"a",NULL), bgcolor=c(NA,"a",-1))),
    htmltools::tags$span(HTML('one example with @#$%'), style = "background-color: rgba(127,127,127,0.2); color: rgba(127,127,127,1); margin: 3px auto 3px auto; font-size:100%")
  )
})
test_that("function_add_colored_str_2", {
  expect_equal(
    suppressWarnings(add_colored_str(NULL, color=c(1,2,-3), bgcolor=c(1,265,3.5), fontsize='wrong', bold='wrong', it=3, alpha='a')),
    htmltools::tags$span(HTML(''), style = "background-color: rgba(1,255,3,0.2); color: rgba(1,2,0,1); margin: 3px auto 3px auto; font-size:100%")
  )
})
test_that("function_add_colored_str_3", {
  expect_equal(
    suppressWarnings(add_colored_str('testing', color=c(1,2,3), bgcolor=c(1,2,3), bgalpha=500 )),
    htmltools::tags$span(HTML('testing'), style = "background-color: rgba(1,2,3,1); color: rgba(1,2,3,1); margin: 3px auto 3px auto; font-size:100%")
  )
})

############################################################

test_that("function_add_colored_box_1", {
  expect_equal(
    suppressWarnings(add_colored_box(info=NULL, bgcolor='wrong', width=-1, halign='wrong', top='wrong' )),
    htmltools::tags$div(HTML("<b> &nbsp; <span style='font-size:110%;'> &#9749; </span><i> Note </i></b> <div style=\"background-color: rgba(255,255,255,0.75); padding: 10px 20px 10px 20px; border-radius: 0px 0px 5px 0px;\"></div>"), style = " background-color: rgba( 127,127,127 , 0.2); margin: 3px auto 3px auto; width: 25% ; border-width: 0px 0px 0px 3px; border-color: rgba( 127,127,127 ,1); border-style: solid; padding: 1px 1px 1px 0px; border-radius: 0px 0px 5px 0px;")
  )
})
test_that("function_add_colored_box_2", {
  expect_equal(
    suppressWarnings(add_colored_box(type='wrong', info=123, bgcolor=c(1,2,3,4,5), width=NULL, halign=NULL, top=NULL )),
    htmltools::tags$div(HTML("<b> &nbsp; <span style='font-size:110%;'>  </span><i>  </i></b> <div style=\"background-color: rgba(255,255,255,0.75); padding: 10px 20px 10px 20px; border-radius: 0px 0px 5px 0px;\">123</div>"), style = " background-color: rgba( 127,127,127 , 0.2); margin: 3px auto 3px auto; width: 50% ; border-width: 0px 0px 0px 3px; border-color: rgba( 127,127,127 ,1); border-style: solid; padding: 1px 1px 1px 0px; border-radius: 0px 0px 5px 0px;")
  )
})
test_that("function_add_colored_box_3", {
  expect_equal(
    suppressWarnings(add_colored_box(type=NULL, label=NULL, info=c(1,2,3), bgcolor=c(1,2,3), width=2, halign='r', top=TRUE )),
    htmltools::tags$div(HTML("<b> &nbsp; <span style='font-size:110%;'> &#9749; </span><i> Note </i></b> <div style=\"background-color: rgba(255,255,255,0.75); padding: 10px 20px 10px 20px; border-radius: 0px 0px 5px 0px;\"></div>"), style = "position: absolute; right:0; top:0; background-color: rgba( 1,2,3 , 0.2); margin: 3px auto 3px auto; width: 35% ; border-width: 0px 0px 0px 3px; border-color: rgba( 1,2,3 ,1); border-style: solid; padding: 1px 1px 1px 0px; border-radius: 0px 0px 5px 0px;")
  )
})


