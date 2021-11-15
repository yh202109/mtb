#  Copyright (C) 2020 Y Hsu <yh202109@gmail.com>
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
#' Plot periods of events
#'
#' @description
#' Create a plot for event periods by ID
#'
#' @importFrom ggplot2 ggplot aes geom_segment geom_label xlab ylab arrow labs scale_y_continuous aes_string
#'
#' @param dt a \code{data.frame} with the following columns
#' \itemize{
#'   \item{id}{for ID of each interval}
#'   \item{\code{idn}}{for order of ID}
#'   \item{start}{for starting time}
#'   \item{end}{for ending time with arrow head}
#'   \item{label}{for labeling the starting time}
#' }
#' @param xlab A string for the label of X-axis
#' @param ylab A string for the label of Y-axis
#' @param legend_title A string for the title of legend
#' @param arrow_wt An integer for the weight of arrow
#' @param arrow_color A string for the color of arrow
#'
#' @return a plot
#'
#' @examples
#' library(ggplot2)
#' dt = data.frame( id=c('ID01','ID12','ID3'), idn=c(1,3,2), start=1800*c(0,1,2), end=1800*c(2,-1,3),
#' label=c('A','B','C') )
#' time_plot_interval( dt, xlab='Time', ylab='ID', legend_title='Group', arrow_wt=3,
#' arrow_color='gray')
#'
#' @export

time_plot_interval <- function( dt, xlab='DateTime', ylab='ID', legend_title='Group', arrow_wt=1, arrow_color='black' ){
  dt$start = mtb_dt_toPOSIXct(dt$start)
  dt$end = mtb_dt_toPOSIXct(dt$end)
  lstn = colnames(dt)
  if( !('id' %in% lstn & 'idn' %in% lstn & 'start'%in%lstn & 'end'%in%lstn & 'label' %in% lstn) ){stop('There were missing columns')}
  dt2=unique(dt[,c('id','idn')])
  if(dim(dt2)[1] != length(unique(dt2$id)) | dim(dt2)[1] != length(unique(dt2$id))){dt2$idn=1;dt2=unique(dt2)}
  dt2$idn=seq(1,dim(dt2)[1])
  dt$id = factor(dt$id, levels=dt2$id)
  dt$idn = NULL
  dt=merge(dt, dt2, by='id', all.x=TRUE)
  dt=dt[order(dt$idn, dt$id, dt$start),]
  maxn=max(table(dt$id))
  dt$adj=0
  for(idx in 2:dim(dt)[1]){
    if(dt$id[idx]==dt$id[idx-1] & (dt$start[idx]<dt$end[idx-1] | (dt$end[idx]<max(dt$start[idx-1],dt$end[idx-1])&dt$end[idx]>min(dt$start[idx-1],dt$end[idx-1])) )){ dt$adj[idx]=dt$adj[idx-1]+1 }
  }
  dt$idl = dt$idn + dt$adj*0.5/(1+max(dt$adj))
  p = ggplot(dt, aes_string(x='start', y='idl'))+geom_segment(aes_string(x='start', y='idl', xend='end',yend='idl'), arrow=arrow(), size=arrow_wt, colour=mtb_color2rgb(arrow_color,totri=FALSE),alpha=0.7)
  p = p + scale_y_continuous(breaks=dt$idn, labels=dt$id)
  p = p + geom_label(aes_string(x='start', y='idl', label='label', fill='label'), alpha=0.5)
  p = p + xlab(xlab) + ylab(ylab) + labs(fill=legend_title)
  p
}

