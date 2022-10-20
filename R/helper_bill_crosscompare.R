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

############################################################

.datatable.aware=TRUE
tmp=""
utils::globalVariables(c("."))
#' Summarize times being purchased for individual items'
#'  from multiple grocery shopping lists.
#'
#' \code{bill_cross_count} returns a data.table showing how many times each
#' items being listed on individual bills.
#'
#' @import data.table
#'
#' @param ldt A list of grocery tables in data.frame format.
#' @param id A column name
#' @param gp A column name or a vector of column names
#' @param type A string in one of the following:
#' \itemize{
#'   \item \code{count} for number of rows in each bill
#'   \item \code{cond} for conditional counting
#'   \item \code{condwt} for conditional counting with total in parenthesis
#' }
#' @param condstr A string for conditional counting
#'
#' @return a data.table
#'
#' @examples
#'
#' bill_cross_count(list(cbind(col1=c('a','b','c'),col2=c(1,2,3)),
#' cbind(col1=c('d','c','d'),col2=c(4,5,6))), id='col1')
#'
#' @export
#'

bill_cross_count <- function(ldt=list(), id=NULL, gp=NULL, type = 'count', condstr="" ) {
  if(!is.list(ldt) | length(ldt)<1){stop('ldt should be a list of tables')}
  ldt=lapply(seq(1,length(ldt)), FUN=function(x)as.data.table(ldt[[x]]))
  sdt=sapply(ldt,dim)
  if(min(apply(sdt,2,min))==0){stop('ldt should be a list of tables')}
  if(!is.null(id)){id=as.character(id)}else{stop('id should be a column name')}
  if(!mtb_col_in_ldt(id, ldt)){stop('id should be a column name')}
  if(!is.null(gp)){
    if(length(gp) >= 1){
      for(idx in 1:length(gp)){if(!mtb_col_in_ldt(gp[idx], ldt)){stop('gp should be a column name')}}
    }else{gp=NULL}
  }
  lldt=rbindlist(ldt, idcol='tbl_id', fill=TRUE)
  if(is.null(names(ldt))){ lldt$tbl_id=paste0('tbl_id:',lldt$tbl_id) }else{ lldt$tbl_id=paste0('t',lldt$tbl_id,':',names(ldt)) }
  if(type=='count'){
    out=dcast(lldt[,.(tbl_count=.N),by=c(gp,id,'tbl_id')], paste(paste(c(gp, id),collapse='+'), '~ tbl_id'), value.var="tbl_count")
  }else if(type=='cond'){
    if(condstr==""){
      out=NULL
    }else{
      lldt=lldt[,`:=` (tmp=eval(parse(text=condstr))), by=c(gp,id,'tbl_id')]
      out=dcast(lldt[, .(tbl_count=sum(tmp)),by=c(gp,id,'tbl_id')], paste(paste(c(gp, id),collapse='+'), '~ tbl_id'), value.var="tbl_count")
    }
  }else if(type=='condwt'){
    if(condstr==""){
      out=NULL
    }else{
      lldt=lldt[,`:=` (total=.N, tmp=eval(parse(text=condstr))), by=c(gp,id,'tbl_id')]
      out=lldt[, .(tbl_count=sum(tmp)),by=c(gp,id,'total','tbl_id')]
      out[['tbl_count']]=paste0(out[['tbl_count']],'(',out[['total']],')')
      out=dcast(out, paste(paste(c(gp, id),collapse='+'), '~ tbl_id'), value.var="tbl_count")
    }
  }else{ out=NULL }
  out
}

############################################################
#' Check two tables with unique matching ids and generate reports
#' on duplicated ids and repeated columns when applicable..
#'
#' \code{bill_cross_check} returns a merged data.table showing
#' information regarding ids and repeated columns.
#'
#' @import data.table
#'
#' @param dt1 A table.
#' @param dt2 A table.
#' @param id A column name or a vector of column names
#' @param chk A column name
#'
#' @return a data.table
#'
#' @examples
#'
#' bill_cross_check(data.frame(col1=c(1,2,3,3), col2=c('a','b','c','c'),
#' col3=c('-','=','+','-')),data.frame(col1=c(1,2,3), col2=c('a','b','c'),
#' col3=c('-','=','+')), id=c('col1','col2'), chk='col3')
#'
#' @export
#'

bill_cross_check <- function(dt1=NULL, dt2=NULL, id=NULL, chk=NULL ) {
  if(is.null(dt1)|is.null(dt2)){stop('dt1 and dt2 should be tables.')}
  dt1$row_id = seq(1,nrow(dt1))
  dt2$row_id = seq(1,nrow(dt2))
  ldt=list(as.data.table(dt1),as.data.table(dt2))
  sdt=sapply(ldt,dim)
  if(min(apply(sdt,2,min))==0){stop('dt1 and dt2 should be tables')}
  if(!is.null(id)){id=as.character(id)}else{stop('id should be a column name')}
  if(!is.null(chk)){
    chk=as.character(chk)
    if(length(chk) == 1){
      if(is.na(chk)){stop('chk should be a column name')}
      if(!mtb_col_in_ldt(chk, ldt)){stop('chk should be a column name')}
    }else{chk=NULL}
  }
  if(length(id) >= 1){
    for(idx in 1:length(id)){
      if(is.na(id[idx])){stop('id should be a column name')}
      if(!mtb_col_in_ldt(id[idx], ldt)){stop('id should be a column name')}
    }
  }else{stop('id should be a column name')}
  lldt=rbindlist(ldt, idcol='tbl_id', fill=TRUE)
  if(is.null(names(ldt))){ lldt$tbl_id=paste0('tbl_id:',lldt$tbl_id) }else{ lldt$tbl_id=paste0('t',lldt$tbl_id,':',names(ldt)) }
  cn=(unique(lldt$tbl_id))
  if(length(chk) == 1){
    out=dcast(lldt[, .(tbl_count=paste(eval(parse(text=chk)), collapse=";")),by=c(id,'tbl_id')], paste(paste(c(id),collapse='+'), '~ tbl_id'), value.var="tbl_count")
  }else{
    out=dcast(lldt[, .(tbl_count=.N),by=c(id,'tbl_id')], paste(paste(c(id),collapse='+'), '~ tbl_id'), value.var="tbl_count")
  }
  nc=ncol(out)
  out$same=(out[[nc]]==out[[nc-1]])
  out
}

############################################################
#' Example dataset
#'
#' @name exdt
#' @docType data
#' @author package author
#' @keywords data

NULL
