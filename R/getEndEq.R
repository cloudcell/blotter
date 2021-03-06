#' Retrieves the most recent value of the capital account
#' @param Account string identifier of account
#' @param Date last date to retrieve calculated equity for, as string
#' @return Numeric value of the equity account
#' @export
getEndEq <- function(Account, Date)
{ # @author Peter Carl
    aname<-Account
    Account<-try(get(paste("account",aname,sep='.'), envir=.blotter), silent=TRUE)
    if(inherits(Account,"try-error"))
        stop("Account ", aname, " not found, use initAcct() to create a new account")

    toDate = paste('::', Date, sep="")
    EndEq = as.numeric(tail(Account$summary[toDate,], n=1)[,"End.Eq"])
    return(EndEq)
}

###############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/) 
# Copyright (c) 2008-2015 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: getEndEq.R 1666 2015-01-07 13:26:09Z braverock $
#
###############################################################################
