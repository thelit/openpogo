% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pokemon.R
\name{movematches.add}
\alias{movematches.add}
\title{Adds a combination of pokemon and move to the movematches dataframe
Need to do this any time you add a new pokemon to the database or
an existing pokemon gets a new move}
\usage{
movematches.add(Pokemon, Attack, Obsolete = FALSE)
}
\arguments{
\item{Pokemon}{name of a pokemon (or vector of names to iterate)}

\item{Attack}{name of an attack (or vector of attacks)}

\item{Obsolete}{can the move still be found in the wild?}
}
\description{
Adds a combination of pokemon and move to the movematches dataframe
Need to do this any time you add a new pokemon to the database or
an existing pokemon gets a new move
}
