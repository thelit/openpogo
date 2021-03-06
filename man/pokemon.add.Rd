% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pokemon.R
\name{pokemon.add}
\alias{pokemon.add}
\title{Add a new pokemon to the pokemon database ('pokedex')}
\usage{
pokemon.add(
  Pokemon,
  Num,
  Type.1,
  Type.2 = "",
  Hatch.km = NA,
  Buddy.km = NA,
  Attack,
  Defense,
  Stamina,
  Gen = NA
)
}
\arguments{
\item{Pokemon}{name of the new mon}

\item{Num}{the pokedex number}

\item{Type.1}{first element type}

\item{Type.2}{second element type, if it has one}

\item{Buddy.km}{how far you need to walk with it to get a candy}

\item{Attack}{base attack stat}

\item{Defense}{base defense stat}

\item{Stamina}{base stamina stat}

\item{Hatck.km}{how far you need to walk to hatch an egg carrying it}
}
\description{
Add a new pokemon to the pokemon database ('pokedex')
}
