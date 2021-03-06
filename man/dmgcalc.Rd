% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pokemon.R
\name{dmgcalc}
\alias{dmgcalc}
\title{Calculates the damage a move will do against a target}
\usage{
dmgcalc(
  attacker,
  target,
  move,
  weather = NA,
  atk = 15,
  def = 15,
  atklvl = 40,
  deflvl = 40,
  raid = F,
  atkraid = F,
  ...
)
}
\arguments{
\item{attacker}{The row number of the mon who's attacking}

\item{target}{The name of the mon under attack}

\item{move}{The name of the move being used}

\item{weather}{Optionally, what the weather type is}

\item{atk}{Attacker's attack}

\item{def}{Defender's defense}

\item{atklvl}{Attacker's level}

\item{deflvl}{Defender's level}

\item{raid}{Is it a raid boss or a regular mon?}

\item{atkraid}{Is the attacker a raid boss?}
}
\description{
Calculates the damage a move will do against a target
}
