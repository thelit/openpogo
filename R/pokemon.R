#' Loads all the basic info about pokemon, their moves, and game constants
#' @export
loadpoke <- function() {
  load(paste0(getwd(), "/pokedata.rda"), envir=globalenv())
}

.onLoad <- function(libname, pkgname) {
  # loadpoke()
  # loadmonlist()
  library(data.table)
  print("First things first: loadpoke() and loadmonlist(), if you have them")
}

# Thanks pokemongohub.net for mon info

#To do:
# Need to add mechanic for Power Up Punch (and similar?).
# Inefficient to pass data between functions (e.g. raidcounters)?
# Only mylist.counters(), derivatives, mylist.dpslvl(), and mylist.statsvs() tested with secondary charge moves.
# Probably a better way to handle ID #s of mons with many varieties (e.g. Alolans, Arceus, Shaymin...).
#   Currently pokemon$Num is a character class, but used as numeric in nearmons(), raidbosses.add(), others?
# Complicate mylist.counter()'s exclude option by allowing to distinguish between single- and dual-types?

# Version:
# 1.54 - Split raidbosses.active() into raidbosses.set() and raidbosses.get()
# 1.53 - Changed column names in DB to "Fast" and "Charge", added partial support for secondary charge moves.
# 1.52 - Added 'exclude' to mylist.counter().
# 1.51 - Couple more helper functions (adding new movematches, pokemon).
# 1.50 - A couple of tweaks for gen 4.
# 1.49 - Added ordering to 'moncat'.
# 1.48 - Made 'mymons' default, but added 'data=' to functions to let it pull from other lists.
# 1.47 - Took 'Powered' column out of mymons, added 'Notes'
# 1.46 - Added shininess to mylist.add()
# 1.45 - Attempting to calculate breakpoints in mylist.dpslvl(), adjusted raid boss CP.
# 1.44 - Added raid boss helper functions raidbosses.add() and raidbosses.active().
# 1.43 - Added combos.better() for showing how good a set of IVs is, minor optimizations.
# 1.42 - Sped things up with data tables and rbindlist and added agenda helpers.
# 1.41 - Added nearmons() for finding evolutions.
# 1.40 - Optimized raid.counter(), dmgcalc() to speed data.frame() and subsetting.
# 1.39 - Added raidweather() to show which raids are enhanced by which weather.
# 1.38 - Added weather (no rounding involved?)
# 1.37 - Moved alldps() to olddps(), new one uses 'movematches' instead of 'movesets'.
# 1.36 - Added 'powered=F' option to possibles().
# 1.35 - Bugfixes.
# 1.34 - Made 'raid' a flag to mylist.counter(), mylist.strong()
# 1.33 - Bugfix related to 1.32.
# 1.32 - Switched the counter calculations to # from mymons[#,]
# 1.31 - Moved 'Move' column in allmoves to rownames.
# 1.30 - Added mylist.raidcounter()
# 1.29 - Updated attack multipliers, added pokeglobals for what it's worth.

# General data (saved with savepoke(), loaded with loadpoke()):
#   monlevels     - How many mats (dust & candies) are required to reach a level from the prior one
#   allmoves      - Information about battle moves
#   movesets      - All the different combinations of pokemon and moves
#   pokemon       - Information about each pokemon
#   effectiveness - Table of strengths of move types vs. strengths/weaknesses of defender types
#   multipliers   - Level multipliers for calculating CP and HP
#   raidbosses    - Data on the raidbosses, inc. stats and catch rates.
#   pokeglobals   - Various stats used many times - STAB multiplier, effectiveness multiplier.
#   legendaries   - Text list of legendary names.
#   movematches   - List of moves possessed by each mon, with obsolete flag if no longer used.
#   weather       - Types affected by different weather.

# Personal data:
#   mymons        - Personal catalog of pokemon.
#   mymonsbackup  - Backup of 'mymons' (updated every time savemonlist() is used).
#   candidates    - List of mons who are good candidates for investing in.
#   movechange    - List of mons with imperfect movesets in need of TMs.
#   backlog       - List of near-top level mons awaiting upgrades (probably unnecessary with 'agenda').
#   agenda        - Simple list of future dust spending priorities.
#   mymons2       - Another pokemon catalog.

# Functions:
#   loadpoke <- function()
#       Loads all data frames from file
#   moveset <- function(mon, inc.old=FALSE, bydef=FALSE)
#       Displays info about a pokemon (more or less overtaken by alldps())
#   matcost <- function(start=1, stop=40, dust=NA)
#       Calculates dust and candy req'd to level from start (or dust lvl) to stop
#   strengths <- function(types)
#       Shows strengths/weaknesses of particular attack types
#   weaknesses <- function(mon, all=FALSE)
#       Shows strengths and weaknesses of a particular pokemon
#   monsOfType <- function(type)
#       Shows all pokemon of a type
#   import.pokemon <- function()
#       Imports the pokemon data frame from the clipboard (Mac-specific)
#   import.movesets <- function()
#       Imports the movesets data frame from the clipboard (Mac-specific)
#   totaldps <- function(move1, move2, target=NA, stab1=TRUE, stab2=TRUE)
#       Calculates total DPS of a moveset, possibly with a target and spec'd STAB
#   alldps <- function(mon, target=NA, inc.old=FALSE, inc.rank=TRUE)
#       Shows DPS for all of a particular pokemon's movesets, possibly w/target
#   dmgcalc <- function(attacker, target, move, weather=NA, atk=15, def=15, atklvl=40, deflvl=40, raid=F, atkraid=F, ...)
#       Calculates a move's damage given particular attack and defense stats and levels.
#   cpcalc <- function(pokemon, level=40, attack=15, defense=15, stamina=15)
#       Calculates CP for certain stats, defaults to max
#   hpcalc <- function(pokemon, level=40, stamina=15)
#       Calculates HP for certain stats, defaults to max
#   levelcalc <- function(name, CP, HP, Dust, Attack, Defense, Stamina)
#       Calculates level for complete set of stats
#   possibles <- function(pokemon, cp, hp, dust, attack="all", defense="all", stamina="all", total="all", evolve=NA)
#       Shows all possible stat value combos for given others stats/descriptors
#   strongest <- function(target, source=NA)
#       Shows which movesets are strongest against a target pokemon
#   savepoke <- function()
#       Saves data frames to file
#   renum <- function(dataname)
#       Takes the name of a data frame (as a string!) and renumbers it (deletes rownames)
#   combos <- function(equals=3:45, min=1, max=15, print=FALSE)
#       Counts the number of combinations that equal a range, with a min roll of min and max max
#   mylist.counter <- function(source, target, raid=F, targetdef=15, targetlvl=40, Atk=NA, Level=NA, Fast=NA, Charge=NA) {
#       Calculates DPS of a source mon against a specified target, factoring stats/levels
#   mylist.counters <- function(target, raid=F,numreturn=10, targetdef=15, targetlvl=40)
#       Iterates mylist.counter() for each mon in mymons, returning top [numreturn]
#   mylist.strong <- function(source, target, Fast=NA, Charge=NA, raid=F, sourceatk=15, sourcelvl=40, Def=NA, Level=NA)
#       Calculates DPS vs. target pokemon given a source pokemon and specified Fast/Charge moves
#   mylist.strongvs <- function(source, numreturn=10, Fast="", Charge="", Name="", raid=F, sourceatk=15, sourcelvl=40, mincp=1500)
#       Iterates mylist.strong() for each mon in mymons, returning bottom [numreturn], optionally limited to [Name] e.g. "Golem"
#   mylist.raidcounters <- function(...)
#       Calls mylist.counters() with raid==T.
#   mylist.raidcounter <- function(...)
#       Calls mylist.counter() with raid==T.
#   mylist.raidstrong <- function(source, Fast="", Charge="", numreturn=10, cpmin=1500)
#       Calls mylist.strongvs() raid==T.
#   mylist.dpslvl <- function(source, target, raid=F, range=20:40)
#       Uses mylist.counter() to show how DPS vs. target changes along a range of levels.
#   mylist.strlvl <- function(source, target, Fast, Charge, raid=F, range=20:40)
#       Uses mylist.strong() to show a source's DPS against target along a range of levels.
#   mylist.statsvs <- function(source, target, Fast, Charge, raid=F, range=20:40)
#       Shows both mylist.dpslvl() and mylist.strlvl() for a source vs. a target.
#   mylist.legendaries <- function()
#       Shows all the legendaries in mymons.
#   raidweather <- function(weather=NA, inc.inactive=F) {
#       Lists all active raid bosses enhanced by current weather.
#   nearmons <- function(name, range=3)
#       Lists pokemon with numbers within range of the named one, presumably shows evolutions.
#   combos.better <- function(mon, atk=10, def=10, sta=10, minstr=10, mindef=10, minsta=10)
#       Counts the number of combinations that are better than one with given stats.
#   raidbosses.set <- function(bosslist, tier=1:5)
#       Changes which raid bosses are currently active, sets bosses in the passed tiers to inactive.
#   raidbosses.get <- function(tier=1:5)
#       Gets list of currently active bosses in selected tier(s).
#   raidbosses.add <- function(name, Tier, CP=0, Attack=0, Defense=0, HP=0, Catch.Rate=NA, Active=TRUE, ID=NA) {
#       Add a previously unknown raid boss to the boss list. Provide at least name and tier.
#   movematches.add <- function(Pokemon, Attack, Obsolete=FALSE)
#       Add a new move or moves to a pokemon or pokemons
#   pokemon.add <- function(Pokemon, Num, Type.1, Type.2="", Hatch.km=NA, Buddy.km=NA, Attack, Defense, Stamina, Gen=NA)
#       Add a new pokemon to the master pokedex
#   mylist.new <- function(type,cp,atk,def,stam, fast="", charge="", charge2="", shiny=F, notes="",data=mymons)
#       Add a new 'mon by its stats
#   levfromstats <- function(name, cp, atk, def, sta)
#       Calculates level and HP given a name, CP, attack, defense, and stamina (convenience for mylist.new)



# Helpers:

#' Copy info to the clipboard (Mac version)
#' @param x The data to copy to the clipboard
#' @param sep The cell separator
#' @param col.names First row has column names
#' @export
copy_to_clipboard = function(x,sep="\t",col.names=T,...) {
  write.table(x
              ,file = pipe("pbcopy")
              ,sep=sep
              ,col.names = col.names
              ,row.names = F
              ,quote = F,...)
}

#' Paste info from the clipboard
#' @param sep The cell separator
#' @param header First row is a header
#' @return Dataframe
#' @export
paste_from_clipboard = function(sep="\t",header=T,...) {
  read.table(pipe("pbpaste")
             ,sep=sep
             ,header=header,...)
}

#' Pass it a pokemon name ("Charizard") and it displays a little info about each move
#' in the pokemon's possible movesets.
#' @param mon The pokemon name to look up
#' @param inc.old Include moves no longer encountered in the wild (e.g. Community Day, obsolete)
#' @param bydef Order by defense rank
#' @return Display of move combinations
#' @export
moveset <- function(mon, inc.old=FALSE, bydef=FALSE) {
  print(paste0("Moveset for ", mon, ":"))
  print(paste0("  Type 1: ", pokemon[mon,"Type.1"]))
  if (pokemon[mon, "Type.2"] != "")
    print(paste0("  Type 2: ", pokemon[mon,"Type.2"]))
  print(paste0("  Max CP: ", pokemon[mon, "Max.CP"]))
  print(paste0("  Tankiness: ", unique(movesets[movesets$Name==mon, "Tankiness"])))

  moves <- if(inc.old==FALSE)
    c(unique(movesets[movesets$Name==mon & movesets$Obsolete==FALSE, "Basic Atk"]),
      unique(movesets[movesets$Name==mon & movesets$Obsolete==FALSE, "Charge Atk"]))
  else
    c(unique(movesets[movesets$Name==mon, "Basic Atk"]),
      unique(movesets[movesets$Name==mon, "Charge Atk"]))

  allmoves[moves,]

  # Old info, very similar to alldps()
  # cols <- c(2:5, 12, 14:16)
  # set <- subset(movesets, Name==mon)
  # if(inc.old==FALSE)
  #   set <- subset(movesets, Name==mon & Obsolete==FALSE)
  # if(bydef==TRUE)
  #   set <- set[order(set$`Def Rank`),]

  # # rownames(set) <- NULL
  # set[,cols]
}

#' Either give a starting level or a starting amount of dust,
#' get how many mats it takes to reach the 'stop' level.
#' @param start The starting level
#' @param stop The level you're trying to get to
#' @param dust Provide dust amount to reach next level if you're unsure about which level you're starting at
#' @return The amount of dust and candy required to reach the level
#' @export
matcost <- function(start=1, stop=40, dust=NA) {
  if(!is.na(dust))
    start <- min(monlevels[which(monlevels$Stardust == dust),"Level"])
  apply(monlevels[
    which(monlevels$Level > start & monlevels$Level <= stop),
    c("Stardust", "Candy")],
    2, sum)
}

# effectiveness[x,] == strengths of x-type attack
# effectiveness[,x] == strengths against x-type defender

# Looks up strengths of a type or types, excludes columns that are
# uninteresting (==1) for all types.
#' Shows the damage type advantages of a given type of attack
#' @param types A vector of types
#' @return A table
#' @export
strengths <- function(types) {
  if(length(types) > 1)
    effectiveness[types,apply(effectiveness[types,], 2, function(x) { any(x!=1)}) ]
  else
    effectiveness[types, effectiveness[types,] != 1]
}

#' Looks up a mon's weaknesses, compounds them if multiple types, excludes uninteresting (==1) values.
#' @param mon The pokemon in question
#' @param all Show the regular effectiveness
#' @export
weaknesses <- function(mon, all=FALSE) {
  types <- pokemon[mon, c("Type.1", "Type.2")]
  result <- effectiveness[,types[1,1]]
  if(types[1,2]!="")
    result <- round(result * effectiveness[,types[1,2]], 3)
  if(all) result
  else result[! result %in% 1]
}

#' Looks up all pokémon who are of a particular type. Also shows max CP, to help pick one to pursue.
#' @param type The type
#' @param decreasing In decreasing order
monsOfType <- function(type, decreasing=F) {
  # For future reference, can combine types via:
  # mymons[,"Types"] <- sapply(mymons, paste0, collapse="/")
  # Then strsplit(mymons[i, "Types"], "/")[[1]]
  # Or maybe mymons[grepl(type %in% unlist(strsplit(mymons[,"Types"], "/"))),]

  typelist <- pokemon[pokemon$Type.1 == type | pokemon$Type.2 == type,
                      c("Type.1", "Type.2", "Max.CP")]
  if(!is.na(decreasing))
    typelist <- typelist[order(as.integer(typelist$Max.CP), decreasing=decreasing),]
  typelist <- cbind(typelist, pokemon[rownames(typelist),
                                      c("Attack", "Defense", "Stamina", "Num")])
  typelist
}

# # Bespoke functions for importing data from specifically formatted tables
# # copied from Excel (read from an OS X clipboard).
# import.pokemon <- function() {
#   pokemonbackup <<- pokemon
#   pokemon <<- read.table(pipe("pbpaste"), sep="\t", header=T)
#   rownames(pokemon) <<- rownames(pokemonbackup)
#   for(i in 2:15) {
#     pokemon[,i] <<- as.character(pokemon[,i])
#   }
# }

# # Copied from https://docs.google.com/spreadsheets/d/1hcFo7-UGWx1k1u1BHOvDhq8foPeRr7YbX2jLjjJK0Qw/edit#gid=1036598933
# # Pasted into Excel, added 'obsolete' column for italicized rows.
# # (Check if 'last active' column = latest date.)
# import.movesets <- function() {
#   movesetsbackup <<- movesets
#   movesets <<- read.table(pipe("pbpaste"), sep="\t", quote="")
#   names(movesets) <<- c(
#     "ID",
#     "O-Rank",
#     "Off %",
#     "D-Rank",
#     "Def %",
#     "PKMN #",
#     "Name",
#     "Tankiness",
#     "Duel Ability",
#     "Gym Offense",
#     "Gym Defense",
#     "Basic Atk",
#     "No Weave Dmg",
#     "Charge Atk",
#     "Weave Dmg",
#     "Gym Weave Dmg",
#     "Last Active",
#     "Obsolete"
#   )
# }

# import.moves <- function() {
#   allmovesbackup <<- allmoves
#   allmoves <<- read.table(pipe("pbpaste"), sep="\t", header=T)
# }

#' Gives the combined average DPS per second of a pair of moves.
#' @param move1 The fast move
#' @param move2 The charge move
#' @param source Source pokemon
#' @param target Target pokemon
#' @param stab1 Does the first move get a STAB bonus?
#' @param stab2 Does the second move get a STAB bonus?
#' @export
totaldps <- function(move1, move2, source=NA, target=NA, stab1=TRUE, stab2=TRUE) {
  if(!is.na(source)) {
    if(move1=="" | move2=="") return(NA)
    stab1 <- allmoves[move1, "Type"] %in% pokemon[source, c("Type.1", "Type.2")]
    stab2 <- allmoves[move2, "Type"] %in% pokemon[source, c("Type.1", "Type.2")]
  }

  dps1 <- allmoves[move1, "DPS"]
  dps2 <- allmoves[move2, "DPS"]
  time1 <- allmoves[move1, "Seconds"]
  time2 <- allmoves[move2, "Seconds"]
  energy1 <- allmoves[move1, "Energy"]
  energy2 <- allmoves[move2, "Energy"]

  if(stab1==TRUE) dps1 <- dps1*pokeglobals$STAB
  if(stab2==TRUE) dps2 <- dps2*pokeglobals$STAB

  if(!is.na(target)) {
    dps1 <- dps1 * weaknesses(target, all=TRUE)[[allmoves[move1, "Type"]]]
    dps2 <- dps2 * weaknesses(target, all=TRUE)[[allmoves[move2, "Type"]]]
  }

  (dps2*time2*100/energy2+dps1*100*time1/energy1) / (time2*100/energy2+100*time1/energy1)
}

#' Lists all combinations of moves for a given 'mon, and their
#' average combined DPS per totaldps(). As with that, can adjust
#' the numbers to show how they perform against a target 'mon.
#' Ends with ratio of damage to specific target to unadjusted DPS.
#' @param mon The pokemon in question
#' @param inc.old Show movesets no longer seen in the wild
#' @param inc.rank Exclude the offense/defense ranks
#' @export
alldps <- function(mon, target=NA, inc.old=FALSE, inc.rank=TRUE) {
  montypes <- pokemon[mon, c("Type.1", "Type.2")]
  print(paste0("Move combinations for ", mon, ":"))
  print(paste0("  Type 1: ", montypes[1]))
  if (montypes[2] != "")
    print(paste0("  Type 2: ", montypes[2]))
  print(paste0("  Max CP: ", pokemon[mon, "Max.CP"]))
  print(paste0("  Atk/Def/Stam: ", pokemon[mon, "Attack"], "-",
               pokemon[mon, "Defense"], "-", pokemon[mon, "Stamina"]))

  variations <- data.frame("Fast" = character(0), "F-Type" = character(0), "FastSTAB" = logical(0),
                           "Charge" = character(0), "C-Type" = character(0), "ChargeSTAB" = logical(0),
                           "DPES" = numeric(0), "Total DPS" = numeric(0), "Target DPS" = numeric(0),
                           "Ratio" = numeric(0))

  # Build the list of movesets.
  fastlist   <- subset(movematches, Pokemon==mon & Speed=="Fast")
  chargelist <- subset(movematches, Pokemon==mon & Speed=="Charge")
  if(inc.old==FALSE) {
    fastlist <- subset(fastlist, Obsolete==FALSE)
    chargelist <- subset(chargelist, Obsolete==FALSE)
  }
  set <- data.frame(Fast=character(0), Charge=character(0), stringsAsFactors = FALSE)
  for(i in 1:nrow(fastlist))
    for(j in 1:nrow(chargelist)) {
      set <- rbind(set, data.frame(Fast=fastlist[i,"Attack"], Charge=chargelist[j,"Attack"], stringsAsFactors = FALSE))
    }

  for(i in 1:nrow(set)) {
    fastmove <- set[i,"Fast"]
    chargemove <- set[i,"Charge"]

          fasttype <- allmoves[fastmove, "Type"]
          faststab <- fasttype %in% montypes
          chargetype <- allmoves[chargemove, "Type"]
          chargestab <-  chargetype %in% montypes
          dps <- totaldps(move1=fastmove, move2=chargemove,
                          stab1=faststab, stab2=chargestab)
          targetdps <- totaldps(move1=fastmove, move2=chargemove,
                                stab1=faststab, stab2=chargestab, target=target)
          variations <- rbind(variations,
                              data.frame("Fast" = fastmove,
                                         "F-Type" = fasttype,
                                         "FastSTAB" = faststab,
                                         "Charge" = chargemove,
                                         "C-Type" = chargetype,
                                         "ChargeSTAB" = chargestab,
                                         "DPES" = round(allmoves[chargemove,"DPS"] /
                                                        allmoves[chargemove,"Energy"], 2),
                                         "Total DPS" = round(dps, 2),
                                         "Target DPS" = round(targetdps, 2),
                                         "Ratio" = round(targetdps/dps, 2)))
  }

  if(is.na(target)) variations <- variations[,!(names(variations) %in% c("Target.DPS", "Ratio"))]

  variations <- if(is.na(target)) variations[order(variations$Total.DPS, decreasing=TRUE),]
  else variations[order(variations$Target.DPS, decreasing=TRUE),]

  print(variations[,-grep("STAB", names(variations))])
}

#' Lists all combinations of moves for a given 'mon, and their
#' average combined DPS per totaldps(). As with that, can adjust
#' the numbers to show how they perform against a target 'mon.
#' Ends with ratio of damage to specific target to unadjusted DPS.
#' @param mon The pokemon in question
#' @param target The target pokemon
#' @param inc.old Show movesets no longer seen in the wild
#' @param inc.rank Exclude the offense/defense ranks
olddps <- function(mon, target=NA, inc.old=FALSE, inc.rank=TRUE) {
  montypes <- pokemon[mon, c("Type.1", "Type.2")]
  print(paste0("Move combinations for ", mon, ":"))
  print(paste0("  Type 1: ", montypes[1]))
  if (montypes[2] != "")
    print(paste0("  Type 2: ", montypes[2]))
  print(paste0("  Max CP: ", pokemon[mon, "Max.CP"]))
  print(paste0("  Atk/Def/Stam: ", pokemon[mon, "Attack"], "-",
               pokemon[mon, "Defense"], "-", pokemon[mon, "Stamina"]))
  print(paste0("  Tankiness: ", unique(movesets[movesets$Name==mon, "Tankiness"])))

  variations <- data.frame("Fast" = character(0), "F-Type" = character(0), "FastSTAB" = logical(0),
                           "Charge" = character(0), "C-Type" = character(0), "ChargeSTAB" = logical(0),
                           "DPES" = numeric(0), "Total DPS" = numeric(0), "Target DPS" = numeric(0),
                           "Ratio" = numeric(0))

  # One way to generate results:
  # set <- movesets[unlist(ifelse(TRUE,
  #                               subset(movesets, Name==mon),
  #                               subset(movesets, Name==mon & Obsolete==FALSE))),]

  # Another way to generate results:
  # set <- subset(movesets, Name==mon)
  # if(inc.old==FALSE) set <- subset(set, Obsolete==FALSE)

  # But this is how it really happened:
  if(inc.old==FALSE) set <- subset(movesets, Name==mon & Obsolete==FALSE)
  else               set <- subset(movesets, Name==mon)

  for(i in 1:nrow(set)) {
    fastmove <- set[i,"Basic Atk"]
    chargemove <- set[i,"Charge Atk"]

    fasttype <- allmoves[fastmove, "Type"]
    faststab <- fasttype %in% montypes
    chargetype <- allmoves[chargemove, "Type"]
    chargestab <-  chargetype %in% montypes
    dps <- totaldps(move1=fastmove, move2=chargemove,
                    stab1=faststab, stab2=chargestab)
    targetdps <- totaldps(move1=fastmove, move2=chargemove,
                          stab1=faststab, stab2=chargestab, target=target)
    variations <- rbind(variations,
                        data.frame("Fast" = fastmove,
                                   "F-Type" = fasttype,
                                   "FastSTAB" = faststab,
                                   "Charge" = chargemove,
                                   "C-Type" = chargetype,
                                   "ChargeSTAB" = chargestab,
                                   "DPES" = round(allmoves[chargemove,"DPS"] /
                                                    allmoves[chargemove,"Energy"], 2),
                                   "Total DPS" = round(dps, 2),
                                   "Target DPS" = round(targetdps, 2),
                                   "Ratio" = round(targetdps/dps, 2)))
  }

  if(inc.rank==TRUE) variations <- cbind(set[,2:5], variations)
  if(inc.old==TRUE)  variations <- cbind(variations, Obsolete=set$Obsolete)
  if(is.na(target))  variations <- variations[,!(names(variations) %in% c("Target.DPS", "Ratio"))]

  print(variations[,-grep("STAB", names(variations))])
}

#' Calculates CP for a given mon using level, attack, defense, and stamina IVs.
#' Defaults to calculating max CP.
#' @param mon The pokemon
#' @param level Level, default 40
#' @param attack Attack, default 15
#' @param defense Defense, default 15
#' @param stamina Stamina, default 15
#' @export
cpcalc <- function(mon, level=40, attack=15, defense=15, stamina=15) {
  if(is.na(level) | is.na(attack) | is.na(defense) | is.na(stamina)) NA
  else {
    stats <- pokemon[mon,]
    max(floor(((stats$Attack+attack)
               *(stats$Defense+defense)^0.5
               *(stats$Stamina+stamina)^0.5
               *multipliers[multipliers$Level==level, "Multiplier"]^2)/10),
        10)
  }
}

#' Calculates HP for a given mon using level and stamina IV. Defaults to calculating max HP.
#' @param mon The pokemon
#' @param level Level, default 40
#' @param stamina Stamina, default 15
#' @export
hpcalc <- function(mon, level=40, stamina=15) {
  max(10, floor((pokemon[mon, "Stamina"] + stamina) *
          multipliers[multipliers$Level==level, "Multiplier"]))
}

#' Calculates the level corresponding to a given bunch of stats.
#' @param name The pokemon
#' @param CP its CP
#' @param HP its HP
#' @param Dust dust to next level
#' @param Attack its attack stat
#' @param Defense its defense stat
#' @param Stamina its stamina stat
#' @export
levelcalc <- function(name, CP, HP, Dust, Attack, Defense, Stamina) {
  if(is.na(CP) | is.na(HP) | is.na(Dust) |
     is.na(Attack) | is.na(Defense) | is.na(Stamina)) NA
  else {
    check <- subset(possibles(name, CP, HP, Dust, powered=T),
                    attack==Attack & defense==Defense & stamina==Stamina)
    if(nrow(check) != 1) {
      print("Something's weird here.")
      print(paste(name, CP, HP, Dust, Attack, Defense, Stamina))
      print(check)
      NA
    }
    else check$level
  }
}

#' Take pokemon, CP, HP, dust to level, and optionally descriptors of stats
#' to find all possible combinations of level and stats.
#' For atk/def/stam, descriptors are "not"/"pos"/"impr"/"incr", for the summary
#' they are "not"/"aa"/"caught"/"wonder". (As they were for Team Mystic under the
#' old assessment procedure. Not so useful anymore!)
#' @param mon The mon in question
#' @param cp Its CP
#' @param hp Its HP
#' @param dust Optionally, the dust it takes to reach its next level
#' @param atk How its attack is described
#' @param def How its defense is described
#' @param stam How its stamina is described
#' @param total How its summary is described
#' @param powered Has it been powered up since being obtained (can it be a 'half' level)?
#' @param evolve Add a different mon to show CP of that other pokemon (at level 40) compared to its max instead.
possibles <- function(mon, cp, hp, dust=NA, atk="all", def="all",
                      stam="all", total="all", powered=F, evolve=NA) {
  atkstat <- switch(atk, all=0:15, not=0:7, pos=8:12, impr=13:14, incr=15)
  defstat <- switch(def, all=0:15, not=0:7, pos=8:12, impr=13:14, incr=15)
  stamstat <- switch(stam, all=0:15, not=0:7, pos=8:12, impr=13:14, incr=15)
  sumstat <- switch(total, all=range(0,1), not=range(0, 0.50), aa=range(0.501, 0.66),
                    caught=range(0.661, 0.80), wonder=range(0.81,1))

  maxcp <- if(is.na(evolve)) cpcalc(mon) else cpcalc(evolve)
  solutions <- data.frame(Name=character(0), CP=integer(0), HP=integer(0),
                          attack=integer(0), defense=integer(0),
                          stamina=integer(0), percent=numeric(0), level=numeric(0),
                          CPat40=integer(0), maxcp=integer(0), shareofmax=numeric(0),
                          dust=numeric(0))

  level <- if(!is.na(dust)) monlevels[which(monlevels$Stardust == dust)-0.5,"Level"]
           else seq(1, 40, 0.5)
  if(!powered) level <- level[level[seq_along(level)] == as.integer(level[seq_along(level)])]

  for(k in stamstat)
    for(l in level)
      if(hpcalc(mon, level=l, stamina=k) == hp)
        for(i in atkstat)
          for(j in defstat) {
            perfection = (i+j+k)/45
            if(cp == cpcalc(mon, level = l, attack = i, defense = j, stamina = k)
               & perfection >= sumstat[1] & perfection <= sumstat[2]) {
              cp40 <- if(is.na(evolve)) cpcalc(mon, attack=i, defense=j, stamina=k)
                      else cpcalc(evolve, attack=i, defense=j, stamina=k)
              candidate <- data.frame(Name=mon, CP=cp, HP=hp,
                                      attack=i, defense=j, stamina=k,
                                      percent=round(perfection*100, digits=1),
                                      level=l, CPat40=cp40, maxcp=maxcp,
                                      shareofmax=round(cp40/maxcp*100, digits=1),
                                      dust=dust)
              statcheck <- c(candidate$attack, candidate$defense, candidate$stamina)

              # Exclude those candidates that do not have stats with specified
              # ranges as the unique max of their stats, unless multiple stats
              # are specified, in which case candidates are allowed only if
              # those stats are equal.
              if((atk == "all") | (candidate$attack == max(statcheck)))
                if((def == "all") | (candidate$defense == max(statcheck)))
                  if((stam == "all") | (candidate$stamina == max(statcheck)))
                    if((atk==def) | (candidate$attack != candidate$defense))
                      if((atk==stam) | (candidate$attack != candidate$stamina))
                        if((stam==def) | (candidate$stamina != candidate$defense))
                          solutions <- rbind(solutions, candidate)
            }
          }

  solutions$Name <- as.character(solutions$Name)
  solutions
}

#' Lists the known movesets that are strongest against the target, with an
#' option to choose a particular pokemon as the attack source, though that's
#' similar to using alldps().
#' @param target The pokemon to attack
#' @param source Limit to a particular attacker
#' @export
strongest <- function(target, source=NA) {
  source <- if(is.na(source)) unique(movesets$Name)
            else source

  summary <- movesets[movesets$Name %in% source, c("Name", "Basic Atk", "Charge Atk")]
  dps <- apply(summary, 1,
               function(x) { totaldps(x[2], x[3],
                             target=target,
                             allmoves[x[2], "Type"] %in% pokemon[x[1], c("Type.1", "Type.2")],
                             allmoves[x[3], "Type"] %in% pokemon[x[1], c("Type.1", "Type.2")]
                           )} )
  summary <- cbind(Pokemon=summary[,"Name"],
                   "Base Atk"=unlist(apply(summary, 1, function(x) pokemon[x[1], "Attack"])),
#                   Max.CP=pokemon[summary$Name, "Max.CP"],
                   summary[,c("Basic Atk", "Charge Atk")],
                   dps)
  summary[order(summary$dps, decreasing=TRUE),]
}

#' Save the key data about pokemon and move attributes, etc.
#' @export
savepoke <- function() {
 save(allmoves, pokemon, movesets, monlevels, effectiveness,
      multipliers, raidbosses, legendaries, pokeglobals, movematches, weather,
      raidlists,
      file="pokedata.rda")
}

#' Loads the database of your pokemon. Careful not to erase important changes! Save early, save often
#' @param data Which data file are you saving to?
#' @export
loadmonlist <- function(data=mymons) {
  load(paste0(getwd(), "/mymonlist.rda"), envir=globalenv())
  tail(data)
}

#' Saves pokemon databases and various other stuff I like
#' @export
savemonlist <- function() {
  mymonsbackup <<- mymons
  save(mymons, candidates, mymonsbackup, movechange, backlog, agenda, mymons2, agenda2, pokedex, pokedex2, file="mymonlist.rda")
}

#' Attempt to calculate levels of all pokemons in the selected database
#' @export
mylist.levels <- function(data=mymons) {
  unname(unlist(apply(data, 1, function(x) {
    levelcalc(name=x["Name"],
              CP=as.numeric(x["CP"]),
              HP=as.numeric(x["HP"]),
              Dust=as.numeric(x["Dust"]),
              Attack=as.numeric(x["Attack"]),
              Defense=as.numeric(x["Defense"]),
              Stamina=as.numeric(x["Stamina"]))
  })))
}

#' Adds a pokemon to the database conforming to the output of possibles()
#' @param poss A pokemon output by possibles() or a facsimile
#' @param fast.move The pokemon's fast move
#' @param charge.move The pokemon's charge move
#' @param notes Anything for the optional notes field
#' @param shiny Is the mon shiny?
#' @param stats Are we including stats or not?
#' @param data Which database file?
#' @return The tail of the revised database
#' @export
mylist.add <- function(poss, fast.move="", charge.move="", notes="", shiny=FALSE, stats=TRUE, data=mymons) {
  print(paste(poss$Name, poss$CP, poss$HP, poss$dust, poss$attack, poss$defense, poss$stamina))
  newmon <- data.frame(
    Name=unique(poss$Name),
    CP=unique(poss$CP), HP=unique(poss$HP), Dust=unique(poss$dust),
    # Assumes poss has only one set of Name, CP, HP, and dust
    Attack=if(length(unique(poss$attack))==1) unique(poss$attack) else NA,
    Defense=if(length(unique(poss$defense))==1) unique(poss$defense) else NA,
    Stamina=if(length(unique(poss$stamina))==1) unique(poss$stamina) else NA,
    Level=if(length(unique(poss$level))==1) unique(poss$level) else NA,
    Fast=fast.move, Charge=charge.move, Charge2="",
    Percent=if(length(unique(poss$percent))==1) unique(poss$percent) else NA,
    Shiny=shiny,
    Notes=notes
  )

  # if(nrow(poss) > 1) {
  #   print("More than one row detected!")
  #   return("Nothing added.")
  # }
  # newmon <- data.frame(Name=poss$Name, CP=poss$CP, HP=poss$HP, Dust=poss$dust,
  #                      Powered="No", Attack=poss$attack, Defense=poss$defense,
  #                      Stamina=poss$stamina, Level=poss$level, Fast.Move=fast.move,
  #                      Charge.Move=charge.move,
  #                      Percent=pctcalc(poss$attack, poss$defense, poss$stamina))
  if(stats==FALSE) newmon[, c("Attack", "Defense", "Stamina", "Percent", "Level")] <- NA
  print(newmon)
  assign(deparse(substitute(data)), rbind(data, newmon), envir=globalenv())
  tail(eval(substitute(data)))
}

#' Lists all pokemon in the database matching a grep for the given string
#' @param name The name or name fragment you're looking for
#' @param target A target to calculate DPS against?
#' @param order The order in which to display results
#' @param data The database
#' @export
moncat <- function(name, target="", order=NA, data=mymons) {
  # subset(mymons, Name==name)
  # Better because it allows partial matching:
  catalog <- data[grep(name, data$Name),]
  if(nrow(catalog)==0) return("Nothing matching that in the catalog.")
  CPat40 <- integer(0)
  DPS <- numeric(0)
  DPS2 <- numeric(0)
  vTarg <- numeric(0)
  vTarg2 <- numeric(0)
  for(i in 1:nrow(catalog)) {
    CPat40[i] <- cpcalc(catalog[i,"Name"], 40,catalog[i,"Attack"], catalog[i,"Defense"], catalog[i,"Stamina"])
    DPS[i] <- round(totaldps(catalog[i,"Fast"], catalog[i,"Charge"], source=catalog[i,"Name"]),2)
    if(catalog[i,"Charge2"] != "") DPS2[i] <- round(totaldps(catalog[i,"Fast"], catalog[i,"Charge2"], source=catalog[i,"Name"]),2)
  }
  catalog <- cbind(catalog, CPat40, DPS)
  if(target!="") {
    for(i in 1:nrow(catalog)) {
      vTarg[i] <- round(totaldps(catalog[i,"Fast"], catalog[i,"Charge"],
                                  source=catalog[i,"Name"], target=target),2)
      if(catalog[i,"Charge2"] != "")
        vTarg2[i] <- round(totaldps(catalog[i,"Fast"], catalog[i,"Charge2"],
                                    source=catalog[i,"Name"], target=target),2)
    }
    catalog <- cbind(catalog, vTarg)
  }
  order <- if(is.na(order)) 1:nrow(catalog)
           else order(eval(parse(text=paste0("catalog$",order))))
  catalog[order,]
}

#' Subtract a pokemon from the database
#' @param num The row number in the database
#' @param data The database
#' @export
mylist.subtract <- function(num, data=mymons) {
  print("Eliminating:")
  print(data[num,])
  assign(deparse(substitute(data)), data[-num,], envir=globalenv())
  # mymons <<- data[-num,]
  renum(deparse(substitute(data)))
}

#' Use to evolve a pokemon in your database, calculating new CP/HP
#' @param num The mon's row number
#' @param to What it's evolving into
#' @param fast Optionally, its new fast move
#' @param charge Optionally, its new charge move
#' @param cp Its new CP
#' @param hp Its new HP
#' @param data The database you're using
#' @return The mon's old stats and its new stats
#' @export
mylist.evolve <- function(num, to, fast="", charge="", cp=NA, hp=NA, data=mymons) {
  dataname <- deparse(substitute(data))
  print("Upgrading:")
  print(data[num,])

  level <- data[num, "Level"]
  atk <- data[num, "Attack"]
  def <- data[num, "Defense"]
  stam <- data[num, "Stamina"]
  if(is.na(cp))
    if(!anyNA(c(level, atk, def, stam)))
      cp <- cpcalc(to, level, atk, def, stam)
  if(is.na(hp))
    if(!anyNA(c(level, stam)))
      hp <- hpcalc(to, level, stam)

  data[num, "Name"] <- to
  data[num, "CP"] <- cp
  data[num, "HP"] <- hp
  data[num, "Fast"] <- fast
  data[num, "Charge"] <- charge

  assign(dataname, data, envir=globalenv())
  print("To:")
  print(eval(parse(text=dataname))[num,])
}

#' Calculates the damage a move will do against a target
#' @param attacker The row number of the mon who's attacking
#' @param target The name of the mon under attack
#' @param move The name of the move being used
#' @param weather Optionally, what the weather type is
#' @param atk Attacker's attack
#' @param def Defender's defense
#' @param atklvl Attacker's level
#' @param deflvl Defender's level
#' @param raid Is it a raid boss or a regular mon?
#' @param atkraid Is the attacker a raid boss?
#' @export
dmgcalc <- function(attacker, target, move, weather=NA, atk=15, def=15, atklvl=40, deflvl=40, raid=F, atkraid=F, ...) {
  movetype <- allmoves[move, "Type"]
  power <- allmoves[move, "Power"]
  weather <- if(is.character(weather) && movetype %in% with(globalenv(), weather)[[weather]])
               weather <- pokeglobals$weather
             else 1
  stab <- if(movetype %in% pokemon[attacker, c("Type.1", "Type.2")])
            pokeglobals$STAB
          else
            1

  atkmulti <- if(atkraid) pokeglobals$raidcpm[[raidbosses[attacker, "Tier"]]]
              else multipliers[multipliers$Level==atklvl, "Multiplier"]
  defmulti <- if(raid) pokeglobals$raidcpm[[raidbosses[target, "Tier"]]]
              else multipliers[multipliers$Level==deflvl, "Multiplier"]
  atk <- (pokemon[attacker, "Attack"] + atk) * atkmulti
  def <- (pokemon[target, "Defense"] + def) * defmulti

  effective <- weaknesses(target, all=TRUE)[[movetype]]

  dmg <- floor(0.5*power*atk/def*stab*weather*effective)+1
  # dps <- round(dmg/allmoves[move, "Seconds"], 1)
  dps <- dmg/allmoves[move, "Seconds"]

  c(Damage=dmg, DPS=dps)
}

#' Change the level of a mon in the list, recalculate CP and HP.
#' Obviously only works if atk/def/sta are known.
#' @param mon The row number of the mon
#' @param level The level it's achieving
#' @param data Name of the database
#' @export
mylist.level <- function(mon, level, data=mymons) {
  dataname <- deparse(substitute(data))
  print("Upgrading:")
  print(data[mon,])

  target <- data[mon,]
  newcp <- cpcalc(target$Name, level, target$Attack, target$Defense, target$Stamina)
  newhp <- hpcalc(target$Name, level, target$Stamina)
  target$CP <- newcp
  target$HP <- newhp
  target$Level <- level
  target$Dust <- if (level < 40) monlevels[monlevels$Level==(level+0.5), "Stardust"]
                 else NA
  data[mon,] <- target
  assign(dataname, data, envir=globalenv())

  print(data[mon,])
}

#' Copy a database to the clipboard (Mac)
#' @param data Name of the database
#' @export
mylist.export <- function(data=mymons) {
  write.table(data, pipe("pbcopy"), sep="\t", row.names=F)
}

#' Just calculate your IV percentage (sum of stats / 45)
#' @param Attack attack stat
#' @param Defense defense stat
#' @param Stamina stamina stat
#' @export
pctcalc <- function(Attack, Defense, Stamina) {
  round(sum(Attack, Defense, Stamina)/0.45, digits=1)
}

#' Find what level you can afford to get to with specified dust and/or candy
#' @param level your mon's starting level
#' @param dust how much dust you have (default=ten million AKA ~unlimited)
#' @param candy how much candy you have (default=ten thousand AKA ~unlimited)
#' @return a couple numbers
#' @export
matcheck <- function(level=1, dust=10000000, candy=10000) {
  dustreq=0
  candyreq=0
  for(i in seq(level+0.5, 40, 0.5)) {
    nextlevel <- monlevels[monlevels$Level==i,]
    if(dustreq + nextlevel$Stardust <= dust &
       candyreq + nextlevel$Candy <= candy) {
      dustreq <- dustreq + nextlevel$Stardust
      candyreq <- candyreq + nextlevel$Candy
      level <- i
    }
    else break
  }

  print(data.frame(Level=level, Dust=dustreq, Candy=candyreq), row.names=FALSE)

}

#' Little helper: Takes the name of a data frame (as a string!) and renumbers it (deletes rownames).
#' Changes done in place.
#' @param dataname any dataframe
#' @export
renum <- function(dataname) {
  thefunc <- parse(text=paste0("rownames(",dataname,") <<- NULL"))
  eval(thefunc)
}

#' Counts the number of combinations that equal a range, with a min roll of min and max max
#' @param equals the range of sum of stats you want to check, from 0 to 45
#' @param min the minimum score for each stat
#' @param max the maximum score for each stat
#' @param print print each combination or no?
#' @export
combos <- function(equals=0:45, min=0, max=15, print=FALSE) {
  count=0
  for (i in min:max)
    for (j in min:max)
      for (k in min:max)
        if(sum(i,j,k) %in% equals) {
          count <- count + 1
          if(print==TRUE)
            print(paste(i,j,k))
        }
  print(count)
}

#' Lists all your mons of a particular type
#' @param type the type of mon
#' @param data name of your database
#' @export
moncat.types <- function(type, data=mymons) {
  summary <- data[data$Name %in% rownames(monsOfType(type)),]
  summary[order(summary$CP, decreasing=FALSE),]
}

#' Shows how much damage a mon in your database could do against a target
#' This is called by most of the other 'find damage against x' functions like mylist.raidcounters()
#' @param sourceID the row number in your database of the attacker
#' @param target the name of the kind of pokemon you're attacking
#' @param raid if the target is a raid boss
#' @param targetdef the target's defense IV
#' @param targetlvl the target's level
#' @param Atk specify a different attack IV for your attacker
#' @param Level specify a different level for your attacker
#' @param Fast specify a different fast attack for your attacker
#' @param Charge specify a different charge attack for your attacker
#' @param data name of your database
#' @param exclude exclude a certain type of mon, I guess?
#' @return one particularly formatted line summarizing the matchup
#' @export
mylist.counter <- function(sourceID, target, raid=F, targetdef=15, targetlvl=40, Atk=NA, Level=NA,
                           Fast=NA, Charge=NA, data=mymons, exclude=NA, ...) {
  source <- data[sourceID,]

  monatk <- if(!is.na(Atk)) Atk
            else source$Attack
  monlvl <- if(!is.na(Level)) Level
            else source$Level
  fastmove <- if(!is.na(Fast)) Fast
              else source$Fast
  chargemove <- if(!is.na(Charge)) Charge
                else source$Charge

  mon <- source$Name
  montypes <- pokemon[mon, c("Type.1", "Type.2")]

  # # Slow!
  # counter <- data.table(Name=character(1), Fast=character(1), F.Type=character(1), Charge=character(1), C.Type=character(1),
  #                       Total.DPS=numeric(1), Vs.DPS=numeric(1), Atk=integer(1), Def=integer(1), Sta=integer(1),
  #                       Level=numeric(1), Net.DPS=numeric(1))

  counter <- data.frame()

  if(!(is.na(monatk) | is.na(monlvl) | fastmove=="" | chargemove=="" | any(exclude %in% montypes))) {

    # if(raid) targetdef <- raidbosses[target, "Defense"] - pokemon[raidbosses[target, "ID"], "Defense"]


    dmg1 <- dmgcalc(mon, target, fastmove, atk=monatk, def=targetdef, atklvl=monlvl, deflvl=targetlvl, raid=raid, ...)
    dmg2 <- dmgcalc(mon, target, chargemove, atk=monatk, def=targetdef, atklvl=monlvl, deflvl=targetlvl, raid=raid, ...)
    time1 <- allmoves[fastmove, "Seconds"]
    time2 <- allmoves[chargemove, "Seconds"]
    energy1 <- allmoves[fastmove, "Energy"]
    energy2 <- allmoves[chargemove, "Energy"]
    thisdps <- (dmg2["DPS"]*time2*100/energy2+dmg1["DPS"]*100*time1/energy1) / (time2*100/energy2+100*time1/energy1)

    fasttype <- allmoves[fastmove, "Type"]
    chargetype <- allmoves[chargemove, "Type"]
    faststab <- fasttype %in% montypes
    chargestab <-  chargetype %in% montypes
    dps <- totaldps(move1=fastmove, move2=chargemove,
                    stab1=faststab, stab2=chargestab)
    targetdps <- totaldps(move1=fastmove, move2=chargemove,
                          stab1=faststab, stab2=chargestab, target=target)

    counter[1, "Number"] <- sourceID
    counter[1, "Name"] <- mon
    counter[1, "Fast"] <- fastmove
    counter[1, "F.Type"] <- fasttype
    counter[1, "Charge"] <- chargemove
    counter[1, "C.Type"] <- chargetype
    counter[1, "Total.DPS"] <- round(dps, 2)
    counter[1, "Vs.DPS"] <- round(targetdps, 2)
    counter[1, "Atk"] <- monatk + pokemon[mon, "Attack"]
    counter[1, "Def"] <- source$Defense + pokemon[mon, "Defense"]
    counter[1, "Sta"] <- source$Stamina + pokemon[mon, "Stamina"]
    counter[1, "Level"] <- monlvl
    counter[1, "CP"] <- source$CP
    counter[1, "Fst.Dmg"] <- dmg1["Damage"]
    counter[1, "Chg.Dmg"] <- dmg2["Damage"]
    counter[1, "Net.DPS"] <- round(thisdps, 2)

    # Used to be this way. A little slower.
    # counter <- data.frame(Name = mon,
    #                       "Fast" = fastmove,
    #                       "F-Type" = fasttype,
    #                       "Charge" = chargemove,
    #                       "C-Type" = chargetype,
    #                       "Total DPS" = round(dps, 2),
    #                       "Target DPS" = round(targetdps, 2),
    #                       Atk = monatk + pokemon[mon, "Attack"],
    #                       Def = source$Defense + pokemon[mon, "Defense"],
    #                       Sta = source$Stamina + pokemon[mon, "Stamina"],
    #                       Level = monlvl,
    #                       DPSvTarget = round(thisdps, 2),
    #                       stringsAsFactors = FALSE)
    # )
  }

  counter
}

#' See your mon's survivability vs. a certain source's attacks
#' @param source the name of the type of attacker
#' @param target the row number of your mon
#' @param tarFast the attacker's fast attack
#' @param tarCharge the attacker's charge attack
#' @param raid is the attacker a raid boss?
#' @param sourceatk the attacker's attack IV
#' @param sourcelvl the attacker's level
#' @param Def an alternate for your mon's defense IV
#' @param Level an alternate for your mon's level
#' @param data the name of your database
#' @export
mylist.strong <- function(source, target, tarFast="", tarCharge="", raid=T, sourceatk=15, sourcelvl=40, Def=NA, Level=NA, data=mymons, ...) {
  counter <- data.frame(Name = character(0), Atk=integer(0), Def=integer(0), Sta=integer(0),
                        Level=numeric(0), CP=numeric(0), DPSvTarget=numeric(0))

  target <- data[target,]

  if(!is.na(Def)) target$Defense <- Def
  if(!is.na(Level)) target$Level <- Level

  # if(raid) sourceatk <- raidbosses[source, "Attack"] - pokemon[raidbosses[source, "ID"], "Attack"]
  #  if(!is.na(Fast)) source$Fast.Move <- Fast
  #  if(!is.na(Charge)) source$Charge.Move <- Charge

  mon <- target$Name
  mondef <- target$Defense
  monlvl <- target$Level
  moncp <- target$CP

  fastmove <- tarFast
  chargemove <- tarCharge

  # if(is.na(mondef)) print(paste("mondef:", mondef))
  # if(is.na(monlvl)) print(paste("monlvl:", monlvl))
  # if(fastmove=="") print(paste("fastmove:", fastmove))
  # if(chargemove=="") print(paste("chargemove:", chargemove))

  if(!(is.na(mondef) | is.na(monlvl) | fastmove=="" | chargemove=="")) {

    montypes <- pokemon[mon, c("Type.1", "Type.2")]

    dmg1 <- dmgcalc(source, mon, fastmove, atk=sourceatk, def=mondef, atklvl=sourcelvl, deflvl=monlvl, atkraid=raid, ...)
    dmg2 <- dmgcalc(source, mon, chargemove, atk=sourceatk, def=mondef, atklvl=sourcelvl, deflvl=monlvl, atkraid=raid, ...)
    time1 <- allmoves[fastmove, "Seconds"]
    time2 <- allmoves[chargemove, "Seconds"]
    energy1 <- allmoves[fastmove, "Energy"]
    energy2 <- allmoves[chargemove, "Energy"]
    thisdps <- (dmg2["DPS"]*time2*100/energy2+dmg1["DPS"]*100*time1/energy1) / (time2*100/energy2+100*time1/energy1)

    fasttype <- allmoves[fastmove, "Type"]
    chargetype <- allmoves[chargemove, "Type"]
    faststab <- fasttype %in% montypes
    chargestab <-  chargetype %in% montypes
    dps <- totaldps(move1=fastmove, move2=chargemove,
                    stab1=faststab, stab2=chargestab)
    counter <- data.frame(Name = mon,
                          Atk = target$Attack + pokemon[mon, "Attack"],
                          Def = mondef + pokemon[mon, "Defense"],
                          Sta = target$Stamina + pokemon[mon, "Stamina"],
                          Level = monlvl,
                          CP = moncp,
                          DPSvTarget = round(thisdps, 2),
                          Fst.Dmg = dmg1["Damage"],
                          Chg.Dmg = dmg2["Damage"])
  }

  counter
}

#' Iterates mylist.strong() for each mon in your database
#' That is, calculates the pokemon who take the least damage from a source
#' @param source the kind of attacker
#' @param tarFast the attacker's fast move
#' @param tarCharge the attacker's charge move
#' @param Name the kind of your mons to limit results to
#' @param raid is the attacker a raid boss?
#' @param numreturn the number of your mons to return, most survivable first
#' @param sourceatk specify the attacker's attack IV
#' @param sourcelvl specify the attacker's level
#' @param cpmin limit results to those over a certain CP
#' @param data the name of your database
#' @export
mylist.strongvs <- function(source, tarFast="", tarCharge="", Name="", raid=T, numreturn=10, sourceatk=15, sourcelvl=40, cpmin=1500, data=mymons) {
  # if(raid) sourceatk <- raidbosses[source, "Attack"] - pokemon[raidbosses[source,"ID"], "Attack"]

  print(paste("Source:", source, "  Attack: ", sourceatk, "  Base: ", pokemon[source, "Attack"]))

  counters <- data.frame(Number = integer(0), Name = character(0), Atk=integer(0),
                         Def=integer(0), Sta=integer(0), Level=numeric(0), DPSvTarget=numeric(0))

  targets <- if(Name=="") 1:nrow(data)
  else(as.integer(rownames(moncat(Name))))

  for(i in targets) {
    if(data[i,"CP"] >= cpmin) {
      counter <- mylist.strong(source, i, tarFast, tarCharge, raid=raid, sourceatk, sourcelvl,
                               Def=data[i,"Defense"], Level=data[i,"Level"])
      if(nrow(counter) == 1)
        counters <- rbind(counters, counter)
    }
  }

  print(head(counters[order(counters$DPSvTarget, decreasing=FALSE),], numreturn), row.names=F)
}

#' Wrapper for mylist.counters(), cycles through your mons to see which are best against
#' a particular target, defaulting to raid versions
#' @export
mylist.raidcounters <- function(...) {
  mylist.counters(..., raid=T)
}

#' Wrapper for mylist.counter(), check how effective one of your mons (row number, first
#' argument) is against a type of raid boss (second argument)
#' @export
mylist.raidcounter <- function(...) {
    mylist.counter(..., raid=T)
}

#' Wrapper for mylist.strongvs(), determining which of your mons are strongest against an
#' attacker, defaulting to raid version
mylist.raidstrong <- function(...) {
  mylist.strongvs(..., raid=T)
}

#' Sees how effective one of your mons is at attacking a type of target at each of
#' a range of your mon's levels. If range is not included, will look for the mon's
#' level and go from there to 40. If the mon has no level, will go from 20:40.
#' @param sourceID the row number of your attacking mon
#' @param target the type of target you're going after
#' @param raid is the target a raid boss?
#' @param range number range of levels you're checking, e.g. 32:40
#' @param data the name of your database
#' @export
mylist.dpslvl <- function(sourceID, target, raid=T, range=NA, data=mymons, ...) {
  range <- if(identical(NA, range))
             if(is.numeric(data[sourceID, "Level"]))
                # c(mymons[sourceID,"Level"] - 0.5, 40)  # Start a half-lvl early to see last gain?
                c(data[sourceID,"Level"], 40)
             else c(20,40)
           else range
  range <- seq(range[1], range[length(range)], by=0.5)
  source <- data[sourceID,]

  if(source$Fast=="" | source$Charge=="") {
    print(source)
    return("Need more moves!")
  }

  # results <- data.frame()
  # for(i in 1:length(range)) {
  #   results <- rbind(results, mylist.counter(source, target, raid=raid, Level=range[i], ...))
  # }
  # PctChg <- round(100*c(NA,diff(results$Net.DPS, 1))/results$Net.DPS,2)
  # results <- cbind(results, PctChg)

  # A little faster (12.54 vs. 113.20)
  results <- rbindlist(lapply(range, function(x)
    mylist.counter(sourceID, target, raid=raid, Level=x, data=data, ...)
  ))
  PctChg <- round(100*c(NA,diff(results$Net.DPS, 1))/results$Net.DPS,2)
  results <- results[, PctChg := PctChg]

  totaldps <- unique(results$Total.DPS)
  vsdps <- unique(results$Vs.DPS)

  movesline <- paste(unique(results[,"Fast"]), "&", unique(results[,"Charge"]))
  totaldpsline <- paste("Total.DPS:", totaldps)
  vsdpsline <- paste("Vs.DPS:", vsdps)
  multiplierline <- paste("Multi:", round(vsdps/totaldps,2))

  results <- results[,c("Level", "Net.DPS", "PctChg", "Fst.Dmg", "Chg.Dmg")]

  if(source$Charge2 != "") {
    results2 <- rbindlist(lapply(range, function(x)
      mylist.counter(sourceID, target, raid=raid, Level=x, data=data, Charge=source$Charge2,...)
    ))
    PctChg <- round(100*c(NA,diff(results2$Net.DPS, 1))/results2$Net.DPS,2)
    results2 <- results2[, PctChg := PctChg]

    totaldps2 <- unique(results2$Total.DPS)
    vsdps2 <- unique(results2$Vs.DPS)

    movesline <- paste(movesline, "/", unique(results2[,"Charge"]))
    totaldpsline <- paste(totaldpsline, "/", totaldps2)
    vsdpsline <- paste(vsdpsline, "/", vsdps2)
    multiplierline <- paste(multiplierline, "/", round(vsdps2/totaldps2,2))

    results2 <- results2[,c("Level", "Net.DPS", "PctChg", "Fst.Dmg", "Chg.Dmg")]
    colnames(results2) <- c("Level", "Net.DPS2", "PctChg2", "Fst.Dmg2", "Chg.Dmg2")
    results <- merge(results, results2, "Level")
  }
  # Add columns for dust/candy costs at each level in the range
  results <- cbind(results, t(sapply(range, function(x) matcost(range[1], x))))

  print(paste(source$Name, "vs.", target))
  print(movesline)
  print(totaldpsline)
  print(vsdpsline)
  print(multiplierline)

  # results[,c("Level", "Net.DPS", "PctChg", "Fst.Dmg", "Chg.Dmg")]
  results
}

#' Shows how strong of a defender one of your mons is against a target across a range of levels
#' @param source the type of attacker
#' @param target the row number of your defender
#' @param tarFast the attacker's fast attack
#' @param tarCharge the attacker's charge attack
#' @param raid is the attacker a raid boss?
#' @param range the range of your mon's levels to try
#' @param data the name of your database
#' @export
mylist.strlvl <- function(source, target, tarFast, tarCharge, raid=T, range=NA, data=mymons, ...) {
  range <- if(identical(NA, range))
    if(is.numeric(data[target, "Level"]))
      c(data[target,"Level"], 40)
    else c(20,40)
    else range
  range <- seq(range[1], range[length(range)], by=0.5)
  results <- data.frame()
  for(i in 1:length(range)) {
    results <- rbind(results, mylist.strong(source, target, tarFast, tarCharge, raid=raid, Level=range[i], data=data, ...))
  }
  PctChg <- round(100*c(NA,diff(results$DPSvTarget, 1))/results$DPSvTarget,2)
  results <- cbind(results, PctChg)
  totaldps <- unique(results$Total.DPS)
  targetdps <- unique(results$Target.DPS)

  target <- data[target,]
  print(paste(source, "vs.", target$Name))
  print(paste(tarFast, "&", tarCharge))

  results[,c("Level", "DPSvTarget", "PctChg", "Fst.Dmg", "Chg.Dmg")]
}

#' Shows how strong of both an attacker and defender one of your mons is against a target
#' across a range of levels -- calls mylist.dpslvl() and mylist.strlvl()
#' @param source the row number of your mon
#' @param target the kind of attacking mon
#' @param tarFast the attacker's fast attack
#' @param tarCharge the attacker's charge attack
#' @param raid is the attacker a raid boss?
#' @param range the range of your mon's levels to try
#' @param data the name of your database
#' @export
mylist.statsvs <- function(source, target, TarFast, TarCharge, raid=T, range=NA, ...) {
  results <- merge(mylist.dpslvl(source, target, raid=raid, range=range, ...),
                   mylist.strlvl(target, source, TarFast, TarCharge, raid=raid, range=range, ...),
                   "Level", all=TRUE)
  colnames(results)[1:9] <- c("Level", "DPSvTarget", "PctChgDPS", "Fst.Atk", "Chg.Atk",
                         "TargetDPS", "PctChgTgt", "Fst.Def", "Chg.Def")
  if(ncol(results) > 9)
    colnames(results) <- c("Level", "DPSvTarget", "PctChgDPS", "Fst.Atk", "Chg.Atk",
                           "DPS2", "PctChg2", "Fst.Dmg2", "Chg.Dmg2",
                           "TargetDPS", "PctChgTgt", "Fst.Def", "Chg.Def")
  results
}

#' Lists all legendaries in mymons.
#' @param data the name of your database
#' @export
mylist.legendaries <- function(data=mymons) {
  data[data$Name %in% legendaries,]
}

#' Lists all active raid bosses enhanced by current weather.
#' @export
raidweather <- function(weather=NA, inc.inactive=F) {
  bossindices <- integer(0)
  for(i in 1:nrow(raidbosses)) {
    mon <- rownames(raidbosses)[i]
    if(any(pokemon[mon,c("Type.1", "Type.2")] %in% with(globalenv(), weather)[[weather]]))
      bossindices <- c(bossindices, i)
  }
  returnset <- raidbosses[bossindices,]
  if(!inc.inactive) returnset <- subset(returnset, Active)
  returnset[order(returnset$CP),]
}

#' Calls mylist.counter() for each of your mons, defaults to not a raid boss
#' @param target the kind of mon you're attacking
#' @param raid is it a raid boss?
#' @param numreturn the number of results to print
#' @param Name the pokemon kind to limit results to
#' @param data the name of your database
#' @export
mylist.counters <- function(target, raid=F, numreturn=10, Name="", data=mymons, ...) {

  sources <- if(Name=="") 1:nrow(data)
             else(grep(Name, data$Name))

  # Used to make a data.frame first, then keep rbinding to it, slightly slower.
  #   counters <- data.frame()

  # counters <- do.call(rbind, lapply(sources, function(x) {
  #
  #   counter <- mylist.counter(x, target=target, raid=raid, targetdef=targetdef,
  #                             targetlvl=targetlvl, ...)
  #
  #   if(nrow(counter)==1) cbind(Number=x, counter)
  # }))
  counters <- rbindlist(lapply(sources, function(x) {

    result <- mylist.counter(x, target=target, raid=raid, data=data, ...)

    if(data[x,"Charge2"] != "") {
      result <- rbind(result, mylist.counter(x, target=target, raid=raid, data=data, Charge=data[x,"Charge2"], ...))
    }

    result
    # if(nrow(counter)==1) cbind(Number=x, counter)
  }))

  # Ever so slightly slower (and more complicated) way.
  # Numberlist <- integer(0)
  # Namelist <- character(0)
  # Fastlist <- character(0)
  # FTypelist <- character(0)
  # Chargelist <- character(0)
  # CTypelist <- character(0)
  # TotalDPSlist <- numeric(0)
  # TargetDPSlist <- numeric(0)
  # Atklist <- integer(0)
  # Deflist <- integer(0)
  # Stalist <- integer(0)
  # Levellist <- numeric(0)
  # DPSvTargetlist <- numeric(0)
  #
  # for(i in sources) {
  #   counter <- mylist.counter(i, target=target, raid=raid, targetdef=targetdef,
  #                             targetlvl=targetlvl, ...)
  #
  #   if(nrow(counter) == 1)
  #     # counters <- rbind(counters, cbind(Number=i, counter))
  #     Numberlist <- c(Numberlist, i)
  #     Namelist <- c(Namelist, counter$Name)
  #     Fastlist <- c(Fastlist, counter$Fast)
  #     FTypelist <- c(FTypelist, counter$F.Type)
  #     Chargelist <- c(Chargelist, counter$Charge)
  #     CTypelist <- c(CTypelist, counter$C.Type)
  #     TotalDPSlist <- c(TotalDPSlist, counter$Total.DPS)
  #     TargetDPSlist <- c(TargetDPSlist, counter$Target.DPS)
  #     Atklist <- c(Atklist, counter$Atk)
  #     Deflist <- c(Deflist, counter$Def)
  #     Stalist <- c(Stalist, counter$Sta)
  #     Levellist <- c(Levellist, counter$Level)
  #     DPSvTargetlist <- c(DPSvTargetlist, counter$DPSvTarget)
  # }
  #
  # counters <- data.frame(Number = Numberlist, Name = Namelist, "Fast" = Fastlist, "F-Type" = FTypelist,
  #                        "Charge" = Chargelist, "C-Type" = CTypelist,
  #                        "Total DPS"=TotalDPSlist, "Target DPS"=TargetDPSlist, Atk=Atklist,
  #                        Def=Deflist, Sta=Stalist, Level=Levellist, DPSvTarget=DPSvTargetlist,
  #                        stringsAsFactors = F)

  print(head(counters[order(-Net.DPS), -c("Total.DPS", "Fst.Dmg", "Chg.Dmg")], numreturn), row.names=F)
  # print(head(counters[order(counters$Net.DPS, decreasing=TRUE),which(colnames(counters)!="Total.DPS")], numreturn), row.names=F)
  # returnset <- head(counters[order(counters$DPSvTarget, decreasing=TRUE),], numreturn)
  # returnset
}

#' Checks the pokedex (more or less), shows the names of those around a given name
#' @param name the name to look up
#' @param range the number of mons above and below to display
#' @export
nearmons <- function(name, range=3) {
  number <- as.numeric(pokemon[name,"Num"])
  subset(pokemon, as.numeric(Num) > (number-range) &
                  as.numeric(Num) < (number+range) &
                  as.numeric(Num) > 0 &
                  as.numeric(Num) <= max(as.numeric(pokemon[,"Num"])))
}

#' Little helper adds an agenda item to the agenda dataframe
#' @param text a description of the agenda item
#' @param start the start level of the mon you're (presumably) leveling
#' @param end the level you're trying to reach
#' @param comments comments
#' @param data the name of the agenda table you're adding to
#' @export
agenda.add <- function(text, start, end, comments="", data=agenda) {
  dataname <- deparse(substitute(data))

  number <- nrow(data)+1
  data[number,"Mon"] <- text
  data[number,"Current"] <- start
  data[number,"Target"] <- end
  data[number,c("Dust Cost", "Candy Cost")] <- matcost(start, end)
  data[number,"Comments"] <- comments

  assign(dataname, data, envir=globalenv())
  renum(dataname)
  data
}

#' Updates the agenda dataframe when you level up one of the mons
#' @param number the row number of the agenda dataframe you're updating
#' @param current the level your mon's currently at
#' @param target the level you're trying to reach, if you want to update that
#' @param data the agenda table you want to update
#' @export
agenda.update <- function(number, current=NA, target=NA, data=agenda) {
  dataname <- deparse(substitute(data))

  from <- data[number,]

  if(!is.na(current))
    data[number, "Current"] <- current
  if(!is.na(target))
    data[number, "Target"] <- target

  data[number,c("Dust Cost", "Candy Cost")] <-
    matcost(data[number, "Current"], data[number, "Target"])
  to <- data[number,]

  if(to$Current==to$Target) {
    print("Conditions satisfied, eliminating:")
    print(to)
    data <- data[-number,]
  }
  else {
    print("Updated from:")
    print(from)
    print("To:")
    print(to)
  }

  assign(dataname, data, envir=globalenv())
  renum(dataname)
  data
}

#' Counts the number of combinations that are better than one with given stats.
#' @param mon what kind of mon you're looking at
#' @param atk your mon's attack IV
#' @param def your mon's defense IV
#' @param sta your mon's stamina IV
#' @param minstr the minimum attack IV for result combos
#' @param mindef the minimum defense IV for result combos
#' @param minsta the minimum stamina IV for result combos
#' @export
combos.better <- function(mon, atk=10, def=10, sta=10, minstr=10, mindef=10, minsta=10) {
  moncp <- cpcalc(mon, 40, atk, def, sta)
  print(paste0("Better combinations than a ", moncp, " ", mon,
               " with attack ", atk, ", defense ", def, ", and stamina ", sta, ":"))

  count=0

  CP <- numeric()
  # Yeah, these names are just capitalized versions of the fucntion arguments. So sue me.
  Atk <- numeric()
  Def <- numeric()
  Sta <- numeric()

  for (i in minstr:15)
    for (j in mindef:15)
      for (k in minsta:15) {
        testcp <- cpcalc(mon, 40, i,j,k)
        if(testcp > moncp) {
          count <- count + 1
          CP[count] <- testcp
          Atk[count] <- i
          Def[count] <- j
          Sta[count] <- k
        }
      }

  betters <- data.frame(CP, Atk, Def, Sta)
  betters <- betters[order(betters$CP),]

  print(betters, row.names=FALSE)
  print(count)
  print(paste0(round(count/216*100, 1), "% of egg/raid combos"))
  print(paste0(round(count/4096*100, 1), "% of all combos"))
}

#' Switch up the list of active raid bosses
#' Defaults to clearing all tiers!
#' @param bosslist a list (actually a character vector) of raid bosses to activate
#' @param tier resets all members of that raid tier to inactive before making new ones active
#' @export
raidbosses.set <- function(bosslist, tier=1:5) {
  raidbosses[raidbosses$Tier %in% tier, "Active"] <<- FALSE
  raidbosses[bosslist, "Active"] <<- TRUE

  cat("Now active:\n")
  subset(raidbosses, Active & Tier %in% tier)
}

#' Get list of current raid bosses
#' @param tier what tier (1:5) to list
#' @return a slice of raidbosses corresponding to the selected tier(s)
#' @export
raidbosses.get <- function(tier=1:5) {
  raidlist <- subset(raidbosses, Active & Tier %in% tier)
  raidlist[order(raidlist$Tier, rownames(raidlist)),]
}

#' Update the raidbosses list with a previously unknown raid boss. Add at least name and tier.
#' Everything but name and tier isn't strictly necessary.
#' @param name the name of the pokemon type
#' @param tier the tier number
#' @param CP its CP
#' @param Attack its attack
#' @param Defense its defense
#' @param HP its HP
#' @param Catch.Rate chance to catch it once the raid's over
#' @param Active is the boss now active?
#' @param ID its pokedex ID
#' @export
raidbosses.add <- function(name, Tier, CP=0, Attack=0, Defense=0, HP=0, Catch.Rate=NA, Active=TRUE, ID=NA) {
  ID <- as.numeric(pokemon[name, "Num"])
  HP <- switch(Tier,
               "1"=600,
               "2"=1800,
               "3"=3600,
               "4"=9000,
               "5"=15000)

  Attack <- if(Attack != 0) Attack else pokemon[name, "Attack"]
  Defense <- if(Defense != 0) Defense else pokemon[name, "Defense"]
  CP <- if(CP != 0) CP else floor((Attack+15) * sqrt(Defense+15) * sqrt(HP)/10)

  raidbosses[name,"Tier"] <<- Tier
  raidbosses[name,"CP"] <<- CP
  raidbosses[name,"Attack"] <<- Attack
  raidbosses[name,"Defense"] <<- Defense
  raidbosses[name,"HP"] <<- HP
  raidbosses[name,"Catch.Rate"] <<- Catch.Rate
  raidbosses[name,"Active"] <<- Active
  raidbosses[name,"ID"] <<- ID

  print(raidbosses[name,])
}

#' Adds a combination of pokemon and move to the movematches dataframe
#' Need to do this any time you add a new pokemon to the database or
#' an existing pokemon gets a new move
#' @param Pokemon name of a pokemon (or vector of names to iterate)
#' @param Attack name of an attack (or vector of attacks)
#' @param Obsolete can the move still be found in the wild?
#' @export
movematches.add <- function(Pokemon, Attack, Obsolete=FALSE) {
  for(mon in Pokemon) {
    for(move in Attack) {
      addition <- data.frame(Pokemon=mon,
                             Speed=allmoves[move,"Speed"],
                             Attack=move,
                             Obsolete=Obsolete)
      movematches <- rbind(movematches, addition)
    }
  }
  movematches <<- movematches
  tail(movematches, length(Pokemon)*length(Attack)+4)
}

# movematches.add <- function(Pokemon, Speed, Attack, Obsolete=FALSE) {
#   for(i in seq(Attack)) {
#     rownum <- nrow(movematches)+1
#     movematches[rownum, "Pokemon"] <<- Pokemon
#     movematches[rownum, "Speed"] <<- Speed
#     movematches[rownum, "Attack"] <<- Attack[i]
#     movematches[rownum, "Obsolete"] <<- Obsolete
#   }
#   tail(movematches, length(Attack)+5)
# }

#' Add a new pokemon to the pokemon database ('pokedex')
#' @param Pokemon name of the new mon
#' @param Num the pokedex number
#' @param Type.1 first element type
#' @param Type.2 second element type, if it has one
#' @param Hatck.km how far you need to walk to hatch an egg carrying it
#' @param Buddy.km how far you need to walk with it to get a candy
#' @param Attack base attack stat
#' @param Defense base defense stat
#' @param Stamina base stamina stat
#' @export
pokemon.add <- function(Pokemon, Num, Type.1, Type.2="", Hatch.km=NA, Buddy.km=NA, Attack, Defense, Stamina, Gen=NA) {
  rownum <- nrow(pokemon)+1
  pokemon[rownum, "Num"] <<- Num
  rownames(pokemon)[rownum] <<- Pokemon
  pokemon[rownum, "Type.1"] <<- Type.1
  pokemon[rownum, "Type.2"] <<- Type.2
  pokemon[rownum, "Hatch.km"] <<- Hatch.km
  pokemon[rownum, "Buddy.km"] <<- Buddy.km
  pokemon[rownum, "Attack"] <<- Attack
  pokemon[rownum, "Defense"] <<- Defense
  pokemon[rownum, "Stamina"] <<- Stamina
  pokemon[rownum, "Total"] <<- sum(Attack, Defense, Stamina)
  pokemon[rownum, "Max.CP"] <<- cpcalc(Pokemon)
  pokemon[rownum, "Gen"] <<- Gen
  tail(pokemon)
}

# One-time function for mass update of pokemon base stats.
# rebalance <- function() {
#   success=0
#   fail=0
#   unmatched <- newdata[0,]
#   for(i in 1:nrow(newdata)) {
#     rownum <- grep(newdata[i, "Name"], rownames(pokemon))
#     if(length(rownum==1)) {
#       success <- success+1
#       pokemon[rownum, c("Attack", "Defense", "Stamina")] <- newdata[i, c("Attack", "Defense", "Stamina")]
#       pokemon[rownum, "Total"] <- sum(newdata[i, c("Attack", "Defense", "Stamina")])
#       pokemon[rownum, "Max.CP"] <- newdata[i, "Max.CP"]
#     }
#     else {
#       fail <- fail+1
#       unmatched <- rbind(unmatched, newdata[i,])
#     }
#   }
#   print(unmatched)
#   print(paste("Successes:", success))
#   print(paste("Failures:", fail))
#   pokemon
# }

#' Calculate CP for every mon in your database
#' @param data the name of your database
#' @export
CPin <- function(data=mymons) {
  dataname <- deparse(substitute(data))

  for(i in 1:nrow(data)) {
    cp <- cpcalc(data[i, "Name"], data[i, "Level"], data[i, "Attack"], data[i, "Defense"], data[i, "Stamina"])
    print(data[i,])
    if(!is.na(cp)) {
      data[i, "CP"] <- cp
    }
    else {
      cp <- readline(prompt="Enter CP: ")
      data[i, "CP"] <- cp
    }
    print(data[i,])
  }

  assign(dataname, data, envir=globalenv())
}

#' Calculate HP for every mon in your database
#' @param data the name of your database
#' @export
HPin <- function(data=mymons) {
  dataname <- deparse(substitute(data))

  for(i in 1:nrow(data)) {
    hp <- hpcalc(data[i, "Name"], data[i, "Level"], data[i, "Stamina"])
    print(data[i,])
    if(!is.na(hp)) {
      data[i, "HP"] <- hp
    }
    else {
      hp <- readline(prompt="Enter HP: ")
      data[i, "HP"] <- hp
    }
    print(data[i,])
  }

  assign(dataname, data, envir=globalenv())
}

#' Updates IV stats for one mon in your database
#' @param num row number of the mon
#' @param Attack new attack stat
#' @param Defense new defense stat
#' @param Stamina new stamina stat
#' @param Notes new note
#' @param data the name of your database
#' @export
mylist.update <- function(num, Attack, Defense, Stamina, Notes=NA, data=mymons) {
  dataname <- deparse(substitute(data))

  print(data[num,])
  data[num, c("Attack", "Defense", "Stamina")] <- c(Attack, Defense, Stamina)
  data[num, "Percent"] <- round(sum(Attack, Defense, Stamina)/0.45, 1)
  if(!is.na(Notes)) data[num, "Notes"] <- Notes
  print(data[num,])
  if(dataname=="mymons") needstats <<- subset(data, is.na(Attack) | is.na(Defense) | is.na(Stamina))

  assign(dataname, data, envir=globalenv())
}

# Not sure what I was trying to do here.
# comparecp <- function(data, range=NA) {
#   range <- if(identical(NA, range))
#     if(length(unique(data$level))==1)
#       c(unique(data$level), 40)
#     else c(20,40)
#   else range
#   range <- seq(range[1], range[length(range)], by=0.5)
#
#   results <- matrix(ncol=nrow(data), data=numeric())
#
#   print(data)
#   for(i in range) {
#     result <- apply(data, 1, function(x) {
#       cpcalc(x["Name"],
#              i,
#              as.numeric(x["attack"]),
#              as.numeric(x["defense"]),
#              as.numeric(x["stamina"]))})
#     results <- rbind(results, result)
#   }
#   rownames(results) <- range
#   # colnames(results) <- apply(data,1,function(x) { paste0(as.numeric(x["attack"]), ",",
#   #                                                        as.numeric(x["defense"]), ",",
#   #                                                        as.numeric(x["stamina"])) })
#
#   # print(results)
#   rows <- numeric()
#   for(i in 1:nrow(results)) if(length(unique(results[i,])) > 1) rows <- c(rows,i)
#   if(length(rows)==0) "No differences in range."
#   else {
#     cat(paste0("Differences in range from levels ", range[1], " to ", range[length(range)], ":\n"))
#     if(length(rows)==1) c(as.numeric(rownames(results)[rows]), results[rows,])
#     else results[rows,]
#   }
# }

#' Add a new move to allmoves
#' @param name the new move's name
#' @param type the move's type
#' @param speed the move's speed (Fast or Charge)
#' @param power the move's power
#' @param seconds the move's duration in seconds
#' @param energy the move's energy generation/cost
#' @param note any special notes for the move (e.g. misc. effect)
#' @export
allmoves.add <- function(name, type, speed, power, seconds, energy, notes=NA) {
  allmoves[name, "Type"]    <<- type
  allmoves[name, "Speed"]   <<- speed
  allmoves[name, "Power"]   <<- power
  allmoves[name, "Seconds"] <<- seconds
  allmoves[name, "Energy"]  <<- energy
  allmoves[name, "DPS"]     <<- round(power/seconds, 2)
  allmoves[name, "EPS"]     <<- round(energy/seconds, 2)
  allmoves[name, "Notes"]   <<- ifelse(is.na(notes), "", notes)
  tail(allmoves)
}

#' Add a new 'mon by its stats
#' @param type the pokemon type
#' @param cp its CP
#' @param atk its attack stat
#' @param def its defense stat
#' @param stam its stamina stat
#' @param fast its fast move
#' @param charge its charge move
#' @param chage2 its second charge move, if any
#' @param shiny whether it's shiny (default=F)
#' @param notes any notes you'd like to add
#' @param data what list you're adding to
#' @export
mylist.new <- function(type,cp,atk,def,stam,
                       fast="", charge="", charge2="", shiny=F, notes="",data=mymons) {
  dataname <- deparse(substitute(data))

  level <- levfromstats(type, cp, atk, def, stam)

  temp <- data[0,]
  temp[1,"Name"] <- type
  temp[1,"CP"] <- cp
  temp[1,"HP"] <- hpcalc(type, level, stam)
  temp[1,"Dust"] <- monlevels[monlevels$Level==level+0.5,"Stardust"]
  temp[1,"Attack"] <- atk
  temp[1,"Defense"] <- def
  temp[1,"Stamina"] <- stam
  temp[1,"Fast"] <- fast
  temp[1,"Charge"] <- charge
  temp[1,"Charge2"] <- charge2
  temp[1,"Percent"] <- round(sum(atk,def,stam)/45*100,1)
  temp[1,"Shiny"] <- shiny
  temp[1,"Notes"] <- notes
  temp[1,"Level"] <- level

  assign(dataname, rbind(data,temp), envir=globalenv())
  tail(eval(substitute(data)))
}

#' Calculates level and HP given a name, CP, attack, defense, and stamina (convenience for mylist.new)
#' @param name the mon's type
#' @param cp its CP
#' @param atk its attack
#' @param def its defense
#' @param sta its stamina
#' @return the level number
#' @export
levfromstats <- function(name, cp, atk, def, sta) {
  level <- 1
  while (cp != cpcalc(name, level, atk, def, sta) & level < 40.5) {
    level=level+0.5
  }
  if(level==40.5) level <- NA
  level
}

