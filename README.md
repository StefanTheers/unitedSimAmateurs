# United Simulation of Amateurs Tournaments

The **unitedSimAmateurs** R package analyses United amateurs
lineups with a focus on tournaments.

## Installation
```R
install.packages("devtools")
library("devtools")
install_github("StefanTheers/unitedSimAmateurs")
```

## One Game
Define two lineups:
```R
library("unitedSimAmateurs")
x1 <- c(t = 0,  a = 10, v = 16, m = 16, s = 48)
x2 <- c(t = 10, a = 10, v = 36, m = 12, s = 12)
```
Play the game twice:
```R
set.seed(2020)
game(x1, x2)
## x1 vs. x2:      1:0
game(x1, x2)
## x1 vs. x2:      0:0 n.V. (2:5 i.E.)
```
The first game is won by team 1.
To determine the winner in the second game, we need a penalty shoot-out.


## One Tournament
Define a matrix of lineups with four teams:
```R
lineups <- cbind(t = c(4,0,0,4), a = c(0,4,4,0), v = c(5,5,8,8), m = c(8,5,5,5), s = c(5,8,5,5))
rownames(lineups) <- c("Dortmund", "Munich", "Paris", "Warwick")
lineups
##          t a v m s
## Dortmund 4 0 5 8 5
## Munich   0 4 5 5 8
## Paris    0 4 8 5 5
## Warwick  4 0 8 5 5
```
Play the tournament (1 point = 1 game won):
```R
set.seed(2020)
(res <- tournament(lineups))
## ----------------------------------------
##  semi-final:
## ----------------------------------------
## Dortmund vs. Munich:	1:2
## Paris vs. Warwick:	1:1 n.V. (12:13 i.E.)
##
## ----------------------------------------
##  final:
## ----------------------------------------
## Munich vs. Warwick:	0:0 n.V. (4:5 i.E.)
##
##
## This leads to points awarded as follows:
##
##  Warwick   Munich Dortmund    Paris
##        2        1        0        0
```
Warwick wins!


## Simulation of a Tournament
Let's play this tournament more often:
```R
set.seed(2020)
(res <- simTournament(lineups, reps = 100))
##        team pointsAvg wonNumber wonProp t a v m s
## 1:  Warwick      1.38        57    0.57 4 0 8 5 5
## 2: Dortmund      0.70        33    0.33 4 0 5 8 5
## 3:   Munich      0.66         3    0.03 0 4 5 5 8
## 4:    Paris      0.26         7    0.07 0 4 8 5 5
##
## number of teams:                         4
## number of tournaments:                   100
## number of games:                         300
## number of games with extra-time:         166
## number of games with penalty shoot-out:  149
```
If we play this tournament 100 times, Warwick wins 57 runs which yields
roughly 1.38 points per tournament. That English side seems to be strong, huh?


## Simulation of a Tournament with Permutations of the Draw
Let's take a closer look at the `lineups` matrix:
```R
lineups
##          t a v m s
## Dortmund 4 0 5 8 5
## Munich   0 4 5 5 8
## Paris    0 4 8 5 5
## Warwick  4 0 8 5 5
```
Up to now, Dortmund have always played against Munich and Paris against Warwick.
Let's change the draw and look at the results for all possible permutations:
```R
set.seed(2020)
(res <- simTournamentPerm(lineups, reps = 100, ncores = 8, sample = FALSE))
##        team pointsAvg wonNumber    wonProp t a v m s
## 1: Dortmund 1.3058333      1400 0.58333333 4 0 5 8 5
## 2:  Warwick 0.9029167       634 0.26416667 4 0 8 5 5
## 3:   Munich 0.4983333       271 0.11291667 0 4 5 5 8
## 4:    Paris 0.2929167        95 0.03958333 0 4 8 5 5
##
## number of teams:                         4
## number of tournaments:                   2400
## number of games:                         7200
## number of games with extra-time:         4001
## number of games with penalty shoot-out:  3174
```
Now the result is vastly different with Dortmund winning the majority
of the tournaments which makes them the clear winners with the best
tournament lineup.

This comes as no surprise of course.


## About United
United is a statistics game inspired by football and is played at the
Faculty of Statistics at TU Dortmund University for more than 40 seasons.
More background info about United and corresponding code can be found in these
two packages:
* [unitedR](https://cran.r-project.org/package=unitedR) by David Schindler and
* [bfbUnited](https://gitlab.com/fchalligalli/bfbunited) by Philipp Hallmeier.
