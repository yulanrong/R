### Overview

This project is about predicting salaries of baseball players in 2012. It is based on the similar data analysis from John Fox's *Applied Regression Analysis And Generalized Linear Models*, Chapter 22 section 1.2. 


### Data

The data is from the 2012 baseball season from Sean Lahmann's databse.

```{r}
load(url("http://www.stat.berkeley.edu/users/nolan/data/baseball2012.rda"))

dim(baseball)
colnames(baseball)
```

Variable | Description
---------|------------
ID       | Player ID code
yearID | Year
teamID | Team
lgID | League
nameFirst | Player’s First Name
nameLast | Player’s Last Name
salary | Salary
Pos | Position
G.x | Games played at Pos
GS | Games Started at Pos
InnOuts | Time played in the field expressed as outs
PO | Putouts
A | Assists
E | Errors
G.y | Games Played (at any position)
G_batting | Games as batter
AB | At Bats
R | Runs
H | Hits
2B | Doubles
3B | Triples
HR | Home Runs
RBI | Runs Batted In
SB | Stolen Bases
CS | Caught Stealing
BB | Bases on Balls (walks)
SO | Strikeouts
IBB | Intentional Walks
HBP | Hit By Pitch
SH | Sacrifice Hits
SF | Sacrifice Flies
GIDP | Grounded Into Double Play
years | Years in Major Leagues
CAB | Career At Bats
CH | Career Hits
CHR | Career Home Runs
CR | Career Runs
CRBI | Career Runs Batted In
CBB | Career Walks




