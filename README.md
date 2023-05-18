
## Pills War

a random, made-up game for Sw Dev F'2023

### The Rough Idea

.. is a turn-based game taking place in a space filled with red pills and blue
pills.  Every player has one fighter and scrambles to collect as many points by
"eating" red pills and/or blue pills.

- When a player eats a red pill, it gains between 1 and 5 points (inclusive) and
  accelerates between 10% and 30% (inclusive). 

- When a player east a blue pill, it gains between 7 and 10 points (inclusive).

Each fighter starts with the same velocity (speed plus direction) and can turn
up to 30deg clockwise or counter-clockwise at any point during the game. 

During a turn, a player may perform one of the following actions:

- eat a pill if its fighter's focus is on the pill;
- move forward one step according to its current velocity; or
- rotate clockwise or counter-clockwise.

A player is eliminated if it navigates its fighter off the game space. 

The game ends when all pills are eaten or all fighters are eliminated. The
player with the most points wins. If two or more player have the same maximal
number of points, the fastest player wins. If, at this point, two or more
players are tied, the players closest to the origin tie for first place. 

#### Strategies

A strategy can be _opponent-agnostic_ or _opponent-sensitive_.

Here is simplistic opponent-agnostic one:

1. The player determines which kind of pills its fighter is going to eat.
2. Then the player tries to eat as many of these pills by choosing fighter
   actions according to the following plan during each turn: 
  
  - 1. if it sits on a pill, _eat_ it. 
  - 2. if any of the pills is reachable in the given direction, _move_ forward.
  - 3. otherwise, change direction counter-clockwise by STEP-RAD until a pill is reachable. _Rotate_.
  - 4. still not? change direction clockwise by STEP-RAD until a pill is - reachable. _Rotate_. (TODO)
  - 5. else: return default action. 

