## Red Blue War 

.. is a turn-based game taking place in a space filled with red and blue pills.
Every player has one fighter and scrambles to collect as many points by
"destroying" red and/or blue enemies.

Destroying a red enemy accelerates the fighter by a hidden percentage.
Destroying a blue enemy merely means collecting points. 

### Installation

In a terminal: 

```
$ git clone git@github.com:mfelleisen/PillWars.git
$ cd PillWars
$ raco pkg install
```

### Playing: Two Players

Open a terminal, navigate to `PillWars`:

```
$ raco test -s server game-step2.rkt &
$ raco test -s local game-step2.rkt
```
This spawns a window for the human player and also runs a game AI,
which isn't all that intelligent but still not easy to beat. 

When the game is over the windows stay up for 30s. Then just re-run
the previous command:  

```
$ !!
```

or

```
$ raco test -s local game-step2.rkt
```

### The Basic Rules 

During a turn, a player may perform one of the following actions:

- destroy a pill if its fighter's focus is on the pill;
- move forward one step according to its current velocity; or
- rotate clockwise or counter-clockwise by some degrees. 

If a fighter attempts to destroy an enemy but its focus point isn't on it, the
owner loses a point. A player's score cannot go below 0. 

A player is eliminated if the fighter navigates outside the game space.

The game ends when all enemies are destroyed or all fighters are eliminated.

The player with the most points wins.

### A Player's Actions

A _human player_ can navigate his fighter with either mouse clicks

- button-down in the yellow space of a fighter changes its direction
- button-down in any white space moves "my" fighter straight ahead
- button-down on an "enemy" "fires" IF the fighter is "on" the enemy 

or key strokes:

- ↑ for forward
- ← for left
- → for right
- SPACE for "fire" ("destroy enemy" that the fighter is sitting on)

An _AI player_ can make plans and navigate its fighter according to some
strategy. A strategy can be _opponent-agnostic_ or _opponent-sensitive_.

Here is simplistic opponent-agnostic one:

1. The player determines which kind of pills its fighter is going to eat.
2. Then the player tries to eat as many of these pills by choosing fighter
   actions according to the following plan during each turn: 
  
  - 1. if it sits on a pill, _eat_ it. 
  - 2. if any of the pills is reachable in the given direction, _move_ forward.
  - 3. otherwise, change direction counter-clockwise by STEP-RAD until a pill is reachable. _Rotate_.
  - 4. still not? change direction clockwise by STEP-RAD until a pill is - reachable. _Rotate_. (TODO)
  - 5. else: return default action. 

