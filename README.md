#HelloWorldOpen 2014 Need for C code
This is the winning code of HelloWorldOpen competition. I will try to explain the competition, code structure and strategies below.

**This is the first version of the code guide. Most of sections will be improved till the end of week!**

##About the competition
The basic idea of the competition was to code AI to compete in slot car racing. The rules were simple. At the beginning you were given the track information, and then 60 times per second you receive information on all cars positions. The only decisions you can make is set Throttle to any value between 0 and 1, switch lane to right or left, or fire turbo once in a while. The only constraint that you have to follow is to keep your drift angle below 60 degrees, as above that your car crashes. You can read more about the rules and protocol in [problem technical specification](https://helloworldopen.com/techspec).

##Architectural decisions
The main decision to make was the choice of language. The protocol required answers within ~5ms so efficency was crucial. The above, and our love to C++ made us choose  it as the bot language.

##Physics used for simulation
One part of the competition was to figure out physics behind certain parts of simulation. I'll try to present them below.

###Velocity
Velocity of the car followed a simple formula

> Velocity(t) = Velocity(t - 1) * X + Throttle * Y

This way two first ticks allowed us to calculate the velocity parameters (X and Y)

###Drift angle
Drift angle physics was a little bit more difficult but easy enough to follow.

Each state of the car (its velocity and radius of the bent that the car is on) defines so called **target angle** computed using following formula:

> TargetAngle(t) = max(0, X * Velocity(t) / sqrt(BentRadius) - Y))

given target angle that the car *wants* to reach, we can compute actual drift angle in the next tick with this formula:

> Angle(t+1) = Angle(t) + A * (Angle(t) - Angle(t-1)) - B * Velocity(t) * (TargetAngle - Angle(t))


Having those formulas allowed us to save each tick of data as an equation, and once we received 3 ticks of data we computed the approximation of all parameters using LS fit algorithm.

###Turbo

Tubo follows very simple rule specified in technical rules: Each turbo is defined by its **duration** and its **modifier**. Once you send Turbo command your throttle value is multiplied by **modifier** for next "**duration**" ticks.

###Bumps

Bump of two or more cars happen if in any moment distance between two cars is less than length of the car.

Lets say car A is behind car B. If newly calculated positions cause distance between them to be less than CarLength, states of those cars are recalculated as follows:

> new car A position stays the same as before

> new car B position = car A position + CarLength

> new car A speed = car B speed * 0.8

> new car B speed = car A speed * 0.9


###Switches 
All switches are modelled as bezier curves. Both lengths and curvature is computed as an approximation.

More in-depth explaination coming soon.

 
##General racing idea

We decided to split the code into separate parts that are (atleast was supposed to be) separated from each other. Before getting into more details I will describe roughly the general  purpose of most of them.


The code is divided into two main parts - recording part managing all physics parameters, predictions and enemy tracking, and decisive part, that figures out the most optimal command.

###Recording code
#### CarTracker
Main class that manage all physics parameters, approximations, and length/radius predictions is **CarTracker**. At the beginning of each *CarPositions* message, **Record** command is issued to save data about the position and generate **CarState** containing all data about car like position velocity, angle etc. All data about position if forwarded to physics models (**VelocityModel**, **DriftModel**, **LaneLengthModel** etc) that take care of figuring out all necessary parameters.

After enough data is collected, **CarTracker** provides some of the most important methods like:

* **Predict** returning a CarState after issuing given command.
* **DistanceBetween** returning real distance between two positions
* **IsSafe** checking if the state is safe. Safe state means that there exist a sequence of commands that lead us to safe state (with velocity of 2).

And bump safety methods (which probably shouldnt be here but due to short deadline got here for convenience)

* **CanBumpAfterNTicks**
* **IsBumpInevitable**
* **IsSafeAttack**
* **IsSafeBehind**

####RaceTracker
**RaceTracker** works in the similar way to **CarTracker**, but instead of predicting general events, it save and predict all cars behavior and interactions. 

It provides car interaction methods like:

* **BumpOccured** checking if bump occured between two cars now
* **ScoreLane** returning score of the lane based on cars there (more details below)

Moreover it hold **EnemyTracker** object for each car, keeping track of its death state, spawn times or lap times. But the most important part is **VelocityPredictor** that keep track of all car states that occured during the race, and use that data to predict car velocity in any position on the map. Thats why there are two important methods:

* **ExpectedVelocity** returning expected velocity of the car in given position the track
* **TimeToPosition** returning expected time for car to reach given position.


###Strategy




*Work in progress, MORE COMING SOON!*

