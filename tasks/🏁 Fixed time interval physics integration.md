# 🏁 Fixed time interval physics integration

Goals:

1. Decouple physics from the frame rate.

2. Improve the accuracy of the simulation.


To this end, this work intends to:

1. Run the physics in a background thread.

2. Reduce the integration intervals.

3. Fix the integration intervals.


## Tasks

- [x] Start a background thread to run the physics.
- [ ] Run the physics in a loop with a fixed time interval.
- [ ] Sleep at the end of each iteration until the next multiple of the interval.
- [x] Communicate state changes (physics in background, controls in foreground) via STM.
- [x] Draw from the read-only copy.
- [ ] Interpolate drawing relative to the integration interval.
