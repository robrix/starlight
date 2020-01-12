# 🏁 Fixed time interval physics integration

- [x] Start a background thread to run the physics.
- [x] Run the physics in a loop with a fixed time interval.
- [ ] Sleep at the end of each iteration until the next multiple of the interval.
- [ ] Communicate state changes (physics in background, controls in foreground) via STM.
- [ ] Draw from the read-only copy.
- [ ] Interpolate drawing relative to the integration interval.
