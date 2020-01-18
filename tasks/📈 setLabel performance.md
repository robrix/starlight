# Trying to improve perf of label rendering

## Baseline:

```
cacheCharactersForDrawing: {min: 384.565ms, mean: 384.565ms, max: 384.565ms}
readTypeface: {min: 114.033ms, mean: 114.033ms, max: 114.033ms}
swap: {min: 0.181ms, mean: 9.988ms, max: 57.074ms}
frame: {min: 1.099ms, mean: 7.049ms, max: 255.006ms}
  draw: {min: 3.576ms, mean: 6.596ms, max: 99.447ms}
    ship: {min: 1.105ms, mean: 1.364ms, max: 31.078ms}
    setLabel: {min: 0.010ms, mean: 1.208ms, max: 10.900ms}
    radar: {min: 0.591ms, mean: 0.930ms, max: 60.849ms}
      realloc/copy: {min: 0.070ms, mean: 0.139ms, max: 0.396ms}
      bodies & npcs: {min: 0.035ms, mean: 0.135ms, max: 59.836ms}
      targets: {min: 0.003ms, mean: 0.035ms, max: 13.457ms}
    starfield: {min: 0.260ms, mean: 0.483ms, max: 13.056ms}
    drawLabel: {min: 0.011ms, mean: 0.393ms, max: 5.380ms}
    body: {min: 0.003ms, mean: 0.005ms, max: 0.210ms}
    laser: {min: 0.002ms, mean: 0.003ms, max: 0.030ms}
  input: {min: 0.032ms, mean: 0.324ms, max: 155.308ms}
label: {min: 2.361ms, mean: 2.506ms, max: 2.652ms}
```
