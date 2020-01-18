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


## drawElementsInstanced

Doing a `drawElementsInstanced` call per glyph instance. Significantly more expensive because we’re reallocating & copying into the `GL_ELEMENT_ARRAY_BUFFER` _n_ times.

```
cacheCharactersForDrawing: {min: 391.602ms, mean: 391.602ms, max: 391.602ms}
readTypeface: {min: 111.088ms, mean: 111.088ms, max: 111.088ms}
frame: {min: 1.078ms, mean: 9.008ms, max: 208.172ms}
  draw: {min: 3.593ms, mean: 8.612ms, max: 106.119ms}
    setLabel: {min: 0.009ms, mean: 2.209ms, max: 13.086ms}
    ship: {min: 1.114ms, mean: 1.329ms, max: 9.454ms}
    radar: {min: 0.601ms, mean: 0.957ms, max: 63.005ms}
      bodies & npcs: {min: 0.035ms, mean: 0.156ms, max: 62.097ms}
      realloc/copy: {min: 0.072ms, mean: 0.144ms, max: 0.399ms}
      targets: {min: 0.003ms, mean: 0.038ms, max: 12.004ms}
    starfield: {min: 0.262ms, mean: 0.487ms, max: 15.092ms}
    drawLabel: {min: 0.010ms, mean: 0.399ms, max: 4.282ms}
    body: {min: 0.003ms, mean: 0.005ms, max: 0.151ms}
    laser: {min: 0.002ms, mean: 0.003ms, max: 0.035ms}
  input: {min: 0.033ms, mean: 0.276ms, max: 101.922ms}
swap: {min: 0.162ms, mean: 8.063ms, max: 53.884ms}
label: {min: 2.572ms, mean: 2.645ms, max: 2.719ms}
```

Plan is to implement non-interleaved arrays to specify the instance offsets in a separate buffer from the vertex data, then write to the buffers for the instance offsets & elements en masse. Hypothesis is improvement due to making a single call; no per glyph instance uniform, write, or draw call.

Reallocating the buffer with the `GL_DRAW` hint had negligible effect.


## Reallocating at most once per `setLabel` call

Pull `realloc` & `copy` out of `drawElementsInstanced` and reallocate once, rather once per glyph instance. Negligible effect.

```
cacheCharactersForDrawing: {min: 380.890ms, mean: 380.890ms, max: 380.890ms}
readTypeface: {min: 109.240ms, mean: 109.240ms, max: 109.240ms}
frame: {min: 1.324ms, mean: 9.291ms, max: 195.824ms}
  draw: {min: 3.736ms, mean: 8.986ms, max: 107.841ms}
    setLabel: {min: 0.010ms, mean: 2.227ms, max: 13.189ms}
    ship: {min: 1.089ms, mean: 1.300ms, max: 9.582ms}
    radar: {min: 0.578ms, mean: 0.977ms, max: 65.654ms}
      realloc/copy: {min: 0.070ms, mean: 0.136ms, max: 0.367ms}
      bodies & npcs: {min: 0.035ms, mean: 0.120ms, max: 64.497ms}
      targets: {min: 0.004ms, mean: 0.062ms, max: 13.745ms}
    drawLabel: {min: 0.011ms, mean: 0.566ms, max: 4.983ms}
    starfield: {min: 0.259ms, mean: 0.503ms, max: 25.816ms}
    body: {min: 0.003ms, mean: 0.005ms, max: 0.161ms}
    laser: {min: 0.002ms, mean: 0.003ms, max: 0.153ms}
  input: {min: 0.033ms, mean: 0.182ms, max: 87.851ms}
swap: {min: 0.187ms, mean: 7.620ms, max: 41.156ms}
label: {min: 2.292ms, mean: 2.400ms, max: 2.507ms}
```
