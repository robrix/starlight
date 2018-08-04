#version 410
precision highp float;
uniform vec4 rect;
in vec4 position4;
out vec2 _coord2;
void main() {
  _coord2 = mix(rect.xy, rect.zw, position4.xy * 0.5 + 0.5);
  gl_Position = vec4(_coord2 * 2.0 - 1.0, 0.0, 1.0);
}
