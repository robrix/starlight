#version 410

uniform vec4 colour;

out vec4 fragColour;

void main() {
  vec2 p = gl_PointCoord - vec2(0.5);
  if (length(p) > 1) {
    discard;
  } else {
    fragColour = colour;
    float mag = length(p) * 2;
    fragColour.a = 1 - mag * mag * mag / 2;
  }
}
