#version 410

uniform vec4 colour;

in vec2 _coord2;

out vec4 fragColor;

void main() {
  if (_coord2.x * _coord2.x - _coord2.y > 0.0) {
    discard;
  }

  // Upper 4 bits: front faces
  // Lower 4 bits: back faces
  fragColor = colour * (gl_FrontFacing ? 16.0 / 255.0 : 1.0 / 255.0);
}
