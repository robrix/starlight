#version 410
precision highp float;
uniform vec4 colour;
out vec4 fragColour;
void main() {
  fragColour = colour;
}
