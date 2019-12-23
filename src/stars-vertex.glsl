#version 410

in vec2 position2;

void main() {
  gl_Position = vec4(position2, 0.0, 1.0);
}
