#version 410

uniform mat3 matrix;

in vec2 position2;

void main() {
  gl_Position = vec4(matrix * vec3(position2, 1.0), 1.0);
}
