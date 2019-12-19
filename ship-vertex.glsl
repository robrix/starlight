#version 410
precision highp float;
uniform mat3 matrix3;
in vec2 position2;
void main() {
  gl_Position = vec4(matrix3 * vec3(position2, 1.0), 1.0);
}
