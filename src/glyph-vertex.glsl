#version 410

uniform mat3 matrix3;

in vec4 position4;

out vec2 _coord2;

void main() {
  _coord2 = position4.zw;
  gl_Position = vec4(matrix3 * vec3(position4.xy, 1.0), 0.0).xywz;
}
