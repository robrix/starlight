#version 410
uniform mat3 matrix;
uniform float pointSize;

in vec2 pos;

void main() {
  gl_Position  = vec4((matrix * vec3(pos, 1)).xy, 0, 1.0);
  gl_PointSize = pointSize;
}
