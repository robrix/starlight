#version 410
uniform mat3  matrix;
uniform float angle;
uniform float sweep;

in float n;

void main() {
  float angle = angle + n * sweep;
  vec2 pos = vec2(cos(angle), sin(angle)) * 150;
  gl_Position = vec4((matrix * vec3(pos, 1)).xy, 0, 1);
}
