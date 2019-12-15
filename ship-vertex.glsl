#version 410
precision highp float;
uniform vec2 translation;
uniform vec2 scale;
uniform float rotation;
in vec2 position2;
void main() {
  mat3 translation = mat3
    ( 1, 0, translation.x
    , 0, 1, translation.y
    , 0, 0, 1);

  mat3 scale = mat3
    ( scale.x, 0,       0
    , 0,       scale.y, 0
    , 0,       0,       1);

  float cosT = cos(rotation)
      , sinT = sin(rotation);
  mat3 rotation = mat3
    ( cosT, sinT,  0
    , -sinT, cosT, 0
    , 0,    0,     1);

  gl_Position = vec4(translation * scale * rotation * vec3(position2, 0.0), 1.0);
}
