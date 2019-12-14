#version 410
precision highp float;
uniform vec2 translation;
uniform vec2 scale;
uniform float rotation;
in vec2 position2;
void main() {
  mat3 matrix3;
  mat3 translation_;
  translation_[0] = vec3(1, 0, translation.x);
  translation_[1] = vec3(0, 1, translation.y);
  translation_[2] = vec3(0, 0, 1);

  mat3 scale_;
  scale_[0] = vec3(scale.x, 0,       0);
  scale_[1] = vec3(0,       scale.y, 0);
  scale_[2] = vec3(0,       0,       1);

  float cosT = cos(rotation)
      , sinT = sin(rotation);
  mat3 rotation_;
  rotation_[0] = vec3(cosT, -sinT, 0);
  rotation_[1] = vec3(sinT, cosT,  0);
  rotation_[2] = vec3(0,    0,     1);
  gl_Position = vec4(translation_ * scale_ * rotation_ * vec3(position2, 0.0), 1.0);
}
