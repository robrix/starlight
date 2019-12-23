#version 410

uniform sampler2D sampler;
uniform vec4 colour;

in vec2 _coord2;

out vec4 fragColour;

void main() {
  // Get samples for -2/3 and -1/3
  vec2 valueL = texture(sampler, vec2(_coord2.x + dFdx(_coord2.x), _coord2.y)).yz * 255.0;
  vec2 lowerL = mod(valueL, 16.0);
  vec2 upperL = (valueL - lowerL) / 16.0;
  vec2 alphaL = min(abs(upperL - lowerL), 2.0);

  // Get samples for 0, +1/3, and +2/3
  vec3 valueR = texture(sampler, _coord2).xyz * 255.0;
  vec3 lowerR = mod(valueR, 16.0);
  vec3 upperR = (valueR - lowerR) / 16.0;
  vec3 alphaR = min(abs(upperR - lowerR), 2.0);

  // Average the energy over the pixels on either side
  vec4 rgba = vec4(
    (alphaR.x + alphaR.y + alphaR.z) / 6.0,
    (alphaL.y + alphaR.x + alphaR.y) / 6.0,
    (alphaL.x + alphaL.y + alphaR.x) / 6.0,
    0.0);

  // Optionally scale by a colour
  fragColour = colour.a == 0.0 ? 1.0 - rgba : colour * rgba;
}
