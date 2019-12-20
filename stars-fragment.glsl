#version 410

// from https://www.shadertoy.com/view/XlfGRj

uniform vec2 resolution; // viewport resolution (in pixels)
uniform vec2 origin;


// Star Nest by Pablo Roman Andrioli

// This content is under the MIT License.

#define iterations 17
#define formuparam 0.53

#define volsteps 8
#define stepsize 0.1

#define zoom   0.800
#define tile   0.850

#define brightness 0.0015
#define darkmatter 0.300
#define distfading 0.730
#define saturation 0.850

out vec4 fragColor;

void main() {
	vec2 uv = gl_FragCoord.xy / resolution.xy - 0.5;
	uv.y *= resolution.y / resolution.x;
	vec3 dir = vec3(uv * zoom, 1.0);

	vec3 origin = vec3(origin / 100 / resolution, 1);

	// volumetric rendering
	float s = 0.1, fade = 1.0;
	vec3 v = vec3(0.0);
	for (int r = 0; r < volsteps; r++) {
		vec3 p = origin + dir * s * 0.5;
		p = abs(vec3(tile) - mod(p, vec3(tile * 2.0))); // tiling fold
		float pa, a = pa = 0.0;
		for (int i = 0; i < iterations; i++) {
			p = abs(p) / dot(p, p) - formuparam; // the magic formula
			a += abs(length(p) - pa); // absolute sum of average change
			pa = length(p);
		}
		float dm = max(0.0, darkmatter - a * a * 0.001); //dark matter
		a *= a * a; // add contrast
		if (r > 6) fade *= 1.0 - dm; // dark matter, don't render near
		//v+=vec3(dm,dm*.5,0.0);
		v += fade;
		v += vec3(s, s * s, s * s * s) * a * brightness * fade; // coloring based on distance
		fade *= distfading; // distance fading
		s += stepsize;
	}
	v = mix(vec3(length(v)), v, saturation); //color adjust
	fragColor = vec4(v * 0.01, 1.0);
}
