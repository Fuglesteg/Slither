#version 330 core

uniform vec2 position;
out vec4 FragColor;

struct Circle {
    vec2 position;
    float radius;
    vec3 color;
};

//uniform Circle[] circles;

Circle circles[] = Circle[](
	Circle(vec2(1, 0.5), 0.2, vec3(0.8, 0.2, 0.8)),
	Circle(vec2(0, 0), 0.2, vec3(0.0, 0.5, 1)));

float smin(float a, float b, float k) {
  float h = clamp(0.5+0.5*(b-a)/k, 0.0, 1.0);
  return mix(b, a, h) - k*h*(1.0-h);
}

float smax(float a, float b, float k) {
  return -smin(-a, -b, k);
}

float sdCircle(vec2 uv, float r, vec2 offset) {
  float x = uv.x - offset.x;
  float y = uv.y - offset.y;

  return length(vec2(x, y)) - r;
}

void main() {
    vec2 uv = gl_FragCoord.xy / vec2(2000, 2000);

    circles[1].position = position;
    vec3 finalColor = vec3(0, 0, 0);
    float lastDistance = 0;
    float distances[circles.length()];

    for (int i = 0; i < circles.length(); i++) {
	float distance = sdCircle(uv, circles[i].radius, circles[i].position);
	finalColor = mix(finalColor, circles[i].color, lastDistance - distance);
	distances[i] = distance;
	lastDistance = distance;
    	float result = smin(lastDistance, distance, 0.8);
    	result = step(0., result);
        finalColor = mix(finalColor, vec3(0.0, 0.0, 0.0), result);
    }
    FragColor = vec4(finalColor, 1.0);
}