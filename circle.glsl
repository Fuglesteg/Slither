#version 430 core

uniform vec2 position;
uniform vec2 screenSize;
out vec4 FragColor;

struct Circle {
    vec2 position;
    float radius;
    vec3 color;
};

layout(location = 2) uniform Circle circles[10];

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
    vec2 uv = gl_FragCoord.xy / (min(screenSize.x, screenSize.y) / 2);

    float lastDistance = sdCircle(uv, circles[0].radius, circles[0].position);
    vec3 finalColor = circles[0].color;
    float result = lastDistance;

    for (int i = 1; i < circles.length(); i++) {
    	if (circles[i].radius <= 0) {
		continue;
	}
        float distance = sdCircle(uv, circles[i].radius, circles[i].position);
        float lastResult = result;
        result = smin(result, distance, circles[i].radius);
        float steppedResult = step(0., result);

        if (steppedResult < 1) {
            finalColor = mix(circles[i].color, finalColor, distance - lastResult);
        } else {
            finalColor = circles[i].color;
        }

        lastDistance = distance;
    }

    result = step(0., result);
    finalColor = mix(finalColor, vec3(0.0, 0.0, 0.0), result);
    FragColor = vec4(finalColor, 1.0);
}
