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
    Circle(vec2(0, 0), 0.2, vec3(0.0, 0.5, 1)),
    Circle(vec2(0.5, 0.7), 0.2, vec3(0.2, 0.2, 0.3)));

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
    vec3 finalColor = circles[0].color;
    float lastDistance = sdCircle(uv, circles[0].radius, circles[0].position);
    float result = lastDistance;

    for (int i = 1; i < circles.length(); i++) {
        float distance = sdCircle(uv, circles[i].radius, circles[i].position);
        finalColor = mix(circles[i].color, finalColor, distance - lastDistance);
        result = smin(result, distance, 0.2);
        lastDistance = distance;
    }

    result = step(0., result);
    finalColor = mix(finalColor, vec3(0.0, 0.0, 0.0), result);
    FragColor = vec4(finalColor, 1.0);
}
