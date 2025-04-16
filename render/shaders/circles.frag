#version 330 core

out vec4 FragColor;
float zoom = 1.0;
vec2 position = vec2(1.0, 1.0);

struct Circle {
    vec2 location;
    vec3 color;
    float radius;
};

uniform Circle circles[10];

float smoothMin(float a, float b, float k) {
    float h = clamp(0.5+0.5*(b-a)/k, 0.0, 1.0);
    return mix(b, a, h) - k*h*(1.0-h);
}

float smax(float a, float b, float k) {
    return -smoothMin(-a, -b, k);
}

float sdCircle(vec2 uv, float r, vec2 offset) {
    float x = uv.x - offset.x;
    float y = uv.y - offset.y;

    return length(vec2(x, y)) - r;
}

void main() {
    vec2 uv = gl_FragCoord.xy / 1000;

    vec3 finalColor = vec3(0.0, 0.0, 0.0);
    float minimumDistance = 2000.0;

    for (int i = 0; i < circles.length(); i++) {
        Circle circle = circles[i];
        float distance = sdCircle(uv, circle.radius, circle.location);
        minimumDistance = smoothMin(minimumDistance, distance, circle.radius);
        float t = smoothstep(0.0, 1.0, (distance + circle.radius) / zoom);
        finalColor = mix(circle.color, finalColor, t);
    }

    finalColor = mix(finalColor, vec3(0.0, 0.0, 0.0), step(0.0, minimumDistance));
    FragColor = vec4(finalColor, 1.0);
}