#version 430 core

uniform vec2 position;
uniform vec2 screenSize;
uniform float zoom = 1.0;
out vec4 FragColor;

struct Circle {
    vec2 position;
    float radius;
    vec3 color;
};

layout(location = 2) uniform Circle circles[20];

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
    vec2 uv = gl_FragCoord.xy / (min(screenSize.x, screenSize.y) / 2);
    //vec2 uv = gl_FragCoord.xy / (screenSize.xy / 4);

    vec3 finalColor = vec3(0.0, 0.0, 0.0);
    float minimumDistance = 2000.0;

    for (int i = 0; i < circles.length(); i++) {
        Circle circle = circles[i];
        float distance = sdCircle(uv, circle.radius, circle.position);
        minimumDistance = smoothMin(minimumDistance, distance, circle.radius);

        //float t = smoothstep(0.0, 1.0, distance * mix(0.0, 1.0, circle.radius));
        //float t = smoothstep(0.0, 1.0, distance - circle.radius);
        float t = smoothstep(0.0, 1.0, (distance + circle.radius) / zoom);
        //float t = smoothstep(0.0, 1.0, distance + circle.radius);

        finalColor = mix(circle.color, finalColor, t);
    }

    //finalColor = mix(finalColor, vec3(0.0, 0.0, 0.0), smoothstep(-0.01, 0.01, minimumDistance));
    finalColor = mix(finalColor, vec3(0.0, 0.0, 0.0), step(0.0, minimumDistance));
    FragColor = vec4(finalColor, 1.0);
}
