#version 330 core

out vec4 FragColor;
in vec2 TexCoord;

uniform vec2 start = vec2(0.0);
uniform vec2 controlPoint = vec2(0.0);
uniform vec2 end = vec2(0.0);

uniform float thickness = 0.02;
uniform vec4 color = vec4(0.0);

float dot2( vec2 v ) { return dot(v,v); }
float cro( vec2 a, vec2 b ) { return a.x*b.y-a.y*b.x; }

// Adapted from: https://iquilezles.org/articles/distfunctions2d
float sdBezier(vec2 p, vec2 v0, vec2 v1, vec2 v2) {
    vec2 i = v0 - v2;
    vec2 j = v2 - v1;
    vec2 k = v1 - v0;
    vec2 w = j-k;

    v0-= p; v1-= p; v2-= p;

    float x = cro(v0, v2);
    float y = cro(v1, v0);
    float z = cro(v2, v1);

    vec2 s = 2.0*(y*j+z*k)-x*i;

    float r =  (y*z-x*x*0.25)/dot2(s);
    float t = clamp( (0.5*x+y+r*dot(s,w))/(x+y+z),0.0,1.0);

    vec2 d = v0+t*(k+k+t*w);
    return length(d);
}

void main() {
    vec2 pixel = TexCoord * 2.0 - 1.0; // Map to [-1, 1] coordinates
    float signedDistance = sdBezier(pixel, start, controlPoint, end);
    signedDistance -= thickness * 0.5;
    float edge = fwidth(signedDistance);
    float alpha = 1.0 - smoothstep(-edge, edge, signedDistance);

    vec4 finalColor = color;
    finalColor.a *= alpha;
    finalColor.rgb *= finalColor.a;
    FragColor = finalColor;
}
