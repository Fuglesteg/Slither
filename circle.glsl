#version 330 core
//uniform float radius;
uniform vec2 position;
out vec4 FragColor;

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
    
    float d1 = sdCircle(uv, 0.2, position);
    float d2 = sdCircle(uv, 0.2, vec2(1, 0.5));
    
    float result = smin(d1, d2, 0.8);
    result = step(0., result);
    
    vec3 color1 = vec3(0.8, 0.2, 0.8);
    vec3 color2 = vec3(0.0, 0.5, 1);
    vec3 mixedColor = mix(color1, color2, d1 - d2);
    
    vec3 colorBg = vec3(0, 0, 0);
    
    FragColor = vec4(mix(mixedColor, colorBg, result), 1.0);
}