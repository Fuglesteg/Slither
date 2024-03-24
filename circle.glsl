#version 330 core
//uniform float radius;
//uniform vec2 position;
out vec4 FragColor;

void main()
{
    float smoothness = 0.01;
    
    vec4 color1 = vec4(1, 0.0, 0.8, 1.0);
    vec2 position1 = vec2(2, 2);
    float radius1 = 0.2;
    vec2 uv1 = (gl_FragCoord.xy / vec2(600.0, 600.0)) - position1;
    float d1 = length(uv1);
    float t1 = smoothstep(radius1, radius1 + smoothness, 1.0 - d1);
    vec4 circle1 = color1 * t1;
    
    vec4 color2 = vec4(1, 0.8, 0.8, 1.0);
    vec2 position2 = vec2(3.8, 2);
    float radius2 = 0.2;
    vec2 uv2 = (gl_FragCoord.xy / vec2(600.0, 600.0)) - position2;
    float d2 = length(uv2);
    float t2 = smoothstep(radius2, radius2 + smoothness, 1.0 - d2);
    vec4 circle2 = color2 * t2;
    
    FragColor = circle1 + circle2;
    //FragColor = vec4(1.0, 1.0, 0.0, 1.0);
}