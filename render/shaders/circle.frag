#version 330 core

out vec4 FragColor;
in vec2 TexCoord;
uniform vec4 color = vec4(1.0);

void main() {
    float dist = length(TexCoord - vec2(0.5));
    float edge = fwidth(dist);
    float alpha = 1.0 - smoothstep(0.5 - edge, 0.5 + edge, dist);

    if (alpha <= 0.0)
        discard;

    FragColor = vec4(color.rgb, color.a * alpha);
}
