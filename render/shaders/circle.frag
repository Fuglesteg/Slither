#version 330 core

out vec4 FragColor;
in vec2 TexCoord;
uniform vec4 color = vec4(1.0);

void main() {
    float dx = TexCoord.x - 0.5;
    float dy = TexCoord.y - 0.5;
    float radius = sqrt(dx * dx + dy * dy);
    FragColor = mix(color,
		    vec4(0.0),
		    radius + 0.4);
}
