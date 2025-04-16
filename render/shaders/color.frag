#version 330 core
out vec4 FragColor;
uniform vec4 color;

void main() {
    FragColor = vec4(color.xy, 0.8, 0.8);
}