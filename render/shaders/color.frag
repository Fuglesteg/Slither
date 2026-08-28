#version 330 core
out vec4 FragColor;
uniform vec4 color = vec4(1.0, 0.1, 0.4, 1.0);

void main() {
    vec4 resultColor = color;
    resultColor.rgb *= resultColor.a;
    FragColor = resultColor;
}
