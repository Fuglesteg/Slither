#version 330 core

out vec4 FragColor;
in vec2 TexCoord;

uniform sampler2D spriteTexture;
uniform vec2 textureScale = vec2(1.0, 1.0);

void main() {
    FragColor = texture(spriteTexture, TexCoord * textureScale);
}
