#version 330 core

out vec4 FragColor;
in vec2 TexCoord;
uniform int textureIndex = 0;
uniform vec4 color = vec4(1.0, 1.0, 1.0, 1.0);

uniform sampler2DArray spriteTexture;

void main() {
    vec4 resultColor = color;
    resultColor.rgb *= resultColor.a;
    FragColor = texture(spriteTexture, vec3(TexCoord, textureIndex)) * resultColor;
}
