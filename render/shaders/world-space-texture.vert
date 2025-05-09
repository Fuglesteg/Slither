#version 330 core

layout (location = 0) in vec2 aPos;
layout (location = 1) in vec2 aTexCoord;

out vec2 TexCoord;

uniform mat3 modelMatrix;
uniform mat3 viewMatrix;

void main() {
    gl_Position = vec4(viewMatrix * modelMatrix * vec3(aPos, 1.0f), 1.0f);
    TexCoord = aTexCoord;
}
