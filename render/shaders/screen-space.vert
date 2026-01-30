#version 330 core

layout (location = 0) in vec2 aPos;

uniform mat3 modelMatrix = mat3(1.0);

void main() {
    gl_Position = vec4(modelMatrix * vec3(aPos, 1.0f), 1.0f);
}
