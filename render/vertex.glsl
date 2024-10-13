#version 330 core
layout (location = 0) in vec2 aPos;
uniform mat3 modelMatrix;

void main() {
    gl_Position = vec4(modelMatrix * aPos, 1.0f, 1.0f);
}