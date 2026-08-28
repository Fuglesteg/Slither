#version 330 core

out vec4 FragColor;
in vec2 TexCoord;

uniform vec4 color = vec4(1.0, 1.0, 1.0, 1.0);
uniform vec4 borderRadius = vec4(0.0, 0.0, 0.0, 0.0);
uniform vec2 rectangleSize = vec2(1.0, 1.0);

float sdRoundedRectangle(vec2 pixel, vec2 boxSize, vec4 radius) {
    radius.xy = pixel.x > 0.0 ? radius.xy : radius.zw;
    radius.x  = pixel.y > 0.0 ? radius.x  : radius.y;
    vec2 cornerDistance = abs(pixel) - boxSize + radius.x;
    return min(max(cornerDistance.x, cornerDistance.y) ,0.0)
           +
           length(max(cornerDistance, 0.0)) - radius.x;
}

void main() {
    vec2 pixel = TexCoord * 2.0 - 1.0; // Map to [-1, 1] coordinates
    float aspectRatio = rectangleSize.x / rectangleSize.y;
    pixel.x *= aspectRatio;

    float signedDistance = sdRoundedRectangle(pixel, vec2(aspectRatio, 1.0), borderRadius);
    float edge = fwidth(signedDistance);
    float alpha = 1.0 - smoothstep(-edge, edge, signedDistance);

    vec4 resultColor = color;
    resultColor.a *= alpha;
    resultColor.rgb *= resultColor.a;

    FragColor = resultColor;
}
