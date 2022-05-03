(in-package :raylib)

(defvar scanline-fragment-shader
  "#version 330
precision mediump float;

in vec2 fragTexCoord;
in vec4 fragColor;

out vec4 finalColor;

uniform sampler2D texture0;
uniform vec4 colDiffuse;

void main() {
    vec3 pixel = texture(texture0, fragTexCoord).rgb;

    // every 3rd pixel should be a scanline
    float fmin = 0.50;
    float fmod = mod(gl_FragCoord.y, 3.0);
    float fstep = fmin + (1.0 - fmin) * fmod;

    // alpha the color by the scanline
    finalColor = vec4(pixel, fstep);
}")
