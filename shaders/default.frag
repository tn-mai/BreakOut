#version 330 core

in vec4 col;
in vec3 normal;

out vec4 fColor;

void main(void)
{
  fColor = col;
}
