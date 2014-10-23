#version 330 core

/*
layout(std140) uniform DefaultParam
{
  mat4 viewProjection;
  mat4 joints[32];
  vec4 lightPositon[4];
  vec4 lightColor[4];
  int lightCount;
};
*/

layout(location = 0) in vec3 vPosition;
layout(location = 1) in vec3 vNormal;
layout(location = 2) in vec4 vColor;

uniform mat4 MVP;

out vec4 col;
out vec3 normal;

void main()
{
  col = vColor;
  normal = vNormal.xyz;
  gl_Position = MVP * vec4(vPosition, 1);
}
