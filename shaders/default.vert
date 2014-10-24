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
uniform mat4 MV;
uniform mat4 N;

smooth out vec4 col;
smooth out vec3 normal;
smooth out vec3 pos;

void main()
{
  col = vColor;
  normal = normalize(N * vec4(vNormal, 1)).xyz;
  pos = (MV * vec4(vPosition, 1)).xyz;
  gl_Position = MVP * vec4(vPosition, 1);
}
