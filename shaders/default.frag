#version 330 core

smooth in vec4 col;
smooth in vec3 normal;
smooth in vec3 pos;

out vec4 fColor;

void main(void)
{
  // material parameter
  const vec4 diffuse = vec4(0.45, 0.5, 0.4, 1.0);
  const vec4 specular = vec4(1.0, 1.0, 1.0, 1.0);
  vec4 ambient = vec4(0.2, 0.1, 0.2, 1.0);
  const float shininess = 64;

  // light parameter
  const vec3 lightPos = vec3(50, 50, 100);

  vec3 eyeVector = normalize(-pos);
  vec3 lightVector = normalize(lightPos - pos);
  vec3 refVector = normalize(-reflect(lightVector, normal));

  vec4 Idiff = col * diffuse * max(dot(normal, lightVector), 0.0);
  Idiff = clamp(Idiff, 0.0, 1.0);

  vec4 Ispec = col * specular * pow(max(dot(eyeVector, refVector), 0.0), shininess);
  Ispec = clamp(Ispec, 0.0, 1.0);

  fColor = ambient + Idiff * 1 + Ispec;
}
