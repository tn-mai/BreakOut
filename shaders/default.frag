#version 330 core

in vec4 col;
in vec3 normal;
in vec3 pos;

out vec4 fColor;

void main(void)
{
  // material parameter
  const vec4 specular = vec4(0.1, 0.1, 0.1, 1.0);
  vec4 ambient = vec4(0.2, 0.2, 0.2, 1.0);
  const float shininess = 0.1;

  // light parameter
  const vec3 eye = vec3(4, 3, 3);
  const vec3 lightPos = vec3(10, -10, 10);

  vec4 spec = vec4(0, 0, 0, 1);

  vec3 lightVector = normalize(lightPos - pos);
  float intencity = max(dot(normal, lightVector), 0.0);
  if (intencity > 0.0) {
    vec3 h = normalize(eye + lightVector);
    float intSpec = max(dot(h, normal), 0.0);
    spec = specular * pow(intSpec, shininess);
    //ambient = vec4(0.4, 0.2, 0.2, 1.0);
  }
  fColor = max(intencity * col + spec, ambient);
}
