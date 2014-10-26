#version 330 core

layout(std140) uniform LightSource {
  vec4 diffuse;
  vec4 specular;
  vec4 position;
  float attenuation;
};

layout (std140) uniform Material {
  vec4 baseColor;
  float metallic;
  float roughness;
};

uniform float shininess;

smooth in vec4 col;
smooth in vec3 normal;
smooth in vec3 pos;

out vec4 fColor;

void main(void)
{
  const float pi = 3.14159265358979323846264;

  // material parameter
  vec4 ambient = vec4(0.2, 0.1, 0.2, 1.0);

  vec3 eyeVector = normalize(-pos);
  vec3 lightVector = normalize(position.xyz - pos);
  vec3 refVector = normalize(reflect(lightVector, normal));

  vec4 Idiff = col * baseColor * diffuse * max(dot(normal, lightVector), 0.0);
  Idiff = clamp(Idiff, 0.0, 1.0);

  // [GGX]
  // D(h) = alpha^2 / ( pi * (dot(n, h)^2 * (alpha^2 - 1) + 1)^2)
  vec3 halfVector = normalize(eyeVector + lightVector);
  float alpha2 = roughness * roughness;
  float dotNH = dot(normal, halfVector);
  float x = dotNH * dotNH * (alpha2 - 1) + 1;
  float D = alpha2 / (pi * x * x);

  // [FGS]
  // F0 : fresnel refrectance of material. we use metallic.
  // v : eye vector.
  // h : half vector.
  // F(v, h) = F0 + (1.0 - F0) * 2^(-5.55473*dot(v, h) - 6.98316)*dot(v, h)
  float dotVH = dot(eyeVector, halfVector);
  float F = metallic + (1.0 - metallic) * pow(2.0, (-5.55473 * dotVH - 6.98316) * dotVH);

  // k = (roughness + 1)^2 / 8
  // G1(v) = dot(n, v) / dot(n, v) * (1 - k) + k
  // G(l, v, h) = G1(l) * G1(v)
  float k = (roughness + 1) * (roughness + 1) / 8;
  float dotNL = dot(normal, lightVector);
  float dotNV = dot(normal, eyeVector);
  float G1 = dotNL / (dotNL * (1 - k) + k);
  float G2 = dotNV / (dotNV * (1 - k) + k);
  float G = G1 * G2;

  // f(l, v) = D(h) * F(v, h) * G(l, v, h) / (4 * dot(n, l) * dot(n, v))
  float f = (D * F * G) / (4 * dotNL * dotNV);

  vec4 Ispec = specular * f;
  Ispec = clamp(Ispec, 0.0, 1.0);

  fColor = ambient + Idiff + Ispec;
}
