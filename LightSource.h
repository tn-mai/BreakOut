typedef struct Vec3 { float x, y, z; } Vec3 __attribute__((aligned(16)));
typedef struct Vec4 { float x, y, z, w; } Vec4 __attribute__((aligned(16)));

typedef struct CLightSource {
  Vec4 diffuse;
  Vec4 specular;
  Vec4 position;
  float attenuation;
} CLightSource __attribute__((aligned(16)));

typedef struct CMaterial {
  Vec4 baseColor;
  float metallic;
  float roughness;
} CMaterial __attribute__((aligned(16)));
