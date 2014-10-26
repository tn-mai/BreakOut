{-# LANGUAGE ForeignFunctionInterface #-}

module LightSource where

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Data.Vec as Vec

#include "LightSource.h"

data LightSource = LightSource
  { diffuse :: Vec.Vec4 CFloat
  , specular :: Vec.Vec4 CFloat
  , position :: Vec.Vec4 CFloat
  , attenuation :: CFloat
  }

instance Storable LightSource where
  sizeOf _ = (#size CLightSource)
  alignment _ = 16
  peek ptr = do
    diff <- (#peek CLightSource, diffuse) ptr
    spec <- (#peek CLightSource, specular) ptr
    pos <- (#peek CLightSource, position) ptr
    att <- (#peek CLightSource, attenuation) ptr
    return (LightSource diff spec pos att)
  poke ptr (LightSource diff spec pos att) = do
    (#poke CLightSource, diffuse) ptr diff
    (#poke CLightSource, specular) ptr spec
    (#poke CLightSource, position) ptr pos
    (#poke CLightSource, attenuation) ptr att

data Material = Material
  { baseColor :: Vec.Vec4 CFloat
  , metallic :: CFloat
  , roughness :: CFloat
  }

instance Storable Material where
  sizeOf _ = (#size CMaterial)
  alignment _ = 16
  peek ptr = do
    bc <- (#peek CMaterial, baseColor) ptr
    metal <- (#peek CMaterial, metallic) ptr
    rough <- (#peek CMaterial, roughness) ptr
    return (Material bc metal rough)
  poke ptr (Material bc metal rough) = do
    (#poke CMaterial, baseColor) ptr bc
    (#poke CMaterial, metallic) ptr metal
    (#poke CMaterial, roughness) ptr rough

