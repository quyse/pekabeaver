{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Assets
	( fieldGeometry
	, beaverGeometry
	, pekaGeometry
	, fieldTexture
	, beaverTexture
	, pekaTexture
	) where

import Flaw.App
import Flaw.Asset.Collada
import Flaw.Graphics
import Flaw.Graphics.Texture
import Flaw.Visual.Geometry
import Flaw.Visual.Texture

fieldGeometry :: AppGraphicsDevice -> IO (Geometry AppGraphicsDevice, IO ())
fieldGeometry = $(embedLoadGeometryExp "assets/field.DAE" (getElementById "geom-field"))

beaverGeometry :: AppGraphicsDevice -> IO (Geometry AppGraphicsDevice, IO ())
beaverGeometry = $(embedLoadGeometryExp "assets/beaver.DAE" (getElementById "geom-Beaver"))

pekaGeometry :: AppGraphicsDevice -> IO (Geometry AppGraphicsDevice, IO ())
pekaGeometry = $(embedLoadGeometryExp "assets/peka.DAE" (getElementById "geom-peka"))

fieldTexture :: AppGraphicsDevice -> IO (TextureId AppGraphicsDevice, IO ())
fieldTexture = $(loadDxtCompressedTextureExp "assets/images/0_castle.jpg")

beaverTexture :: AppGraphicsDevice -> IO (TextureId AppGraphicsDevice, IO ())
beaverTexture = $(loadDxtCompressedTextureExp "assets/images/0_beaver.jpg")

pekaTexture :: AppGraphicsDevice -> IO (TextureId AppGraphicsDevice, IO ())
pekaTexture = $(loadDxtCompressedTextureExp "assets/images/0_peka0.png")
