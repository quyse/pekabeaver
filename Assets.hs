{-# LANGUAGE CPP, TemplateHaskell #-}

module Assets
	( fieldGeometry
	, beaverGeometry
	, pekaGeometry
	, fieldTexture
	, beaverTexture
	, pekaTexture
	) where

import Flaw.Asset.Util
import Flaw.App
import Flaw.App.Texture
import Flaw.Graphics
import Flaw.Graphics.Texture

import AssetTypes

type LoadedGeometry = (VertexBufferId AppGraphicsDevice, IndexBufferId AppGraphicsDevice, Int)

fieldGeometry :: AppGraphicsDevice -> IO (LoadedGeometry, IO ())
fieldGeometry = $(embedGeometry "assets/field.DAE" "geom-field")

beaverGeometry :: AppGraphicsDevice -> IO (LoadedGeometry, IO ())
beaverGeometry = $(embedGeometry "assets/beaver.DAE" "geom-Beaver")

pekaGeometry :: AppGraphicsDevice -> IO (LoadedGeometry, IO ())
pekaGeometry = $(embedGeometry "assets/peka.DAE" "geom-peka")

fieldTexture :: AppGraphicsDevice -> IO (TextureId AppGraphicsDevice, IO ())
fieldTexture = $(loadTextureExp "assets/images/0_castle.jpg")

beaverTexture :: AppGraphicsDevice -> IO (TextureId AppGraphicsDevice, IO ())
beaverTexture = $(loadTextureExp "assets/images/0_beaver.jpg")

pekaTexture :: AppGraphicsDevice -> IO (TextureId AppGraphicsDevice, IO ())
pekaTexture = $(loadTextureExp "assets/images/0_peka0.png")
