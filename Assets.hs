{-# LANGUAGE CPP, TemplateHaskell #-}

module Assets
	( Vertex(..)
	, fieldGeometry
	, beaverGeometry
	, pekaGeometry
	, fieldTexture
	, beaverTexture
	, pekaTexture
	) where

import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Data.Word
import Foreign.Storable
import Language.Haskell.TH

import Flaw.Asset.Collada
import Flaw.Asset.Geometry
import Flaw.Book
import Flaw.Build
import Flaw.App
import Flaw.App.Texture
import Flaw.Graphics
import Flaw.Graphics.Texture

import AssetTypes

type Geometry = (VertexBufferId AppGraphicsDevice, IndexBufferId AppGraphicsDevice, Int)

fieldGeometry :: AppGraphicsDevice -> IO (Geometry, IO ())
#if ghcjs_HOST_OS
fieldGeometry device = do
	bk <- newBook
	verticesBytes <- $(embedIOExp =<< loadFile "assets/field.vertices")
	indicesBytes <- $(embedIOExp =<< loadFile "assets/field.indices")
	let indicesCount = 50868
	let isIndices32Bit = False
	vb <- book bk $ createStaticVertexBuffer device verticesBytes (sizeOf (undefined :: Vertex))
	ib <- book bk $ createStaticIndexBuffer device indicesBytes isIndices32Bit
	return ((vb, ib, indicesCount), freeBook bk)
#else
fieldGeometry = $(loadGeometry "assets/field.DAE" "geom-field")
#endif

beaverGeometry :: AppGraphicsDevice -> IO (Geometry, IO ())
beaverGeometry = $(loadGeometry "assets/beaver.DAE" "geom-Beaver")

pekaGeometry :: AppGraphicsDevice -> IO (Geometry, IO ())
pekaGeometry = $(loadGeometry "assets/peka.DAE" "geom-peka")

fieldTexture :: AppGraphicsDevice -> IO (TextureId AppGraphicsDevice, IO ())
fieldTexture = $(loadTextureExp "assets/images/0_castle.jpg")

beaverTexture :: AppGraphicsDevice -> IO (TextureId AppGraphicsDevice, IO ())
beaverTexture = $(loadTextureExp "assets/images/0_beaver.jpg")

pekaTexture :: AppGraphicsDevice -> IO (TextureId AppGraphicsDevice, IO ())
pekaTexture = $(loadTextureExp "assets/images/0_peka0.png")
