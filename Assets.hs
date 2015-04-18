{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

module Assets
	( Vertex(..)
	, fieldGeometry
	, beaverGeometry
	, pekaGeometry
	, fieldTexture
	, beaverTexture
	, pekaTexture
	) where

import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import Data.Word
import Language.Haskell.TH

import Flaw.Asset.Collada
import Flaw.Asset.Geometry
import Flaw.Build
import Flaw.Game
import Flaw.Game.Texture
import Flaw.Graphics

import AssetTypes

type Geometry = (VertexBufferId GameGraphicsDevice, IndexBufferId GameGraphicsDevice, Int)

fieldGeometry :: (MonadResource m, MonadBaseControl IO m) => GameGraphicsDevice -> m Geometry
fieldGeometry = $(loadGeometry "assets/field.DAE" "geom-field")

beaverGeometry :: (MonadResource m, MonadBaseControl IO m) => GameGraphicsDevice -> m Geometry
beaverGeometry = $(loadGeometry "assets/beaver.DAE" "geom-Beaver")

pekaGeometry :: (MonadResource m, MonadBaseControl IO m) => GameGraphicsDevice -> m Geometry
pekaGeometry = $(loadGeometry "assets/peka.DAE" "geom-peka")

fieldTexture :: (MonadResource m, MonadBaseControl IO m) => GameGraphicsDevice -> m (ReleaseKey, TextureId GameGraphicsDevice)
fieldTexture = $(loadPngTextureExp "assets/0_lightTower.png")

beaverTexture :: (MonadResource m, MonadBaseControl IO m) => GameGraphicsDevice -> m (ReleaseKey, TextureId GameGraphicsDevice)
beaverTexture = $(loadPngTextureExp "assets/images/0_beaver.jpg")

pekaTexture :: (MonadResource m, MonadBaseControl IO m) => GameGraphicsDevice -> m (ReleaseKey, TextureId GameGraphicsDevice)
pekaTexture = $(loadPngTextureExp "assets/images/0_peka0.png")
