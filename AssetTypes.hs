{-# LANGUAGE RankNTypes, StandaloneDeriving, TemplateHaskell #-}

module AssetTypes
	( Vertex(..)
	, vertexConstructor
	, loadGeometry
	) where

import Control.Monad
import Data.Word
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import Foreign.Storable
import Language.Haskell.TH

import Flaw.Asset.Collada
import Flaw.Asset.Geometry
import Flaw.Book
import Flaw.Build
import Flaw.FFI
import Flaw.Graphics
import Flaw.Math

genStruct "Vertex"
	[ ([t| Vec3f |], "position")
	, ([t| Vec3f |], "normal")
	, ([t| Vec2f |], "texcoord")
	]

deriving instance Eq Vertex
deriving instance Ord Vertex

vertexConstructor :: ColladaSettings -> (forall a v. Parse a v => String -> ColladaM (v a, Int)) -> ColladaM (V.Vector Vertex)
vertexConstructor ColladaSettings
	{ colladaUnit = unit
	} f = do
	positions <- liftM chunks3stride $ f "VERTEX"
	normals <- liftM chunks3stride $ f "NORMAL"
	texcoords <- liftM chunks3stride $ f "TEXCOORD"
	let create ((px, py, pz), (nx, ny, nz), (tx, ty, _tz)) = Vertex
		{ f_Vertex_position = Vec3 (px * unit) (py * unit) (pz * unit)
		, f_Vertex_normal = Vec3 nx ny nz
		, f_Vertex_texcoord = Vec2 tx (1 - ty)
		}
	return $ V.map create $ V.zip3 positions normals texcoords

loadGeometry :: String -> String -> Q Exp
loadGeometry fileName elementId = do
	fileData <- loadFile fileName
	let eitherGeometry = runCollada $ do
		initColladaCache fileData
		parseGeometry vertexConstructor =<< getElementById elementId
	case eitherGeometry of
		Right rawVertices -> do
			d <- runIO $ do
				(vertices, indices) <- indexVertices rawVertices
				verticesBytes <- packVector vertices
				(isIndices32Bit, indicesBytes) <- do
					if length vertices > 0x10000 then do
						indicesBytes <- packVector ((VG.map fromIntegral indices) :: VU.Vector Word32)
						return (True, indicesBytes)
					else do
						indicesBytes <- packVector ((VG.map fromIntegral indices) :: VU.Vector Word16)
						return (False, indicesBytes)
				return (verticesBytes, VG.length indices, isIndices32Bit, indicesBytes)
			[|
				\device -> do
					bk <- newBook
					(verticesBytes, indicesCount, isIndices32Bit, indicesBytes) <- $(embedIOExp d)
					vb <- book bk $ createStaticVertexBuffer device verticesBytes (sizeOf (undefined :: Vertex))
					ib <- book bk $ createStaticIndexBuffer device indicesBytes isIndices32Bit
					return ((vb, ib, indicesCount), freeBook bk)
				|]
		Left s -> fail s
