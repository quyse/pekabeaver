{-# LANGUAGE RankNTypes, StandaloneDeriving, TemplateHaskell #-}

module AssetTypes
	( Vertex(..)
	, vertexConstructor
	, loadGeometry
	) where

import Control.Monad.IO.Class
import Data.Word
import Foreign.Storable
import Language.Haskell.TH

import Flaw.Asset.Collada
import Flaw.Asset.Geometry
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

vertexConstructor :: (forall a. Parse a => String -> ColladaM [[a]]) -> ColladaM [Vertex]
vertexConstructor f = do
	positions <- f "VERTEX"
	normals <- f "NORMAL"
	texcoords <- f "TEXCOORD"
	return [Vertex
		{ f_Vertex_position = Vec3 px py pz
		, f_Vertex_normal = Vec3 nx ny nz
		, f_Vertex_texcoord = Vec2 tx (1 - ty)
		} | ([px, py, pz], [nx, ny, nz], [tx, ty, _tz]) <- zip3 positions normals texcoords]

loadGeometry :: String -> String -> Q Exp
loadGeometry fileName elementId = do
	fileData <- loadFile fileName
	let eitherGeometry = runCollada $ do
		initColladaCache fileData
		parseGeometry vertexConstructor =<< getElementById elementId
	case eitherGeometry of
		Right rawVertices -> do
			d <- runIO $ do
				let (vertices, indices) = indexVertices rawVertices
				verticesBytes <- packList vertices
				(isIndices32Bit, indicesBytes) <- do
					if length vertices > 0x10000 then do
						indicesBytes <- packList ((map fromIntegral indices) :: [Word32])
						return (True, indicesBytes)
					else do
						indicesBytes <- packList ((map fromIntegral indices) :: [Word16])
						return (False, indicesBytes)
				return (verticesBytes, length indices, isIndices32Bit, indicesBytes)
			[|
				\device -> do
					(verticesBytes, indicesCount, isIndices32Bit, indicesBytes) <- liftIO $ $(embedIOExp d)
					(_, vb) <- createStaticVertexBuffer device verticesBytes (sizeOf (undefined :: Vertex))
					(_, ib) <- createStaticIndexBuffer device indicesBytes isIndices32Bit
					return (vb, ib, indicesCount)
				|]
		Left s -> fail s
