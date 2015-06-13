import qualified Data.ByteString.Lazy as BL

import Flaw.Asset.Collada

import Data.Word
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import Foreign.Storable
import Language.Haskell.TH

import Flaw.Asset.Collada
import Flaw.Asset.Geometry
import Flaw.Build
import Flaw.FFI
import Flaw.Graphics
import Flaw.Math

import AssetTypes

main :: IO ()
main = do
	let elementId = "geom-field"
	fileData <- BL.readFile "assets/field.DAE"
	let eitherGeometry = runCollada $ do
		initColladaCache fileData
		parseGeometry vertexConstructor =<< getElementById elementId
	case eitherGeometry of
		Right rawVertices -> do
			(vertices, indices) <- indexVertices rawVertices
			verticesBytes <- packVector vertices
			(isIndices32Bit, indicesBytes) <- do
				if length vertices > 0x10000 then do
					indicesBytes <- packVector ((VG.map fromIntegral indices) :: VU.Vector Word32)
					return (True, indicesBytes)
				else do
					indicesBytes <- packVector ((VG.map fromIntegral indices) :: VU.Vector Word16)
					return (False, indicesBytes)
			putStrLn $ show (VG.length indices, isIndices32Bit)
			BL.writeFile "assets/field.vertices" $ BL.fromStrict verticesBytes
			BL.writeFile "assets/field.indices" $ BL.fromStrict indicesBytes
		Left s -> fail s
