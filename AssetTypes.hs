{-# LANGUAGE RankNTypes, StandaloneDeriving, TemplateHaskell #-}

module AssetTypes
	( embedGeometry
	) where

import qualified Data.Vector as V
import Language.Haskell.TH

import Flaw.Asset.Collada
import Flaw.Asset.Util
import Flaw.Asset.Vertex
import Flaw.Build

embedGeometry :: String -> String -> Q Exp
embedGeometry fileName elementId = do
	fileData <- loadFile fileName
	let Right vertices = runCollada $ do
		initColladaCache fileData
		createColladaVertices =<< parseGeometry =<< getElementById elementId
	geometry <- runIO $ packGeometry (vertices :: V.Vector VertexPNT)
	[| \device -> loadGeometry device $(embedExp geometry) |]
