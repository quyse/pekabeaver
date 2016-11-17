{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Assets () where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Serialize as S
import Data.Serialize.Text()
import qualified Data.Text as T
import Language.Haskell.TH
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as H

import Flaw.Asset
import Flaw.Asset.Collada
import Flaw.Asset.FolderAssetPack
import Flaw.Asset.RemapAssetPack
import Flaw.Build
import Flaw.Visual.Geometry

do
	packBuilder <- runIO $ newRemapAssetPackBuilder (FolderAssetPackBuilder "assetpack/") remapAssetWithHash
	let addAsset assetId asset = runIO $ putAsset packBuilder assetId asset

	let emitTextureAsset = runIO . B.readFile

	addAsset "field.bin" =<< emitGeometryAsset "assets/field.DAE" (getElementById "geom-field")
	addAsset "beaver.bin" =<< emitGeometryAsset "assets/beaver.DAE" (getElementById "geom-Beaver")
	addAsset "peka.bin" =<< emitGeometryAsset "assets/peka.DAE" (getElementById "geom-peka")
	addAsset "castle.jpg" =<< emitTextureAsset "assets/images/0_castle.jpg"
	addAsset "beaver.jpg" =<< emitTextureAsset "assets/images/0_beaver.jpg"
	addAsset "peka.png" =<< emitTextureAsset "assets/images/0_peka0.png"
	addAsset "beaver_icon.png" =<< emitTextureAsset "assets/images/beaver_icon.png"
	addAsset "peka_icon.png" =<< emitTextureAsset "assets/images/peka_icon.png"
	addAsset "beaver_win.jpg" =<< emitTextureAsset "assets/images/beaver_win.jpg"
	addAsset "peka_win.jpg" =<< emitTextureAsset "assets/images/peka_win.jpeg"
	addAsset "styles.css" =<< BL.toStrict <$> loadFile "assets/styles.css"
	addAsset "pekabeaver.min.js" =<< BL.toStrict <$> loadFile "pekabeaver.min.js"

	remapAssetPackContainer <- runIO $ finalizeRemapAssetPackBuilder packBuilder
	runIO $ B.writeFile "pack.bin" $ S.encode remapAssetPackContainer

	let pack = loadRemapAssetPack remapAssetPackContainer (FolderAssetPack "assetpack/")

	[ stylesCss, pekabeaverJs
		, beaverIconPng, pekaIconPng
		, beaverWinPng, pekaWinPng
		] <- runIO $ mapM (fmap H.toValue . getWebAssetUrl pack)
		[ "styles.css" :: T.Text
		, "pekabeaver.min.js"
		, "beaver_icon.png"
		, "peka_icon.png"
		, "beaver_win.jpg"
		, "peka_win.jpg"
		]

	runIO $ BL.writeFile "index.html" $ H.renderHtml $ H.docTypeHtml $ do
		H.head $ do
			H.title "PEKABEAVER"
			H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href stylesCss
			H.script H.! A.src pekabeaverJs $ mempty
		H.body $ do
			H.div H.! A.class_ "progress beaver" $ do
				H.div H.! A.id "beaver_lives" H.! A.class_ "bar" H.! A.style "width:100%" $ mempty
				H.img H.! A.src beaverIconPng
			H.div H.! A.class_ "progress peka" $ do
				H.div H.! A.id "peka_lives" H.! A.class_ "bar" H.! A.style "width:100%" $ mempty
				H.img H.! A.src pekaIconPng
			H.div H.! A.id "start" H.! A.class_ "start" $ do
				H.p H.! A.class_ "title" $ "Select your side"
				H.p H.! A.class_ "select" $ do
					H.img H.! A.id "start-beaver" H.! A.src beaverWinPng
					H.img H.! A.id "start-peka" H.! A.src pekaWinPng
			H.div H.! A.id "end" H.! A.class_ "end" $ do
				H.p H.! A.id "end-title" H.! A.class_ "title" $ mempty
				H.p H.! A.id "end-beaver" H.! A.class_ "picture" $ H.img H.! A.src beaverWinPng
				H.p H.! A.id "end-peka" H.! A.class_ "picture" $ H.img H.! A.src pekaWinPng

	return []
