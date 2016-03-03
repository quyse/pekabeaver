{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Assets () where

import Language.Haskell.TH

import Flaw.App.PlainTexture
import Flaw.Asset
import Flaw.Asset.Collada
import Flaw.Asset.FolderAssetPack
import Flaw.Visual.Geometry

do
	let packBuilder = FolderAssetPackBuilder "assetpack/"
	let addAsset assetId asset = runIO $ putAsset packBuilder assetId asset

	addAsset "field.bin" =<< emitGeometryAsset "assets/field.DAE" (getElementById "geom-field")
	addAsset "beaver.bin" =<< emitGeometryAsset "assets/beaver.DAE" (getElementById "geom-Beaver")
	addAsset "peka.bin" =<< emitGeometryAsset "assets/peka.DAE" (getElementById "geom-peka")
	addAsset "castle.jpg" =<< emitPlainTextureAsset "assets/images/0_castle.jpg"
	addAsset "beaver.jpg" =<< emitPlainTextureAsset "assets/images/0_beaver.jpg"
	addAsset "peka.png" =<< emitPlainTextureAsset "assets/images/0_peka0.png"
	addAsset "beaver_icon.png" =<< emitPlainTextureAsset "assets/images/beaver_icon.png"
	addAsset "peka_icon.png" =<< emitPlainTextureAsset "assets/images/peka_icon.png"
	addAsset "beaver_win.jpg" =<< emitPlainTextureAsset "assets/images/beaver_win.jpg"
	addAsset "peka_win.jpg" =<< emitPlainTextureAsset "assets/images/peka_win.jpeg"
	addAsset "styles.css" =<< emitPlainTextureAsset "assets/styles.css"

	return []
