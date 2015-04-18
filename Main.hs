{-# LANGUAGE CPP, FlexibleContexts, OverloadedStrings #-}

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Text.Show.Pretty(ppShow)

import Flaw.Game
import Flaw.Graphics
import Flaw.Graphics.Program
import Flaw.Graphics.Sampler
import Flaw.Math
import Flaw.Math.Geometry
import Flaw.Input

import Assets

data GameState = GameState
	{ gameStateCameraAlpha :: Float
	, gameStateCameraBeta :: Float
	, gameStateCameraDistance :: Float
	, gameStateLightAngle :: Float
	, gameStateActors :: [Actor]
	}

data Actor = Actor
	{ actorType :: ActorType
	, actorPosition :: !Vec3f
	, actorTarget :: !Vec3f
	, actorAngle :: Float
	}

data ActorType = Peka | Beaver

getFrontScreenPoint :: Fractional a => Mat4x4 a -> Vec3 a -> Vec3 a
getFrontScreenPoint (Mat4x4
	m11 m12 m13 m14
	m21 m22 m23 m24
	m31 m32 m33 m34
	m41 m42 m43 m44
	) (Vec3 sx sy sz) = Vec3 (dx / d) (dy / d) (dz / d) where
	a11 = m11 - sx * m41
	a12 = m12 - sx * m42
	a13 = m13 - sx * m43
	a14 = sx * m44 - m14
	a21 = m21 - sy * m41
	a22 = m22 - sy * m42
	a23 = m23 - sy * m43
	a24 = sy * m44 - m24
	a31 = m31 - sz * m41
	a32 = m32 - sz * m42
	a33 = m33 - sz * m43
	a34 = sz * m44 - m34
	d = a11 * (a22 * a33 - a23 * a32) - a12 * (a21 * a33 - a23 * a31) + a13 * (a21 * a32 - a22 * a31)
	dx = a14 * (a22 * a33 - a23 * a32) - a12 * (a24 * a33 - a23 * a34) + a13 * (a24 * a32 - a22 * a34)
	dy = a11 * (a24 * a33 - a23 * a34) - a14 * (a21 * a33 - a23 * a31) + a13 * (a21 * a34 - a24 * a31)
	dz = a11 * (a22 * a34 - a24 * a32) - a12 * (a21 * a34 - a24 * a31) + a14 * (a21 * a32 - a22 * a31)

intersectRay :: Fractional a => Vec3 a -> Vec3 a -> Vec3 a -> a -> Vec3 a
intersectRay a d n nq = a + d * vecFromScalar ((nq - dot a n) / (dot d n))

main :: IO ()
main = do
	let
		handler :: SomeException -> IO ()
		handler = putStrLn . show
	handle handler $ do
		runResourceT $ do

			-- init game
			(_, (window, device, context, presenter, inputManager)) <- initGame "PEKABEAVER" 1024 768 True

#if !defined(ghcjs_HOST_OS)
			windowLoopVar <- liftIO $ newEmptyMVar
			liftIO $ addWindowCallback window $ \event -> case event of
				DestroyWindowEvent -> putMVar windowLoopVar True
				_ -> return ()
#endif

			-- load field
			(vbField, ibField, icField) <- fieldGeometry device
			(_, tField) <- fieldTexture device

			-- load beaver
			(vbBeaver, ibBeaver, icBeaver) <- beaverGeometry device
			(_, tBeaver) <- beaverTexture device

			-- load peka
			(vbPeka, ibPeka, icPeka) <- pekaGeometry device
			(_, tPeka) <- pekaTexture device

			(_, samplerState) <- createSamplerState device defaultSamplerStateInfo

			let spawnActor state at position target = do
				--let position = case at of
				--	Peka -> Vec3 0 500 10
				--	Beaver -> Vec3 0 (-500) 10
				return state
					{ gameStateActors = Actor
						{ actorType = at
						, actorPosition = position
						, actorTarget = target
						, actorAngle = 0
						} : gameStateActors state
					}

			-- program
			ubsCamera <- liftIO $ uniformBufferSlot 0
			uViewProj <- liftIO $ uniform ubsCamera
			uCameraPosition <- liftIO $ uniform ubsCamera
			ubsLight <- liftIO $ uniformBufferSlot 1
			uLightPosition <- liftIO $ uniform ubsLight
			--ubsMaterial <- liftIO $ uniformBufferSlot 2
			--uDiffuseColor <- liftIO $ uniform ubsMaterial
			ubsObject <- liftIO $ uniformBufferSlot 3
			uWorld <- liftIO $ uniform ubsObject
			(_, usCamera) <- createUniformStorage device ubsCamera
			(_, usLight) <- createUniformStorage device ubsLight
			--(_, usMaterial) <- createUniformStorage device ubsMaterial
			(_, usObject) <- createUniformStorage device ubsObject
			(_, program) <- createProgram device $ do
				aPosition <- attribute 0 0 0 (AttributeVec3 AttributeFloat32)
				aNormal <- attribute 0 12 0 (AttributeVec3 AttributeFloat32)
				aTexcoord <- attribute 0 24 0 (AttributeVec2 AttributeFloat32)
				worldPosition <- temp $ mul uWorld $ combineVec (aPosition, constf 1)
				worldNormal <- temp $ mul uWorld $ combineVec (aNormal, constf 0)
				rasterize (mul uViewProj worldPosition) $ do
					let toLight = normalize $ (xyz__ worldPosition) - uLightPosition
					--diffuse <- temp $ max_ 0 $ dot toLight $ xyz__ worldNormal
					diffuse <- temp $ abs $ dot toLight $ xyz__ worldNormal
					diffuseColor <- temp $ sample (sampler2D3f 0) aTexcoord
					colorTarget 0 $ combineVec (diffuseColor * vecFromScalar diffuse, constf 1)

			-- main loop
			liftIO $ runGame GameState
				{ gameStateCameraAlpha = 0
				, gameStateCameraBeta = 0
				, gameStateCameraDistance = 200
				, gameStateLightAngle = 0
				, gameStateActors = []
				} $ \frameTime state@GameState
				{ gameStateCameraAlpha = cameraAlpha
				, gameStateCameraBeta = cameraBeta
				, gameStateCameraDistance = cameraDistance
				} -> do
				-- check exit
#if !defined(ghcjs_HOST_OS)
				loop <- tryTakeMVar windowLoopVar
				case loop of
					Just True -> exitGame
					_ -> return ()
#endif

				let cameraPosition = Vec3 (cameraDistance * (cos cameraAlpha * cos cameraBeta)) (cameraDistance * (sin cameraAlpha * cos cameraBeta)) (cameraDistance * sin cameraBeta)

				(viewProj, viewportWidth, viewportHeight) <- render context $ do
					present presenter $ do
						renderClearColor 0 (Vec4 0.5 0.5 0.5 1)
						renderClearDepth 1
						renderProgram program

						(viewportWidth, viewportHeight) <- renderGetViewport
						let aspect = (fromIntegral viewportWidth) / (fromIntegral viewportHeight)

						let view = affineLookAt cameraPosition (Vec3 0 0 0) (Vec3 0 0 1)
						let proj = projectionPerspectiveFov (pi / 4) aspect 0.1 (1000 :: Float)
						let viewProj = mul proj view
						renderUniform usCamera uViewProj viewProj
						renderUniform usCamera uCameraPosition cameraPosition
						renderUploadUniformStorage usCamera
						renderUniformStorage usCamera

						renderUniform usLight uLightPosition $ Vec3 30 30 100
						renderUploadUniformStorage usLight
						renderUniformStorage usLight

						--renderUniform usMaterial uDiffuseColor $ Vec3 1 0 0
						--renderUploadUniformStorage usMaterial
						--renderUniformStorage usMaterial

						-- render field
						renderUniform usObject uWorld $ affineTranslation ((Vec3 0 0 0) :: Vec3f)
						renderUploadUniformStorage usObject
						renderUniformStorage usObject
						renderVertexBuffer 0 vbField
						renderIndexBuffer ibField
						renderSampler 0 tField samplerState
						renderDraw icField

						-- render actors
						forM_ (gameStateActors state) $ \Actor
							{ actorType = at
							, actorPosition = position
							} -> do
							let (vb, ib, ic, t) = case at of
								Peka -> (vbPeka, ibPeka, icPeka, tPeka)
								Beaver -> (vbBeaver, ibBeaver, icBeaver, tBeaver)
							renderUniform usObject uWorld $ affineTranslation position
							renderUploadUniformStorage usObject
							renderUniformStorage usObject
							renderVertexBuffer 0 vb
							renderIndexBuffer ib
							renderSampler 0 t samplerState
							renderDraw ic

						-- render test beaver
						renderUniform usObject uWorld $ affineTranslation ((Vec3 0 0 1) :: Vec3f)
						renderUploadUniformStorage usObject
						renderUniformStorage usObject
						renderVertexBuffer 0 vbBeaver
						renderIndexBuffer ibBeaver
						renderSampler 0 tBeaver samplerState
						renderDraw icBeaver

						-- render test peka
						renderUniform usObject uWorld $ affineTranslation ((Vec3 0 0 1) :: Vec3f)
						renderUploadUniformStorage usObject
						renderUniformStorage usObject
						renderVertexBuffer 0 vbPeka
						renderIndexBuffer ibPeka
						renderSampler 0 tPeka samplerState
						renderDraw icPeka

						return (viewProj, viewportWidth, viewportHeight)

				-- process input
				inputFrame <- nextInputFrame inputManager
				let process s = do
					maybeEvent <- nextInputEvent inputFrame
					case maybeEvent of
						Just event -> do
							--putStrLn $ show event
							case event of
								EventMouse (MouseDownEvent LeftMouseButton) -> do
									(cursorX, cursorY) <- getMouseCursor inputFrame
									putStrLn $ ppShow $ Vec3
										((fromIntegral cursorX) / (fromIntegral viewportWidth) * 2 - 1)
										(1 - (fromIntegral cursorY) / (fromIntegral viewportHeight) * 2)
										0
									let frontPoint = getFrontScreenPoint viewProj $ Vec3
										((fromIntegral cursorX) / (fromIntegral viewportWidth) * 2 - 1)
										(1 - (fromIntegral cursorY) / (fromIntegral viewportHeight) * 2)
										0
									let position = intersectRay cameraPosition (normalize (frontPoint - cameraPosition)) (Vec3 0 0 1) 0
									putStrLn $ ppShow position
									process =<< (spawnActor s Peka position $ Vec3 0 0 0)
								EventMouse (RawMouseMoveEvent _dx _dy dz) -> process s
									{ gameStateCameraDistance = max 100 $ min 1000 $ dz * 0.1 + gameStateCameraDistance s
									}
								_ -> process s
						Nothing -> return s
				newState <- process state

				-- process camera rotation
				(newCameraAlpha, newCameraBeta) <- do
					up <- getKeyState inputFrame KeyUp
					down <- getKeyState inputFrame KeyDown
					left <- getKeyState inputFrame KeyLeft
					right <- getKeyState inputFrame KeyRight
					return
						( cameraAlpha + ((if right then 1 else 0) - (if left then 1 else 0)) * frameTime
						, max 0.5 $ min 1.5 $ cameraBeta + ((if up then 1 else 0) - (if down then 1 else 0)) * frameTime
						)

				-- step actors
				newActors <- forM (gameStateActors newState) $ \actor@Actor
					{ actorPosition = position
					, actorTarget = target
					} -> do
					let newPosition = position + (vecFromScalar 1) * normalize (target - position)
					return actor
						{ actorPosition = newPosition
						}

				return newState
					{ gameStateCameraAlpha = newCameraAlpha
					, gameStateCameraBeta = newCameraBeta
					, gameStateLightAngle = gameStateLightAngle newState + frameTime
					, gameStateActors = newActors
					}
