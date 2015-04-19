{-# LANGUAGE CPP, FlexibleContexts, OverloadedStrings #-}

import Control.Concurrent.MVar
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
import Flaw.Window

import Assets

data GameState = GameState
	{ gameStateCameraAlpha :: Float
	, gameStateCameraBeta :: Float
	, gameStateCameraDistance :: Float
	, gameStateLightAngle :: Float
	, gameStateActors :: [Actor]
	, gameStateFirstCursor :: Maybe ((Int, Int), (Int, Int))
	, gameStateUserActorType :: ActorType
	, gameStateUserGun :: GunState
	, gameStateComputerGun :: GunState
	} deriving Show

data GunState = GunState
	{ gunStateTime :: Float
	} deriving Show

data Actor = Actor
	{ actorType :: ActorType
	, actorStartPosition :: !Vec2f
	, actorFinishPosition :: !Vec2f
	, actorTime :: Float
	, actorTotalTime :: Float
	, actorState :: ActorState
	, actorAngle :: Float
	} deriving Show

data ActorType = Peka | Beaver deriving Show

actorFlySpeed :: Float
actorFlySpeed = 100

actorGroundSpeed :: Float
actorGroundSpeed = 50

gravity :: Float
gravity = 50

actorOffset :: Float
actorOffset = 5

gunCoolDown :: Float
gunCoolDown = 1

data ActorState = ActorFlying Float | ActorRunning deriving Show

calcActorPosition :: Actor -> Vec3f
calcActorPosition Actor
	{ actorStartPosition = Vec2 sx sy
	, actorFinishPosition = Vec2 fx fy
	, actorTime = t
	, actorTotalTime = tt
	, actorState = as
	} = case as of
	ActorFlying angle -> Vec3 (sx * (1 - k) + fx * k) (sy * (1 - k) + fy * k) z where
		k = t / tt
		z = actorOffset + actorFlySpeed * (sin angle) * t - gravity * t * t / 2
	ActorRunning -> Vec3 (sx * (1 - k) + fx * k) (sy * (1 - k) + fy * k) actorOffset where
		k = t / tt

spawnActor :: ActorType -> Vec2f -> Vec2f -> Maybe Actor
spawnActor at s f = maybeActor where
	sin2angle = (norm $ s - f) * gravity / (actorFlySpeed * actorFlySpeed)
	angle = 0.5 * (pi - asin sin2angle)
	maybeActor = if sin2angle >= 1 then Nothing else Just Actor
		{ actorType = at
		, actorStartPosition = s
		, actorFinishPosition = f
		, actorTime = 0
		, actorTotalTime = 2 * actorFlySpeed * (sin angle) / gravity
		, actorState = ActorFlying angle
		, actorAngle = 0
		}

castlePosition :: ActorType -> Vec2f
castlePosition at = case at of
	Peka -> Vec2 0 200
	Beaver -> Vec2 0 (-200)

castleLine :: ActorType -> Float
castleLine at = case at of
	Peka -> 150
	Beaver -> -150

enemyActor :: ActorType -> ActorType
enemyActor at = case at of
	Peka -> Beaver
	Beaver -> Peka

initialGameState :: GameState
initialGameState = GameState
	{ gameStateCameraAlpha = 0
	, gameStateCameraBeta = 0
	, gameStateCameraDistance = 200
	, gameStateLightAngle = 0
	, gameStateActors = []
	, gameStateFirstCursor = Nothing
	, gameStateUserActorType = Peka
	, gameStateUserGun = GunState
		{ gunStateTime = 0
		}
	, gameStateComputerGun = GunState
		{ gunStateTime = 0
		}
	}

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

affineActorLookAt :: Floating a => Vec3 a -> Vec3 a -> Vec3 a -> Mat4x4 a
affineActorLookAt position@(Vec3 px py pz) target direction = r where
	y@(Vec3 yx yy yz) = normalize $ target - position
	x@(Vec3 xx xy xz) = normalize $ cross y direction
	Vec3 zx zy zz = cross x y
	r = Mat4x4
		xx yx zx px
		xy yy zy py
		xz yz zz pz
		0 0 0 1

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
					diffuse <- temp $ min_ (constf 1) $ constf 0.5 + (abs $ dot toLight $ xyz__ worldNormal)
					diffuseColor <- temp $ sample (sampler2D3f 0) aTexcoord
					colorTarget 0 $ combineVec (diffuseColor * vecFromScalar diffuse, constf 1)

			-- main loop
			liftIO $ runGame initialGameState $ \frameTime state@GameState
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

						renderUniform usLight uLightPosition $ let
							angle = gameStateLightAngle state
							in Vec3 (30 * cos angle) (30 * sin angle) 30
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
						forM_ (gameStateActors state) $ \actor@Actor
							{ actorType = at
							, actorFinishPosition = Vec2 fx fy
							} -> do
							let (vb, ib, ic, t) = case at of
								Peka -> (vbPeka, ibPeka, icPeka, tPeka)
								Beaver -> (vbBeaver, ibBeaver, icBeaver, tBeaver)
							let position = calcActorPosition actor
							let world = affineActorLookAt position (Vec3 fx fy actorOffset) (Vec3 0 0 1)
							--let world = affineTranslation position
							renderUniform usObject uWorld world
							renderUploadUniformStorage usObject
							renderUniformStorage usObject
							renderVertexBuffer 0 vb
							renderIndexBuffer ib
							renderSampler 0 t samplerState
							renderDraw ic

						return (viewProj, viewportWidth, viewportHeight)

				-- process input
				inputFrame <- nextInputFrame inputManager
				let getMousePoint = do
					(cursorX, cursorY) <- getMouseCursor inputFrame
					let frontPoint = getFrontScreenPoint viewProj $ Vec3
						((fromIntegral cursorX) / (fromIntegral viewportWidth) * 2 - 1)
						(1 - (fromIntegral cursorY) / (fromIntegral viewportHeight) * 2)
						0
					return $ intersectRay cameraPosition (normalize (frontPoint - cameraPosition)) (Vec3 0 0 1) 0
				let process s = do
					maybeEvent <- nextInputEvent inputFrame
					case maybeEvent of
						Just event -> do
							--putStrLn $ show event
							process =<< case event of
								EventMouse (MouseDownEvent LeftMouseButton) -> do
									cursor <- getMouseCursor inputFrame
									return s
										{ gameStateFirstCursor = Just (cursor, cursor)
										}
								EventMouse (MouseUpEvent LeftMouseButton) -> do
									(cursorX, cursorY) <- getMouseCursor inputFrame
									case gameStateFirstCursor s of
										Just ((firstCursorX, firstCursorY), _) -> do
											ss <- do
												if (abs $ cursorX - firstCursorX) < 20 && (abs $ cursorY - firstCursorY) < 20 && gunStateTime (gameStateUserGun s) <= 0 then do
													(Vec3 fx fy _) <- getMousePoint
													let at = gameStateUserActorType s
													let startPosition = castlePosition at
													let maybeActor = spawnActor at startPosition (Vec2 fx fy)
													return $ case maybeActor of
														Just actor -> s
															{ gameStateActors = actor : gameStateActors s
															, gameStateUserGun = (gameStateUserGun s)
																{ gunStateTime = gunCoolDown
																}
															}
														Nothing -> s
												else return s
											return ss
												{ gameStateFirstCursor = Nothing
												}
										Nothing -> return s
								EventMouse (CursorMoveEvent cursorX cursorY) -> do
									case gameStateFirstCursor s of
										Just (firstCursor@(firstCursorX, firstCursorY), (moveCursorX, moveCursorY)) -> do
											if (abs $ cursorX - firstCursorX) >= 20 || (abs $ cursorY - firstCursorY) >= 20 then do
												return s
													{ gameStateCameraAlpha = gameStateCameraAlpha s - (fromIntegral $ cursorX - moveCursorX) * 0.005
													, gameStateCameraBeta = gameStateCameraBeta s + (fromIntegral $ cursorY - moveCursorY) * 0.01
													, gameStateFirstCursor = Just (firstCursor, (cursorX, cursorY))
													}
											else
												return s
													{ gameStateFirstCursor = Just (firstCursor, (cursorX, cursorY))
													}
										Nothing -> return s
								EventMouse (RawMouseMoveEvent _dx _dy dz) -> return s
									{ gameStateCameraDistance = max 100 $ min 500 $ dz * (-0.1) + gameStateCameraDistance s
									}
								_ -> return s
						Nothing -> return s
				newState <- process state

				-- process camera rotation
				(newCameraAlpha, newCameraBeta) <- do
					up <- getKeyState inputFrame KeyUp
					down <- getKeyState inputFrame KeyDown
					left <- getKeyState inputFrame KeyLeft
					right <- getKeyState inputFrame KeyRight
					return
						( gameStateCameraAlpha newState + ((if right then 1 else 0) - (if left then 1 else 0)) * frameTime
						, max 0.5 $ min 1.5 $ gameStateCameraBeta newState + ((if up then 1 else 0) - (if down then 1 else 0)) * frameTime
						)

				-- step actors
				newActors <- liftM concat $ forM (gameStateActors newState) $ \actor@Actor
					{ actorState = as
					, actorFinishPosition = f
					, actorType = at
					, actorTime = t
					, actorTotalTime = tt
					} -> do
					case as of
						ActorFlying _ -> do
							if t >= tt then do
								let finishPosition = Vec2 (x_ f) $ castleLine $ enemyActor at
								return [actor
									{ actorTime = 0
									, actorTotalTime = norm (finishPosition - f) / actorGroundSpeed
									, actorStartPosition = f
									, actorFinishPosition = finishPosition
									, actorState = ActorRunning
									}]
							else
								return [actor
									{ actorTime = t + frameTime
									}]
						ActorRunning -> do
							if t >= tt then do
								let finishPosition = castlePosition $ enemyActor at
								let len = norm $ finishPosition - f
								if len < 10 then return []
								else return [actor
									{ actorTime = 0
									, actorTotalTime = len / actorGroundSpeed
									, actorStartPosition = f
									, actorFinishPosition = finishPosition
									, actorState = ActorRunning
									}]
							else
								return [actor
									{ actorTime = t + frameTime
									}]

				-- process gun cooldowns
				let processGun gs = gs { gunStateTime = gunStateTime gs - frameTime }
				let newState2 = newState
					{ gameStateUserGun = processGun $ gameStateUserGun newState
					, gameStateComputerGun = processGun $ gameStateComputerGun newState
					}

				return newState2
					{ gameStateCameraAlpha = newCameraAlpha
					, gameStateCameraBeta = newCameraBeta
					, gameStateLightAngle = gameStateLightAngle newState2 + frameTime * 3
					, gameStateActors = newActors
					}
