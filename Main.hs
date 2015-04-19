{-# LANGUAGE CPP, FlexibleContexts, OverloadedStrings #-}

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.State
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
	{ gsCameraAlpha :: Float
	, gsCameraBeta :: Float
	, gsCameraDistance :: Float
	, gsLightAngle :: Float
	, gsActors :: [Actor]
	, gsFirstCursor :: Maybe ((Int, Int), (Int, Int))
	, gsUserActorType :: ActorType
	, gsUserGun :: GunState
	, gsComputerGun :: GunState
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
	{ gsCameraAlpha = 0
	, gsCameraBeta = 0
	, gsCameraDistance = 200
	, gsLightAngle = 0
	, gsActors = []
	, gsFirstCursor = Nothing
	, gsUserActorType = Peka
	, gsUserGun = GunState
		{ gunStateTime = 0
		}
	, gsComputerGun = GunState
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

			let
				gameStep :: Float -> StateT GameState IO ()
				gameStep frameTime = do
				-- check exit
#if !defined(ghcjs_HOST_OS)
					loop <- tryTakeMVar windowLoopVar
					case loop of
						Just True -> liftIO $ exitGame
						_ -> return ()
#endif

					cameraPosition <- do
						s <- get
						let alpha = gsCameraAlpha s
						let beta = gsCameraBeta s
						let distance = gsCameraDistance s
						return $ Vec3 (distance * (cos alpha * cos beta)) (distance * (sin alpha * cos beta)) (distance * sin beta)

					rs <- get
					(viewProj, viewportWidth, viewportHeight) <- liftIO $ render context $ do
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
								angle = gsLightAngle rs
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
							forM_ (gsActors rs) $ \actor@Actor
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
					inputFrame <- liftIO $ nextInputFrame inputManager

					let process = do
						let getMousePoint = do
							(cursorX, cursorY) <- liftIO $ getMouseCursor inputFrame
							let frontPoint = getFrontScreenPoint viewProj $ Vec3
								((fromIntegral cursorX) / (fromIntegral viewportWidth) * 2 - 1)
								(1 - (fromIntegral cursorY) / (fromIntegral viewportHeight) * 2)
								0
							return $ intersectRay cameraPosition (normalize (frontPoint - cameraPosition)) (Vec3 0 0 1) 0
						maybeEvent <- liftIO $ nextInputEvent inputFrame
						case maybeEvent of
							Just event -> do
								--putStrLn $ show event
								case event of
									EventMouse (MouseDownEvent LeftMouseButton) -> do
										cursor <- liftIO $ getMouseCursor inputFrame
										state $ \s -> ((), s
											{ gsFirstCursor = Just (cursor, cursor)
											})
									EventMouse (MouseUpEvent LeftMouseButton) -> do
										(cursorX, cursorY) <- liftIO $ getMouseCursor inputFrame
										s1 <- get
										case gsFirstCursor s1 of
											Just ((firstCursorX, firstCursorY), _) -> do
												if (abs $ cursorX - firstCursorX) < 20 && (abs $ cursorY - firstCursorY) < 20 && gunStateTime (gsUserGun s1) <= 0 then do
													(Vec3 fx fy _) <- getMousePoint
													let at = gsUserActorType s1
													let startPosition = castlePosition at
													let maybeActor = spawnActor at startPosition (Vec2 fx fy)
													case maybeActor of
														Just actor -> do
															liftIO $ putStrLn (show actor)
															state $ \s -> ((), s
																{ gsActors = actor : gsActors s
																, gsUserGun = (gsUserGun s)
																	{ gunStateTime = gunCoolDown
																	}
																})
														Nothing -> return ()
												else return ()
												state $ \s -> ((), s
													{ gsFirstCursor = Nothing
													})
											Nothing -> return ()
									EventMouse (CursorMoveEvent cursorX cursorY) -> do
										s <- get
										case gsFirstCursor s of
											Just (firstCursor@(firstCursorX, firstCursorY), (moveCursorX, moveCursorY)) -> do
												if (abs $ cursorX - firstCursorX) >= 20 || (abs $ cursorY - firstCursorY) >= 20 then do
													put $ s
														{ gsCameraAlpha = gsCameraAlpha s - (fromIntegral $ cursorX - moveCursorX) * 0.005
														, gsCameraBeta = gsCameraBeta s + (fromIntegral $ cursorY - moveCursorY) * 0.01
														, gsFirstCursor = Just (firstCursor, (cursorX, cursorY))
														}
												else
													put $ s
														{ gsFirstCursor = Just (firstCursor, (cursorX, cursorY))
														}
											Nothing -> return ()
									EventMouse (RawMouseMoveEvent _dx _dy dz) -> state $ \s -> ((), s
										{ gsCameraDistance = max 100 $ min 500 $ dz * (-0.1) + gsCameraDistance s
										})
									_ -> return ()
								process
							Nothing -> return ()
					process

					-- process camera rotation
					do
						up <- liftIO $ getKeyState inputFrame KeyUp
						down <- liftIO $ getKeyState inputFrame KeyDown
						left <- liftIO $ getKeyState inputFrame KeyLeft
						right <- liftIO $ getKeyState inputFrame KeyRight
						state $ \s -> ((), s
							{ gsCameraAlpha = gsCameraAlpha s + ((if right then 1 else 0) - (if left then 1 else 0)) * frameTime
							, gsCameraBeta = max 0.5 $ min 1.5 $ gsCameraBeta s + ((if up then 1 else 0) - (if down then 1 else 0)) * frameTime
							})

					-- step actors
					let stepActor actor@Actor
						{ actorState = as
						, actorFinishPosition = f
						, actorType = at
						, actorTime = t
						, actorTotalTime = tt
						} = case as of
						ActorFlying _ -> if t >= tt then let
							finishPosition = Vec2 (x_ f) $ castleLine $ enemyActor at
							in [actor
								{ actorTime = 0
								, actorTotalTime = norm (finishPosition - f) / actorGroundSpeed
								, actorStartPosition = f
								, actorFinishPosition = finishPosition
								, actorState = ActorRunning
								}]
							else [actor
									{ actorTime = t + frameTime
									}]
						ActorRunning -> if t >= tt then
							let
								finishPosition = castlePosition $ enemyActor at
								len = norm $ finishPosition - f
							in
								if len < 10 then []
								else [actor
									{ actorTime = 0
									, actorTotalTime = len / actorGroundSpeed
									, actorStartPosition = f
									, actorFinishPosition = finishPosition
									, actorState = ActorRunning
									}]
							else [actor
								{ actorTime = t + frameTime
								}]

					state $ \s -> ((), s
						{ gsActors = concat $ map stepActor $ gsActors s
						})

					get >>= (liftIO . putStrLn . show)

					-- process gun cooldowns
					let processGun gs = gs { gunStateTime = gunStateTime gs - frameTime }
					state $ \s -> ((), s
						{ gsUserGun = processGun $ gsUserGun s
						, gsComputerGun = processGun $ gsComputerGun s
						})

					-- step light
					state $ \s -> ((), s
						{ gsLightAngle = gsLightAngle s + frameTime * 3
						})

			-- main loop
			liftIO $ runGame initialGameState $ \frameTime s -> execStateT (gameStep frameTime) s
