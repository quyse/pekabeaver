{-# LANGUAGE CPP, FlexibleContexts, JavaScriptFFI, OverloadedStrings #-}

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.Text as T
import System.Random
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

#if defined(ghcjs_HOST_OS)
import GHCJS.Types
import GHCJS.Foreign
#endif

data GameState = GameState
	{ gsPhase :: GamePhase
	, gsCameraAlpha :: Float
	, gsCameraBeta :: Float
	, gsCameraDistance :: Float
	, gsLightAngle :: Float
	, gsActors :: [Actor]
	, gsFirstCursor :: Maybe ((Int, Int), (Int, Int))
	, gsUserActorType :: ActorType
	, gsUserGun :: GunState
	, gsComputerGun :: GunState
	, gsDamages :: [Damage]
	, gsBeaverLives :: Int
	, gsPekaLives :: Int
	, gsUserSpawn :: Maybe Vec2f
	} deriving Show

data GamePhase = GameBattle | GameFinish deriving (Eq, Show)

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

data ActorType = Peka | Beaver deriving (Eq, Show)

actorFlySpeed :: Float
actorFlySpeed = 445--200

actorGroundSpeed :: Float
actorGroundSpeed = 50

actorAngleSpeed :: Float
actorAngleSpeed = actorGroundSpeed / actorOffset

gravity :: Float
gravity = 889--180

actorOffset :: Float
actorOffset = 5

gunCoolDown :: Float
gunCoolDown = 0.5

actorDeadTime :: Float
actorDeadTime = 5

actorExplodeTime :: Float
actorExplodeTime = 0.5

actorExplodeDistance :: Float
actorExplodeDistance = 5

actorWinningOffset :: Float
actorWinningOffset = 50

actorWinningScale :: Float
actorWinningScale = 5

actorWinningTime :: Float
actorWinningTime = 1

livesAmount :: Int
livesAmount = 50

moveClickThreshold :: Num a => a
moveClickThreshold = 50

data ActorState = ActorFlying Float | ActorRunning | ActorDead | ActorExplode | ActorWinning deriving (Eq, Show)

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
	ActorDead -> Vec3 sx sy actorOffset
	ActorExplode -> Vec3 (sx * (1 - k) + fx * k) (sy * (1 - k) + fy * k) actorOffset where
		k = t / tt
	ActorWinning -> Vec3 sx sy actorOffset

spawnActor :: ActorType -> Vec2f -> Vec2f -> Maybe Actor
spawnActor at s f = maybeActor where
	sin2angle = (norm $ s - f) * gravity / (actorFlySpeed * actorFlySpeed)
	angle = 0.5 * (pi - asin sin2angle)
	maybeActor = if sin2angle >= 1 || norm (castlePosition at - f) < 50 then Nothing else Just Actor
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

fieldWidth :: Float
fieldWidth = 100

enemyActor :: ActorType -> ActorType
enemyActor at = case at of
	Peka -> Beaver
	Beaver -> Peka

data Damage = Damage ActorType Vec2f deriving Show

initialGameState :: GameState
initialGameState = GameState
	{ gsPhase = GameBattle
	, gsCameraAlpha = 0
	, gsCameraBeta = 0.35
	, gsCameraDistance = 400
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
	, gsDamages = []
	, gsBeaverLives = livesAmount
	, gsPekaLives = livesAmount
	, gsUserSpawn = Nothing
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
					diffuse <- temp $ min_ (constf 1) $ constf 0.5 + (abs $ dot toLight $ normalize $ xyz__ worldNormal)
					diffuseColor <- temp $ sample (sampler2D3f 0) aTexcoord
					colorTarget 0 $ combineVec (diffuseColor * vecFromScalar diffuse, constf 1)

			let
				gameStep :: Float -> StateT GameState IO ()
				gameStep frameTime = do
				-- check exit
#if !defined(ghcjs_HOST_OS)
					loop <- liftIO $ tryTakeMVar windowLoopVar
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
								, actorState = as
								, actorFinishPosition = Vec2 fx fy
								, actorTime = t
								, actorTotalTime = tt
								, actorAngle = aa
								} -> do
								let (vb, ib, ic, tex) = case at of
									Peka -> (vbPeka, ibPeka, icPeka, tPeka)
									Beaver -> (vbBeaver, ibBeaver, icBeaver, tBeaver)
								let k = t / tt
								let position = calcActorPosition actor
								let translation = affineActorLookAt position (Vec3 fx fy actorOffset) (Vec3 0 0 1)
								let world = case as of
									ActorFlying _ -> mul translation $ affineFromQuaternion $ affineAxisRotation (Vec3 (-1) 0 0) $ k * pi * 2
									ActorRunning -> if at == Peka then mul translation $ affineFromQuaternion $ affineAxisRotation (Vec3 (-1) 0 0) aa else translation
									ActorDead -> mul translation $ mul (affineTranslation $ Vec3 0 0 $ 2 - actorOffset) $ affineScaling (Vec3 1.5 1.5 (0.1 :: Float))
									ActorExplode ->
										--mul translation $ mul (affineTranslation $ Vec3 0 0 $ k * 10) $ affineScaling $ vecFromScalar $ 1 + k * 0.5
										mul translation $ affineScaling $ Vec3 1 (1 * (1 - k) + 0.1 * k) 1
									ActorWinning -> mul translation $ mul (affineTranslation $ Vec3 0 0 $ k * actorWinningOffset) $ affineScaling $ vecFromScalar $ 1 - k + actorWinningScale * k
								renderUniform usObject uWorld world
								renderUploadUniformStorage usObject
								renderUniformStorage usObject
								renderVertexBuffer 0 vb
								renderIndexBuffer ib
								renderSampler 0 tex samplerState
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
												if (abs $ cursorX - firstCursorX) < moveClickThreshold && (abs $ cursorY - firstCursorY) < moveClickThreshold then do
													(Vec3 fx fy _) <- getMousePoint
													state $ \s -> ((), s
														{ gsUserSpawn = Just $ Vec2 fx fy
														})
												else return ()
												state $ \s -> ((), s
													{ gsFirstCursor = Nothing
													})
											Nothing -> return ()
									EventMouse (CursorMoveEvent cursorX cursorY) -> do
										s <- get
										case gsFirstCursor s of
											Just (firstCursor@(firstCursorX, firstCursorY), (moveCursorX, moveCursorY)) -> do
												if (abs $ cursorX - firstCursorX) >= moveClickThreshold || (abs $ cursorY - firstCursorY) >= moveClickThreshold then do
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
							, gsCameraBeta = max 0.1 $ min 1.5 $ gsCameraBeta s + ((if up then 1 else 0) - (if down then 1 else 0)) * frameTime
							})

					-- step actors
					let stepActor actor@Actor
						{ actorState = as
						, actorFinishPosition = f
						, actorType = at
						, actorTime = t
						, actorTotalTime = tt
						} = do
						case as of
							ActorFlying _ -> if t >= tt then do
								let finishPosition = Vec2 (x_ f) $ castleLine $ enemyActor at
								state $ \s -> ((), s
									{ gsDamages = (Damage at f) : gsDamages s
									})
								return [actor
									{ actorTime = 0
									, actorTotalTime = norm (finishPosition - f) / actorGroundSpeed
									, actorStartPosition = f
									, actorFinishPosition = finishPosition
									, actorState = ActorRunning
									, actorAngle = 0
									}]
								else return [actor
										{ actorTime = t + frameTime
										}]
							ActorRunning -> do
								if t >= tt then do
									let finishPosition = castlePosition $ enemyActor at
									let len = norm $ finishPosition - f
									if len < 10 then do
										state $ \s -> ((), case at of
											Beaver -> s { gsPekaLives = gsPekaLives s - 1 }
											Peka -> s { gsBeaverLives = gsBeaverLives s - 1 }
											)
										return [actor
											{ actorTime = 0
											, actorTotalTime = actorWinningTime
											, actorState = ActorWinning
											, actorStartPosition = finishPosition
											, actorFinishPosition = finishPosition
											}]
									else return [actor
										{ actorTime = 0
										, actorTotalTime = len / actorGroundSpeed
										, actorStartPosition = f
										, actorFinishPosition = finishPosition
										, actorState = ActorRunning
										, actorAngle = 0
										}]
								else return [actor
									{ actorTime = t + frameTime
									, actorAngle = actorAngle actor + actorAngleSpeed * frameTime
									}]
							ActorDead -> do
								if t >= tt then return []
								else return [actor
									{ actorTime = t + frameTime
									}]
							ActorExplode -> do
								if t >= tt then return []
								else return [actor
									{ actorTime = t + frameTime
									}]
							ActorWinning -> do
								if t >= tt then return []
								else return [actor
									{ actorTime = t + frameTime
									}]

					do
						s1 <- get
						newActors <- liftM concat $ mapM stepActor $ gsActors s1
						state $ \s -> ((), s
							{ gsActors = newActors
							})

					-- apply damages
					do
						s <- get
						let applyDamages (Damage eat ep) actors = map (\actor@Actor
							{ actorType = at
							, actorState = as
							} -> let
								Vec3 px py _pz = calcActorPosition actor
								in
									if at == eat || as == ActorDead || norm (ep - (Vec2 px py)) > 20 then actor
									else actor
										{ actorState = ActorDead
										, actorTime = 0
										, actorTotalTime = actorDeadTime
										, actorStartPosition = Vec2 px py
										}) actors
						put s
							{ gsActors = foldr applyDamages (gsActors s) $ gsDamages s
							, gsDamages = []
							}

					-- annigilate ground actors
					state $ \s -> let actors = gsActors s in ((), s
						{ gsActors = map (\actor -> let
							p@(Vec3 px py _pz) = calcActorPosition actor
							at = actorType actor
							keep = actorState actor /= ActorRunning || all (\actor2 -> let
								p2 = calcActorPosition actor2
								at2 = actorType actor2
								as2 = actorState actor2
								in at == at2 || as2 /= ActorRunning || norm (p - p2) > 10
								) actors
							in if keep then actor
								else let startPosition = Vec2 px py in actor
									{ actorState = ActorExplode
									, actorTime = 0
									, actorTotalTime = actorExplodeTime
									, actorStartPosition = startPosition
									, actorFinishPosition = startPosition + normalize (actorFinishPosition actor - startPosition) * vecFromScalar actorExplodeDistance
									}
							) actors
						})

					-- process user's gun
					do
						s1 <- get
						if gsPhase s1 == GameBattle && gunStateTime (gsUserGun s1) <= 0 then do
							case gsUserSpawn s1 of
								Just position -> do
									let at = gsUserActorType s1
									case spawnActor at (castlePosition at) position of
										Just actor -> state $ \s -> ((), s
											{ gsActors = actor : gsActors s
											, gsUserGun = (gsUserGun s)
												{ gunStateTime = gunCoolDown
												}
											})
										Nothing -> return ()
									state $ \s -> ((), s
										{ gsUserSpawn = Nothing
										})
								Nothing -> return ()
						else return ()

					-- process computer's gun
					do
						s <- get
						if gsPhase s == GameBattle && gunStateTime (gsComputerGun s) <= 0 then do
							let minx = -fieldWidth
							let maxx = fieldWidth
							let at = enemyActor $ gsUserActorType s
							let cl = castleLine at
							let miny = if cl > 0 then 0 else cl
							let maxy = if cl > 0 then cl else 0
							x <- liftIO $ getStdRandom $ randomR (minx, maxx)
							y <- liftIO $ getStdRandom $ randomR (miny, maxy)
							case spawnActor at (castlePosition at) (Vec2 x y) of
								Just actor -> put s
									{ gsActors = actor : gsActors s
									, gsComputerGun = (gsComputerGun s)
										{ gunStateTime = gunCoolDown
										}
									}
								Nothing -> return ()
						else return ()

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

#if defined(ghcjs_HOST_OS)

					-- update lives
					get >>= \s -> liftIO $ do
						if gsPhase s == GameBattle then do
							js_setStyleWidth (toJSString $ T.pack "beaver_lives") $ toJSString $ T.pack $ show $ (fromIntegral $ gsBeaverLives s) * (100 :: Float) / fromIntegral livesAmount
							js_setStyleWidth (toJSString $ T.pack "peka_lives") $ toJSString $ T.pack $ show $ (fromIntegral $ gsPekaLives s) * (100 :: Float) / fromIntegral livesAmount
						else return ()

					-- check end
					get >>= \s -> do
						if gsPhase s == GameBattle then do
							let beaverLives = gsBeaverLives s
							let pekaLives = gsPekaLives s
							if beaverLives <= 0 || pekaLives <= 0 then do
								put s { gsPhase = GameFinish }
								let beaverWon = beaverLives > 0
								let userWon = beaverWon == (gsUserActorType s == Beaver)
								liftIO $ js_end (toJSString $ T.pack $ if beaverWon then "beaver" else "peka") $ toJSString $ T.pack $ if userWon then "You won!" else "You lose!"
							else return ()
						else return ()

			-- register start functions
			startMVar <- liftIO $ newEmptyMVar
			liftIO $ do
				let start at alpha = do
					js_start
					putMVar startMVar initialGameState
						{ gsUserActorType = at
						, gsCameraAlpha = alpha
						}
				beaverStart <- syncCallback AlwaysRetain False $ start Beaver $ (-pi) / 2
				pekaStart <- syncCallback AlwaysRetain False $ start Peka $ pi / 2
				js_registerStart beaverStart pekaStart

			-- main loop
			gameState <- liftIO $ takeMVar startMVar
			liftIO $ runGame gameState $ \frameTime s -> execStateT (gameStep frameTime) s

foreign import javascript unsafe "document.getElementById($1).style.width=$2+'%'" js_setStyleWidth :: JSString -> JSString -> IO ()
foreign import javascript unsafe "document.getElementById('start-beaver').addEventListener('click', $1, false);document.getElementById('start-peka').addEventListener('click', $2, false);" js_registerStart :: JSFun (IO ()) -> JSFun (IO ()) -> IO ()
foreign import javascript unsafe "document.getElementById('start').style.display='none';" js_start :: IO ()
foreign import javascript unsafe "document.getElementById('end-'+$1).style.display='block'; document.getElementById('end-title').innerText=$2; document.getElementById('end').style.display='block';" js_end :: JSString -> JSString -> IO ()

#else

			-- main loop
			liftIO $ runGame initialGameState
				{ gsUserActorType = Peka
				, gsCameraAlpha = pi / 2
				} $ \frameTime s -> execStateT (gameStep frameTime) s

#endif
