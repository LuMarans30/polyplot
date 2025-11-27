{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Graphics.Rendering.OpenGL hiding (Matrix)
import Graphics.UI.GLUT hiding (Matrix)
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra.Data

type Vec3 = Vector Double

data Polyhedron = Polyhedron {aMatrix :: Matrix Double, bVector :: Vec3}

data ViewState = ViewState
  { rotationX :: IORef Double,
    rotationY :: IORef Double,
    zoom :: IORef Double,
    isRotating :: IORef Bool,
    lastMousePos :: IORef (GLint, GLint)
  }

isFeasible :: Polyhedron -> Vec3 -> Bool
isFeasible (Polyhedron a b) x = all (<= 1e-8) (toList (a LA.#> x - b))

vectorsNearEqual :: Double -> Vec3 -> Vec3 -> Bool
vectorsNearEqual eps v1 v2 = LA.norm_2 (v1 - v2) < eps

nubVectors :: Double -> [Vec3] -> [Vec3]
nubVectors eps = nubBy (vectorsNearEqual eps)

getVertices :: Polyhedron -> [Vec3]
getVertices poly@(Polyhedron a b) = nubVectors 1e-6 $ mapMaybe solveVertex combos
  where
    combos = filter ((== 3) . length) $ subsequences [0 .. rows a - 1]
    solveVertex idx = do
      let aSub = fromLists [toList (a ! i) | i <- idx]
          bSub = fromList [b ! i | i <- idx]
      sol <- LA.linearSolve aSub (LA.asColumn bSub)
      let solVec = flatten sol
      guard (isFeasible poly solVec) >> return solVec

normalizeVec :: Vec3 -> Vec3
normalizeVec v
  | n > 1e-10 = LA.scale (recip n) v
  | otherwise = v
  where
    n = LA.norm_2 v

orderFaceVertices :: [Vec3] -> [Vec3]
orderFaceVertices vs@(v0 : v1 : v2 : _) = sortOn angle vs
  where
    centroid = sum vs / fromIntegral (length vs)
    basisX = normalizeVec (v1 - v0)
    faceNormal = normalizeVec (LA.cross basisX (v2 - v0))
    basisY = LA.cross faceNormal basisX
    angle w = atan2 (basisY LA.<.> w') (basisX LA.<.> w')
      where
        w' = w - centroid
orderFaceVertices vs = vs

getFaces :: Polyhedron -> [[Vec3]]
getFaces poly@(Polyhedron a b) =
  [ orderFaceVertices vs
  | i <- [0 .. rows a - 1],
    let vs = filter (onPlane i) (getVertices poly),
    length vs >= 3
  ]
  where
    onPlane i v = abs ((a ! i) LA.<.> v - (b ! i)) < 1e-6

normalizePolyhedron :: Double -> Polyhedron -> Polyhedron
normalizePolyhedron factor poly = Polyhedron (LA.scale (recip s) (aMatrix poly)) (bVector poly)
  where
    s = case [LA.norm_2 v | v <- getVertices poly] of
      [] -> factor
      xs -> factor / maximum xs

toVertex3 :: Vec3 -> Vertex3 GLdouble
toVertex3 v = Vertex3 (v ! 0) (v ! 1) (v ! 2)

drawFace :: [Vec3] -> IO ()
drawFace vs = do
  depthMask $= Disabled
  color $ Color4 0.3 0.5 0.8 (0.5 :: GLdouble)
  renderPrimitive Polygon $ mapM_ (vertex . toVertex3) vs
  depthMask $= Enabled
  lineWidth $= 2
  color $ Color4 0 0 0 (1 :: GLdouble)
  renderPrimitive LineLoop $ mapM_ (vertex . toVertex3) vs

drawPolyhedron :: ViewState -> Polyhedron -> IO ()
drawPolyhedron state poly = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  [zoomLevel, rotX, rotY] <- mapM get [zoom state, rotationX state, rotationY state]
  translate $ Vector3 0 0 (-zoomLevel)
  rotate rotX $ Vector3 1 0 0
  rotate rotY $ Vector3 0 1 0
  mapM_ drawFace (getFaces poly)
  flush >> swapBuffers

initGL :: IO ()
initGL = do
  clearColor $= Color4 1 1 1 1
  depthFunc $= Just Less
  multisample $= Enabled
  lineSmooth $= Enabled
  hint LineSmooth $= Nicest
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  matrixMode $= Projection
  loadIdentity
  perspective 45 1 0.1 100
  matrixMode $= Modelview 0

fromPosition :: Position -> (GLint, GLint)
fromPosition (Position x y) = (x, y)

mouseHandler :: ViewState -> MouseButton -> KeyState -> Position -> IO ()
mouseHandler state LeftButton Down pos = do
  writeIORef (lastMousePos state) (fromPosition pos)
  writeIORef (isRotating state) True
mouseHandler state LeftButton Up _ = writeIORef (isRotating state) False
mouseHandler state WheelUp _ _ = modifyIORef (zoom state) (max 1.5 . subtract 0.5) >> postRedisplay Nothing
mouseHandler state WheelDown _ _ = modifyIORef (zoom state) (+ 0.5) >> postRedisplay Nothing
mouseHandler _ _ _ _ = return ()

motionHandler :: ViewState -> Position -> IO ()
motionHandler state pos = do
  rotating <- get (isRotating state)
  when rotating $ do
    (lastX, lastY) <- get (lastMousePos state)
    let (x, y) = fromPosition pos
        dx = fromIntegral (x - lastX)
        dy = fromIntegral (y - lastY)
    modifyIORef (rotationY state) (+ dx * 0.5)
    modifyIORef (rotationX state) (+ dy * 0.5)
    writeIORef (lastMousePos state) (x, y)
    postRedisplay Nothing

debugPolyhedron :: Polyhedron -> IO ()
debugPolyhedron poly = putStrLn $ "Vertices: " ++ show (length (getVertices poly)) ++ "\nFaces: " ++ show (length (getFaces poly))

readPolyhedron :: FilePath -> IO Polyhedron
readPolyhedron path = do
  contents <- readFile path
  let parseLine line = case words line of
        [a1, a2, a3, b] -> Just ([read a1, read a2, read a3], read b)
        _ -> Nothing
      planes = mapMaybe parseLine (lines contents)
  return $ Polyhedron (fromLists [xs | (xs, _) <- planes]) (fromList [b | (_, b) <- planes])

main :: IO ()
main = do
  (_progName, args) <- getArgsAndInitialize
  initialWindowSize $= Size 800 800
  initialDisplayMode $= [DoubleBuffered, RGBAMode, WithDepthBuffer, WithSamplesPerPixel 4]
  _window <- createWindow "3D Polyhedron Plotter - Mouse: drag to rotate, scroll to zoom in/out"
  initGL
  polyhedron <- case args of
    [filename] -> readPolyhedron filename
    _ -> error "Usage: program <filename> (file format: each line: a1 a2 a3 b)"
  let normalizedPolyhedron = normalizePolyhedron 1.75 polyhedron
  debugPolyhedron normalizedPolyhedron
  state <- ViewState <$> newIORef 0 <*> newIORef 0 <*> newIORef 5 <*> newIORef False <*> newIORef (0, 0)
  displayCallback $= drawPolyhedron state normalizedPolyhedron
  mouseCallback $= Just (mouseHandler state)
  motionCallback $= Just (motionHandler state)
  mainLoop