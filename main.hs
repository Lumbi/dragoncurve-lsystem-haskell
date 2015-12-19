import Graphics.Rendering.OpenGL as Open
import Graphics.UI.GLUT as GLUT
import Data.IORef

-- module LSystem where

type Axiom a = [a]
type Rule a = (a -> [a])
type State a = [a]
type LSystem a = (Axiom a, Rule a)

at :: LSystem a -> Integer -> State a
at (axiom, _) 0 = axiom
at system t | t > 0 = curr
    where
        (_, rule) = system
        prev = at system (t-1)
        curr = concat $ fmap rule prev
at _ _ = undefined

-- Main

main :: IO ()
main = do
    (_, _) <- GLUT.getArgsAndInitialize
    _ <- GLUT.createWindow "LSystem"
    GLUT.windowSize $= GLUT.Size 800 800
    GLUT.displayCallback $= display
    GLUT.mainLoop

-- Rendering

display :: GLUT.DisplayCallback
display = do
    clear [ColorBuffer]
    loadIdentity
    scale 0.05 0.05 (0.05::GLfloat)
    color $ Color3 0 1 (0::GLfloat)

    drawLSystem

    flush

addVertex :: GLfloat -> GLfloat -> GLfloat -> IO ()
addVertex x y z =
    vertex $ Vertex3 x y z

drawLine :: (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> IO ()
drawLine (sx,sy) (ex, ey) =
    renderPrimitive Lines $ do
        addVertex sx sy 0
        addVertex ex ey 0

drawRectangle :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
drawRectangle x y w h =
    renderPrimitive Quads $ do
        addVertex x y 0
        addVertex (x+w) y 0
        addVertex (x+w) (y+h) 0
        addVertex x (y+h) 0

drawSquare :: GLfloat -> GLfloat -> GLfloat -> IO ()
drawSquare x y s = drawRectangle x y s s

drawUnitSquare :: GLfloat -> GLfloat -> IO ()
drawUnitSquare x y = drawSquare x y 1

-- LSystem of Graphics Operations

data Direction = Up | Down | Left | Right
data PenState = PenState (GLfloat, GLfloat) Direction
data Symbol = A | B | F | P | M deriving Eq

data DrawOperation = DrawOperation (IORef PenState) (IO ())

forward :: (GLfloat, GLfloat) -> Direction -> (GLfloat, GLfloat)
forward (x, y) Main.Up = (x , y+1)
forward (x, y) Main.Down = (x , y-1)
forward (x, y) Main.Left = (x-1 , y)
forward (x, y) Main.Right = (x+1 , y)

left :: Direction -> Direction
left Main.Up = Main.Left
left Main.Left = Main.Down
left Main.Down = Main.Right
left Main.Right = Main.Up

right :: Direction -> Direction
right Main.Up = Main.Right
right Main.Right = Main.Down
right Main.Down = Main.Left
right Main.Left = Main.Up

drawForward :: IORef PenState -> IO ()
drawForward penStateRef = do
    PenState (x,y) d <- readIORef penStateRef
    let next = forward (x,y) d
    drawLine (x,y) next
    writeIORef penStateRef $ PenState next d
    return ()

turnLeft :: IORef PenState -> IO ()
turnLeft penStateRef = do
    PenState (x,y) d <- readIORef penStateRef
    writeIORef penStateRef $ PenState (x,y) (left d)
    return ()

turnRight :: IORef PenState -> IO ()
turnRight penStateRef = do
    PenState (x,y) d <- readIORef penStateRef
    writeIORef penStateRef $ PenState (x,y) (right d)
    return ()

doNothing :: IO ()
doNothing = return ()

operationF :: IORef PenState -> DrawState
operationF penStateRef = (F, DrawOperation penStateRef io)
    where io = drawForward penStateRef

operationP :: IORef PenState -> DrawState
operationP penStateRef = (P, DrawOperation penStateRef io)
    where io = turnRight penStateRef

operationM :: IORef PenState -> DrawState
operationM penStateRef = (M, DrawOperation penStateRef io)
    where io = turnLeft penStateRef

operationA :: IORef PenState -> DrawState
operationA penStateRef = (A, DrawOperation penStateRef doNothing)

operationB :: IORef PenState -> DrawState
operationB penStateRef = (B, DrawOperation penStateRef doNothing)

type DrawState = (Symbol, DrawOperation)

rule :: Rule DrawState
rule (A, DrawOperation pen _) = [
    operationA pen,
    operationP pen,
    operationB pen,
    operationF pen,
    operationP pen
    ]

rule (B, DrawOperation pen _) = [
    operationM pen,
    operationF pen,
    operationA pen,
    operationM pen,
    operationB pen
    ]

rule (sym, drawOp) = [(sym, drawOp)]

getIO :: DrawState -> IO ()
getIO (_, drawOp) = let DrawOperation _ io =  drawOp in io

initialState :: IORef PenState -> [DrawState]
initialState penStateRef = [ operationF penStateRef, operationA penStateRef ]

drawLSystem :: IO ()
drawLSystem = do
    penStateRef <- newIORef $ PenState (0,0) Main.Up
    let systemAt = at (initialState penStateRef, rule) 10
    let drawOperations = map getIO systemAt
    sequence_ drawOperations
    return ()
