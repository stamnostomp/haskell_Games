import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Data representing the game state
data Game = Game
  { ballLoc :: (Float, Float)       -- (x, y) location of the ball
  , ballVel :: (Float, Float)       -- (vx, vy) velocity of the ball
  , player1 :: Float                -- Left player's paddle position
  , player2 :: Float                -- Right player's paddle position
  , score1  :: Int                  -- Left player's score
  , score2  :: Int                  -- Right player's score
  , keys    :: (Bool, Bool, Bool, Bool) -- (wPressed, sPressed, iPressed, kPressed)
  , gameMode :: GameMode            -- Current game mode
  , screen  :: ScreenState          -- Current screen state
  } deriving Show

-- Game mode data type
data GameMode = TwoPlayers | PlayerVsComputer deriving (Eq, Show)

-- Screen state data type
data ScreenState = Menu | Playing deriving (Eq, Show)

-- Window size
windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600

-- Ball size
ballRadius :: Float
ballRadius = 10

-- Paddle size
paddleWidth, paddleHeight :: Float
paddleWidth = 20
paddleHeight = 80

-- Paddle corner radius
cornerRadius :: Float
cornerRadius = 10

-- Speed of paddle movement
paddleSpeed :: Float
paddleSpeed = 200

-- AI speed
aiSpeed :: Float
aiSpeed = 150

-- Font size for the score
scoreFontSize :: Int
scoreFontSize = 36

-- Initial game state
initialState :: Game
initialState = Game
  { ballLoc = (0, 0)
  , ballVel = (0, 0)  -- Ball is stationary at the start
  , player1 = 0
  , player2 = 0
  , score1 = 0
  , score2 = 0
  , keys = (False, False, False, False) -- No keys pressed initially
  , gameMode = TwoPlayers              -- Default game mode
  , screen = Menu                      -- Start with the menu screen
  }

-- Rendering function
render :: Game -> Picture
render game =
  case screen game of
    Menu -> renderMenu
    Playing -> pictures [ ball, walls, mkPaddle rose (-fromIntegral windowWidth / 2 + paddleWidth / 2) (player1 game)
                         , mkPaddle blue (fromIntegral windowWidth / 2 - paddleWidth / 2) (player2 game)
                         , renderScore
                         ]
  where
    -- Ball rendering (only if playing)
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid ballRadius
    ballColor = dark red

    -- Wall rendering
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
      color wallColor $
      rectangleSolid (fromIntegral windowWidth) 10

    walls = pictures [wall (fromIntegral windowHeight / 2), wall (-fromIntegral windowHeight / 2)]
    wallColor = greyN 0.5

    -- Paddle rendering with rounded corners
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ -- Main rectangle with rounded corners
        translate x y $ color col $ pictures
          [ rectangleSolid (paddleWidth - 2 * cornerRadius) (paddleHeight - 2 * cornerRadius)
          , translate (cornerRadius - paddleWidth / 2) (cornerRadius - paddleHeight / 2) $ circleSolid cornerRadius
          , translate (cornerRadius - paddleWidth / 2) (-cornerRadius + paddleHeight / 2) $ circleSolid cornerRadius
          , translate (-cornerRadius + paddleWidth / 2) (cornerRadius - paddleHeight / 2) $ circleSolid cornerRadius
          , translate (-cornerRadius + paddleWidth / 2) (-cornerRadius + paddleHeight / 2) $ circleSolid cornerRadius
          ]
      ]

    -- Score rendering
    renderScore :: Picture
    renderScore = pictures
      [ translate (-fromIntegral windowWidth / 4) (fromIntegral windowHeight / 2 - 50) $
          scale 0.2 0.2 $ color white $ text (show (score1 game))
      , translate (fromIntegral windowWidth / 4) (fromIntegral windowHeight / 2 - 50) $
          scale 0.2 0.2 $ color white $ text (show (score2 game))
      ]

    -- Menu rendering
    renderMenu :: Picture
    renderMenu = pictures
      [ translate (-200) 100 $ scale 0.3 0.3 $ color white $ text "Pong Game"
      , translate (-200) 0 $ scale 0.2 0.2 $ color white $ text "Press 1 for Player vs Computer"
      , translate (-200) (-50) $ scale 0.2 0.2 $ color white $ text "Press 2 for Two Players"
      ]
-- Move ball and handle collisions
moveBall :: Float -> Game -> Game
moveBall seconds game
  | x' < -windowWidthFloat / 2 = game { ballLoc = (0, 0), ballVel = (200, 200), score2 = score2 game + 1 } -- Player 2 scores
  | x' > windowWidthFloat / 2 = game { ballLoc = (0, 0), ballVel = (-200, 200), score1 = score1 game + 1 } -- Player 1 scores
  | otherwise = game { ballLoc = (x', y'), ballVel = (vx', vy') }
  where
    -- Old locations and velocities
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- Convert window dimensions to Float
    windowWidthFloat = fromIntegral windowWidth
    windowHalfHeightFloat = fromIntegral windowHeight / 2

    -- New locations
    x' = x + vx * seconds
    y' = y + vy * seconds

    -- Bounce off top and bottom walls
    vy' = if (y' + ballRadius > windowHalfHeightFloat) || (y' - ballRadius < -windowHalfHeightFloat)
          then -vy
          else vy

    -- Bounce off paddles
    vx' = if ballPaddleCollision (player1 game) (-windowHalfHeightFloat)
          then -vx
          else if ballPaddleCollision (player2 game) (windowHalfHeightFloat)
               then -vx
               else vx

    -- Check for ball-paddle collision
    ballPaddleCollision :: Float -> Float -> Bool
    ballPaddleCollision paddleY paddleX =
      let paddleTop = paddleY + paddleHeight / 2
          paddleBottom = paddleY - paddleHeight / 2
          paddleLeft = paddleX - paddleWidth / 2
          paddleRight = paddleX + paddleWidth / 2
      in (x' - ballRadius < paddleRight) &&
         (x' + ballRadius > paddleLeft) &&
         (y' - ballRadius < paddleTop) &&
         (y' + ballRadius > paddleBottom)

-- Move paddles based on key presses and AI
movePaddles :: Float -> Game -> Game
movePaddles seconds game =
  case gameMode game of
    PlayerVsComputer -> moveAiPaddle seconds (movePlayer1 seconds game)
    TwoPlayers -> movePlayer1 seconds (movePlayer2 seconds game)
  where
    -- Move player 1 paddle
    movePlayer1 :: Float -> Game -> Game
    movePlayer1 secs g = g { player1 = clamp (-windowHalfHeight) windowHalfHeight (player1 g + paddleSpeed * secs * move1) }
    move1 = if wPressed game then 1 else if sPressed game then -1 else 0

    -- Move player 2 paddle (AI or second player)
    movePlayer2 :: Float -> Game -> Game
    movePlayer2 secs g = g { player2 = clamp (-windowHalfHeight) windowHalfHeight (player2 g + paddleSpeed * secs * move2) }
    move2 = if iPressed game then 1 else if kPressed game then -1 else 0

    -- AI paddle movement
    moveAiPaddle :: Float -> Game -> Game
    moveAiPaddle secs g = g { player2 = clamp (-windowHalfHeight) windowHalfHeight (player2 g + aiSpeed * secs * move) }
    move = if ballY > player2 game + paddleHeight / 2 then 1 else if ballY < player2 game - paddleHeight / 2 then -1 else 0
    ballY = snd (ballLoc game)

    -- Helper function to clamp values
    clamp minVal maxVal x
      | x < minVal = minVal
      | x > maxVal = maxVal
      | otherwise = x

    -- Convert window dimensions to Float
    windowHalfHeight = fromIntegral windowHeight / 2

-- Update the game state
update :: Float -> Game -> Game
update seconds game =
  case screen game of
    Menu -> game
    Playing -> moveBall seconds $ movePaddles seconds game

-- Input handler for player movement and game mode selection
handleKeys :: Event -> Game -> Game
handleKeys (EventKey (Char '1') Down _ _) game =
  game { gameMode = PlayerVsComputer, screen = Playing, ballVel = (200, 200) }
handleKeys (EventKey (Char '2') Down _ _) game =
  game { gameMode = TwoPlayers, screen = Playing, ballVel = (200, 200) }
handleKeys (EventKey (Char 'w') Down _ _) game = updateKeys game True (sPressed game) (iPressed game) (kPressed game)
handleKeys (EventKey (Char 's') Down _ _) game = updateKeys game (wPressed game) True (iPressed game) (kPressed game)
handleKeys (EventKey (Char 'i') Down _ _) game = updateKeys game (wPressed game) (sPressed game) True (kPressed game)
handleKeys (EventKey (Char 'k') Down _ _) game = updateKeys game (wPressed game) (sPressed game) (iPressed game) True
handleKeys (EventKey (Char 'w') Up _ _) game = updateKeys game False (sPressed game) (iPressed game) (kPressed game)
handleKeys (EventKey (Char 's') Up _ _) game = updateKeys game (wPressed game) False (iPressed game) (kPressed game)
handleKeys (EventKey (Char 'i') Up _ _) game = updateKeys game (wPressed game) (sPressed game) False (kPressed game)
handleKeys (EventKey (Char 'k') Up _ _) game = updateKeys game (wPressed game) (sPressed game) (iPressed game) False
handleKeys _ game = game

-- Update key states in the game
updateKeys :: Game -> Bool -> Bool -> Bool -> Bool -> Game
updateKeys game w s i k = game { keys = (w, s, i, k) }

-- Helper functions to get current key states
wPressed, sPressed, iPressed, kPressed :: Game -> Bool
wPressed game = fst4 (keys game)
sPressed game = snd4 (keys game)
iPressed game = thd4 (keys game)
kPressed game = fth4 (keys game)

-- Helper functions to extract elements from a tuple
fst4 (a, _, _, _) = a
snd4 (_, b, _, _) = b
thd4 (_, _, c, _) = c
fth4 (_, _, _, d) = d

-- Main function
main :: IO ()
main = play window background fps initialState render handleKeys update
  where
    window = InWindow "Pong" (windowWidth, windowHeight) (100, 100)
    background = black
    fps = 60
