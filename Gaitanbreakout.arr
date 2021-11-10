import lists as L
include image
include shared-gdrive("image115.arr", "1eSV5o5JQqKA8bBpA3ijknR7KOV6SUYiw")
include reactors

### COMP 115: How to Design Programs
### Fernando Gaitan, Spring 2019
### Project: Arcade Game (Breakout)
   
#|
  
   ==============================
   == ------- BREAKOUT ------- ==
   ==============================
   
   Implement a Breakout-style game:
   
     https://www.youtube.com/watch?v=Up-a5x3coC0
   
   Breakout was originally released in 1976, and was an early "collaboration"
   between Apple co-founders Steve Wozniak and Steve Jobs).  
  
   Minimum Viable Product (70%): 
   * There should be multiple rows of blocks, and a ball.
   * The left and right keys should move the paddle.  
   * When the ball bounces off a block, that block disappears.
   * The ball also bounces off the top, left, and right of the screen.
   * A score counter shows how many blocks have been cleared.
   * When the ball falls off the bottom of the screen, pressing a key
     should start a new ball.
   
   Additional features (30%):
   * Give the player some control over the angle/speed at which the ball
     bounces, by making the place at which the ball hits a paddle
     or block change the way the ball bounces.  
   * Give more points in the score when blocks in higher rows are cleared.
   * Keep track of how many new balls the player has used (how many misses).  
   * Add the ability to have multiple paddles at once, like in 1979
     SuperBreakout:
   
       https://www.youtube.com/watch?v=QIs3UOTdsJM
   
     The number of rows, and the placement of the blocks and balls
     should be configurable by changing names in the program.
   
   OPTIONAL BONUS TASKS:
   * Make the game have a few different levels with different
     initial arrangements of blocks; clearing one level takes
     you to a new one.
   * Make different blocks have different physics (e.g. slower or faster
     or different angle bounces).

|#

###################################################################

# some list functions that might be helpful 

fun contains<A>(f :: (A -> Boolean), l :: List<A>) -> Boolean:
  doc: "check if there is some element of l for which f returns true"
  L.any(f, l)
where:
  fun positive(n): n > 0 end
  contains(positive, [list: 1, 2, 3, -2, 4, -5]) is true
end

fun append<A>(l1 :: List<A>, l2 :: List<A>) -> List<A>:
  doc: "merge l1 and l2 into one list, with l1 before l2"
  L.append(l1, l2)
where:
  append([list:4,6,8], [list:1,2,3]) is [list: 4,6,8,1,2,3]
end

fun length<A>(l :: List<A>) -> Number:
  doc: "get the length of the list l"
  L.length(l)
end

# Don't forget: map is built in to Pyret.

fun keep<A>(f :: (A -> Boolean), l :: List<A>) -> List<A>:
  doc: "keep all and only the elements of l for which f returns true"
  L.filter(f, l)
where:
  fun positive(n): n > 0 end
  keep(positive, [list: 1, 2, 3, -2, 4, -5]) is [list: 1, 2, 3, 4]
end

fun take<A>(l :: List<A>, n :: Number) -> List<A>:
  doc: "get a list containing the first n elements of l"
  l.take(n)
where:
  take([list: 3, 7, 6, 2], 3) is [list: 3, 7, 6]
end

fun sort<A>(before :: (A, A -> Boolean), l :: List<A>) -> List<A>:
  doc: "sort the list l, where x goes before y when before(x,y) is true"
  L.sort-by(l,
    lam(x,y): before(x, y) and not(before(y, x)) end,
    lam(x,y): before(x, y) and before(y, x) end)
end

###################################################################


data Block:
  | block(x :: Number, y :: Number, color :: String)
end

data World:
  | world(
      paddle-x :: Number,
      ball-x :: Number, 
      ball-y :: Number,
      ball-velocity-x :: Number,
      ball-velocity-y :: Number,
      list-of-blocks :: List<Block>,
      score :: Number,
      lives :: Number,
      two-paddles :: Boolean)
end



scale1 :: Number = 1


number-of-blocks :: Number = 15
border :: Number = 6
row-1 = block(0,1,"red")
row-2 = block(0,2,"blue")
row-3 = block(0,3,"green")
row-4 = block(0,4,"white")
row-5 = block(0,5,"orange")


block-height :: Number = scale1 * 20
block-width :: Number = scale1 * 40
paddle-move :: Number = scale1 * 10
screen-height :: Number = (number-of-blocks * block-height)
screen-width :: Number = (number-of-blocks * block-width)
ball-radius :: Number = block-width / 6
paddle-width :: Number = block-width * 1.5
paddle-height :: Number = block-height / 2
BG :: Image = rectangle(screen-width ,screen-height, "solid", "black")
BORDER :: Image = rectangle(screen-width, (number-of-blocks + border) * block-height, "solid", "gray")
ball :: Image = circle(ball-radius, "solid", "green")
paddle :: Image = rectangle(paddle-width, paddle-height, "solid", "white")
paddle-y :: Number = (number-of-blocks - 1) * block-height


fun list-blocks(b :: Block) -> List<Block>:
  doc: "creates a list of 12 blocks that are the same color"
  cases (Block) b:
    |block(x,y,color) => 
      [list: 
        block(x, y, color), block(x + 1, y, color), block(x + 2, y, color),
        block(x + 3, y, color), block(x + 4, y, color), block(x + 5, y, color),
        block(x + 6, y, color), block(x + 7, y, color), block(x + 8, y, color),
        block(x + 9, y, color), block(x + 10, y, color), block(x + 11, y, color),
        block(x + 12, y, color), block(x + 13, y, color), block(x + 14, y, color)]
  end
where: 
  list-blocks(block(0,0, "red")) is [list:block(0,0, "red"), block(1,0, "red"), block(2,0, "red"), block(3,0, "red"), block(4,0, "red"), block(5,0, "red"), block(6,0, "red"), block(7,0, "red"), block(8,0, "red"), block(9,0, "red"), block(10,0, "red"), block(11,0, "red"), block(12,0, "red"), block(13,0, "red"), block(14,0, "red")]
end




all-rows :: List<Block> = 
  append<Block>(list-blocks(row-1),
    append<Block>(list-blocks(row-2),
      append<Block>(list-blocks(row-3),
        append<Block>(list-blocks(row-4),list-blocks(row-5)))))


fun blocks-show(l :: List<Block>, bg :: Image) -> Image:
  doc:"puts a list of blocks on a background"
  cases (List<Block>) l:
    |empty => BG
    |link(first, rest) => 
      cases (Block) first:
        |block(a,b,c) => 
          overlay-at(
            rectangle(block-width, block-height, "outline", c),
            a * block-width, 
            b * block-height, 
            blocks-show(rest, BG))
      end
  end
end



fun show-score-lives(n :: Number, l :: Number) -> Image:
  doc:"given a score, puts it on a background"
  overlay-at(
    text(num-to-string(n), scale1 * 48, "black"),
    block-width * (number-of-blocks - (border / 2)),
    block-height - border,
    overlay-at(text(num-to-string(l), scale1 * 48, "red"), 
      block-width * (border / 2),
      block-height - border, BORDER))
end
    


fun show(w :: World) -> Image:
  doc: "display the breakout game"
  cases (World) w:
    |world(paddle-x,
        ball-x, 
        ball-y,
        ball-velocity-x,
        ball-velocity-y,
        list-of-blocks,
        score, lives, two-paddles) =>   
      if list-of-blocks == [list: ]:
        overlay(text("YOU WIN!", 64, "sky-blue"), overlay(overlay-at(ball, 
                ball-x - ball-radius, 
                ball-y - ball-radius, 
                overlay-at(paddle, 
                  paddle-x - (paddle-width / 2), 
                  paddle-y - (paddle-height / 2), 
                  blocks-show(list-of-blocks, BG))), show-score-lives(score, lives)))
      else if (lives == 0) and (list-of-blocks == not([list: ])):
        overlay(text("YOU LOSE", 64, "crimson"), overlay(overlay-at(ball, 
                ball-x - ball-radius, 
                ball-y - ball-radius, 
                overlay-at(paddle, 
                  paddle-x - (paddle-width / 2), 
                  paddle-y - (paddle-height / 2), 
                  blocks-show(list-of-blocks, BG))), show-score-lives(score, lives)))
      else:
        if two-paddles:
          overlay(overlay-at(ball, 
              ball-x - ball-radius, 
              ball-y - ball-radius, 
              overlay-at(paddle, 
              paddle-x - (paddle-width / 2), 
              paddle-y - (paddle-height / 2) - (block-height * 3), 
              overlay-at(paddle, 
                paddle-x - (paddle-width / 2), 
                paddle-y - (paddle-height / 2), 
                blocks-show(list-of-blocks, BG)))), show-score-lives(score, lives))
        else: 
           overlay(overlay-at(ball, 
              ball-x - ball-radius, 
              ball-y - ball-radius, 
              overlay-at(paddle, 
                paddle-x - (paddle-width / 2), 
                paddle-y - (paddle-height / 2), 
                blocks-show(list-of-blocks, BG))), show-score-lives(score, lives))
        end
      
         
        end
      end
        
  end



Initial-world :: World = world(
  screen-width / 2,
  screen-width / 2, 
  paddle-y - paddle-height,
  0, 0, all-rows, 0, 3, false)



fun paddle-key(w :: World, key :: String) -> World:
  doc: "moves the paddle, starts the ball, and restarts the world when lives are gone"
  cases (World) w:
    |world(paddle-x, 
        ball-x, 
        ball-y,
        ball-velocity-x,
        ball-velocity-y,
        list-of-blocks, 
        score, lives, two-paddles) =>
      if (lives == 0) and (key == " "):
          Initial-world
      else if (key == " ") and (lives == 3):
        world(screen-width / 2,
  screen-width / 2, 
  paddle-y - paddle-height,
            3, 3, list-of-blocks, score, lives, two-paddles)
        else if (key == "shift"):
           world(paddle-x, 
          ball-x, 
          ball-y,
          ball-velocity-x,
          ball-velocity-y,
          list-of-blocks,
            score, lives, not(two-paddles))
        else if (key == "left")
          and (((paddle-x - (paddle-width / 2)) - paddle-move) >= 0):
        world(paddle-x - paddle-move, 
          ball-x, 
          ball-y,
          ball-velocity-x,
          ball-velocity-y,
          list-of-blocks,
            score, lives, two-paddles)
        else if (key == "right")
        and (((paddle-x + (paddle-width / 2)) + paddle-move) <= screen-width):
        world(paddle-x + paddle-move,
            ball-x, 
            ball-y,
            ball-velocity-x,
            ball-velocity-y,
            list-of-blocks,
            score, lives, two-paddles)
      else:
        w
      end
  end
where:
  paddle-key(Initial-world, " ") is world(300,300, 270, 3, 3, all-rows, 0, 3, false)
  paddle-key(Initial-world, "shift") is world(300,300, 270, 0, 0, all-rows, 0, 3, true)
end





fun block-hit(l :: List<Block>, x :: Number, y :: Number) -> Boolean:
  doc:"determines if the ball is about to hit a block"
      cases (List<Block>) l:
        |empty => false
        |link(first, rest) => 
          cases (Block) first:
            |block(a,b,c) =>
          ((a <= x) and ((a + 1) >= x)
            and ((b <= y) and ((b + 1) >= y)))
                  or (block-hit(rest, x, y))
  end
  end
where: 
  block-hit(all-rows, 9,4) is true
  block-hit(all-rows, 0,4) is true
  block-hit(all-rows, 17,0) is false
  block-hit(all-rows, 0,7) is false
end

fun remove-block(l :: List<Block>, x :: Number, y :: Number) -> List<Block>:
  doc: "removes the block given coordinates"
  cases (List<Block>) l:
    |empty => [list: ]
    |link(first, rest) => 
      cases (Block) first:
        |block(a,b,c) =>
          if (a <= x) and ((a + 1) >= x)
            and (b <= y) and ((b + 1) >= y):
            remove-block(rest, x, y)
          else:
            link(first,remove-block(rest, x, y))
          end
      end
  end
where:
  remove-block(list-blocks(row-1), 2, 4) is list-blocks(row-1)
  remove-block(list-blocks(row-1), 5, 1) is [list:block(0,1,"red"), block(1,1,"red"), block(2,1,"red"), block(3,1,"red"), block(6,1,"red"), block(7,1,"red"), block(8,1,"red"), block(9,1,"red"), block(10,1,"red"), block(11,1,"red"), block(12,1,"red"), block(13,1,"red"), block(14,1,"red")]
end


fun which-block(l :: List<Block>, x :: Number, y :: Number) -> Number:
  doc:"produces the row number of the block that just deleted that was just deleted"
  cases (List<Block>) l:
    |empty => 0
    |link(first, rest) => 
      cases (Block) first:
        |block(a,b,c) =>
          if (a <= x) and ((a + 1) >= x)
            and (b <= y) and ((b + 1) >= y):
            b
          else:
            which-block(rest, x, y)
          end
      end
  end
where:
  which-block(all-rows,2,3) is 2
  which-block(all-rows,9,4) is 3
end
    


fun update-score(w :: World) -> Number:
  doc:"updates the score given a world"
  cases (World) w:
    |world(paddle-x,
        ball-x, 
        ball-y,
        ball-velocity-x,
        ball-velocity-y,
        list-of-blocks, 
        score, lives, two-paddles) =>
      if block-hit(list-of-blocks, 
          (ball-x + ball-velocity-x) / block-width, 
          (ball-y + ball-velocity-y) / block-height):
        if which-block(list-of-blocks, 
          (ball-x + ball-velocity-x) / block-width, 
            (ball-y + ball-velocity-y) / block-height) == 1:
          score + 5
        else if which-block(list-of-blocks, 
          (ball-x + ball-velocity-x) / block-width, 
            (ball-y + ball-velocity-y) / block-height) == 2:
          score + 4
        else if which-block(list-of-blocks, 
          (ball-x + ball-velocity-x) / block-width, 
            (ball-y + ball-velocity-y) / block-height) == 3:
          score + 3
        else if which-block(list-of-blocks, 
          (ball-x + ball-velocity-x) / block-width, 
            (ball-y + ball-velocity-y) / block-height) == 4:
          score + 2
        else:        
        score + 1
        end
      else:
        score
      end
  end
where: 
  update-score(Initial-world) is 0
  update-score(world(290, 300, 270, 0,0, append<Block>(list-blocks(row-1),
        append<Block>(list-blocks(row-2), append<Block>(list-blocks(row-3), 
            list-blocks(row-4)))), 3, 0, false)) is 3
end





fun update-lives(w :: World) -> Number:
  doc:"updates the lives given a world"
    cases (World) w:
    |world(paddle-x,
        ball-x, 
        ball-y,
        ball-velocity-x,
        ball-velocity-y,
        list-of-blocks, 
        score, lives, two-paddles) =>
      if ((ball-y + ball-velocity-y) >= screen-height) and (lives <= 3):
          lives - 1
      else:
        lives
      end
  end
where:
  update-lives(Initial-world) is 3
  update-lives(world(290, 300, 300, 0, 4, all-rows, 0, 3, false)) is 2
end


fun between(
    first-position :: Number, 
    middle-position :: Number,
    second-position :: Number) -> Boolean:
  doc: ```return true if the middle position is 
          between the two outer positions```
  (first-position <= middle-position) and
  (middle-position <= second-position)
where:
  between(0, 1, 5) is true
  between(1, 5, 3) is false
end
  
  

fun bounce-tick(w :: World) -> World:
  doc: "make the ball bounce and delete things when its hit off of things"
  cases (World) w:
    |world(paddle-x, 
        ball-x, 
        ball-y,
        ball-velocity-x,
        ball-velocity-y,
        list-of-blocks, 
        score, lives, two-paddles) =>
      if ((ball-velocity-y) >= 0)
        and between(paddle-x, ball-x + (ball-radius * 2) + ball-velocity-x, 
          paddle-x + (paddle-width / 3))
        and between(paddle-y, ball-y + (ball-radius * 2) + ball-velocity-y, 
            paddle-y + paddle-height):
        #bounces off the left corner paddle
        world(paddle-x, 
            ball-x + ball-velocity-x, 
            ball-y + ball-velocity-y,
          ball-velocity-x * 1.25,
          ball-velocity-y * -1,
            list-of-blocks,
            score, lives, two-paddles)
      else if ((ball-velocity-y) >= 0)
        and between(paddle-x + (paddle-width / 3), 
          ball-x + (ball-radius * 2) + ball-velocity-x, 
          paddle-x + ((paddle-width * 2) / 3))
        and between(paddle-y, ball-y + (ball-radius * 2) + ball-velocity-y, 
            paddle-y + paddle-height):
        #bounces off the center paddle
          world(paddle-x, 
            ball-x + ball-velocity-x, 
            ball-y + ball-velocity-y,
          ball-velocity-x * 0.75,
          ball-velocity-y * -1.25,
            list-of-blocks,
            score, lives, two-paddles)
      else if ((ball-velocity-y) >= 0)
        and between(paddle-x + ((paddle-width * 2) / 3), 
          ball-x + (ball-radius * 2) + ball-velocity-x, 
          paddle-x + paddle-width)
        and between(paddle-y, 
          ball-y + (ball-radius * 2) + ball-velocity-y, 
          paddle-y + paddle-height):
        #bounces off the right corner paddle
          world(paddle-x, 
            ball-x + ball-velocity-x, 
            ball-y + ball-velocity-y,
          ball-velocity-x * -1.25,
          ball-velocity-y * -1,
            list-of-blocks,
            score, lives, two-paddles)
      else if two-paddles:
        if ((ball-velocity-y) >= 0)
          and between(paddle-x, 
            ball-x + (ball-radius * 2) + ball-velocity-x, 
            paddle-x + (paddle-width / 3))
          and between(paddle-y - (block-height * 3), 
            ball-y + (ball-radius * 2) + ball-velocity-y, 
            paddle-y + (paddle-height - (block-height * 3))):
          #bounces off the top corner paddle
        world(paddle-x, 
            ball-x + ball-velocity-x, 
            ball-y + ball-velocity-y,
            ball-velocity-x * 1.25,
            ball-velocity-y * -0.75,
            list-of-blocks,
            score, lives, two-paddles)
        else if ((ball-velocity-y) >= 0)
          and between(paddle-x + (paddle-width / 3), 
          ball-x + (ball-radius * 2) + ball-velocity-x, 
          paddle-x + ((paddle-width * 2) / 3))
          and between(paddle-y - (block-height * 3), 
            ball-y + (ball-radius * 2) + ball-velocity-y, 
            paddle-y + (paddle-height - (block-height * 3))):
          #bounces off the top center paddle
          world(paddle-x, 
            ball-x + ball-velocity-x, 
            ball-y + ball-velocity-y,
          ball-velocity-x,
          ball-velocity-y * -1,
            list-of-blocks,
            score, lives, two-paddles)
        else if ((ball-velocity-y) >= 0)
          and between(paddle-x + ((paddle-width * 2) / 3), 
          ball-x + (ball-radius * 2) + ball-velocity-x, 
          paddle-x + paddle-width)
          and between(paddle-y - (block-height * 3),
            ball-y + (ball-radius * 2) + ball-velocity-y, 
            paddle-y + (paddle-height - (block-height * 3))):
          #bounces off the top other corner paddle
          world(paddle-x, 
            ball-x + ball-velocity-x, 
            ball-y + ball-velocity-y,
          ball-velocity-x * 1.25,
            ball-velocity-y * -0.75,
            list-of-blocks,
            score, lives, two-paddles) 
        else if (((ball-x + (ball-radius * 2)) + ball-velocity-x) >= screen-width)
        or  ((ball-x + ball-velocity-x) <= 0):
        #bounces off the sides
        world(paddle-x, 
          ball-x + ball-velocity-x, 
          ball-y + ball-velocity-y,
          ball-velocity-x * -1,
          ball-velocity-y,
          list-of-blocks,
          score, lives, two-paddles)
      else if (((ball-y - ball-radius) + ball-velocity-y) <= 0):
        #bounces off the top
        world(paddle-x, 
          ball-x + ball-velocity-x, 
          ball-y + ball-velocity-y,
          ball-velocity-x,
          ball-velocity-y * -1,
          list-of-blocks,
          score, lives, two-paddles)
      else if ((ball-y + ball-velocity-y) >= screen-height):
        #resets when it's at the bottom
        world(
          screen-width / 2,
        screen-width / 2, 
        paddle-y - paddle-height,
          0, 0, list-of-blocks, score, update-lives(w), two-paddles)
        else if block-hit(list-of-blocks, 
          (ball-x + ball-velocity-x) / block-width, 
          (ball-y + ball-velocity-y) / block-height):
        world(paddle-x, 
          ball-x + ball-velocity-x, 
          ball-y + ball-velocity-y,
          ball-velocity-x,
          ball-velocity-y * -1,
          remove-block(list-of-blocks, 
            (ball-x + ball-velocity-x) / block-width, 
            (ball-y + ball-velocity-y) / block-height),
          update-score(w), lives, two-paddles)
      else:
        world(paddle-x,
          ball-x + ball-velocity-x,
          ball-y + ball-velocity-y,
          ball-velocity-x,
          ball-velocity-y,
          list-of-blocks,
          score, lives, two-paddles)
        end
      else if (((ball-x + (ball-radius * 2)) + ball-velocity-x) >= screen-width)
        or  ((ball-x + ball-velocity-x) <= 0):
        #bounces off the sides
        world(paddle-x, 
          ball-x + ball-velocity-x, 
          ball-y + ball-velocity-y,
          ball-velocity-x * -1,
          ball-velocity-y,
          list-of-blocks,
          score, lives, two-paddles)
      else if (((ball-y - ball-radius) + ball-velocity-y) <= 0):
        #bounces off the top
        world(paddle-x, 
          ball-x + ball-velocity-x, 
          ball-y + ball-velocity-y,
          ball-velocity-x,
          ball-velocity-y * -1,
          list-of-blocks,
          score, lives, two-paddles)
      else if ((ball-y + ball-velocity-y) >= screen-height):
        #resets when it's at the bottom
        world(
          screen-width / 2,
        screen-width / 2, 
        paddle-y - paddle-height,
          0, 0, list-of-blocks, score, update-lives(w), two-paddles)
        else if block-hit(list-of-blocks, 
          (ball-x + ball-velocity-x) / block-width, 
          (ball-y + ball-velocity-y) / block-height):
        world(paddle-x, 
          ball-x + ball-velocity-x, 
          ball-y + ball-velocity-y,
          ball-velocity-x,
          ball-velocity-y * -1,
          remove-block(list-of-blocks, 
            (ball-x + ball-velocity-x) / block-width, 
            (ball-y + ball-velocity-y) / block-height),
          update-score(w), lives, two-paddles)
      else:
        world(paddle-x,
          ball-x + ball-velocity-x,
          ball-y + ball-velocity-y,
          ball-velocity-x,
          ball-velocity-y,
          list-of-blocks,
          score, lives, two-paddles)
      end
  end
where:
  bounce-tick(Initial-world) is Initial-world
  bounce-tick(world(290, 300, 300, 0, 4, all-rows, 0, 3, false)) is 
  world(300, 300, 270, 0, 0, all-rows, 0, 2, false)
  bounce-tick(world(300, 100, 100, 3,3, all-rows, 0, 3, false)) is 
  world(300, 103, 103, 3, -3, [list: block(0,1,"red"), block(1,1,"red"), 
      block(2,1,"red"), block(3,1,"red"),block(4,1,"red"), block(5,1,"red"),
      block(6,1,"red"), block(7,1,"red"), block(8,1,"red"), block(9,1,"red"),
      block(10,1,"red"), block(11,1,"red"), block(12,1,"red"), 
      block(13,1,"red"), block(14,1,"red"), block(0,2,"blue"), 
      block(1,2,"blue"), block(2,2,"blue"), block(3,2,"blue"),
      block(4,2,"blue"), block(5,2,"blue"), block(6,2,"blue"),
      block(7,2,"blue"), block(8,2,"blue"), block(9,2,"blue"), 
      block(10,2,"blue"), block(11,2,"blue"), block(12,2,"blue"), 
      block(13,2,"blue"), block(14,2,"blue"), block(0,3,"green"), 
      block(1,3,"green"), block(2,3,"green"), block(3,3,"green"), 
      block(4,3,"green"), block(5, 3,"green"), block(6,3,"green"), 
      block(7,3,"green"), block(8,3,"green"), block(9,3,"green"), 
      block(10,3,"green"), block(11,3,"green"), block(12,3,"green"), 
      block(13,3,"green"), block(14,3,"green"), block(0,4,"white"), 
      block(1,4,"white"), block(2,4,"white"), block(3,4,"white"),
      block(4,4,"white"), block(5,4,"white"), block(6,4,"white"), 
      block(7,4,"white"), block(8,4,"white"), block(9,4,"white"), 
      block(10,4,"white"), block(11,4,"white"), block(12,4,"white"), 
      block(13,4,"white"), block(14,4,"white"), block(0,5,"orange"), 
      block(1,5,"orange"), block(3,5,"orange"),block(4,5,"orange"), 
      block(5, 5,"orange"), block(6,5,"orange"), block(7,5,"orange"), 
      block(8,5,"orange"), block(9,5,"orange"), block(10,5,"orange"), 
      block(11,5,"orange"), block(12,5,"orange"), block(13,5,"orange"), block(14,5,"orange")], 1, 3, false)
    
end


interact(reactor:
    init: Initial-world,
    to-draw: show,
    on-tick: bounce-tick,
    on-key: paddle-key
  end)