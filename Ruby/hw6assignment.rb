# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = All_Pieces + [
                rotations([[0, 0], [1, 0], [-1, 0], [-1, -1], [0, -1]]), # P
                [[[0, 0], [1, 0], [2, 0], [-1, 0], [-2, 0]], # long long |
                 [[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]],
                rotations([[0, 0], [0, 1], [1, 0]])] # small >
  Cheat_Piece = [[[0,0]]]

  # your enhancements here
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece (board)
    MyPiece.new(Cheat_Piece, board)
  end
end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    super(game)
    next_piece
  end

  def next_piece
    if @cheat
      @current_block = MyPiece.cheat_piece(self)
      @score -= 100
    else
      @current_block = MyPiece.next_piece(self)
    end
    @cheat = false
    @current_pos = nil
  end

  def cheat_mode
    @cheat = true if !@cheat && @score >= 100
  end

  def store_current
    displacement = @current_block.position
    @current_block.current_rotation.each_with_index { |rotation,index|
       @grid[rotation[1]+displacement[1]][rotation[0]+displacement[0]] =
          @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  # your enhancements here
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_clockwise;@board.rotate_clockwise})
    @root.bind('c', proc {@board.cheat_mode})
  end
end


