# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
	# The constant All_My_Pieces should be declared here
	All_My_Pieces = All_Pieces + [[[[0, 0], [-1, 0], [1, 0], [-2, 0], [2, 0]], # long (only needs two)
               					        [[0, 0], [0, -1], [0, 1], [0, -2], [0, 2]]],
							rotations([[0, 0], [-1, 0], [0, -1], [1, -1], [-1, -1]]),
							rotations([[0, 0], [0, 1], [1, 0]])]
	MyCheater = [[[0, 0]]]
	# your enhancements here
	def initalize
		super()
	end
	def self.next_piece(board)
		Piece.new(All_My_Pieces.sample, board)
	end
	def self.cheat(board)
		Piece.new(MyCheater, board)
	end
end

class MyBoard < Board
  # your enhancements here
	def initalize(game)
		super(game)
		@cheat = false
	end
	def store_current
		locations = @current_block.current_rotation
		displacement = @current_block.position
		(0..(locations.size-1)).each{|index|
			current = locations[index];
			@grid[current[1]+displacement[1]][current[0]+displacement[0]] =
			@current_pos[index]
		}
		remove_filled
		@delay = [@delay - 2, 80].max
	end
	def rotate180
		rotate_clockwise
		rotate_clockwise
	end
	def cheat
		if @score >= 100 && !@cheat then
			@score = @score - 100
			@cheat = true
		end
	end
  	def next_piece
		if @cheat then
			@cheat = false
			@current_block = MyPiece.cheat(self)
		else
			@current_block = MyPiece.next_piece(self)
		end
		@current_pos = nil
  	end
end

class MyTetris < Tetris
	# your enhancements here
	def initalize
		super()
	end
	def set_board
		@canvas = TetrisCanvas.new
		@board = MyBoard.new(self)
		@canvas.place(@board.block_size * @board.num_rows + 3,
			@board.block_size * @board.num_columns + 6, 24, 80)
		@board.draw
	end
	def key_bindings
		super()
		@root.bind('u', proc {@board.rotate180})
		@root.bind('c', proc {@board.cheat})
	end
end


