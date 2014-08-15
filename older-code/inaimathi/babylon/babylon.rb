$board = [:green, :red, :white, :black].flat_map { |c| [[c]] * 3}
$player= 1

def merge (a, b)
    a + b
end

def valid? (a, b)
  (a.length == b.length or a[0] == b[0])
end

def on_board? (board, a, b)
  board.member?(a) && board.member?(b)
end

def any_moves? (board)
  len = board.length
  !((board.map{|elem| elem.length }.uniq.length == len) && (board.map{|elem| elem[0]}.uniq.length == len))
end

def remove_stack(board, stack)
  board.delete_at(board.index(stack) || board.length)
end

def make_move (upper, lower)
  if valid?(upper, lower) && on_board?($board, upper, lower)
    remove_stack($board, upper)
    remove_stack($board, lower)
    $board.push (upper + lower)
    $player = ($player % 2) + 1
  else
    puts
    puts "\033[01;31m"
    puts "NO  >:("
    puts "\033[00m"
  end
  put_board
end

def put_board ()
  puts
  puts "It's Player #{$player}s turn."
  print $board
  puts
end

if [ valid?([:green], [:green]),
     valid?([:green], [:red]),
     valid?([:green], [:green, :red]),
     !valid?([:green], [:red, :green])
   ].all?
  puts "passed valid?"
else 
  puts "FAIL   valid?"
end 
if [ any_moves?([[:red, :green], [:red]]),
     !any_moves?([[:red, :green], [:green]]),
     any_moves?([[:red, :green], [:green, :red]]),
     !any_moves?([[:red, :green], [:green, :red, :white]]),
     any_moves?($board)
   ].all?
  puts "passed any_moves?"
else
  puts "FAIL   any_moves?"
end

put_board
puts "(Use make_move to play)"
