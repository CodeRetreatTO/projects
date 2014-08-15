names = ["Alex", "Beatrice", "Clarence", "Diego", "Ernest", "Francesca", "Gwen", "Heidi", "Ingrid"]

test = [["Heidi", "Alex"], ["Diego", "Beatrice"], ["Alex", "Clarence"], ["Ernest", "Diego"], ["Gwen", "Ernest"], ["Ingrid", "Francesca"], ["Clarence", "Gwen"], ["Francesca", "Heidi"], ["Beatrice", "Ingrid"]]

def santas(name_list)
  bag = name_list.clone
  name_list.map do |name|
    possible_santas = bag.find_all {|s| s != name}
    santa = possible_santas.shuffle.pop
    bag.delete santa
    [santa, name]
  end
end

def any_reciprocating?(pair_list)
  pair_list.each do |a, b|
    if pair_list.select { |pair| pair == [b, a]}
      return true
    end
  end
  false
end

def non_reciprocating_santas(name_list)
  pairs = santas(name_list)
  while any_reciprocating? pairs
    pairs = santas(name_list)
  end 
  pairs
end


## Additional Constraints:

# Not for each other
# Not for spouses
# Not for same person as last year
