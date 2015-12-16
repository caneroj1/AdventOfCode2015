f = File.open("/Users/jcanero/Code/Repositories/haskell/advent_calendar/December8_Input/input.txt")
first = 0
second = 0
third = 0
f.each_line do |line|
  puts "#{line.length - 1}, #{line}"
  puts "#{eval(line.chop).length}, #{eval(line.chop)}"
  puts line.chop.inspect.length
  puts "---"

  first += line.length - 1
  second += eval(line.chop).length
  third += line.chop.inspect.length
end

puts third - first
