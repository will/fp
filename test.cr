require "./a"

v = ARGV[0].to_f
buff = Slice.new(128, 0_u8)
p grisu3(v, buff)
# slice = pointerof(buff).to_slice(128)
puts String.new(buff)
