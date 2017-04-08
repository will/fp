require "./a"

buff = Array(UInt8).new(128)
p grisu3(1.23, buff)
p buff
