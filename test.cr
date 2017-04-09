require "./a"
require "benchmark"

NUM    = 123.456
TIMES  = ARGV[0].to_i
BUFFER = Slice.new(128, 0_u8)

raise "not same" unless 123.456.to_s == 123.456.fast_to_s

Benchmark.ips do |x|
  x.report "stdlib" { TIMES.times { NUM.to_s } }
  x.report "grisu3" { TIMES.times { NUM.fast_to_s } }
  x.report "grisu3_reuse" { TIMES.times { NUM.fast_to_s(BUFFER) } }
end
