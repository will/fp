require "./a"
require "benchmark"

NUM   = 123.456
TIMES = (ARGV[0]? || 1_500_000).to_i

pp 123.456.to_s
pp 123.456.fast_to_s
raise "not same" unless 123.456.to_s == 123.456.fast_to_s

Benchmark.ips do |x|
  x.report "stdlib" { TIMES.times { NUM.to_s } }
  x.report "grisu3" { TIMES.times { NUM.fast_to_s } }
end
# stdlib   1.04  (957.66ms) (± 3.40%)  1.72× slower
# grisu3   1.79  (557.27ms) (± 3.65%)       fastest # => with new slice each time
# grisu3   2.54  (393.32ms) (± 4.81%)       fastest # => with staticarray
