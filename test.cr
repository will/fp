require "./src/float_printer"
require "benchmark"

TIMES = (ARGV[0]? || 1_500_000).to_i

def bench(num)
  puts "stdlib: #{num.to_s}"
  puts "grisu3: #{num.fast_to_s}"
  Benchmark.ips do |x|
    x.report "stdlib" { TIMES.times { num.to_s } }
    x.report "grisu3" { TIMES.times { num.fast_to_s } }
  end
  puts
  puts
end

bench 123.456
bench -1.234678e-123
bench 0.0
