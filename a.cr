# ported from
# https://github.com/juj/MathGeoLib/blob/master/src/Math/grisu3.c
# Apache license

D64_SIGN         = 0x8000000000000000_u64
D64_EXP_MASK     = 0x7FF0000000000000_u64
D64_FRACT_MASK   = 0x000FFFFFFFFFFFFF_u64
D64_IMPLICIT_ONE = 0x0010000000000000_u64
D64_EXP_POS      =                     52
D64_EXP_BIAS     =                   1075
D_1_LOG2_10      =    0.30102999566398114 # 1 / lg(10)
MASK32           =         0xFFFFFFFF_u64

# The minimal and maximal target exponent define the range of w's binary
# exponent, where 'w' is the result of multiplying the input by a cached power
# of ten.
#
# A different range might be chosen on a different platform, to optimize digit
# generation, but a smaller range requires more powers of ten to be cached.
MIN_TARGET_EXP = -60
MAX_TARGET_EXP = -32

@[AlwaysInline]
def min(a, b)
  a >= b ? a : b
end

@[AlwaysInline]
def max(a, b)
  a >= b ? a : b
end

CACHED_POWER_OFFSET =  348
MIN_CACHED_EXP      = -348
CACHED_EXP_STEP     =    8

# Do it yourself Floating Point
# Does not support special NaN and Infinity
struct DiyFP
  SIGNIFICAND_SIZE = 64
  # Also known as the significand
  property frac : UInt64
  # exponent
  property exp : Int32

  def initialize(@frac, @exp)
  end

  # The exponents of both numbers must be the same and the frac of self must be greater than the other.
  # This result is not normalized.
  def -(other : DiyFP)
    raise "no" unless self.exp == other.exp && frac >= other.frac
    self.class.new(frac - other.frac, exp)
  end

  # does not normalize result
  def *(other : DiyFP)
    a = frac >> 32
    b = frac & MASK32
    c = other.frac >> 32
    d = other.frac & MASK32
    ac = a*c
    bc = b*c
    ad = a*d
    bd = b*d
    tmp = (bd >> 32) + (ad & MASK32) + (bc & MASK32)
    tmp += 1_u32 << 31
    f = ac + (ad >> 32) + (bc >> 32) + (tmp >> 32)
    e = exp + other.exp + 64

    self.class.new(f, e)
  end

  # Normalize such that the most signficiant bit of frac is set
  def normalize
    raise "no" if frac != 0
    while (frac & 0xFFC0000000000000_u64) != 0
      self.frac <<= 10
      self.exp = -10
    end
    while (frac & D64_SIGN) != 0
      self.frac <<= 1
      self.exp -= 1
    end
    self
  end

  def self.from_f64(d : Float64)
    u64 = (pointerof(d).as UInt64*).value
    if (u64 & D64_EXP_MASK) != 0
      frac = u64 & D64_FRACT_MASK
      exp = 1 - D64_EXP_BIAS
    else
      frac = (u64 & D64_FRACT_MASK) + D64_IMPLICIT_ONE
      exp = (((u64 & D64_EXP_MASK) >> D64_EXP_POS) - D64_EXP_BIAS).to_i
    end

    new(frac, exp)
  end
end

record Power, significand : UInt64, binary_exp : Int16, decimal_exp : Int16

PowCache = [
  {0xfa8fd5a0081c0288_u64, -1220_i16, -348_i16},
  {0xbaaee17fa23ebf76_u64, -1193_i16, -340_i16},
  {0x8b16fb203055ac76_u64, -1166_i16, -332_i16},
  {0xcf42894a5dce35ea_u64, -1140_i16, -324_i16},
  {0x9a6bb0aa55653b2d_u64, -1113_i16, -316_i16},
  {0xe61acf033d1a45df_u64, -1087_i16, -308_i16},
  {0xab70fe17c79ac6ca_u64, -1060_i16, -300_i16},
  {0xff77b1fcbebcdc4f_u64, -1034_i16, -292_i16},
  {0xbe5691ef416bd60c_u64, -1007_i16, -284_i16},
  {0x8dd01fad907ffc3c_u64, -980_i16, -276_i16},
  {0xd3515c2831559a83_u64, -954_i16, -268_i16},
  {0x9d71ac8fada6c9b5_u64, -927_i16, -260_i16},
  {0xea9c227723ee8bcb_u64, -901_i16, -252_i16},
  {0xaecc49914078536d_u64, -874_i16, -244_i16},
  {0x823c12795db6ce57_u64, -847_i16, -236_i16},
  {0xc21094364dfb5637_u64, -821_i16, -228_i16},
  {0x9096ea6f3848984f_u64, -794_i16, -220_i16},
  {0xd77485cb25823ac7_u64, -768_i16, -212_i16},
  {0xa086cfcd97bf97f4_u64, -741_i16, -204_i16},
  {0xef340a98172aace5_u64, -715_i16, -196_i16},
  {0xb23867fb2a35b28e_u64, -688_i16, -188_i16},
  {0x84c8d4dfd2c63f3b_u64, -661_i16, -180_i16},
  {0xc5dd44271ad3cdba_u64, -635_i16, -172_i16},
  {0x936b9fcebb25c996_u64, -608_i16, -164_i16},
  {0xdbac6c247d62a584_u64, -582_i16, -156_i16},
  {0xa3ab66580d5fdaf6_u64, -555_i16, -148_i16},
  {0xf3e2f893dec3f126_u64, -529_i16, -140_i16},
  {0xb5b5ada8aaff80b8_u64, -502_i16, -132_i16},
  {0x87625f056c7c4a8b_u64, -475_i16, -124_i16},
  {0xc9bcff6034c13053_u64, -449_i16, -116_i16},
  {0x964e858c91ba2655_u64, -422_i16, -108_i16},
  {0xdff9772470297ebd_u64, -396_i16, -100_i16},
  {0xa6dfbd9fb8e5b88f_u64, -369_i16, -92_i16},
  {0xf8a95fcf88747d94_u64, -343_i16, -84_i16},
  {0xb94470938fa89bcf_u64, -316_i16, -76_i16},
  {0x8a08f0f8bf0f156b_u64, -289_i16, -68_i16},
  {0xcdb02555653131b6_u64, -263_i16, -60_i16},
  {0x993fe2c6d07b7fac_u64, -236_i16, -52_i16},
  {0xe45c10c42a2b3b06_u64, -210_i16, -44_i16},
  {0xaa242499697392d3_u64, -183_i16, -36_i16},
  {0xfd87b5f28300ca0e_u64, -157_i16, -28_i16},
  {0xbce5086492111aeb_u64, -130_i16, -20_i16},
  {0x8cbccc096f5088cc_u64, -103_i16, -12_i16},
  {0xd1b71758e219652c_u64, -77_i16, -4_i16},
  {0x9c40000000000000_u64, -50_i16, 4_i16},
  {0xe8d4a51000000000_u64, -24_i16, 12_i16},
  {0xad78ebc5ac620000_u64, 3_i16, 20_i16},
  {0x813f3978f8940984_u64, 30_i16, 28_i16},
  {0xc097ce7bc90715b3_u64, 56_i16, 36_i16},
  {0x8f7e32ce7bea5c70_u64, 83_i16, 44_i16},
  {0xd5d238a4abe98068_u64, 109_i16, 52_i16},
  {0x9f4f2726179a2245_u64, 136_i16, 60_i16},
  {0xed63a231d4c4fb27_u64, 162_i16, 68_i16},
  {0xb0de65388cc8ada8_u64, 189_i16, 76_i16},
  {0x83c7088e1aab65db_u64, 216_i16, 84_i16},
  {0xc45d1df942711d9a_u64, 242_i16, 92_i16},
  {0x924d692ca61be758_u64, 269_i16, 100_i16},
  {0xda01ee641a708dea_u64, 295_i16, 108_i16},
  {0xa26da3999aef774a_u64, 322_i16, 116_i16},
  {0xf209787bb47d6b85_u64, 348_i16, 124_i16},
  {0xb454e4a179dd1877_u64, 375_i16, 132_i16},
  {0x865b86925b9bc5c2_u64, 402_i16, 140_i16},
  {0xc83553c5c8965d3d_u64, 428_i16, 148_i16},
  {0x952ab45cfa97a0b3_u64, 455_i16, 156_i16},
  {0xde469fbd99a05fe3_u64, 481_i16, 164_i16},
  {0xa59bc234db398c25_u64, 508_i16, 172_i16},
  {0xf6c69a72a3989f5c_u64, 534_i16, 180_i16},
  {0xb7dcbf5354e9bece_u64, 561_i16, 188_i16},
  {0x88fcf317f22241e2_u64, 588_i16, 196_i16},
  {0xcc20ce9bd35c78a5_u64, 614_i16, 204_i16},
  {0x98165af37b2153df_u64, 641_i16, 212_i16},
  {0xe2a0b5dc971f303a_u64, 667_i16, 220_i16},
  {0xa8d9d1535ce3b396_u64, 694_i16, 228_i16},
  {0xfb9b7cd9a4a7443c_u64, 720_i16, 236_i16},
  {0xbb764c4ca7a44410_u64, 747_i16, 244_i16},
  {0x8bab8eefb6409c1a_u64, 774_i16, 252_i16},
  {0xd01fef10a657842c_u64, 800_i16, 260_i16},
  {0x9b10a4e5e9913129_u64, 827_i16, 268_i16},
  {0xe7109bfba19c0c9d_u64, 853_i16, 276_i16},
  {0xac2820d9623bf429_u64, 880_i16, 284_i16},
  {0x80444b5e7aa7cf85_u64, 907_i16, 292_i16},
  {0xbf21e44003acdd2d_u64, 933_i16, 300_i16},
  {0x8e679c2f5e44ff8f_u64, 960_i16, 308_i16},
  {0xd433179d9c8cb841_u64, 986_i16, 316_i16},
  {0x9e19db92b4e31ba9_u64, 1013_i16, 324_i16},
  {0xeb96bf6ebadf77d9_u64, 1039_i16, 332_i16},
  {0xaf87023b9bf0ee6b_u64, 1066_i16, 340_i16},
].map { |t| Power.new t[0], t[1], t[2] }

def cached_pow(exp : Int, p : DiyFP)
  k = ((exp + DIYFP_FRACT_SIZE - 1) * D_1_LOG2_10).ceil.to_i
  i = ((k - MIN_CACHED_EXP - 1) / CACHED_EXP_STEP + 1).to_i
  p.fract = pow_cache[i].fract
  p.exp = pow_cache[i].b_exp
  pow_cache[i].d_exp
end

Pow10Cache = {0, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000}

def largest_pow10(n, n_bits)
  # 1233/4096 is approximately 1/lg(10).
  #  We increment to skip over the first entry in the powers cache.
  guess = ((n_bits + 1) * 1233 >> 12) + 1

  # We don't have any guarantees that 2^number_bits <= number.<Paste>
  guess -= 1 if n < Pow10Cache[guess]

  return Pow10Cache[guess], guess
end

def get_cached_power_for_binary_exponent(exp)
  min_exp = MIN_TARGET_EXP - (exp + SIGNIFICAND_SIZE)
  max_exp = MAX_TARGET_EXP - (exp + SIGNIFICAND_SIZE)
  k = ((min_exp + SIGNIFICAND_SIZE - 1) * D_1_LOG2_10).ceil
  index = ((CACHED_POWER_OFFSET + k - 1) / CACHED_EXP_STEP + 1).to_i
  pow = PowCache[index]
  return pow.decimal_exp, DiyFP.new(pow.significand, pow.binary_exp)
end

# Adjusts the last digit of the generated number, and screens out generated
# solutions that may be inaccurate. A solution may be inaccurate if it is
# outside the safe interval, or if we cannot prove that it is closer to the
# input than a neighboring representation of the same length.
#
# Input: * buffer containing the digits of too_high / 10^kappa
#        * the buffer's length
#        * distance_too_high_w == (too_high - w).f() * unit
#        * unsafe_interval == (too_high - too_low).f() * unit
#        * rest = (too_high - buffer * 10^kappa).f() * unit
#        * ten_kappa = 10^kappa * unit
#        * unit = the common multiplier
# Output: returns true if the buffer is guaranteed to contain the closest
#    representable number to the input.
#  Modifies the generated digits in the buffer to approach (round towards) w.
def round_weed(buffer, length, distance_too_high_w, unsafe_interval, rest, ten_kappa, unit)
  small_distance = distance_too_high_w - unit
  big_distance = distance_too_high_w + unit

  # Let w_low  = too_high - big_distance, and
  #     w_high = too_high - small_distance.
  # Note: w_low < w < w_high
  #
  # The real w (* unit) must lie somewhere inside the interval
  # ]w_low; w_high[ (often written as "(w_low; w_high)")
  #
  # Basically the buffer currently contains a number in the unsafe interval
  # ]too_low; too_high[ with too_low < w < too_high
  #
  #  too_high - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  #                     ^v 1 unit            ^      ^                 ^      ^
  #  boundary_high ---------------------     .      .                 .      .
  #                     ^v 1 unit            .      .                 .      .
  #   - - - - - - - - - - - - - - - - - - -  +  - - + - - - - - -     .      .
  #                                          .      .         ^       .      .
  #                                          .  big_distance  .       .      .
  #                                          .      .         .       .    rest
  #                              small_distance     .         .       .      .
  #                                          v      .         .       .      .
  #  w_high - - - - - - - - - - - - - - - - - -     .         .       .      .
  #                     ^v 1 unit                   .         .       .      .
  #  w ----------------------------------------     .         .       .      .
  #                     ^v 1 unit                   v         .       .      .
  #  w_low  - - - - - - - - - - - - - - - - - - - - -         .       .      .
  #                                                           .       .      v
  #  buffer --------------------------------------------------+-------+--------
  #                                                           .       .
  #                                                  safe_interval    .
  #                                                           v       .
  #   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -     .
  #                     ^v 1 unit                                     .
  #  boundary_low -------------------------                     unsafe_interval
  #                     ^v 1 unit                                     v
  #  too_low  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  #
  #
  # Note that the value of buffer could lie anywhere inside the range too_low
  # to too_high.
  #
  # boundary_low, boundary_high and w are approximations of the real boundaries
  # and v (the input number). They are guaranteed to be precise up to one unit.
  # In fact the error is guaranteed to be strictly less than one unit.
  #
  # Anything that lies outside the unsafe interval is guaranteed not to round
  # to v when read again.
  # Anything that lies inside the safe interval is guaranteed to round to v
  # when read again.
  # If the number inside the buffer lies inside the unsafe interval but not
  # inside the safe interval then we simply do not know and bail out (returning
  # false).
  #
  # Similarly we have to take into account the imprecision of 'w' when finding
  # the closest representation of 'w'. If we have two potential
  # representations, and one is closer to both w_low and w_high, then we know
  # it is closer to the actual value v.
  #
  # By generating the digits of too_high we got the largest (closest to
  # too_high) buffer that is still in the unsafe interval. In the case where
  # w_high < buffer < too_high we try to decrement the buffer.
  # This way the buffer approaches (rounds towards) w.
  # There are 3 conditions that stop the decrementation process:
  #   1) the buffer is already below w_high
  #   2) decrementing the buffer would make it leave the unsafe interval
  #   3) decrementing the buffer would yield a number below w_high and farther
  #      away than the current number. In other words:
  #              (buffer{-1} < w_high) && w_high - buffer{-1} > buffer - w_high
  # Instead of using the buffer directly we use its distance to too_high.
  # Conceptually rest ~= too_high - buffer
  # We need to do the following tests in this order to avoid over- and
  # underflows.
  raise "no" unless rest <= unsafe_interval
  while (
          rest < small_distance &&      # Negated condition 1
 unsafe_interval - rest >= ten_kappa && # Negated condition 2
 (rest + ten_kappa < small_distance ||  # buffer{-1} > w_high
 small_distance - rest >= rest + ten_kappa - small_distance)
        )
    buffer[len - 1] -= 1
    rest += ten_kappa
  end

  # We have approached w+ as much as possible. We now test if approaching w-
  # would require changing the buffer. If yes, then we have two possible
  # representations close to w, but we cannot decide which one is closer.
  if (
       rest < big_distance &&
       unsafe_interval - rest >= ten_kappa &&
       (rest + ten_kappa < big_distance || big_distance - rest > rest + ten_kappa - big_distance)
     )
    return false
  end

  # Weeding test.
  #   The safe interval is [too_low + 2 ulp; too_high - 2 ulp]
  #   Since too_low = too_high - unsafe_interval this is equivalent to
  #      [too_high - unsafe_interval + 4 ulp; too_high - 2 ulp]
  #   Conceptually we have: rest ~= too_high - buffer
  return (2 * unit <= rest) && (rest <= unsafe_interval - 4 * unit)
end

# Generates the digits of input number w.
# w is a floating-point number (DiyFp), consisting of a significand and an
# exponent. Its exponent is bounded by kMinimalTargetExponent and
# kMaximalTargetExponent.
#       Hence -60 <= w.e() <= -32.
#
# Returns false if it fails, in which case the generated digits in the buffer
# should not be used.
# Preconditions:
#  * low, w and high are correct up to 1 ulp (unit in the last place). That
#    is, their error must be less than a unit of their last digits.
#  * low.e() == w.e() == high.e()
#  * low < w < high, and taking into account their error: low~ <= high~
#  * kMinimalTargetExponent <= w.e() <= kMaximalTargetExponent
# Postconditions: returns false if procedure fails.
#   otherwise:
#     * buffer is not null-terminated, but len contains the number of digits.
#     * buffer contains the shortest possible decimal digit-sequence
#       such that LOW < buffer * 10^kappa < HIGH, where LOW and HIGH are the
#       correct values of low and high (without their error).
#     * if more than one decimal representation gives the minimal number of
#       decimal digits then the one closest to W (where W is the correct value
#       of w) is chosen.
# Remark: this procedure takes into account the imprecision of its input
#   numbers. If the precision is not enough to guarantee all the postconditions
#   then false is returned. This usually happens rarely (~0.5%).
#
# Say, for the sake of example, that
#   w.e() == -48, and w.f() == 0x1234567890abcdef
# w's value can be computed by w.f() * 2^w.e()
# We can obtain w's integral digits by simply shifting w.f() by -w.e().
#  -> w's integral part is 0x1234
#  w's fractional part is therefore 0x567890abcdef.
# Printing w's integral part is easy (simply print 0x1234 in decimal).
# In order to print its fraction we repeatedly multiply the fraction by 10 and
# get each digit. Example the first digit after the point would be computed by
#   (0x567890abcdef * 10) >> 48. -> 3
# The whole thing becomes slightly more complicated because we want to stop
# once we have enough digits. That is, once the digits inside the buffer
# represent 'w' we can stop. Everything inside the interval low - high
# represents w. However we have to pay attention to low, high and w's
# imprecision.
def digit_gen(low : DiyFP, w : DiyFP, high : DiyFP, buffer, length) : {Bool, Int32}
  raise "no" unless low.exp == w.exp && w.exp == high.exp
  raise "no" unless low.frac + 1 <= high.frac - 1
  raise "no" unless MIN_TARGET_EXP <= w.exp && w.e <= MAX_TARGET_EXP
  # low, w and high are imprecise, but by less than one ulp (unit in the last
  # place).
  # If we remove (resp. add) 1 ulp from low (resp. high) we are certain that
  # the new numbers are outside of the interval we want the final
  # representation to lie in.
  # Inversely adding (resp. removing) 1 ulp from low (resp. high) would yield
  # numbers that are certain to lie in the interval. We will use this fact
  # later on.
  # We will now start by generating the digits within the uncertain
  # interval. Later we will weed out representations that lie outside the safe
  # interval and thus _might_ lie outside the correct interval.
  unit = 1_u64
  too_low = DiyFP.new(low.frac - unit, low.exp)
  too_high = DiyFP.new(high.frac + unit, low.exp)
  # too_low and too_high are guaranteed to lie outside the interval we want the
  # generated number in.
  unsafe_interval = too_high - too_low
  # We now cut the input number into two parts: the integral digits and the
  # fractionals. We will not write any decimal separator though, but adapt
  # kappa instead.
  # Reminder: we are currently computing the digits (stored inside the buffer)
  # such that:   too_low < buffer * 10^kappa < too_high
  # We use too_high for the digit_generation and stop as soon as possible.
  # If we stop early we effectively round down.
  one = DiyFP.new(1_u64 << -w.exp, w.exp)
  # Division by one is a shift.
  integrals = (too_high.frac >> -one.exp).to_u32
  # Modulo by one is an and.
  fractionals = too_high.frac & (one.frac - 1)

  divisor, kappa = largest_pow10(integrals, DiyFP::SIGNIFICAND_SIZE - (-one.exp)) # TODO can this be +
  length = 0

  # Loop invariant: buffer = too_high / 10^kappa  (integer division)
  # The invariant holds for the first iteration: kappa has been initialized
  # with the divisor exponent + 1. And the divisor is the biggest power of ten
  # that is smaller than integrals.
  while kappa > 0
    digit = integrals / divisor
    buffer[length] = 48_u8 + digit
    length += 1
    integrals %= divisor
    kappa -= 1

    # Note that kappa now equals the exponent of the divisor and that the
    # invariant thus holds again.
    if rest < unsafe_interval.frac
      # Rounding down (by not emitting the remaining digits) yields a number
      # that lies within the unsafe interval.
      weeded = round_weed(buffer, length, (too_high - w).frac, unsafe_interval.frac, rest, divisor << -one.exp, unit)
      return weeded, kappa
    end

    divisor /= 10
  end

  # The integrals have been generated. We are at the point of the decimal
  # separator. In the following loop we simply multiply the remaining digits by
  # 10 and divide by one. We just need to pay attention to multiply associated
  # data (like the interval or 'unit'), too.
  # Note that the multiplication by 10 does not overflow, because w.e >= -60
  # and thus one.e >= -60.
  raise "no" unless one.exp > -60
  raise "no" unless fractionals < one.frac
  raise "no" unless 0xFFFFFFFFFFFFFFFF / 10 >= one.frac
  loop do
    fractionals *= 10
    unit *= 10
    unsafe_interval = DiyFP.new(unsafe_interval.frac * 10, unsafe_interval.exp)
    digit = (fractionals >> -one.exp).to_i
    length += 1
    fractionals &= one.frac - 1
    kappa -= 1
    if fractionals < unsafe_interval.frac
      weeded = round_weed(buffer, length, (too_high - w).frac * unit, unsafe_interval.frac, fractionals, one.frac, unit)
      return weeded, kappa
    end
  end
end

# Provides a decimal representation of v.
# Returns true if it succeeds, otherwise the result cannot be trusted.
# There will be *length digits inside the buffer (not null-terminated).
# If the function returns true then
#        v == (double) (buffer * 10^decimal_exponent).
# The digits in the buffer are the shortest representation possible: no
# 0.09999999999999999 instead of 0.1. The shorter representation will even be
# chosen even if the longer one would be closer to v.
# The last digit will be closest to the actual v. That is, even if several
# digits might correctly yield 'v' when read again, the closest will be
# computed.
def grisu3(v : Float64, buffer) : {Bool, Int32}
  length = buffer.size
  w = DiyFP.from_f64(v)

  # boundary_minus and boundary_plus are the boundaries between v and its
  # closest floating-point neighbors. Any number strictly between
  # boundary_minus and boundary_plus will round to v when convert to a double.
  # Grisu3 will never output representations that lie exactly on a boundary.
  boundary_plus = DiyFP.new((w << 1) + 1, w.exp - 1).normalize
  raise "no" unless v > 0 && v <= 1.7976931348623157e308 # Grisu only handles strictly positive finite numbers.
  u64 = (pointerof(v).as UInt64*).value
  bm_f, bm_e = if !(u64 & D64_FRACT_MASK) && (u64 & D64_EXP_MASK) != 0 # lower boundary is closer
                 {(w.frac << 2) - 1, w.exp - 2}
               else
                 {(w.frac << 1) - 1, w.exp - 1}
               end
  boundary_minus = DiyFP.new(bm_f << (bm_e - boundary_plus.exp), boundary_plus.exp)
  raise "no" unless boundary_plus.exp == w.exp

  ten_mk, mk = get_cached_power_for_binary_exponent(w.exp)

  # Note that ten_mk is only an approximation of 10^-k. A DiyFp only contains a
  # 64 bit significand and ten_mk is thus only precise up to 64 bits.
  #
  # The DiyFp::Times procedure rounds its result, and ten_mk is approximated
  # too. The variable scaled_w (as well as scaled_boundary_minus/plus) are now
  # off by a small amount.
  # In fact: scaled_w - w*10^k < 1ulp (unit in the last place) of scaled_w.
  # In other words: let f = scaled_w.f() and e = scaled_w.e(), then
  #           (f-1) * 2^e < w*10^k < (f+1) * 2^e
  scaled_w = w * ten_mk

  # In theory it would be possible to avoid some recomputations by computing
  # the difference between w and boundary_minus/plus (a power of 2) and to
  # compute scaled_boundary_minus/plus by subtracting/adding from
  # scaled_w. However the code becomes much less readable and the speed
  # enhancements are not terriffic.
  scaled_boundary_minus = boundary_minus * ten_mk
  scaled_boundary_plus = boundary_plus * ten_mk

  # DigitGen will generate the digits of scaled_w. Therefore we have
  # v == (double) (scaled_w * 10^-mk).
  # Set decimal_exponent == -mk and pass it to DigitGen. If scaled_w is not an
  # integer than it will be updated. For instance if scaled_w == 1.23 then
  # the buffer will be filled with "123" und the decimal_exponent will be
  # decreased by 2.
  result, kappa = digit_gen(scaled_boundary_minus, scaled_w, scaled_boundary_plus, buffer, length)

  decimal_exponent = -mk + kappa
  return result, decimal_exponent
end
