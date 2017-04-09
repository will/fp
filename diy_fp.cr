# Do it yourself Floating Point
# Does not support special NaN and Infinity
struct DiyFP
  private macro assert(exp, file = __FILE__, line = __LINE__)
    {% if !flag?(:release) %}
      unless {{exp}}
        raise "Assertion Failed #{{{file}}}:#{{{line}}}"
      end
    {% end %}
  end
  SIGNIFICAND_SIZE = 64
  # Also known as the significand
  property frac : UInt64
  # exponent
  property exp : Int32

  def initialize(@frac, @exp)
  end

  def initialize(@frac, exp : Int16)
    @exp = exp.to_i32
  end

  def new(frac : Int32, exp)
    new frac.to_u64, exp
  end

  # The exponents of both numbers must be the same and the frac of self must be greater than the other.
  # This result is not normalized.
  def -(other : DiyFP)
    assert self.exp == other.exp && frac >= other.frac
    self.class.new(frac - other.frac, exp)
  end

  # does not normalize result
  # Simply "emulates" a 128 bit multiplication.
  # However: the resulting number only contains 64 bits. The least
  # significant 64 bits are only used for rounding the most significant 64
  # bits.
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
    # By adding 1U << 31 to tmp we round the final result.
    # Halfway cases will be round up.
    tmp += 1_u32 << 31
    f = ac + (ad >> 32) + (bc >> 32) + (tmp >> 32)
    e = exp + other.exp + 64

    self.class.new(f, e)
  end

  def normalize
    assert frac != 0
    f = frac
    e = exp

    # This method is mainly called for normalizing boundaries. In general
    # boundaries need to be shifted by 10 bits. We thus optimize for this case.
    k10MSBits = 0xFFC0000000000000_u64
    kUint64MSB = 0x8000000000000000_u64
    while (f & k10MSBits) == 0
      # puts "  sig: #{f}"
      #  puts "  exp: #{e}"
      f <<= 10_u64
      e -= 10
    end
    while (f & kUint64MSB) == 0
      # puts "  sig: #{f}"
      # puts "  exp: #{e}"
      f <<= 1_u64
      e -= 1
    end
    DiyFP.new(f, e)
  end

  def self.from_f64(d : Float64)
    assert d > 0
    d64 = (pointerof(d).as UInt64*).value
    assert (d64 & D64_EXP_MASK) != D64_EXP_MASK

    if (d64 & D64_EXP_MASK) == 0 # denormal float
      frac = d64 & D64_FRACT_MASK
      exp = 1 - D64_EXP_BIAS
    else
      frac = (d64 & D64_FRACT_MASK) + D64_IMPLICIT_ONE
      exp = (((d64 & D64_EXP_MASK) >> D64_EXP_POS) - D64_EXP_BIAS).to_i
    end

    new(frac, exp)
  end

  # Normalize such that the most signficiant bit of frac is set
  def self.from_f64_normalized(v : Float64)
    pre_normalized = from_f64(v)
    f = pre_normalized.frac
    e = pre_normalized.exp

    # could be a denormal
    while (f & D64_IMPLICIT_ONE) == 0
      f <<= 1
      e -= 1
    end

    # do the final shifts in one go
    f <<= DiyFP::SIGNIFICAND_SIZE - 53 # f64 significand size
    e -= DiyFP::SIGNIFICAND_SIZE - 53
    DiyFP.new(f, e)
  end
end
