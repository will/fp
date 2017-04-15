module IEEE
  extend self
  private macro assert(exp, file = __FILE__, line = __LINE__)
    {% if !flag?(:release) %}
      unless {{exp}}
        raise "Assertion Failed #{{{file}}}:#{{{line}}}"
      end
    {% end %}
  end

  EXPONENT_MASK             = 0x7FF0000000000000_u64
  SIGNIFICAND_MASK          = 0x000FFFFFFFFFFFFF_u64
  HIDDEN_BIT                = 0x0010000000000000_u64 # hiden bit
  PHYSICAL_SIGNIFICAND_SIZE =                     52 # Excludes the hidden bit
  SIGNIFICAND_SIZE          =                     53 # float64
  EXPONENT_BIAS             = 0x3FF + PHYSICAL_SIGNIFICAND_SIZE
  DENORMAL_EXPONENT         = -EXPONENT_BIAS + 1

  # Computes the two boundaries of v.
  # The bigger boundary (m_plus) is normalized. The lower boundary has the same
  # exponent as m_plus.
  # Precondition: the value encoded by this Double must be greater than 0.
  def normalized_boundaries(v : Float64)
    assert v > 0
    w = DiyFP.from_f64(v)
    # pp w
    # p "inner: #{DiyFP.new((w.frac << 1) + 1, w.exp - 1).inspect}"
    m_plus = DiyFP.new((w.frac << 1) + 1, w.exp - 1).normalize
    # pp m_plus

    u64 = (pointerof(v).as UInt64*).value

    # The boundary is closer if the significand is of the form f == 2^p-1 then
    # the lower boundary is closer.
    # Think of v = 1000e10 and v- = 9999e9.
    # Then the boundary (== (v - v-)/2) is not just at a distance of 1e9 but
    # at a distance of 1e8.
    # The only exception is for the smallest normal: the largest denormal is
    # at the same distance as its successor.
    # Note: denormals have the same exponent as the smallest normals.
    physical_significand_is_zero = (u64 & SIGNIFICAND_MASK) == 0
    # pp physical_significand_is_zero

    lower_bound_closer = physical_significand_is_zero && (exponent(u64) != DENORMAL_EXPONENT)
    calcualted_exp = exponent(u64)
    # pp calcualted_exp
    calc_denormal = denormal?(u64)
    # pp calc_denormal
    # pp lower_bound_closer
    # pp w
    f, e = if lower_bound_closer
             {(w.frac << 2) - 1, w.exp - 2}
           else
             {(w.frac << 1) - 1, w.exp - 1}
           end
    # pp ["pre", f,e]
    m_minus = DiyFP.new(f << (e - m_plus.exp), m_plus.exp)
    # pp m_minus
    return {minus: m_minus, plus: m_plus}
  end

  def frac_and_exp(v : Float64)
    d64 = (pointerof(v).as UInt64*).value
    assert (d64 & EXPONENT_MASK) != EXPONENT_MASK

    if (d64 & EXPONENT_MASK) == 0 # denormal float
      frac = d64 & SIGNIFICAND_MASK
      exp = 1 - EXPONENT_BIAS
    else
      frac = (d64 & SIGNIFICAND_MASK) + HIDDEN_BIT
      exp = (((d64 & EXPONENT_MASK) >> PHYSICAL_SIGNIFICAND_SIZE) - EXPONENT_BIAS).to_i
    end

    {frac, exp}
  end

  private def denormal?(d64 : UInt64) : Bool
    (d64 & EXPONENT_MASK) == 0
  end

  private def exponent(d64 : UInt64)
    # pp (denormal?(d64))
    return DENORMAL_EXPONENT if denormal?(d64)
    baised_e = ((d64 & EXPONENT_MASK) >> PHYSICAL_SIGNIFICAND_SIZE).to_i
    # puts [(d64 & EXPONENT_MASK).to_i, PHYSICAL_SIGNIFICAND_SIZE]
    # pp [baised_e, EXPONENT_BIAS]
    baised_e - EXPONENT_BIAS
  end
end
