require "./grisu3"

module FloatPrinter
  extend self
  def fast_to_s(v : Float64, io : IO)
    d64 = IEEE.to_d64(v)

    if IEEE.sign(d64) < 0
      io << '-'
      v = -v
    end

    if v == 0.0
      io << "0.0"
    elsif IEEE.special?(d64)
      if IEEE.inf?(d64)
        io << "Infinity"
      else
        io << "NaN"
      end
    else
      internal(v, io)
    end
  end

  def internal(v : Float64, io : IO)
    buffer = StaticArray(UInt8, 128).new(0_u8)
    status, decimal_exponent, length = Grisu3.grisu3(v, buffer.to_unsafe)
    point = decimal_exponent + length

    digits_after_point = decimal_exponent < 0 ? -decimal_exponent : 0

    _str = String.new(buffer.to_unsafe); pp [v, _str, point, decimal_exponent, length, digits_after_point]

    # add leading zero
    io << '0' if point < 1

    i = 0

    # add integer part digits
    while i < point
      io.write_byte buffer[i]
      i += 1
    end

    io << '.'

    # add leading zeros after point
    if point < 0
      (-point).times { io << '0' }
    end

    # add fractional part digits
    while i < length
      io.write_byte buffer[i]
      i += 1
    end

    # whole number, print trailing 0
    if decimal_exponent == 0
      io << '0'
    end
  end
end

struct Float64
  def fast_to_s
    String.build(22) do |buff|
      FloatPrinter.fast_to_s(self, buff)
    end
  end

  def fast_to_s(buffer)
    String.build(22) do |buff|
      fast_to_s(buff, buffer)
    end
  end
end
