require "./grisu3"

module FloatPrinter
  extend self

  def fast_to_s(v : Float64, io : IO)
    d64 = IEEE.to_d64(v)

    if IEEE.sign(d64) < 0
      io.write_byte '-'.ord.to_u8
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

    # _str = String.new(buffer.to_unsafe); pp [v, _str, point, decimal_exponent, length ]

    exp = point
    exp_mode = point > 15 || point < -3
    point = 1 if exp_mode

    # add leading zero
    io.write_byte '0'.ord.to_u8 if point < 1

    i = 0

    # add integer part digits
    if decimal_exponent > 0 && !exp_mode
      # whole number but not big enough to be exp form
      while i < length
        io.write_byte buffer[i]
        i += 1
      end
      (point - length).times do
        io.write_byte '0'.ord.to_u8
      end
    else
      while i < point
        io.write_byte buffer[i]
        i += 1
      end
    end

    io.write_byte '.'.ord.to_u8

    # add leading zeros after point
    if point < 0
      (-point).times { io.write_byte '0'.ord.to_u8 }
    end

    # add fractional part digits
    while i < length
      io.write_byte buffer[i]
      i += 1
    end

    # print trailing 0 if whole number or exp notation of power of ten
    if (decimal_exponent >= 0 && !exp_mode) || (exp != point && length == 1)
      io.write_byte '0'.ord.to_u8
    end

    # exp notation
    if exp != point
      io.write_byte 'e'.ord.to_u8
      io.write_byte '+'.ord.to_u8 if exp > 0
      (exp - 1).to_s(io)
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
