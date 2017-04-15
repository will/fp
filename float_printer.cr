require "./grisu3"

module FloatPrinter
  extend self
  def fast_to_s(v : Float64, io : IO)
    buffer = StaticArray(UInt8, 128).new(0_u8)
    status, decimal_exponent, length = Grisu3.grisu3(v, buffer.to_unsafe)
    point = decimal_exponent + length
    i = 0
    while i < length
      io << '.' if i == point
      io.write_byte buffer[i]
      i += 1
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
