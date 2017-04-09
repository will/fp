require "spec"
require "./a"

describe DiyFP do
  it "multiply" do
    fp1 = DiyFP.new(3_u64, 0)
    fp2 = DiyFP.new(2_u64, 0)
    prod = fp1 * fp2

    prod.frac.should eq 0
    prod.exp.should eq 64
  end

  it "multiply" do
    fp1 = DiyFP.new(0x8000000000000000, 11)
    fp2 = DiyFP.new(2_u64, 13)
    prod = fp1 * fp2

    prod.frac.should eq 1
    prod.exp.should eq 11 + 13 + 64
  end

  it "multiply rounding" do
    fp1 = DiyFP.new(0x8000000000000001_u64, 11)
    fp2 = DiyFP.new(1_u64, 13)
    prod = fp1 * fp2

    prod.frac.should eq 1
    prod.exp.should eq 11 + 13 + 64
  end

  it "multiply rounding" do
    fp1 = DiyFP.new(0x7fffffffffffffff_u64, 11)
    fp2 = DiyFP.new(1_u64, 13)
    prod = fp1 * fp2

    prod.frac.should eq 0
    prod.exp.should eq 11 + 13 + 64
  end

  it "multiply big numbers" do
    fp1 = DiyFP.new(0xffffffffffffffff_u64, 11)
    fp2 = DiyFP.new(0xffffffffffffffff_u64, 13)
    prod = fp1 * fp2

    prod.frac.should eq 0xfffffffffffffffe_u64
    prod.exp.should eq 11 + 13 + 64
  end

  it "converts ordered" do
    ordered = 0x0123456789ABCDEF_u64
    f = pointerof(ordered).as(Float64*).value
    f.should eq 3512700564088504e-318 # ensure byte order

    fp = DiyFP.from_f64(f)

    fp.exp.should eq 0x12 - 0x3FF - 52
    # The 52 mantissa bits, plus the implicit 1 in bit 52 as a UINT64.
    fp.frac.should eq 0x0013456789ABCDEF
  end

  it "converts min f64" do
    min = 0x0000000000000001_u64
    f = pointerof(min).as(Float64*).value
    f.should eq 5e-324 # ensure byte order

    fp = DiyFP.from_f64(f)

    fp.exp.should eq -0x3FF - 52 + 1
    # This is denormal, so no hidden bit
    fp.frac.should eq 1
  end

  it "converts max f64" do
    max = 0x7fefffffffffffff_u64
    f = pointerof(max).as(Float64*).value
    f.should eq 1.7976931348623157e308 # ensure byte order

    fp = DiyFP.from_f64(f)

    fp.exp.should eq 0x7FE - 0x3FF - 52
    fp.frac.should eq 0x001fffffffffffff_u64
  end

  it "normalizes ordered" do
    ordered = 0x0123456789ABCDEF_u64
    f = pointerof(ordered).as(Float64*).value

    fp = DiyFP.from_f64_normalized(f)

    fp.exp.should eq 0x12 - 0x3FF - 52 - 11
    fp.frac.should eq 0x0013456789ABCDEF_u64 << 11
  end

  it "normalizes min f64" do
    min = 0x0000000000000001_u64
    f = pointerof(min).as(Float64*).value

    fp = DiyFP.from_f64_normalized(f)

    fp.exp.should eq -0x3FF - 52 + 1 - 63
    # This is a denormal; so no hidden bit
    fp.frac.should eq 0x8000000000000000
  end

  it "normalizes max f64" do
    max = 0x7fefffffffffffff_u64
    f = pointerof(max).as(Float64*).value

    fp = DiyFP.from_f64_normalized(f)

    fp.exp.should eq 0x7FE - 0x3FF - 52 - 11
    fp.frac.should eq 0x001fffffffffffff << 11
  end
end


private def gen_bound(v : UInt64)
  f = pointerof(v).as(Float64*).value
  gen_bound(f)
end

private def gen_bound(v : Float64)
  a = DiyFP.from_f64(v)
  fp = DiyFP.from_f64_normalized(v)
  b = normalized_boundaries(v)
  b[:minus].exp.should eq fp.exp
  b[:plus].exp.should eq fp.exp

  return fp.frac, b[:minus].frac, b[:plus].frac
end

describe "boundaires" do
  it "boundaries 1.5" do
    fp, mi, pl = gen_bound(1.5)
    # 1.5 does not have a significand of the form 2^p (for some p).
    # Therefore its boundaries are at the same distance.
    (pl - fp).should eq(fp - mi)
    (fp - mi).should eq(1 << 10)
  end

  it "boundaries 1.0" do
    fp, mi, pl = gen_bound(1.0)
    # 1.0 does have a significand of the form 2^p (for some p).
    # Therefore its lower boundary is twice as close as the upper boundary.
    (pl - fp).should be > fp - mi
    (fp - mi).should eq 1 << 9
    (pl - fp).should eq 1 << 10
  end

  it "boundaries min float64" do
    fp, mi, pl = gen_bound(0x0000000000000001_u64)
    # min-value does not have a significand of the form 2^p (for some p).
    # Therefore its boundaries are at the same distance.
    (pl - fp).should eq fp - mi
    (fp - mi).should eq 1_u64 << 62
  end

  it "boundaries min normal f64" do
    fp, mi, pl = gen_bound(0x0010000000000000_u64);
    # Even though the significand is of the form 2^p (for some p), its boundaries
    # are at the same distance. (This is the only exception).
    (fp - mi).should eq(pl - fp)
    (fp - mi).should eq(1 << 10)
  end

  it "boundaries max denormal f64" do
    fp, mi, pl = gen_bound(0x000FFFFFFFFFFFFF_u64)

    (fp - mi).should eq(pl - fp)
    (fp - mi).should eq(1 << 11)
  end

  it "boundaries max f64" do
    fp, mi, pl = gen_bound(0x7fEFFFFFFFFFFFFF_u64)
    # max-value does not have a significand of the form 2^p (for some p).
    # Therefore its boundaries are at the same distance.
    (fp - mi).should eq(pl - fp)
    (fp - mi).should eq(1 << 10)
  end
end

private def test_grisu(v : Float64)
  buffer = Slice.new(128, 0_u8)
  status, point = grisu3(v, buffer)
  return status, point, String.new(buffer.pointer(0))
end

describe "grisu3" do
  it "regular" do
    status, point, str = test_grisu 4294967272.0
    status.should eq true
    str.should eq "4294967272"
    point.should eq 10
  end
end
