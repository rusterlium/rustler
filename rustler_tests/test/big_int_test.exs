defmodule RustlerTest.BigIntTest do
  use ExUnit.Case, async: true

  test "add one large big integer" do
    # 2**2048 - 1
    large_number =
      32_317_006_071_311_007_300_714_876_688_669_951_960_444_102_669_715_484_032_130_345_427_524_655_138_867_890_893_197_201_411_522_913_463_688_717_960_921_898_019_494_119_559_150_490_921_095_088_152_386_448_283_120_630_877_367_300_996_091_750_197_750_389_652_106_796_057_638_384_067_568_276_792_218_642_619_756_161_838_094_338_476_170_470_581_645_852_036_305_042_887_575_891_541_065_808_607_552_399_123_930_385_521_914_333_389_668_342_420_684_974_786_564_569_494_856_176_035_326_322_058_077_805_659_331_026_192_708_460_314_150_258_592_864_177_116_725_943_603_718_461_857_357_598_351_152_301_645_904_403_697_613_233_287_231_227_125_684_710_820_209_725_157_101_726_931_323_469_678_542_580_656_697_935_045_997_268_352_998_638_215_525_166_389_437_335_543_602_135_433_229_604_645_318_478_604_952_148_193_555_853_611_059_596_230_655

    assert RustlerTest.big_int_add_one(large_number) == large_number + 1
    assert RustlerTest.big_int_add_one(-large_number) == -large_number + 1
  end

  test "add one small big integer" do
    # 2**128 - 1
    large_number = 340_282_366_920_938_463_463_374_607_431_768_211_455

    assert RustlerTest.big_int_add_one(large_number) == large_number + 1
    assert RustlerTest.big_int_add_one(-large_number) == -large_number + 1
  end

  test "add one to small integer" do
    assert RustlerTest.big_int_add_one(1) == 2
  end

  test "add one to negative integer" do
    assert RustlerTest.big_int_add_one(-2) == -1
  end

  test "add one to normal integer" do
    assert RustlerTest.big_int_add_one(512) == 513
  end
end
