# frozen_string_literal: true

require "test_helper"

module CobolParser
  class MbsMixinTest < Test::Unit::TestCase
    sub_test_case ".mbs_length" do
      data do
        [
          ["ABCDEF", 6],
          ["ＡＢＣＤＥＦ", 12],
          ["abcＤＥＦ", 9],
          ["abＣＤef", 8],
          [" 　 ", 4],
          ["    ", 4],
          ["　", 2],
        ].to_h { |x| ["(#{x[0].inspect}) => #{x[1].inspect}", x] }
      end
      test "The number of characters should be counted as 1 for single-byte characters " \
           "and 2 for multi-byte characters." do |test_data|
        str, expected = test_data
        assert_equal(expected, MbsMixin.mbs_length(str))
      end
    end

    sub_test_case ".mbs_slice_nth" do
      data do
        [
          ["ABCDEF", 0, "A"],
          ["ABCDEF", 2, "C"],
          ["ABCDEF", 5, "F"],
          ["ABCDEF", 6, nil],
          ["ABCDEF", -1, "F"],
          ["ABCDEF", -3, "D"],
          ["ABCDEF", -6, "A"],
          ["ABCDEF", -7, nil],
          ["ＡＢＣＤＥＦ", 0, "Ａ"],
          ["ＡＢＣＤＥＦ", 2, "Ｂ"],
          ["ＡＢＣＤＥＦ", 4, "Ｃ"],
          ["ＡＢＣＤＥＦ", 5, "Ｃ"],
          ["ＡＢＣＤＥＦ", 10, "Ｆ"],
          ["ＡＢＣＤＥＦ", 11, "Ｆ"],
          ["ＡＢＣＤＥＦ", 12, nil],
          ["ＡＢＣＤＥＦ", -1, "Ｆ"],
          ["ＡＢＣＤＥＦ", -2, "Ｆ"],
          ["ＡＢＣＤＥＦ", -3, "Ｅ"],
          ["ＡＢＣＤＥＦ", -4, "Ｅ"],
          ["ＡＢＣＤＥＦ", -5, "Ｄ"],
          ["ＡＢＣＤＥＦ", -6, "Ｄ"],
          ["ＡＢＣＤＥＦ", -7, "Ｃ"],
          ["ＡＢＣＤＥＦ", -11, "Ａ"],
          ["ＡＢＣＤＥＦ", -12, "Ａ"],
          ["ＡＢＣＤＥＦ", -13, nil],
          ["abＣＤef", 0, "a"],
          ["abＣＤef", 2, "Ｃ"],
          ["abＣＤef", 3, "Ｃ"],
          ["abＣＤef", 7, "f"],
          ["abＣＤef", 8, nil],
          ["abＣＤef", -1, "f"],
          ["abＣＤef", -2, "e"],
          ["abＣＤef", -3, "Ｄ"],
          ["abＣＤef", -4, "Ｄ"],
          ["abＣＤef", -8, "a"],
          ["abＣＤef", -9, nil],
        ].to_h { |x| ["(#{x[0].inspect}, #{x[1].inspect}) => #{x[2].inspect}", x] }
      end
      test "Like String#slice(nth), " \
           "but the nth should be counted as 1 for single-byte characters " \
           "and 2 for multi-byte characters" do |test_data|
        str, nth, expected = test_data
        assert_equal(expected, MbsMixin.mbs_slice_nth(str, nth))
      end
    end

    sub_test_case ".mbs_slice_nth_length" do
      data do
        [
          ["ABCDEF", 2, 3, "CDE"],
          ["ABCDEF", 2, 4, "CDEF"],
          ["ABCDEF", 2, 5, "CDEF"],
          ["ＡＢＣＤＥＦ", 4, 6, "ＣＤＥ"],
          ["ＡＢＣＤＥＦ", 4, 7, "ＣＤＥＦ"],
          ["ＡＢＣＤＥＦ", 4, 8, "ＣＤＥＦ"],
          ["ＡＢＣＤＥＦ", 4, 9, "ＣＤＥＦ"],
          ["ＡＢＣＤＥＦ", 1, 3, "ＡＢ"],
          ["ＡＢＣＤＥＦ", 1, 4, "ＡＢＣ"],
          ["ＡＢＣＤＥＦ", 1, 5, "ＡＢＣ"],
          ["ＡＢＣＤＥＦ", 1, 6, "ＡＢＣＤ"],
          ["ＡＢＣＤＥＦ", 1, 9, "ＡＢＣＤＥ"],
          ["ＡＢＣＤＥＦ", 1, 10, "ＡＢＣＤＥＦ"],
          ["ＡＢＣＤＥＦ", 1, 11, "ＡＢＣＤＥＦ"],
          ["ＡＢＣＤＥＦ", 1, 12, "ＡＢＣＤＥＦ"],
          ["abＣＤef", 1, 3, "bＣ"],
          ["abＣＤef", 1, 4, "bＣＤ"],
          ["abＣＤef", 1, 5, "bＣＤ"],
          ["abＣＤef", 1, 6, "bＣＤe"],
        ].to_h { |x| ["(#{x[0].inspect}, #{x[1].inspect}, #{x[2].inspect}) => #{x[3].inspect}", x] }
      end
      test "Like String#slice(nth, length), " \
           "but the nth and length should be counted as 1 for single-byte characters " \
           "and 2 for multi-byte characters" do |test_data|
        str, nth, length, expected = test_data
        assert_equal(expected, MbsMixin.mbs_slice_nth_length(str, nth, length))
      end
    end

    sub_test_case ".mbs_slice_range" do
      data do
        [
          ["ABCDEF", 2..4, "CDE"],
          ["ABCDEF", 2..5, "CDEF"],
          ["ABCDEF", 2..6, "CDEF"],
          ["ABCDEF", 2...4, "CD"],
          ["ABCDEF", 2...5, "CDE"],
          ["ABCDEF", 2...6, "CDEF"],
          ["ABCDEF", 2...7, "CDEF"],
          ["ＡＢＣＤＥＦ", 4..9, "ＣＤＥ"],
          ["ＡＢＣＤＥＦ", 4..11, "ＣＤＥＦ"],
          ["ＡＢＣＤＥＦ", 4..12, "ＣＤＥＦ"],
          ["ＡＢＣＤＥＦ", 1..3, "ＡＢ"],
          ["ＡＢＣＤＥＦ", 1..4, "ＡＢＣ"],
          ["ＡＢＣＤＥＦ", 1..5, "ＡＢＣ"],
          ["ＡＢＣＤＥＦ", 1..6, "ＡＢＣＤ"],
          ["ＡＢＣＤＥＦ", 4...8, "ＣＤ"],
          ["ＡＢＣＤＥＦ", 4...9, "ＣＤＥ"],
          ["ＡＢＣＤＥＦ", 4...10, "ＣＤＥ"],
          ["ＡＢＣＤＥＦ", 4...11, "ＣＤＥＦ"],
          ["ＡＢＣＤＥＦ", 4...12, "ＣＤＥＦ"],
          ["ＡＢＣＤＥＦ", 1...2, "Ａ"],
          ["ＡＢＣＤＥＦ", 1...3, "ＡＢ"],
          ["ＡＢＣＤＥＦ", 1...4, "ＡＢ"],
          ["ＡＢＣＤＥＦ", 1...5, "ＡＢＣ"],
          ["ＡＢＣＤＥＦ", 1...6, "ＡＢＣ"],
          ["ＡＢＣＤＥＦ", 1...7, "ＡＢＣＤ"],
          ["abＣＤef", 1..3, "bＣ"],
          ["abＣＤef", 1..4, "bＣＤ"],
          ["abＣＤef", 1..5, "bＣＤ"],
          ["abＣＤef", 1..6, "bＣＤe"],
          ["abＣＤef", 1...2, "b"],
          ["abＣＤef", 1...3, "bＣ"],
          ["abＣＤef", 1...4, "bＣ"],
          ["abＣＤef", 1...5, "bＣＤ"],
          ["abＣＤef", 1...6, "bＣＤ"],
          ["abＣＤef", 1...7, "bＣＤe"],
        ].to_h { |x| ["(#{x[0].inspect}, #{x[1].inspect}) => #{x[2].inspect}", x] }
      end
      test "Like String#slice(nth, range), " \
           "but the range's value should be counted as 1 for single-byte characters " \
           "and 2 for multi-byte characters" do |test_data|
        str, range, expected = test_data
        assert_equal(expected, MbsMixin.mbs_slice_range(str, range))
      end
    end
  end
end
