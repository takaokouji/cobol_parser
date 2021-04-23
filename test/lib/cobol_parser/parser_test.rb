# frozen_string_literal: true

require "test_helper"
require "tempfile"

class CobolParser::ParserTest < Test::Unit::TestCase
  setup do
    @cb = create_context
    @options = create_common_options
    @parser = CobolParser::Parser.new(@cb, @options)
  end

  def parse_and_assert_ast_equal(pp_cobol_code, ruby_code)
    actual = create_tempfile(pp_cobol_code) { |f|
      @parser.parse(f)
    }
    expected = Parser::CurrentRuby.parse(ruby_code)
    assert_ast_equal(expected, actual)
  end

  sub_test_case "#parse" do
    sub_test_case "IDENTIFICATION DIVISION" do
      test "PROGRAM-ID" do
        parse_and_assert_ast_equal(<<-COBOL, <<-RUBY)
# 1 "PG1.CBL"
IDENTIFICATION DIVISION.
PROGRAM-ID. PG1.
        COBOL
require "ostruct"

class Pg1
end
        RUBY
      end
    end

    sub_test_case "DATA DIVISION" do
      sub_test_case "WORKING-STORAGE SECTION" do
        test "only PIC 9(n)" do
          parse_and_assert_ast_equal(<<-COBOL, <<-RUBY)
# 1 "PG1.CBL"
IDENTIFICATION DIVISION.
PROGRAM-ID. PG1.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 WRK-AREA.
 03 WRK-SYSYY PIC 9(04).
 03 WRK-SYSMM PIC 9(02).
 03 WRK-SYSDD PIC 9(02).
          COBOL
require "ostruct"

class Pg1
  def initialize
    @wrk_area = new_wrk_area
  end

  private

  def new_wrk_area
    OpenStruct.new(
      wrk_sysyy: 0,
      wrk_sysmm: 0,
      wrk_sysdd: 0
    )
  end
end
          RUBY
        end

        test "some PIC types with VALUE" do
          parse_and_assert_ast_equal(<<-COBOL, <<-RUBY)
# 1 "PG1.CBL"
IDENTIFICATION DIVISION.
PROGRAM-ID. PG1.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 WRK-AREA.
 03 WRK-NUMBER PIC 9(04).
 03 WRK-STRING PIC X(50).
 03 WRK-FLOAT PIC ZZZZ9.999.
01 WRK2-AREA.
 03 WRK2-G.
 05 WRK2-NUMBER PIC 9(04) VALUE 2021.
 05 WRK2-STRING PIC X(50) VALUE "あいうえお".
 05 WRK2-FLOAT PIC ZZZZ9.999 VALUE 87654.123.
          COBOL
require "ostruct"

class Pg1
  def initialize
    @wrk_area = new_wrk_area
    @wrk2_area = new_wrk2_area
  end

  private

  def new_wrk_area
    OpenStruct.new(
      wrk_number: 0,
      wrk_string: "",
      wrk_float: 0.0
    )
  end

  def new_wrk2_area
    OpenStruct.new(
      wrk2_g: OpenStruct.new(
        wrk2_number: 2021,
        wrk2_string: "あいうえお",
        wrk2_float: 87654.123
      )
    )
  end
end
          RUBY
        end

        test "REDEFINES" do
          parse_and_assert_ast_equal(<<-COBOL, <<-RUBY)
# 1 "PG1.CBL"
IDENTIFICATION DIVISION.
PROGRAM-ID. PG1.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 WRK-AREA.
 05 WRK-NUM PIC 9(05).9(03).
 05 WRK-NUM-G REDEFINES WRK-NUM.
 10 WRK-NUM-INTEGER PIC 9(05).
 10 WRK-NUM-X PIC X(01).
 10 WRK-NUM-FRACTION PIC 9(03).
          COBOL
require "ostruct"

class Pg1
  def initialize
    @wrk_area = new_wrk_area
  end

  private

  def new_wrk_area
    OpenStruct.new(
      wrk_num: 0.0,
      wrk_num_g: OpenStruct.new(
        redefines: :wrk_num,
        wrk_num_integer: 0,
        wrk_num_x: "",
        wrk_num_fraction: 0
      )
    )
  end
end
          RUBY
        end

        test "table (OCCURS)" do
          parse_and_assert_ast_equal(<<-COBOL, <<-RUBY)
# 1 "PG1.CBL"
IDENTIFICATION DIVISION.
PROGRAM-ID. PG1.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 WRK-AREA.
 03 WRK-G OCCURS 30.
 05 WRK-SYSYY PIC 9(04).
 05 WRK-SYSMM PIC 9(02).
 05 WRK-SYSDD PIC 9(02).
          COBOL
require "ostruct"

class Pg1
  def initialize
    @wrk_area = new_wrk_area
  end

  private

  def new_wrk_area
    OpenStruct.new(
      wrk_g: Array.new(30) {
        OpenStruct.new(
          wrk_sysyy: 0,
          wrk_sysmm: 0,
          wrk_sysdd: 0
        )
      }
    )
  end
end
          RUBY
        end
      end
    end

    sub_test_case "PROCEDURE DIVISION" do
      test "MOVE" do
        omit("not implemented yet")
        parse_and_assert_ast_equal(<<-COBOL, <<-RUBY)
# 1 "PG1.CBL"
IDENTIFICATION DIVISION.
PROGRAM-ID. PG1.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 WRK-AREA.
 03 WRK-SYSYY PIC 9(04).
 03 WRK-SYSMM PIC 9(02).
 03 WRK-SYSDD PIC 9(02).

PROCEDURE DIVISION.
000-PROC-SEC SECTION.
 MOVE 2021 TO WRK-SYSYY.
 MOVE 4 TO WRK-SYSMM.
 MOVE 21 TO WRK-SYSMM.
000-PROC-EXT.
 EXIT PROGRAM.
        COBOL
require "ostruct"

class Pg1
  def initialize
    @wrk_area = new_wrk_area
  end

  def _000_proc_sec
    @wrk_area.wrk_sysyy = 2021
    @wrk_area.wrk_sysmm = 4
    @wrk_area.wrk_sysmm = 21
  end

  private

  def new_wrk_area
    OpenStruct.new(
      wrk_sysyy: 0,
      wrk_sysmm: 0,
      wrk_sysdd: 0
    )
  end
end
        RUBY
      end
    end
  end
end
