# frozen_string_literal: true

require "test_helper"
require "tempfile"
require "parser/current"

class CobolParser::ParserTest < Test::Unit::TestCase
  setup do
    @cb = create_context
    @options = create_common_options
    @parser = CobolParser::Parser.new(@cb, @options)
  end

  sub_test_case "#parse" do
    sub_test_case "IDENTIFICATION DIVISION" do
      test "PROGRAM-ID" do
        create_tempfile(<<-EOS) do |f|
# 1 "PG1.CBL"
IDENTIFICATION DIVISION.
PROGRAM-ID. PG1.
        EOS
          actual = @parser.parse(f)

          expected = Parser::CurrentRuby.parse(<<-EOS)
require "ostruct"

class Pg1
end
          EOS

          assert_text_equal(expected.to_s, actual.to_s)
        end
      end
    end

    sub_test_case "DATA DIVISION / WORKING-STORAGE SECTION" do
      test "only PIC 9(n)" do
        create_tempfile(<<-EOS) do |f|
# 1 "PG1.CBL"
IDENTIFICATION DIVISION.
PROGRAM-ID. PG1.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 WRK-AREA.
 03 WRK-SYSYY PIC 9(04).
 03 WRK-SYSMM PIC 9(02).
 03 WRK-SYSDD PIC 9(02).
        EOS
          ast = @parser.parse(f)

          expected_sexp = <<-EOS.chomp
(begin
  (send nil :require
    (str "ostruct"))
  (class
    (const nil :Pg1) nil
    (begin
      (def :initialize
        (args)
        (ivasgn :@wrk_area
          (send nil :new_wrk_area)))
      (send nil :private)
      (def :new_wrk_area
        (args)
        (send
          (const nil :OpenStruct) :new
          (kwargs
            (pair
              (sym :wrk_sysyy)
              (int 0))
            (pair
              (sym :wrk_sysmm)
              (int 0))
            (pair
              (sym :wrk_sysdd)
              (int 0))))))))
          EOS
          assert_text_equal(expected_sexp, ast.to_s)
        end
      end

      test "some PIC types with VALUE" do
        create_tempfile(<<-EOS) do |f|
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
        EOS
          ast = @parser.parse(f)

          expected_sexp = <<-EOS.chomp
(begin
  (send nil :require
    (str "ostruct"))
  (class
    (const nil :Pg1) nil
    (begin
      (def :initialize
        (args)
        (begin
          (ivasgn :@wrk_area
            (send nil :new_wrk_area))
          (ivasgn :@wrk2_area
            (send nil :new_wrk2_area))))
      (send nil :private)
      (def :new_wrk_area
        (args)
        (send
          (const nil :OpenStruct) :new
          (kwargs
            (pair
              (sym :wrk_number)
              (int 0))
            (pair
              (sym :wrk_string)
              (str ""))
            (pair
              (sym :wrk_float)
              (float 0.0)))))
      (def :new_wrk2_area
        (args)
        (send
          (const nil :OpenStruct) :new
          (kwargs
            (pair
              (sym :wrk2_g)
              (send
                (const nil :OpenStruct) :new
                (kwargs
                  (pair
                    (sym :wrk2_number)
                    (int 2021))
                  (pair
                    (sym :wrk2_string)
                    (str "あいうえお"))
                  (pair
                    (sym :wrk2_float)
                    (float 87654.123)))))))))))
          EOS
          assert_text_equal(expected_sexp, ast.to_s)
        end
      end

      test "table (OCCURS)" do
        create_tempfile(<<-EOS) do |f|
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
        EOS
          ast = @parser.parse(f)

          expected_sexp = <<-EOS.chomp
(begin
  (send nil :require
    (str "ostruct"))
  (class
    (const nil :Pg1) nil
    (begin
      (def :initialize
        (args)
        (ivasgn :@wrk_area
          (send nil :new_wrk_area)))
      (send nil :private)
      (def :new_wrk_area
        (args)
        (send
          (const nil :OpenStruct) :new
          (kwargs
            (pair
              (sym :wrk_g)
              (block
                (send
                  (const nil :Array) :new
                  (int 30))
                (args)
                (send
                  (const nil :OpenStruct) :new
                  (kwargs
                    (pair
                      (sym :wrk_sysyy)
                      (int 0))
                    (pair
                      (sym :wrk_sysmm)
                      (int 0))
                    (pair
                      (sym :wrk_sysdd)
                      (int 0))))))))))))
          EOS
          assert_text_equal(expected_sexp, ast.to_s)
        end
      end
    end

    test "MOVE" do
      omit("not implemented yet")
      create_tempfile(<<-EOS) do |f|
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
      EOS
        ast = @parser.parse(f)

        expected_sexp = <<-EOS.chomp
(begin
  (send nil :require
    (str "ostruct"))
  (class
    (const nil :Pg1) nil
    (begin
      (def :initialize
        (args)
        (ivasgn :@wrk_area
          (send nil :new_wrk_area)))
      (def :_000_proc_sec
        (args)
        (begin
          (send
            (ivar :@wrk_area) :wrk_sysyy=
            (int 2021))
          (send
            (ivar :@wrk_area) :wrk_sysmm=
            (int 4))
          (send
            (ivar :@wrk_area) :wrk_sysdd=
            (int 21))))
      (send nil :private)
      (def :new_wrk_area
        (args)
        (send
          (const nil :OpenStruct) :new
          (kwargs
            (pair
              (sym :wrk_sysyy)
              (int 0))
            (pair
              (sym :wrk_sysmm)
              (int 0))
            (pair
              (sym :wrk_sysdd)
              (int 0))))))))
        EOS
        assert_text_equal(expected_sexp, ast.to_s)
      end
    end
  end
end
