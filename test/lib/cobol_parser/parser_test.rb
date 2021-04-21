# frozen_string_literal: true

require "test_helper"
require "tempfile"

class CobolParser::ParserTest < Test::Unit::TestCase
  setup do
    @cb = create_context
    @options = create_common_options
    @parser = CobolParser::Parser.new(@cb, @options)
  end

  sub_test_case "#parse" do
    test "IDENTIFICATION DIVISION" do
      create_tempfile(<<-EOS) do |f|
# 1 "PG1.CBL"
IDENTIFICATION DIVISION.
PROGRAM-ID. PG1.
      EOS
        ast = @parser.parse(f)

        expected_sexp = <<-EOS.chomp
(begin
  (send nil :require
    (str "ostruct"))
  (class
    (const nil "Pg1") nil
    (def :initialize
      (args))))
        EOS
        assert_equal(expected_sexp, ast.to_s)
      end
    end

    test "DATA DIVISION" do
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
      EOS
        ast = @parser.parse(f)

        expected_sexp = <<-EOS
(begin
  (send nil :require
    (str "ostruct"))
  (class
    (const nil "Pg1") nil
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
          (hash
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
        assert_equal(expected_sexp, ast.to_s)
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

        expected_sexp = <<-EOS
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
          (hash
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
        assert_equal(expected_sexp, ast.to_s)
      end
    end
  end
end
