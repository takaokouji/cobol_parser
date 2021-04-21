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
    test "MOVE" do
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
        # TODO: check AST.to_s is expected s expressions
        assert_nil(ast)
      end
    end
  end
end
