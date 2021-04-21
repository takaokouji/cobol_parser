# frozen_string_literal: true

require "test_helper"

class CobolParserTest < Test::Unit::TestCase
  setup do
    @options = create_common_options
  end

  def create_tempfile(source)
    Tempfile.open do |f|
      f.write(source)
      f.rewind
      f.close(false)

      yield f
    end
  end

  sub_test_case ".parse" do
    test "first" do
      create_tempfile(<<-EOS) do |f|
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             PG1.
      *
       DATA                    DIVISION.
       WORKING-STORAGE             SECTION.
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-PAGE                PIC 9(03).
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
       PROCEDURE                   DIVISION.
       000-PROC-SEC                SECTION.
           PERFORM 100-INIT-SEC
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
       100-INIT-SEC                SECTION.
           INITIALIZE                  WRK-AREA
           MOVE    ZERO                TO  WRK-PAGE
           ACCEPT  SYS-TIME            FROM    TIME
           PERFORM 110-PARA-HENSYU-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
       110-PARA-HENSYU-SEC                   SECTION.
           IF      WRK-PAGE    NOT =   ZERO
               MOVE    10                   TO  WRK-PAGE
           END-IF
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      EOS

        ast = CobolParser.parse(f.path)
        # TODO: check AST.to_s is expected s expressions
        assert_nil(ast)
      end
    end
  end
end
