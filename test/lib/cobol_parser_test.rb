# frozen_string_literal: true

require "test_helper"
require "parser/current"

class CobolParserTest < Test::Unit::TestCase
  setup do
    @options = create_common_options
  end

  def parse_and_assert_ast_equal(pp_cobol_code, ruby_code)
    actual = create_tempfile(pp_cobol_code) { |f|
      f.close(false)
      CobolParser.parse(f.path)
    }
    expected = ::Parser::CurrentRuby.parse(ruby_code)
    assert_ast_equal(expected, actual)
  end

  sub_test_case ".parse" do
    test "COBOL to AST" do
      omit("not implemented yet")
      parse_and_assert_ast_equal(<<-COBOL, <<-RUBY)
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
           PERFORM 110-PARA-HENSHU-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
       110-PARA-HENSHU-SEC                   SECTION.
           IF      WRK-PAGE    NOT =   ZERO
               MOVE    10                   TO  WRK-PAGE
           END-IF
           .
       110-PARA-HENSHU-EXT.
           EXIT.
      COBOL
require "ostruct"

class Pg1
  def initialize
    @wrk_area = new_wrk_area
    @sys_area = new_sys_area
  end

  def _000_proc_sec
    _100_init_sec
  end

  private

  def _100_init_sec
    @wrk_area = new_wrk_area
    @wrk_area.wrk_page = 0
    @sys_area.sys_tiime = Time.now.strftime("%H%M%S00").to_i
    _110_para_henshu_sec
  end

  def _110_para_henshu_sec
    if @wrk_area.wrk_page != 0
      @wrk_area.wrk_page = 10
    end
  end

  def new_wrk_area
    OpenStruct.new(
      wrk_sysymd: OpenStruct.new(
        wrk_sysyy: 0,
        wrk_sysmm: 0,
        wrk_sysdd: 0
      ),
      wrk_page: 0
    )
  end

  def new_sys_area
    OpenStruct.new(
      sys_time: 0
    )
  end
end
      RUBY
    end
  end
end
