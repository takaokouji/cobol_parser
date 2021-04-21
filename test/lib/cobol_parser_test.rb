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
    test "COBOL to AST" do
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

        expected = <<-SEXP
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
          (ivasgn :@sys_area
            (send nil :new_sys_area))))
      (def :_000_proc_sec
        (args)
        (send nil :_100_init_sec))
      (send nil :private)
      (def :_100_init_sec
        (args)
        (begin
          (ivasgn :@wrk_area
            (send nil :new_wrk_area))
          (send
            (ivar :@wrk_area) :wrk_page=
            (int 0))
          (send
            (ivar :@sys_area) :sys_time=
            (send
              (send
                (send
                  (const nil :Time) :now) :strftime
                (str "%H%M%S00")) :to_i))
          (send nil :_110_para_hensyu_sec)))
      (def :_110_para_hensyu_sec
        (args)
        (if
          (send
            (send
              (ivar :@wrk_area) :wrk_page) :!=
            (int 0))
          (send
            (ivar :@wrk_area) :wrk_page=
            (int 10)) nil))
      (def :new_wrk_area
        (args)
        (send
          (const nil :OpenStruct) :new
          (kwargs
            (pair
              (sym :wrk_sysymd)
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
                    (int 0)))))
            (pair
              (sym :wrk_page)
              (int 0)))))
      (def :new_sys_area
        (args)
        (send
          (const nil :OpenStruct) :new
          (kwargs
            (pair
              (sym :sys_time)
              (int 0))))))))
        SEXP
        assert_text_equal(expected, ast.to_s)
      end
    end
  end
end
