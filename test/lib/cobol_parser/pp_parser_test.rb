# frozen_string_literal: true

require "test_helper"
require "tempfile"

class CobolParser::PPParserTest < Test::Unit::TestCase
  setup do
    @cb = create_context
    @options = create_common_options
    @pp_parser = CobolParser::PPParser.new(@cb, @options)
  end

  sub_test_case "#parse" do
    test "COPY" do
      create_tempfile(<<-COB) do |f|
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             PG1.
      *
       DATA                    DIVISION.
       WORKING-STORAGE             SECTION.
       COPY "ADDRESS.INC".
      *
       PROCEDURE                   DIVISION.
       000-PROC-SEC                SECTION.
      COB
        f.close(false)

        result = @pp_parser.parse(f.path)

        expected = <<-COB
# 1 "#{f.path}"
IDENTIFICATION DIVISION.
PROGRAM-ID. PG1.

DATA DIVISION.
WORKING-STORAGE SECTION.

# 1 "test/fixtures/copy/ADDRESS.INC"

01 ADDRESS-AREA.

 05 ADDRESS-NAME PIC X(50).

 05 ADDRESS-KANA PIC X(50).

 05 ADDRESS-CODE PIC X(7).

 05 ADDRESS-ADDRESS1 PIC X(100).

 05 ADDRESS-ADDRESS2 PIC X(100).
# 6 "#{f.path}"


PROCEDURE DIVISION.
000-PROC-SEC SECTION.
        COB
        assert_text_equal(expected, result)
      end
    end

    test "COPY REPLACING" do
      create_tempfile(<<-COB) do |f|
       DATA                    DIVISION.
       WORKING-STORAGE             SECTION.
       COPY "ADDRESS.INC" REPLACING //ADDRESS//
                          BY        //ADR//.
      COB
        f.close(false)

        result = @pp_parser.parse(f.path)

        expected = <<-COB
# 1 "#{f.path}"
DATA DIVISION.
WORKING-STORAGE SECTION.


# 1 "test/fixtures/copy/ADDRESS.INC"

01 ADR-AREA.

 05 ADR-NAME PIC X(50).

 05 ADR-KANA PIC X(50).

 05 ADR-CODE PIC X(7).

 05 ADR-ADDRESS1 PIC X(100).

 05 ADR-ADDRESS2 PIC X(100).
# 4 "#{f.path}"
        COB
        assert_text_equal(expected, result)
      end
    end
  end
end
