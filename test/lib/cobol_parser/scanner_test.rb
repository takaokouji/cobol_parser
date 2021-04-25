# frozen_string_literal: true

require "test_helper"
require "stringio"

class CobolParser::ScannerTest < Test::Unit::TestCase
  def create_scanner
    @context = create_context
    options = create_common_options
    CobolParser::Scanner.new(@context, options)

    @context.current_program = CobolParser::Tree::Program.new(@context, nil, 0)
    @context.cb_source_line = 1
  end

  def assert_tree_equal(expected_class, expected_attributes, actual, msg = nil)
    assert_equal(expected_class, actual.class, msg)
    msg += ": " if msg
    expected_attributes.each do |name, value|
      assert_equal(
        value,
        actual.instance_variable_get("@#{name}"),
        "#{msg}CobolParser::Tree object attribute is different: #{name}"
      )
    end
  end

  def assert_tokens(expected_tokens, actual_tokens)
    expected_tokens.each.with_index do |expected_token, i|
      msg = "expected_tokens[#{i}]"
      token = assert_nothing_raised(msg) { actual_tokens.next }

      if expected_token.key?(:value)
        assert_token_equal(expected_token[:name], expected_token[:value], token, msg)
      else
        assert_equal(expected_token[:name], token.name, "#{msg}[:name]")
        assert_tree_equal(expected_token[:class], expected_token[:attributes], token.value, msg)
      end

      if expected_token.key?(:line)
        assert_equal(expected_token[:line], token.value.source_line, "#{msg}[:line]")
      end
    end

    assert_raise(StopIteration) { actual_tokens.next }
  end

  sub_test_case "#each_token" do
    test "line directive" do
      yyin = StringIO.new(<<-EOS)
# 1 "test/fixtures/copy/ADDRESS.INC"
"string literal"
      EOS

      scanner = create_scanner
      scanner.current_buffer = scanner.yy_create_buffer(yyin)

      tokens = scanner.each_token

      token = assert_nothing_raised { tokens.next }
      assert_equal("test/fixtures/copy/ADDRESS.INC", token.value.source_file)
      assert_equal(2, token.value.source_line)

      assert_raise(StopIteration) { tokens.next }
    end

    test "IDENTIFICATION DIVISION" do
      yyin = StringIO.new(<<-EOS)
IDENTIFICATION DIVISION.
PROGRAM-ID. PG1.
      EOS

      scanner = create_scanner
      scanner.current_buffer = scanner.yy_create_buffer(yyin)

      expected_tokens = [
        {
          name: :PROGRAM_ID,
          value: "PROGRAM-ID",
        },
        {
          name: ".",
          value: nil,
        },
        {
          name: :PROGRAM_NAME,
          class: CobolParser::Tree::Literal,
          attributes: {
            category: :ALPHANUMERIC,
            data: "PG1",
          },
          line: 2,
        },
        {
          name: ".",
          value: nil,
        },
      ]

      tokens = scanner.each_token
      assert_tokens(expected_tokens, tokens)
    end

    test "string literal, X string literal" do
      yyin = StringIO.new(<<-EOS)
"string literal"
X"01"
X'0D'
X"abcd"
X"GG"
""
      EOS

      scanner = create_scanner
      scanner.current_buffer = scanner.yy_create_buffer(yyin)

      expected_tokens = [
        {
          name: :LITERAL,
          class: CobolParser::Tree::Literal,
          attributes: {
            category: :ALPHANUMERIC,
            data: "string literal",
          },
          line: 1,
        },
        {
          name: :LITERAL,
          class: CobolParser::Tree::Literal,
          attributes: {
            category: :ALPHANUMERIC,
            data: ["01"].pack("H*"),
          },
          line: 2,
        },
        {
          name: :LITERAL,
          class: CobolParser::Tree::Literal,
          attributes: {
            category: :ALPHANUMERIC,
            data: ["0D"].pack("H*"),
          },
          line: 3,
        },
        {
          name: :LITERAL,
          class: CobolParser::Tree::Literal,
          attributes: {
            category: :ALPHANUMERIC,
            data: ["abcd"].pack("H*"),
          },
          line: 4,
        },
        {
          name: :LITERAL,
          value: @cb.constants.error_node,
        },
        {
          name: :LITERAL,
          class: CobolParser::Tree::Literal,
          attributes: {
            category: :ALPHANUMERIC,
            data: " ",
          },
          line: 6,
        },
      ]

      tokens = scanner.each_token
      assert_tokens(expected_tokens, tokens)
    end

    test "numeric literal, H numeric literal" do
      yyin = StringIO.new(<<-EOS)
(10)
-123.456
H'0D'
H"abcd"
H"GG"
+123.4
      EOS

      scanner = create_scanner
      scanner.current_buffer = scanner.yy_create_buffer(yyin)

      expected_tokens = [
        {
          name: "(",
          value: "(",
        },
        {
          name: :LITERAL,
          class: CobolParser::Tree::Literal,
          attributes: {
            category: :NUMERIC,
            data: "10",
            sign: :UNSIGNED,
            scale: 0,
          },
          line: 1,
        },
        {
          name: ")",
          value: ")",
        },
        {
          name: :LITERAL,
          class: CobolParser::Tree::Literal,
          attributes: {
            category: :NUMERIC,
            data: "123456",
            sign: :NEGATIVE,
            scale: 3,
          },
          line: 2,
        },
        {
          name: :LITERAL,
          class: CobolParser::Tree::Literal,
          attributes: {
            category: :NUMERIC,
            data: "0D".to_i(16).to_s,
            sign: :UNSIGNED,
            scale: 0,
          },
          line: 3,
        },
        {
          name: :LITERAL,
          class: CobolParser::Tree::Literal,
          attributes: {
            category: :NUMERIC,
            data: "abcd".to_i(16).to_s,
            sign: :UNSIGNED,
            scale: 0,
          },
          line: 4,
        },
        {
          name: :LITERAL,
          value: @cb.constants.error_node,
        },
        {
          name: :LITERAL,
          class: CobolParser::Tree::Literal,
          attributes: {
            category: :NUMERIC,
            data: "1234",
            sign: :POSITIVE,
            scale: 1,
          },
          line: 6,
        },
      ]

      tokens = scanner.each_token
      assert_tokens(expected_tokens, tokens)
    end

    test "picture state" do
      yyin = StringIO.new(<<-EOS)
PIC 9(02).
PICTURE IS XXXXX.
PIC 9(5)V99.
PIC ZZZ9.9999.
      EOS

      scanner = create_scanner
      scanner.current_buffer = scanner.yy_create_buffer(yyin)

      expected_tokens = [
        {
          name: :PICTURE,
          class: CobolParser::Tree::Picture,
          attributes: {
            size: 2,
            orig: "9(02)",
            str: nil,
            field_category: :NUMERIC,
            digits: 2,
            scale: 0,
            have_sign: 0,
          },
        },
        {
          name: ".",
          value: nil,
        },
        {
          name: :PICTURE,
          class: CobolParser::Tree::Picture,
          attributes: {
            size: 5,
            orig: "XXXXX",
            str: nil,
            field_category: :ALPHANUMERIC,
            digits: 0,
            scale: 0,
            have_sign: 0,
          },
        },
        {
          name: ".",
          value: nil,
        },
        {
          name: :PICTURE,
          class: CobolParser::Tree::Picture,
          attributes: {
            size: 7,
            orig: "9(5)V99",
            str: nil,
            field_category: :NUMERIC,
            digits: 7,
            scale: 2,
            have_sign: 0,
          },
        },
        {
          name: ".",
          value: nil,
        },
        {
          name: :PICTURE,
          class: CobolParser::Tree::Picture,
          attributes: {
            size: 9,
            orig: "ZZZ9.9999",
            str: "Z(3)9(1).(1)9(4)",
            field_category: :NUMERIC_EDITED,
            digits: 8,
            scale: 4,
            have_sign: 0,
          },
        },
        {
          name: ".",
          value: nil,
        },
      ]

      tokens = scanner.each_token
      assert_tokens(expected_tokens, tokens)
    end

    test "DATA DIVISION" do
      yyin = StringIO.new(<<-EOS)
DATA DIVISION.
WORKING-STORAGE SECTION.
01 WRK-AREA.
 03 WRK-SYSYMD.
 05 WRK-SYSYY PIC 9(04).
      EOS

      scanner = create_scanner
      scanner.current_buffer = scanner.yy_create_buffer(yyin)

      %w[
        WRK-AREA
        WRK-SYSYMD
        WRK-SYSYY
        01
        03
        05
      ].each do |name|
        n = name.upcase
        w = CobolParser::Word.new(name)
        @cb.current_program.word_table[n] ||= []
        @cb.current_program.word_table[n] << w
      end

      expected_tokens = [
        {
          name: :DATA,
          value: nil,
        },
        {
          name: :DIVISION,
          value: "DIVISION",
        },
        {
          name: ".",
          value: nil,
        },
        {
          name: :WORKING_STORAGE,
          value: nil,
        },
        {
          name: :SECTION,
          value: nil,
        },
        {
          name: ".",
          value: nil,
        },
        {
          name: :WORD,
          class: CobolParser::Tree::Reference,
          attributes: {
            category: :UNKNOWN,
            word: @cb.current_program.word_table["01"].first,
          },
          line: 3,
        },
        {
          name: :WORD,
          class: CobolParser::Tree::Reference,
          attributes: {
            category: :UNKNOWN,
            word: @cb.current_program.word_table["WRK-AREA"].first,
          },
          line: 3,
        },
        {
          name: ".",
          value: nil,
        },
        {
          name: :WORD,
          class: CobolParser::Tree::Reference,
          attributes: {
            category: :UNKNOWN,
            word: @cb.current_program.word_table["03"].first,
          },
          line: 4,
        },
        {
          name: :WORD,
          class: CobolParser::Tree::Reference,
          attributes: {
            category: :UNKNOWN,
            word: @cb.current_program.word_table["WRK-SYSYMD"].first,
          },
          line: 4,
        },
        {
          name: ".",
          value: nil,
        },
        {
          name: :WORD,
          class: CobolParser::Tree::Reference,
          attributes: {
            category: :UNKNOWN,
            word: @cb.current_program.word_table["05"].first,
          },
          line: 5,
        },
        {
          name: :WORD,
          class: CobolParser::Tree::Reference,
          attributes: {
            category: :UNKNOWN,
            word: @cb.current_program.word_table["WRK-SYSYY"].first,
          },
          line: 5,
        },
        {
          name: :PICTURE,
          class: CobolParser::Tree::Picture,
          attributes: {
            size: 4,
            orig: "9(04)",
            str: nil,
            field_category: :NUMERIC,
            digits: 4,
            scale: 0,
            have_sign: 0,
          },
        },
        {
          name: ".",
          value: nil,
        },
      ]

      tokens = scanner.each_token
      assert_tokens(expected_tokens, tokens)
    end
  end
end
