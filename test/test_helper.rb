# frozen_string_literal: true

if ENV["COVERAGE"]
  require "simplecov"

  excludes = %w[
    lib/cobol_parser/pp_parser.rule.rb
  ].map { |x| File.expand_path(x, File.join(__dir__, "..")) }
  SimpleCov.start do
    enable_coverage :branch

    add_filter { |src| excludes.include?(src.filename) }
  end
end

require "test/unit"
require "cobol_parser"

Parser::Builders::Default.emit_lambda              = true
Parser::Builders::Default.emit_procarg0            = true
Parser::Builders::Default.emit_encoding            = true
Parser::Builders::Default.emit_index               = true
Parser::Builders::Default.emit_arg_inside_procarg0 = true
Parser::Builders::Default.emit_forward_arg         = true
Parser::Builders::Default.emit_kwargs              = true
Parser::Builders::Default.emit_match_pattern       = true

class Test::Unit::TestCase
  def fixture_path(path)
    File.expand_path(path, File.join(__dir__, "fixtures"))
  end

  def create_context
    cb = CobolParser::Context.new
    cb.current_program = cb.build_program(nil, 0)
    cb.include_list << "test/fixtures/copy"
    cb
  end

  def create_common_options
    {
      debug: ENV["COBOL_PARSER_DEBUG"],
      log_path: ENV["COBOL_PARSER_LOG_PATH"],
    }
  end

  def root_dir
    File.expand_path("..", __dir__)
  end

  def assert_token_equal(expected_name, expected_value, actual, message = nil)
    assert_equal(expected_name, actual.name, message)
    if expected_value.is_a?(Regexp)
      assert_match(expected_value, actual.value, message)
    else
      assert_equal(expected_value, actual.value, message)
    end
  end

  def assert_ast_equal(expected, actual, message = nil)
    assert_text_equal(expected.to_s, actual.to_s, message)
  end

  def assert_text_equal(expected_text, actual_text, message = nil)
    if expected_text == actual_text
      assert_true(true)
      return
    end

    # :nocov:
    diff = Test::Unit::Diff.diff(
      Test::Unit::Diff::UnifiedDiffer, expected_text, actual_text,
      from_label: "expected", to_label: "actual"
    )
    assert_empty(diff, message)
    # :nocov:
  end

  def create_tempfile(source)
    Tempfile.open do |f|
      f.write(source)
      f.rewind

      yield f
    end
  end
end
