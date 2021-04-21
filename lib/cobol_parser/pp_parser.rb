# frozen_string_literal: true

require "racc/parser.rb" # rubocop:disable Style/RedundantFileExtensionInRequire
require "stringio"
require_relative "pp_lexer"
require_relative "pp_parser.rule"

class CobolParser::PPParser < Racc::Parser
  extend Forwardable

  attr_reader :pp_lexer

  def_delegator :@pp_lexer, :output_file, :ppout
  def_delegator :@pp_lexer, :output_file=, :ppout=

  def initialize(context, options = {})
    super()

    @cb = context
    @pp_lexer = CobolParser::PPLexer.new(context, options)
    @pp_lexer.output_file = StringIO.new
  end

  def parse(path)
    pp_lexer.open(path)
    @tokens = pp_lexer.each_token

    do_parse

    ppout.rewind
    ppout.read
  end

  def next_token
    token = @tokens.next
    [token.name, token.value]
  rescue StopIteration
    [false, "$end"]
  end

  private

  def fix_filename(name)
    name.sub(/^["'](.*)["']$/, "\\1")
  end
end
