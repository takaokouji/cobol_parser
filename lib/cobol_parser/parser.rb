# frozen_string_literal: true

require "racc/parser.rb" # rubocop:disable Style/RedundantFileExtensionInRequire
require "stringio"
require_relative "scanner"
require_relative "parser.rule"

class CobolParser::Parser < Racc::Parser
  attr_reader :scanner

  def initialize(context, options = {})
    super()

    @cb = context
    @scanner = CobolParser::Scanner.new(context, options)
  end

  def parse(path_or_io)
    s = if path_or_io.is_a?(IO)
          path_or_io.read
        else
          File.read(path_or_io)
        end
    @tokens = scanner.lex(s)

    do_parse
  end

  def next_token
    token = @tokens.next
    [token.name, token.value]
  rescue StopIteration
    [false, "$end"]
  end
end
