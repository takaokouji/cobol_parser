# frozen_string_literal: true

require "racc/parser.rb" # rubocop:disable Style/RedundantFileExtensionInRequire
require "stringio"
require_relative "pp_lexer"
require_relative "pp_parser.rule"
require_relative "context"

module CobolParser
  class PPParser < Racc::Parser
    extend Forwardable

    include Context::Helper

    ReplaceListItem = Struct.new(:old_text, :new_text, keyword_init: true)

    attr_reader :pp_lexer

    def_delegator :@pp_lexer, :output_file, :ppout
    def_delegator :@pp_lexer, :output_file=, :ppout=

    def initialize(context, options = {})
      super()

      @context = context
      @pp_lexer = PPLexer.new(context, options)
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

    def cb_replace_list_add(replace_list, old_text, new_text)
      replace_list ||= []
      replace_list << ReplaceListItem.new(old_text: old_text, new_text: new_text)
      replace_list
    end
  end
end
