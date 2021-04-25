# frozen_string_literal: true

require "racc/parser.rb" # rubocop:disable Style/RedundantFileExtensionInRequire
require "stringio"
require_relative "context"
require_relative "pp_lexer"
require_relative "pp_parser.rule"

module CobolParser
  class PPParser < Racc::Parser
    extend Forwardable

    include Context::Helper
    include PPLexer::Helper

    ReplaceListItem = Struct.new(:old_text, :new_text, keyword_init: true)

    def initialize(context, options = {})
      super()

      @context = context
      @context.pp_parser = self

      PPLexer.new(context, options)
      self.ppout = StringIO.new
    end

    def parse(path)
      ppopen(path)
      @tokens = pplex

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
