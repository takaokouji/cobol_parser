# frozen_string_literal: true

require "lex"
begin
  old_verbose, $VERBOSE = $VERBOSE, nil
  require_relative "monkey_patches/001_lex_more_like_flex"
ensure
  $VERBOSE = old_verbose
end

require_relative "cobol_parser/version"
require_relative "cobol_parser/pp_lexer"
require_relative "cobol_parser/pp_parser"
require_relative "cobol_parser/scanner"
require_relative "cobol_parser/parser"

require "tempfile"

module CobolParser
  def self.parse(path, options = {})
    Tempfile.open do |tempfile|
      cb = Context.new
      cb.current_program = cb.build_program(nil, 0)
      cb.include_list += Array(options[:include]) if options.key?(:include)

      pp_parser = PPParser.new(cb, options)
      pp_parser.ppout = tempfile
      pp_parser.parse(path)

      tempfile.rewind

      parser = Parser.new(cb, options)
      parser.parse(tempfile)
    end
  end
end
