# frozen_string_literal: true

require "lex"
begin
  old_verbose, $VERBOSE = $VERBOSE, nil
  require_relative "monkey_patches/001_lex_more_like_flex"
ensure
  $VERBOSE = old_verbose
end

require "tempfile"

module CobolParser
  COB_DISPLAY_SIGN_EBCDIC = :EBCDIC
  COB_DISPLAY_SIGN_ASCII = :ASCII

  COB_MAX_SUBSCRIPTS = 16

  CB_FORMAT_FIXED = :FIXED
  CB_FORMAT_FREE = :FREE

  CB_ASSIGN_COBOL2002 = :COBOL2002 # COBOL 2002 standard
  CB_ASSIGN_MF = :MF # Micro Focus COBOL compatibility
  CB_ASSIGN_IBM = :IBM # IBM COBOL compatibility

  CB_BYTEORDER_NATIVE = :NATIVE
  CB_BYTEORDER_BIG_ENDIAN = :BIG_ENDIAN

  CB_BINARY_SIZE_2_4_8 = :"2_4_8" # 2,4,8 bytes
  CB_BINARY_SIZE_1_2_4_8 = "1_2_4_8" # 1,2,4,8 bytes
  CB_BINARY_SIZE_1__8 = :"1__8" # 1,2,3,4,5,6,7,8 bytes

  CB_OPERATION_READ = :READ
  CB_OPERATION_WRITE = :WRITE
  CB_OPERATION_ASSIGN = :ASSIGN

  CB_OK = :OK
  CB_WARNING = :WARNING
  CB_ARCHAIC = :ARCHAIC
  CB_OBSOLETE = :OBSOLETE
  CB_SKIP = :SKIP
  CB_IGNORE = :IGNORE
  CB_ERROR = :ERROR
  CB_UNCONFORMABL = :UNCONFORMABL

  def self.parse(path, options = {})
    Tempfile.open do |tempfile|
      cb = Context.new
      cb.include_list += Array(options[:include]) if options.key?(:include)
      cb.extension_list += Array(options[:extension]) if options.key?(:extension)

      pp_parser = PPParser.new(cb, options)
      pp_parser.ppout = tempfile
      pp_parser.parse(path)

      tempfile.rewind

      parser = Parser.new(cb, options)
      parser.parse(tempfile)
    end
  end
end

require_relative "cobol_parser/version"
require_relative "cobol_parser/pp_lexer"
require_relative "cobol_parser/pp_parser"
require_relative "cobol_parser/scanner"
require_relative "cobol_parser/parser"
