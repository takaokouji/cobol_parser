# frozen_string_literal: true

require "forwardable"
require "ostruct"
require "set"
require_relative "error_helper"
require_relative "reserved_helper"
require_relative "program"
require_relative "type_check_helper"
require_relative "tree_helper"
require_relative "config"
require_relative "warning"
require_relative "flag"

class CobolParser::Context
  extend Forwardable

  include CobolParser::ErrorHelper
  include CobolParser::ReservedHelper
  include CobolParser::Program::Helper
  include CobolParser::TypeCheckHelper
  include CobolParser::TreeHelper

  FORMAT = {
    FIXED: 0,
    FREE: 1,
  }.freeze

  INVALID_NAMES = Set.new([
                            "NULL",
                            "L_initextern",
                            "LRET_initextern",
                            "P_switch",
                            "alignof",
                            "asm",
                            "auto",
                            "break",
                            "case",
                            "char",
                            "const",
                            "continue",
                            "default",
                            "do",
                            "double",
                            "else",
                            "enum",
                            "exit_program",
                            "extern",
                            "float",
                            "for",
                            "frame_pointer",
                            "frame_stack",
                            "goto",
                            "if",
                            "inline",
                            "int",
                            "long",
                            "offsetof",
                            "register",
                            "restrict",
                            "return",
                            "short",
                            "signed",
                            "sizeof",
                            "static",
                            "struct",
                            "switch",
                            "typedef",
                            "typeof",
                            "union",
                            "unsigned",
                            "void",
                            "volatile",
                            "_Bool",
                            "_Complex",
                            "_Imaginary",
                          ])

  ReplaceListItem = Struct.new(:old_text, :new_text, keyword_init: true)

  attr_accessor :id
  attr_accessor :attr_id
  attr_accessor :literal_id
  attr_accessor :field_id
  attr_accessor :storage_id
  attr_accessor :flag_main

  attr_accessor :alt_ebcdic
  attr_accessor :optimize_flag
  attr_accessor :has_external

  # Global variables
  attr_accessor :current_program
  attr_accessor :current_statement
  attr_accessor :current_section
  attr_accessor :current_paragraph
  attr_accessor :functions_are_all
  attr_accessor :non_const_word

  attr_accessor :source_format
  attr_accessor :source_line
  attr_accessor :source_file

  attr_accessor :depend_file
  attr_accessor :depend_list

  attr_accessor :extension_list
  attr_accessor :include_list

  attr_accessor :norestab

  attr_accessor :needs_01

  attr_reader :max_subscripts

  def_delegators :@config, :verify, *CobolParser::Config.types.values.map { |x| x[:var] }
  def_delegators :@warning, *CobolParser::Warning.warnings.values.map { |x| x[:var] }
  def_delegators :@flag, *CobolParser::Flag.flags.values.map { |x| x[:var] }

  def initialize
    @cb = self

    @config = CobolParser::Config.new(self)
    @warning = CobolParser::Warning.new(self)
    @flag = CobolParser::Flag.new(self)

    @id = 1
    @attr_id = 1
    @literal_id = 1
    @field_id = 1
    @storage_id = 1
    @flag_main = false

    @warningcount = 0
    @errorcount = 0
    @alt_ebcdic = false
    @optimize_flag = false
    @has_external = false

    @needs_01 = false

    @functions_are_all = false
    @non_const_word = 0

    @source_format = FORMAT[:FIXED]
    @source_line = 0
    @source_file = nil

    init_constants

    @depend_file = nil
    @depend_list = []

    @extension_list = %w[
      .CPY
      .CBL
      .COB
      .cpy
      .cbl
      .cob
    ]
    @extension_list << ""
    @include_list = []

    @current_section = nil
    @current_paragraph = nil

    @norestab = []

    @max_subscripts = 16
  end

  def fixed_source_format?
    source_format == FORMAT[:FIXED]
  end

  def free_source_format?
    source_format == FORMAT[:FREE]
  end

  def replace_list_add(replace_list, old_text, new_text)
    replace_list ||= []
    replace_list << ReplaceListItem.new(old_text: old_text, new_text: new_text)
    replace_list
  end

  def check_valid_name(name)
    INVALID_NAMES.include?(name)
  end
end
