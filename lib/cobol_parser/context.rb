# frozen_string_literal: true

require "forwardable"
require "ostruct"
require "set"
require_relative "attribute_helper"
require_relative "error_helper"
require_relative "reserved_helper"
require_relative "type_check_helper"
require_relative "tree_helper"
require_relative "config"
require_relative "warning"
require_relative "flag"

module CobolParser
  class Context
    extend Forwardable

    extend AttributeHelper

    include CobolParser::ErrorHelper
    include CobolParser::ReservedHelper
    include CobolParser::TypeCheckHelper
    include CobolParser::TreeHelper

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

    attribute :cb_source_format
    attribute :cb_display_sign

    attribute :cb_id
    attribute :cb_attr_id
    attribute :cb_literal_id
    attribute :cb_field_id
    attribute :cb_storage_id
    attribute :cb_flag_main

    attribute :warningcount
    attribute :errorcount
    attribute :alt_ebcdic
    attribute :optimize_flag
    attribute :has_external

    attribute :cb_oc_build_stamp
    attribute :cb_source_line
    attribute :cb_source_file

    attribute :cob_config_dir

    attribute :source_name
    attribute :demangle_name
    attribute :cb_storage_file
    attribute :cb_storage_file_name

    attribute :cb_listing_file
    attribute :cb_depend_file
    attribute :cb_depend_target
    attribute :cb_depend_list
    attribute :cb_include_list
    attribute :cb_extension_list

    attribute :current_program
    attribute :current_statement
    attribute :current_section
    attribute :current_paragraph
    attribute :functions_are_all

    attribute :norestab

    # TODO: what is it?
    attribute :sending_id
    # TODO: what is it?
    attribute :suppress_warn

    attr_accessor :non_const_word

    attr_accessor :needs_01

    attr_accessor :pp_lexer
    attr_accessor :pp_parser
    attr_accessor :scanner
    attr_accessor :parser

    def_delegators :@config, *CobolParser::Config.types.values.map { |x| x[:var] }
    def_delegators :@warning, *CobolParser::Warning.warnings.values.map { |x| x[:var] }
    def_delegators :@flag, *CobolParser::Flag.flags.values.map { |x| x[:var] }

    def initialize
      @cb = self
      @context = self

      @config = CobolParser::Config.new
      @warning = CobolParser::Warning.new
      @flag = CobolParser::Flag.new

      @cb_source_format = CB_FORMAT_FIXED
      @cb_display_sign = COB_DISPLAY_SIGN_ASCII

      @cb_id = 1
      @cb_attr_id = 1
      @cb_literal_id = 1
      @cb_field_id = 1
      @cb_storage_id = 1
      @cb_flag_main = false

      @errorcount = 0
      @warningcount = 0
      @alt_ebcdic = false
      @optimize_flag = false

      @cb_source_file = nil
      @cb_oc_build_stamp = nil
      @source_name = nil
      @demangle_name = nil
      @cb_source_line = 0

      @cb_storage_file = nil
      @cb_storage_file_name = nil

      @cb_listing_file = nil
      @cb_depend_file = nil
      @cb_depend_target = nil
      @cb_depend_list = []
      @cb_include_list = []
      @cb_extension_list = [""] + %w[
        .CPY
        .CBL
        .COB
        .cpy
        .cbl
        .cob
      ]

      @cob_config_dir = nil

      @current_section = nil
      @current_paragraph = nil

      @has_external = false

      # TODO: move define place, rename cb_needs_01
      @needs_01 = false

      # TODO: move define place
      @non_const_word = 0

      @norestab = []

      @pp_lexer = nil
      @pp_parser = nil
      @scanner = nil
      @parser = nil
    end

    def method_storage
      method_name = caller[0].slice(/`([^']+)'/, 1).to_sym
      @method_storage ||= {}
      @method_storage[method_name] ||= {}
    end

    def check_valid_name(name)
      INVALID_NAMES.include?(name)
    end

    module Helper
      extend Forwardable

      def_delegators :@context, *Context.read_attributes
      def_delegators :@context, *Context.write_attributes.map { |x| "#{x}=".to_sym }
      def_delegators :@context, *CobolParser::Config.types.values.map { |x| x[:var] }
      def_delegators :@context, *CobolParser::Warning.warnings.values.map { |x| x[:var] }
      def_delegators :@context, *CobolParser::Flag.flags.values.map { |x| x[:var] }
    end
  end
end
