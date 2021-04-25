# frozen_string_literal: true

require "forwardable"
require "set"
require_relative "attribute_helper"
require_relative "config"
require_relative "warning"
require_relative "flag"

module CobolParser
  class Context
    extend Forwardable
    extend AttributeHelper

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

    # cobc/tree.h
    attribute :non_const_word
    attribute :cb_needs_01

    attribute :cb_error_node
    attribute :cb_any
    attribute :cb_true
    attribute :cb_false
    attribute :cb_null
    attribute :cb_zero
    attribute :cb_one
    attribute :cb_space
    attribute :cb_low
    attribute :cb_norm_low
    attribute :cb_high
    attribute :cb_norm_high
    attribute :cb_quote
    attribute :cb_int0
    attribute :cb_int1
    attribute :cb_int2
    attribute :cb_int3
    attribute :cb_int4
    attribute :cb_int5
    attribute :cb_i
    attribute :cb_standard_error_handler
    attribute :cb_intr_whencomp
    attribute :cb_intr_pi
    attribute :cb_intr_e

    attr_accessor :pp_lexer
    attr_accessor :pp_parser
    attr_accessor :scanner
    attr_accessor :parser

    def_delegators :@config, *Config.types.values.map { |x| x[:var] }
    def_delegators :@warning, *Warning.warnings.values.map { |x| x[:var] }
    def_delegators :@flag, *Flag.flags.values.map { |x| x[:var] }

    def initialize
      @context = self

      @config = Config.new
      @warning = Warning.new
      @flag = Flag.new

      # cobc/cobc.h
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

      @norestab = []

      # cobc/parser.y
      @current_program = nil
      @current_statement = nil
      @current_section = nil
      @current_paragraph = nil
      @functions_are_all = false
      @non_const_word = false

      # cobc/field.c
      @cb_needs_01 = false

      # lexers, parsers
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
      def_delegators :@context, *Config.types.values.map { |x| x[:var] }
      def_delegators :@context, *Warning.warnings.values.map { |x| x[:var] }
      def_delegators :@context, *Flag.flags.values.map { |x| x[:var] }
    end
  end
end
