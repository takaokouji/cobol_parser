# frozen_string_literal: true

require "ostruct"
require_relative "tree"
require "forwardable"

module CobolParser::TreeHelper
  extend Forwardable

  attr_reader :constants

  def to_cname(s)
    s.upcase.gsub("-", "_")
  end

  def make_const(category, val)
    CobolParser::Tree::Const.make_const(self, category, val)
  end

  def int(val)
    CobolParser::Tree::Integer.int(self, val)
  end

  def make_constant_label(name)
    p = build_label(build_reference(name), nil)
    p.need_begin = true
    p
  end

  def init_constants
    attrs = {
      error_node: make_const(:UNKNOWN, nil),
      any: make_const(:UNKNOWN, nil),
      "true": make_const(:BOOLEAN, "1"), # rubocop:disable Lint/BooleanSymbol, Lint/SymbolConversion
      "false": make_const(:BOOLEAN, "0"), # rubocop:disable Lint/BooleanSymbol, Lint/SymbolConversion
      null: make_const(:DATA_POINTER, "0"),
      zero: make_const(:NUMERIC, "&cob_zero"),
      one: make_const(:NUMERIC, "&cob_one"),
      space: make_const(:ALPHANUMERIC, "&cob_space"),
      low: make_const(:ALPHANUMERIC, "&cob_low"),
      high: make_const(:ALPHANUMERIC, "&cob_high"),
      quote: make_const(:ALPHANUMERIC, "&cob_quote"),
      int0: int(0),
      int1: int(1),
      int2: int(2),
      int3: int(3),
      int4: int(4),
      int5: int(5),
      i: (1...8).map { |i| make_const(:NUMERIC, "i#{i}") },
      standard_error_handler: make_constant_label("Default Error Handler"),
    }
    attrs[:norm_low] = attrs[:low]
    attrs[:norm_high] = attrs[:high]

    @constants = OpenStruct.new(attrs)
  end

  def_delegators :@constants,
                 :error_node,
                 :any,
                 :null,
                 :zero,
                 :one,
                 :space,
                 :low,
                 :norm_low,
                 :high,
                 :norm_high,
                 :quote,
                 :int0,
                 :int1,
                 :int2,
                 :int3,
                 :int4,
                 :int5,
                 :cb_intr_whencomp,
                 :cb_intr_pi,
                 :cb_intr_e,
                 :standard_error_handler

  def build_picture(str)
    CobolParser::Tree::Picture.build(self, str)
  end

  def build_system_name(category, token)
    CobolParser::Tree::SystemName.build(self, category, token)
  end

  def build_label(name, section)
    CobolParser::Tree::Label.build(self, name, section)
  end

  def build_cast(name, type)
    CobolParser::Tree::Cast.build(self, name, type)
  end

  def build_alphabet_name(name, type)
    CobolParser::Tree::AlphabetName.build(self, name, type)
  end

  def build_locale_name(name, list)
    CobolParser::Tree::LocaleName.build(self, name, list)
  end

  include CobolParser::Tree::List::Helper
  include CobolParser::Tree::File::Helper
  include CobolParser::Tree::Field::Helper
  include CobolParser::Tree::Reference::Helper
  include CobolParser::Tree::Literal::Helper
end
