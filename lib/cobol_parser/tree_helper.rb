# frozen_string_literal: true

require "ostruct"
require_relative "tree"

module CobolParser::TreeHelper
  attr_reader :constants

  def make_const(category, val)
    CobolParser::Tree::Const.new(self, category: category, val: val)
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
      # TODO: cb_int
      # cb_int0 = cb_int (0);
      # cb_int1 = cb_int (1);
      # cb_int2 = cb_int (2);
      # cb_int3 = cb_int (3);
      # cb_int4 = cb_int (4);
      # cb_int5 = cb_int (5);
      i: (1...8).map { |i| make_const(:NUMERIC, "i#{i}") },
      # TODO: make_constant_label
      # cb_standard_error_handler = make_constant_label ("Default Error Handler");
    }
    attrs[:norm_low] = attrs[:low]
    attrs[:norm_high] = attrs[:high]

    @constants = OpenStruct.new(attrs)
  end

  def build_literal(category, data)
    CobolParser::Tree::Literal.new(self, category: category, data: data)
  end

  def build_numeric_literal(sign, data, scale)
    p = build_literal(:NUMERIC, data)
    p.sign = sign
    p.scale = scale
    p
  end

  def build_alphanumeric_literal(data)
    build_literal(:ALPHANUMERIC, data)
  end

  def build_reference(name)
    CobolParser::Tree::Reference.new(self, category: :UNKNOWN, word: name)
  end

  def build_picture(str)
    CobolParser::Tree::Picture.new(self, category: :UNKNOWN, orig: str)
  end

  def build_system_name(category, token)
    CbolParser::Tree::SystemName.new(self, category: :UNKNOWN, system_category: category, token: token)
  end
end
