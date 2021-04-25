# frozen_string_literal: true

require_relative "gettext_helper"

module CobolParser
  module TreeHelper
    include Tree::AlphabetName::Helper
    include Tree::BinaryOp::Helper
    include Tree::Cast::Helper
    include Tree::Field::Helper
    include Tree::File::Helper
    include Tree::Integer::Helper
    include Tree::Label::Helper
    include Tree::List::Helper
    include Tree::Literal::Helper
    include Tree::LocaleName::Helper
    include Tree::Perform::Helper
    include Tree::PerformVarying::Helper
    include Tree::Picture::Helper
    include Tree::Program::Helper
    include Tree::Reference::Helper
    include Tree::SystemName::Helper

    def init_constants
      self.cb_error_node = make_const(:UNKNOWN, nil)
      self.cb_any = make_const(:UNKNOWN, nil)
      self.cb_true = make_const(:BOOLEAN, "1")
      self.cb_false = make_const(:BOOLEAN, "0")
      self.cb_null = make_const(:DATA_POINTER, "0")
      self.cb_zero = make_const(:NUMERIC, "&cob_zero")
      self.cb_one = make_const(:NUMERIC, "&cob_one")
      self.cb_space = make_const(:ALPHANUMERIC, "&cob_space")
      self.cb_low = make_const(:ALPHANUMERIC, "&cob_low")
      self.cb_norm_low = cb_low
      self.cb_high = make_const(:ALPHANUMERIC, "&cob_high")
      self.cb_norm_high = cb_high
      self.cb_quote = make_const(:ALPHANUMERIC, "&cob_quote")
      self.cb_int0 = cb_int(0)
      self.cb_int1 = cb_int(1)
      self.cb_int2 = cb_int(2)
      self.cb_int3 = cb_int(3)
      self.cb_int4 = cb_int(4)
      self.cb_int5 = cb_int(5)
      self.cb_i = (1...8).map { |i| make_const(:NUMERIC, "i#{i}") }
      self.cb_standard_error_handler = make_constant_label("Default Error Handler")
    end

    private

    def to_cname(name)
      name.upcase.gsub("-", "_")
    end

    def make_const(category, val)
      Tree::Const.new(self, category, val)
    end

    def make_constant_label(name)
      p = cb_build_label(cb_build_reference(name), nil)
      p.need_begin = true
      p
    end
  end
end
