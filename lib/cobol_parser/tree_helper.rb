# frozen_string_literal: true

require_relative "gettext_helper"
require_relative "tree/alphabet_name"
require_relative "tree/binary_op"
require_relative "tree/cast"
require_relative "tree/const"
require_relative "tree/field"
require_relative "tree/file"
require_relative "tree/integer"
require_relative "tree/label"
require_relative "tree/list"
require_relative "tree/literal"
require_relative "tree/locale_name"
require_relative "tree/perform"
require_relative "tree/perform_varying"
require_relative "tree/picture"
require_relative "tree/program"
require_relative "tree/reference"
require_relative "tree/system_name"

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

    # CB_INDEX_P
    def cb_index?(x)
      cb_ref_or_field?(x) && cb_field(x).usage == :INDEX
    end

    # CB_REF_OR_FIELD_P
    def cb_ref_or_field?(x)
      x.is_a?(Tree::Field) || x.is_a?(Tree::Reference)
    end

    def cb_tree_category(x)
      x.resolve_category
    end

    private

    def to_cname(name)
      name.upcase.gsub("-", "_")
    end

    def make_const(category, val)
      Tree::Const.new(@context, category, val)
    end

    def make_constant_label(name)
      p = cb_build_label(cb_build_reference(name), nil)
      p.need_begin = true
      p
    end
  end
end
