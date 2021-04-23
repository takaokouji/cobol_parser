# frozen_string_literal: true

# Literal
class CobolParser::Tree::Literal < CobolParser::Tree
  SIGN = %i[
    UNSIGNED
    NEGATIVE
    POSITIVE
  ].freeze

  attribute :data
  attribute :all
  attribute :sign
  attribute :scale

  module Helper
    def build_literal(category, data)
      CobolParser::Tree::Literal.new(self, category, data)
    end

    def build_numeric_literal(sign, data, scale)
      if sign.is_a?(Integer)
        sign = case sign
               when 0
                 :UNSIGNED
               when 1
                 :POSITIVE
               when -1
                 :NEGATIVE
               else
                 raise ArgumentError, "sign must -1, 0, 1: sign=<#{sign.inspect}>"
               end
      elsif !SIGN.include?(sign)
        raise ArgumentError, "Invalid sign: #{sign.inspect}"
      end

      p = build_literal(:NUMERIC, data)
      p.sign = sign
      p.scale = scale

      p
    end

    def build_alphanumeric_literal(data)
      build_literal(:ALPHANUMERIC, data)
    end

    def concat_literals(x1, x2)
      return @cb.error_node if x1 == @cb.error_node || x2 == @cb.error_node

      data1 = case x1
              when CobolParser::Tree::Literal
                x1.data
              when CobolParser::Tree::Const
                case x1
                when @cb.space
                  " "
                when @cb.zero
                  "0"
                when @cb.quote
                  "\""
                when @cb.norm_low
                  "\0"
                when @cb.norm_high
                  "\255"
                when @cb.null # rubocop:disable Lint/DuplicateBranch
                  "\0"
                else
                  return @cb.error_node
                end
              else
                return @cb.error_node
              end

      data2 = case x2
              when CobolParser::Tree::Literal
                x2.data
              when CobolParser::Tree::Const
                case x2
                when @cb.space
                  " "
                when @cb.zero
                  "0"
                when @cb.quote
                  "\""
                when @cb.norm_low
                  "\0"
                when @cb.norm_high
                  "\255"
                when @cb.null # rubocop:disable Lint/DuplicateBranch
                  "\0"
                else
                  return @cb.error_node
                end
              else
                return @cb.error_node
              end

      @cb.build_alphanumeric_literal(data1 + data2)
    end

    def get_int(x)
      x.int
    end

    alias_method :get_long_long, :get_int
  end

  def initialize(cb, category, data)
    super(cb, category: category, data: data)
  end

  def int
    val = data.to_i
    val = -val if sign == :NEGATIVE
    val
  end

  alias_method :long_long, :int
end
