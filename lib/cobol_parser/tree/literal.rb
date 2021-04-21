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

  class << self
    def build(cb, category, data)
      new(cb, category: category, data: data)
    end

    def build_numeric(cb, sign, data, scale)
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

      p = build(cb, :NUMERIC, data)
      p.sign = sign
      p.scale = scale

      p
    end

    def build_alphanumeric(cb, data)
      build(cb, :ALPHANUMERIC, data)
    end
  end
end
