# frozen_string_literal: true

module CobolParser
  # Literal
  class Tree::Literal < Tree
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
      def cb_build_literal(category, data)
        Tree::Literal.new(@context, category, data)
      end

      def cb_build_numeric_literal(sign, data, scale)
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

        p = cb_build_literal(:NUMERIC, data)
        p.sign = sign
        p.scale = scale

        p
      end

      def cb_build_alphanumeric_literal(data)
        cb_build_literal(:ALPHANUMERIC, data)
      end

      def concat_literals(x1, x2)
        return cb_error_node if x1 == cb_error_node || x2 == cb_error_node

        data1 = case x1
                when Tree::Literal
                  x1.data
                when Tree::Const
                  case x1
                  when cb_space
                    " "
                  when cb_zero
                    "0"
                  when cb_quote
                    "\""
                  when cb_norm_low
                    "\0"
                  when cb_norm_high
                    "\255"
                  when cb_null # rubocop:disable Lint/DuplicateBranch
                    "\0"
                  else
                    return cb_error_node
                  end
                else
                  return cb_error_node
                end

        data2 = case x2
                when Tree::Literal
                  x2.data
                when Tree::Const
                  case x2
                  when cb_space
                    " "
                  when cb_zero
                    "0"
                  when cb_quote
                    "\""
                  when cb_norm_low
                    "\0"
                  when cb_norm_high
                    "\255"
                  when cb_null # rubocop:disable Lint/DuplicateBranch
                    "\0"
                  else
                    return cb_error_node
                  end
                else
                  return cb_error_node
                end

        cb_build_alphanumeric_literal(data1 + data2)
      end

      def cb_get_int(x)
        x.int
      end

      alias_method :cb_get_long_long, :cb_get_int
    end

    def initialize(context, category, data)
      super(context, category: category, data: data)
    end

    def int
      val = data.to_i
      val = -val if sign == :NEGATIVE
      val
    end

    alias_method :long_long, :int
  end
end
