# frozen_string_literal: true

module CobolParser
  # Binary operation
  #
  # '+' x + y
  # '-' x - y
  # '*' x * y
  # '/' x / y
  # '^' x ** y
  # '=' x = y
  # '>' x > y
  # '<' x < y
  # '[' x <= y
  # ']' x >= y
  # '~' x != y
  # '!' not x
  # '&' x and y
  # '|' x or y
  # '@' ( x )
  class Tree::BinaryOp < Tree
    attribute :op
    attribute :x
    attribute :y

    module Helper
      def cb_build_binary_op(x, op, y)
        category = :UNKNOWN
        case op
        when "+", "-", "*", "/", "^"
          # arithmetic operators
          if x.tree_class == :POINTER || y.tree_class == :POINTER
            category = :DATA_POINTER
          else
            # TODO: cb_check_numeric_value(x)
            x = cb_check_numeric_value(x)
            y = cb_check_numeric_value(y)
            return cb_error_node if x == cb_error_node || y == cb_error_node

            category = :NUMERIC
          end
        when "=", "~", "<", ">", "[", "]"
          # relational operators
          category = :BOOLEAN
        when "!", "&", "|"
          # logical operators
          if x.tree_class :BOOLEAN || (y && y.tree_class != :BOOLEAN)
            cb_error("Invalid expression")
            return cb_error_node
          end
          category = :BOOLEAN
        when "@"
          # parentheses
          category = cb_tree_category(x)
        else
          $stderr.printf("Unexpected operator -> %s\n", op)
          abort
        end

        Tree::BinaryOp.new(@context, category, x, op, y)
      end

      def cb_build_binary_list(list, op)
        e = list.value
        list.each_chain do |l|
          e = cb_build_binary_op(e, op, l.value)
        end
        e
      end

      def cb_build_parenthesis(x)
        cb_build_binary_op(x, "@", nil)
      end

      def cb_build_negation(_cb, x)
        cb_build_binary_op(x, "!", nil)
      end
    end

    def initialize(context, category, x, op, y)
      super(context, category: category, op: op, x: x, y: y)
    end
  end
end
