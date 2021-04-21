# frozen_string_literal: true

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
class CobolParser::Tree::BinaryOp < CobolParser::Tree
  attribute :op
  attribute :x
  attribute :y

  class << self
    def build(cb, x, op, y)
      category = :UNKNOWN
      case op
      when "+", "-", "*", "/", "^"
        # arithmetic operators
        if x.tree_class == :POINTER || y.tree_class == :POINTER
          category = :DATA_POINTER
        else
          # TODO: cb.check_numeric_value(x)
          x = cb.check_numeric_value(x)
          y = cb.check_numeric_value(y)
          return cb.error_node if x == cb.error_node || y == cb.error_node

          category = :NUMERIC
        end
      when "=", "~", "<", ">", "[", "]"
        # relational operators
        category = :BOOLEAN
      when "!", "&", "|"
        # logical operators
        if x.tree_class :BOOLEAN || (y && y.tree_class != :BOOLEAN)
          cb.error("Invalid expression")
          return cb.error_node
        end
        category = :BOOLEAN
      when "@"
        # parentheses
        category = x.category
      else
        $stderr.printf("Unexpected operator -> %s\n", op)
        abort
      end

      new(category: category, op: op, x: x, y: y)
    end

    def build_binary_list(cb, list, op)
      e = list.value
      # TODO: CobolParser::Tree::List, each_chain
      list.each_chain do |l|
        e = build(cb, e, op, l.value)
      end
      e
    end

    def build_parenthesis(cb, x)
      build(cb, x, "@", nil)
    end

    def build_negation(cb, x)
      build(cb, x, "!", nil)
    end
  end
end
