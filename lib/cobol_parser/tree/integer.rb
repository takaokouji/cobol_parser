# frozen_string_literal: true

# Integer
class CobolParser::Tree::Integer < CobolParser::Tree
  attribute :val

  class << self
    def int(cb, val)
      @int_node_table ||= {}
      return @int_node_table[val] if @int_node_table.key?(val)

      x = new(cb, category: :NUMERIC, val: val)
      @int_node_table[val] = x

      x
    end
  end
end
