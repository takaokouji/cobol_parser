# frozen_string_literal: true

module CobolParser
  # Integer
  class Tree::Integer < Tree
    attribute :val

    module Helper
      def cb_int(val)
        int_node_table = (method_storage[:int_node_table] ||= {})
        return int_node_table[val] if int_node_table.key?(val)

        x = new(self, val)
        int_node_table[val] = x

        x
      end
    end

    def initialize(context, val)
      super(context, category: :NUMERIC, val: val)
    end
  end
end
