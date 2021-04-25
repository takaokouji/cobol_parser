# frozen_string_literal: true

module CobolParser
  # Constants
  class Tree::Const < Tree
    attribute :val

    def initialize(context, category, val)
      super(context, category: category, val: val)
    end
  end
end
