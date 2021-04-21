# frozen_string_literal: true

# Constants
class CobolParser::Tree::Const < CobolParser::Tree
  attribute :val

  class << self
    def make_const(cb, category, val)
      new(cb, category: category, val: val)
    end
  end
end
