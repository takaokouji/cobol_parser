# frozen_string_literal: true

# Cast
class CobolParser::Tree::Cast < CobolParser::Tree
  attribute :type
  attribute :val

  class << self
    def build(_cb, type, val)
      category = if type == :INTEGER
                   :NUMERIC
                 else
                   :UNKNOWN
                 end
      new(category: category, type: type, val: val)
    end
  end
end
