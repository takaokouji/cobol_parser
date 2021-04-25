# frozen_string_literal: true

module CobolParser
  # Cast
  class Tree::Cast < Tree
    attribute :type
    attribute :val

    module Helper
      def cb_build_cast(type, val)
        Tree::Cast.new(type, val)
      end
    end

    def initialize(context, type, val)
      category = if type == :INTEGER
                   :NUMERIC
                 else
                   :UNKNOWN
                 end
      super(context, category: category, type: type, val: val)
    end
  end
end
