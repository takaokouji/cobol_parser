# frozen_string_literal: true

module CobolParser
  # Locale name
  class Tree::LocaleName < Tree
    attribute :name
    attribute :cname
    attribute :list

    module Helper
      def cb_build_local_name(name, list)
        Tree::LocaleName.new(name, list)
      end
    end

    def initialize(context, name, list)
      super(context, category: :UNKNOWN)

      @name = cb_define(name, self)
      @cname = to_cname(name)
      @list = list
    end
  end
end
