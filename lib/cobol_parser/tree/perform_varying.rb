# frozen_string_literal: true

module CobolParser
  # PERFORM
  class Tree::PerformVarying < Tree
    attribute :name
    attribute :from
    attribute :step
    attribute :until

    module Helper
      def cb_build_perform_varying(name, from, by, until_)
        Tree::PerformVarying.new(self, name, from, by, until_)
      end
    end

    def initialize(context, name, from, by, until_)
      super(context, category: :UNKNOWN, name: name, from: from, until: until_)
      @step = name ? cb_build_add(name, by, cb_high) : nil
    end
  end
end
