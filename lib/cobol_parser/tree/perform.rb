# frozen_string_literal: true

module CobolParser
  # PERFORM
  class Tree::Perform < Tree
    TYPE = %i[
      EXIT
      ONCE
      TIMES
      UNTIL
      FOREVER
    ].freeze

    attribute :type
    attribute :test
    attribute :body
    attribute :data
    attribute :varying
    attribute :exit_label
    attribute :cycle_label

    module Helper
      def cb_build_perform(type)
        Tree::Perform.new(self, type)
      end
    end

    def initialize(context, type)
      super(context, category: :UNKNOWN, type: type)
    end
  end
end
