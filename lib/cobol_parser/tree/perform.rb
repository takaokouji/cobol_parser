# frozen_string_literal: true

# PERFORM
class CobolParser::Tree::Perform < CobolParser::Tree
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
    def build_perform(type)
      CobolParser::Tree::Perform.new(self, type)
    end
  end

  def initialize(cb, type)
    super(cb, category: :UNKNOWN, type: type)
  end
end
