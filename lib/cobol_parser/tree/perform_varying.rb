# frozen_string_literal: true

# PERFORM
class CobolParser::Tree::PerformVarying < CobolParser::Tree
  attribute :name
  attribute :from
  attribute :step
  attribute :until

  module Helper
    def build_perform_varying(name, from, by, until_)
      CobolParser::Tree::PerformVarying.new(self, name, from, by, until_)
    end
  end

  def initialize(cb, name, from, by, until_)
    super(cb, category: :UNKNOWN, name: name, from: from, until: until_)
    @step = name ? @cb.build_add(name, by, @cb.high) : nil
  end
end
