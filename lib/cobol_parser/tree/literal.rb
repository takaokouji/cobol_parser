# frozen_string_literal: true

# Literal
class CobolParser::Tree::Literal < CobolParser::Tree
  SIGN = %i[
    UNSIGNED
    NEGATIVE
    POSITIVE
  ].freeze

  attribute :data
  attribute :all
  attribute :sign
  attribute :scale
end
