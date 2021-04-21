# frozen_string_literal: true

# Alphabet-name
class CobolParser::Tree::AlphabetName < CobolParser::Tree
  attribute :name
  attribute :cname
  attribute :custom_list
  attribute :type
  attribute :low_val_char
  attribute :high_val_char

  class << self
    def build(cb, name, type)
      p = new(cb, category: :UNKNOWN)
      p.name = cb.define(name, p)
      p.cname = cb.to_cname(p.name)
      p.type = type

      p
    end
  end
end
