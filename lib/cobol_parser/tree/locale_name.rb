# frozen_string_literal: true

# Locale name
class CobolParser::Tree::LocaleName < CobolParser::Tree
  attribute :name
  attribute :cname
  attribute :list

  class << self
    def build(cb, name, list)
      p = new(cb, category: :UNKNOWN)
      p.name = cb.define(name, p)
      p.cname = to_cname(p.name)
      p.list = list
      p
    end
  end
end
