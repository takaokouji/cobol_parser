# frozen_string_literal: true

# List
class CobolParser::Tree::List < CobolParser::Tree
  attribute :purpose
  attribute :value
  attribute :chain
  attribute :sizes

  class << self
    def build(cb, purpose, value, rest)
      new(cb, category: :UNKNOWN, purpose: purpose, value: value, chain: rest)
    end

    def init(cb, x)
      build(cb, nil, x, nil)
    end

    def append(l1, l2)
      return l2 if !l1

      l = l1
      l = l.chain while l.chain
      l.chain = l2

      l1
    end

    def add(cb, l, x)
      append(l, init(cb, x))
    end
  end

  def reverse
    last = nil

    l = self
    while l
      n = l.chain
      l.chain = last
      last = l
      l = n
    end

    last
  end

  def length
    n = 0
    l = self
    while l
      n += 1
      l = l.chain
    end
    n
  end
end
