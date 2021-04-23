# frozen_string_literal: true

# List
class CobolParser::Tree::List < CobolParser::Tree
  attribute :purpose
  attribute :value
  attribute :chain
  attribute :sizes

  module Helper
    def build_list(purpose, value, rest)
      CobolParser::Tree::List.new(self, purpose, value, rest)
    end

    def list_init(x)
      build_list(nil, x, nil)
    end

    def cons(x, l)
      build_list(nil, x, l)
    end

    def list_append(l1, l2)
      return l2 if !l1

      l1.append(l2)
    end

    def list_add(l, x)
      list_append(l, list_init(x))
    end

    def build_pair(x, y)
      build_list(x, y, nil)
    end

    def pair?(x)
      x.is_a?(CobolParser::Tree::List) && pair_x(x)
    end
  end

  def initialize(cb, purpose, value, rest)
    super(cb, category: :UNKNOWN, purpose: purpose, value: value, chain: rest)
  end

  def append(list)
    l = self
    l = l.chain while l.chain
    l.chain = list

    self
  end

  def reverse
    last = nil

    l = self
    while l
      c = l.chain
      l.chain = last
      last = l
      l = c
    end

    last
  end

  def length
    n = 0
    each_chain { |_| n += 1 }
    n
  end

  def each_chain
    return enum_for(:each_chain) if !block_given?

    l = self
    while l
      yield l
      l = l.chain
    end
  end

  def x
    purpose
  end

  def y
    value
  end
end
