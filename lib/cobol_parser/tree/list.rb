# frozen_string_literal: true

module CobolParser
  # List
  class Tree::List < Tree
    attribute :purpose
    attribute :value
    attribute :chain
    attribute :sizes

    module Helper
      def cb_list?(x)
        x.is_a?(Tree::List)
      end

      def cb_build_list(purpose, value, rest)
        Tree::List.new(self, purpose, value, rest)
      end

      def cb_list_init(x)
        cb_build_list(nil, x, nil)
      end

      def cb_cons(x, l)
        cb_build_list(nil, x, l)
      end

      def cb_list_append(l1, l2)
        return l2 if !l1

        l1.append(l2)
      end

      def cb_list_add(l, x)
        cb_list_append(l, cb_list_init(x))
      end

      def cb_build_pair(x, y)
        cb_build_list(x, y, nil)
      end

      def cb_pair?(x)
        cb_list?(x) && x.x
      end
    end

    def initialize(context, purpose, value, rest)
      super(context, category: :UNKNOWN, purpose: purpose, value: value, chain: rest)
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
end
