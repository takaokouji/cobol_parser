# frozen_string_literal: true

module CobolParser
  # Alphabet-name
  class Tree::AlphabetName < Tree
    attribute :name
    attribute :cname
    attribute :custom_list
    attribute :type
    attribute :low_val_char
    attribute :high_val_char

    module Helper
      def cb_build_alphabet_name(name, type)
        Tree::AlphabetName.new(name, type)
      end
    end

    def initialize(context, name, type)
      super(context, category: :UNKNOWN)

      @name = cb_define(name, self)
      @cname = to_cname(name)
      @type = type
    end
  end
end
