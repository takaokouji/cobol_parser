# frozen_string_literal: true

require_relative "../word"

# Literal
class CobolParser::Tree::Reference < CobolParser::Tree
  attribute :word
  attribute :type
  attribute :value # item referred by this reference
  attribute :subs; # the list of subscripts
  attribute :offset # 1st operand of reference modification
  attribute :length # 2nd operand of reference modification
  attribute :check
  attribute :chain # next qualified name
  attribute :all

  def initialize(context, attributes = {})
    super

    @word = lookup_word(attributes[:word])
  end

  private

  def lookup_word(name)
    key = name.upcase
    if current_program
      word = current_program.word_table[key]&.detect { |x| x.name.casecmp?(name) }
      return word if word
    end

    word = CobolParser::Word.new(name)
    if current_program
      current_program.word_table[key] ||= []
      current_program.word_table[key] << word
    end

    word
  end
end
