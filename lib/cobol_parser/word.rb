# frozen_string_literal: true

class CobolParser::Word
  attr_accessor :name # word name
  attr_accessor :items # objects associated with this word
  attr_accessor :count # number of words with the same name
  attr_accessor :error # set to 1 if error displayed

  def initialize(name)
    @name = name
    @items = []
    @count = 0
    @error = false
  end
end
