# frozen_string_literal: true

require_relative "../word"

# Literal
class CobolParser::Tree::Reference < CobolParser::Tree
  attribute :word
  attribute :type
  attribute :value # item referred by this reference
  attribute :subs # the list of subscripts
  attribute :offset # 1st operand of reference modification
  attribute :length # 2nd operand of reference modification
  attribute :check
  attribute :chain # next qualified name
  attribute :all

  def_delegator :@word, :name

  class << self
    def build(cb, name)
      new(cb, category: :UNKNOWN, word: lookup_word(name, cb.current_program))
    end

    def define(name, val)
      w = name.word
      w.items << val
      w.count += 1
      val.source_file = name.source_file
      val.source_line = name.source_line
      name.value = val
      w.name
    end

    def lookup_word(name, current_program)
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

  def ref # rubocop:disable Metrics/AbcSize
    # if this reference has already been resolved (and the value
    # has been cached), then just return the value
    return value if value

    candidate = nil
    ambiguous = false

    # resolve the value
    word.items.each do |v|
      # find a candidate value by resolving qualification
      c = chain
      case v
      when CobolParser::Tree::Field
        # in case the value is a field, it might be qualified
        # by its parent names and a file name
        p = if v.flag_indexed_by
              v.index_qual
            else
              v.parent
            end
        # resolve by parents
        while p
          if c&.name&.casecmp?(p.name)
            c = c.chain
          end
          p = p.parent
        end

        # resolve by file
        if c && !c.chain && (c.word.count == 1 && @cb.ref(c).is_a?(CobolParser::Tree::File) &&
             @cb.ref(c) == @cb.field_founder(v).file)
          c = c.chain
        end
        # TODO: case CB_TAG_LABEL:
      else # rubocop:disable Style/EmptyElse
        # other values cannot be qualified
      end

      # a well qualified value is a good candidate
      if !c
        if candidate
          # multiple candidates and possibly ambiguous
          ambiguous = true
        else
          # keep the first candidate
          candidate = v
          # continue search because the reference might not
          # be ambiguous and exit loop by "goto end" later
        end
      end
    end

    # there is no candidate
    if !candidate
      if current_program.nested_level > 0
        # TODO: tree.c:1895
      end
      # TODO: undefined_error(x)
      undefined_error(self)
      # TODO: goto error
    end

    # the reference is ambiguous
    if ambiguous
      # TODO: ambiguous_error(x)
      ambiguous_error(self)
      # TODO: goto error
    end

    if candidate.is_a?(CobolParser::Tree::Field)
      candidate.count += 1
      if candidate.flag_invalid
        # TODO: goto error
      end
    end

    self.value = candidate
    value
  rescue StandardError
    self.value = @cb.constants.error_node
    @cb.constants.error_node
  end

  def each_chain
    return enum_for(:each_chain) unless block_given?

    c = chain
    while c
      yield c
      c = c.chain
    end
  end
end
