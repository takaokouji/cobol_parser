# frozen_string_literal: true

require_relative "../word"

# Literal
class CobolParser::Tree::Reference < CobolParser::Tree
  class RefError < StandardError; end

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

  module Helper
    def build_reference(name)
      CobolParser::Tree::Reference.new(self, name)
    end

    def build_filler
      @filler_id ||= 1

      name = "WORK$#{@filler_id}"
      @filler_id += 1
      x = build_reference(name)
      x.source_line = @cb.source_line

      x
    end

    def define(name, val)
      w = name.word
      w.items = @cb.list_add(w.items, val)
      w.count += 1
      val.source_file = name.source_file
      val.source_line = name.source_line
      name.value = val
      w.name
    end

    def define_system_name(name)
      x = build_reference(name)
      return if x.word.count > 0

      @cb.define(x, @cb.lookup_system_name(name))
    end
  end

  def initialize(cb, name)
    super(cb, category: :UNKNOWN)

    @word = lookup_word(name)
  end

  def ref
    # if this reference has already been resolved (and the value
    # has been cached), then just return the value
    return value if value

    candidate = resolve_reference
    if candidate.is_a?(CobolParser::Tree::Field)
      candidate.count += 1
      candidate = nil if candidate.flag_invalid
    end

    if candidate
      self.value = candidate
      return value
    end

    self.value = @cb.error_node
    @cb.error_node
  end

  def each_chain
    return enum_for(:each_chain) unless block_given?

    c = chain
    while c
      yield c
      c = c.chain
    end
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

  def resolve_reference
    candidate = nil
    ambiguous = false

    # resolve the value
    word.items.each_chain do |items|
      # find a candidate value by resolving qualification
      v = items.value
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
        if c && !c.chain &&
           c.word.count == 1 && c.ref.is_a?(CobolParser::Tree::File) && c.ref == v.founder.file
          c = c.chain
        end
      when CobolParser::Tree::Label
        # in case the value is a label, it might be qualified
        # by its section name
        s = v.section

        # unqualified paragraph name referenced within the section
        # is resolved without ambiguity check if not duplicated
        if c.nil? && offset && s == offset
          items.chain.each_chain do |cb1|
            cb2 = cb1.value
            if s == cb2.section
              @cb.ambiguous_error(self)
              return nil
            end
          end
          candidate = v
          return candidate
        end

        # resolve by section name
        c = c.chain if c && s && c.name.casecmp?(s.name)
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
      @cb.undefined_error(self)
      return nil
    end

    # the reference is ambiguous
    if ambiguous
      @cb.ambiguous_error(self)
      return nil
    end

    candidate
  end
end
