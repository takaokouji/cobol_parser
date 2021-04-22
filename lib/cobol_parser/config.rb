# frozen_string_literal: true

class CobolParser::Config
  ASSIGN_CLAUSE = [
    :COBOL2002, # COBOL 2002 standard
    :MF, # Micro Focus COBOL compatibility
    :IBM, # IBM COBOL compatibility
  ].freeze

  BINARY_BYTEORDER = %i[
    NATIVE
    BIG_ENDIAN
  ].freeze

  BINARY_SIZE = [
    :"2_4_8", # 2,4,8 bytes
    :"1_2_4_8", # 1,2,4,8 bytes
    :"1__8", # 1,2,3,4,5,6,7,8 bytes
  ].freeze

  SUPPORT = {
    OK: 0,
    WARNING: 1,
    ARCHAIC: 2,
    OBSOLETE: 3,
    SKIP: 4,
    IGNORE: 5,
    ERROR: 6,
    UNCONFORMABLE: 7,
  }.freeze

  class << self
    attr_accessor :types

    def config_string(var, name)
      attr_accessor(var)

      add_types(
        type: :string,
        var: var,
        name: name
      )
    end

    def config_int(var, name)
      attr_accessor(var)

      add_types(
        type: :int,
        var: var,
        name: name
      )
    end

    def config_any(items, var, name)
      attr_accessor(var)

      add_types(
        type: :any,
        items: items,
        var: var,
        name: name
      )
    end

    def config_boolean(var, name)
      attr_accessor(var)

      add_types(
        type: :boolean,
        var: var,
        name: name
      )
    end

    def config_support(var, name)
      attr_accessor(var)

      add_types(
        type: :support,
        var: var,
        name: name
      )
    end

    private

    def add_types(hash)
      self.types ||= {}
      types[hash[:name]] = hash
    end
  end

  config_string(:config_name, "name")
  config_int(:tab_width, "tab-width")
  config_int(:text_column, "text-column")
  config_any(ASSIGN_CLAUSE, :assign_clause, "assign-clause")
  config_any(BINARY_BYTEORDER, :binary_size, "binary-size")
  config_any(BINARY_SIZE, :binary_byteorder, "binary-byteorder")
  config_boolean(:filename_mapping, "filename-mapping")
  config_boolean(:pretty_display, "pretty-display")
  config_boolean(:binary_truncate, "binary-truncate")
  config_boolean(:auto_initialize, "auto-initialize")
  config_boolean(:complex_odo, "complex-odo")
  config_boolean(:indirect_redefines, "indirect-redefines")
  config_boolean(:larger_redefines_ok, "larger-redefines-ok")
  config_boolean(:relaxed_syntax_check, "relaxed-syntax-check")
  config_boolean(:perform_osvs, "perform-osvs")
  config_boolean(:sticky_linkage, "sticky-linkage")
  config_boolean(:relax_level_hierarchy, "relax-level-hierarchy")
  config_support(:author_paragraph, "author-paragraph")
  config_support(:memory_size_clause, "memory-size-clause")
  config_support(:multiple_file_tape_clause, "multiple-file-tape-clause")
  config_support(:label_records_clause, "label-records-clause")
  config_support(:value_of_clause, "value-of-clause")
  config_support(:data_records_clause, "data-records-clause")
  config_support(:top_level_occurs_clause, "top-level-occurs-clause")
  config_support(:synchronized_clause, "synchronized-clause")
  config_support(:goto_statement_without_name, "goto-statement-without-name")
  config_support(:stop_literal_statement, "stop-literal-statement")
  config_support(:debugging_line, "debugging-line")
  config_support(:padding_character_clause, "padding-character-clause")
  config_support(:next_sentence_phrase, "next-sentence-phrase")
  config_support(:eject_statement, "eject-statement")
  config_support(:entry_statement, "entry-statement")
  config_support(:move_noninteger_to_alphanumeric, "move-noninteger-to-alphanumeric")
  config_support(:odo_without_to, "odo-without-to")

  def initialize(cb)
    @cb = cb

    @config_name = "OpenCOBOL"
    @tab_width = 8
    @text_column = 72
    @assign_clause = :MF
    @filename_mapping = true
    @pretty_display = true
    @auto_initialize = true
    @complex_odo = false
    @indirect_redefines = false
    @binary_size = :"1-2-4-8"
    @binary_truncate = true
    @binary_byteorder = :NATIVE
    @larger_redefines_ok = false
    @relaxed_syntax_check = false
    @perform_osvs = false
    @sticky_linkage = false
    @relax_level_hierarchy = false

    @author_paragraph = SUPPORT[:OBSOLETE]
    @memory_size_clause = SUPPORT[:OBSOLETE]
    @multiple_file_tape_clause = SUPPORT[:OBSOLETE]
    @label_records_clause = SUPPORT[:OBSOLETE]
    @value_of_clause = SUPPORT[:OBSOLETE]
    @data_records_clause = SUPPORT[:OBSOLETE]
    @top_level_occurs_clause = SUPPORT[:SKIP]
    @synchronized_clause = SUPPORT[:OK]
    @goto_statement_without_name = SUPPORT[:OBSOLETE]
    @stop_literal_statement = SUPPORT[:OBSOLETE]
    @debugging_line = SUPPORT[:OBSOLETE]
    @padding_character_clause = SUPPORT[:OBSOLETE]
    @next_sentence_phrase = SUPPORT[:ARCHAIC]
    @eject_statement = SUPPORT[:SKIP]
    @entry_statement = SUPPORT[:OBSOLETE]
    @move_noninteger_to_alphanumeric = SUPPORT[:ERROR]
    @odo_without_to = SUPPORT[:OK]
  end

  def verify(tag, feature)
    case tag
    when SUPPORT[:OK], SUPPORT[:WARNING]
      true
    when SUPPORT[:ARCHAIC]
      @cb.warning("%s is archaic in %s", feature, config_name) if @cb.warn_archaic
      true
    when SUPPORT[:OBSOLETE]
      @cb.warning("%s is obsolete in %s", feature, config_name) if @cb.warn_obsolate
      true
    when SUPPORT[:SKIP], SUPPORT[:ERROR]
      false
    when SUPPORT[:IGNORE]
      @cb.warning("%s ignored", feature)
      false
    when SUPPORT[:UNCONFORMABLE]
      @cb.error("%s does not conform to %s", feature, config_name)
      false
    else # rubocop:disable Lint/DuplicateBranch
      false
    end
  end
end
