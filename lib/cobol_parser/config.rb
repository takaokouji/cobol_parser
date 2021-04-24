# frozen_string_literal: true

module CobolParser
  class Config
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

    config_string(:cb_config_name, "name")
    config_int(:cb_tab_width, "tab-width")
    config_int(:cb_text_column, "text-column")
    config_any([CB_ASSIGN_COBOL2002, CB_ASSIGN_MF, CB_ASSIGN_IBM],
               :cb_assign_clause, "assign-clause")
    config_any([CB_BYTEORDER_NATIVE, CB_BYTEORDER_BIG_ENDIAN],
               :cb_binary_size, "binary-size")
    config_any([CB_BINARY_SIZE_2_4_8, CB_BINARY_SIZE_1_2_4_8, CB_BINARY_SIZE_1__8],
               :cb_binary_byteorder, "binary-byteorder")
    config_boolean(:cb_filename_mapping, "filename-mapping")
    config_boolean(:cb_pretty_display, "pretty-display")
    config_boolean(:cb_binary_truncate, "binary-truncate")
    config_boolean(:cb_auto_initialize, "auto-initialize")
    config_boolean(:cb_complex_odo, "complex-odo")
    config_boolean(:cb_indirect_redefines, "indirect-redefines")
    config_boolean(:cb_larger_redefines_ok, "larger-redefines-ok")
    config_boolean(:cb_relaxed_syntax_check, "relaxed-syntax-check")
    config_boolean(:cb_perform_osvs, "perform-osvs")
    config_boolean(:cb_sticky_linkage, "sticky-linkage")
    config_boolean(:cb_relax_level_hierarchy, "relax-level-hierarchy")
    config_support(:cb_author_paragraph, "author-paragraph")
    config_support(:cb_memory_size_clause, "memory-size-clause")
    config_support(:cb_multiple_file_tape_clause, "multiple-file-tape-clause")
    config_support(:cb_label_records_clause, "label-records-clause")
    config_support(:cb_value_of_clause, "value-of-clause")
    config_support(:cb_data_records_clause, "data-records-clause")
    config_support(:cb_top_level_occurs_clause, "top-level-occurs-clause")
    config_support(:cb_synchronized_clause, "synchronized-clause")
    config_support(:cb_goto_statement_without_name, "goto-statement-without-name")
    config_support(:cb_stop_literal_statement, "stop-literal-statement")
    config_support(:cb_debugging_line, "debugging-line")
    config_support(:cb_padding_character_clause, "padding-character-clause")
    config_support(:cb_next_sentence_phrase, "next-sentence-phrase")
    config_support(:cb_eject_statement, "eject-statement")
    config_support(:cb_entry_statement, "entry-statement")
    config_support(:cb_move_noninteger_to_alphanumeric, "move-noninteger-to-alphanumeric")
    config_support(:cb_odo_without_to, "odo-without-to")

    def initialize
      @cb_config_name = "OpenCOBOL"
      @cb_tab_width = 8
      @cb_text_column = 72
      @cb_assign_clause = :MF
      @cb_filename_mapping = true
      @cb_pretty_display = true
      @cb_auto_initialize = true
      @cb_complex_odo = false
      @cb_indirect_redefines = false
      @cb_binary_size = :"1-2-4-8"
      @cb_binary_truncate = true
      @cb_binary_byteorder = :NATIVE
      @cb_larger_redefines_ok = false
      @cb_relaxed_syntax_check = false
      @cb_perform_osvs = false
      @cb_sticky_linkage = false
      @cb_relax_level_hierarchy = false

      @cb_author_paragraph = CB_OBSOLETE
      @cb_memory_size_clause = CB_OBSOLETE
      @cb_multiple_file_tape_clause = CB_OBSOLETE
      @cb_label_records_clause = CB_OBSOLETE
      @cb_value_of_clause = CB_OBSOLETE
      @cb_data_records_clause = CB_OBSOLETE
      @cb_top_level_occurs_clause = CB_SKIP
      @cb_synchronized_clause = CB_OK
      @cb_goto_statement_without_name = CB_OBSOLETE
      @cb_stop_literal_statement = CB_OBSOLETE
      @cb_debugging_line = CB_OBSOLETE
      @cb_padding_character_clause = CB_OBSOLETE
      @cb_next_sentence_phrase = CB_ARCHAIC
      @cb_eject_statement = CB_SKIP
      @cb_entry_statement = CB_OBSOLETE
      @cb_move_noninteger_to_alphanumeric = CB_ERROR
      @cb_odo_without_to = CB_OK
    end

    module Helper
      def cb_verify(tag, feature)
        case tag
        when CB_OK, CB_WARNING
          true
        when CB_ARCHAIC
          cb_warning("%s is archaic in %s", feature, config_name) if cb_warn_archaic
          true
        when CB_OBSOLETE
          cb_warning("%s is obsolete in %s", feature, config_name) if cb_warn_obsolate
          true
        when CB_SKIP, CB_ERROR
          false
        when CB_IGNORE
          cb_warning("%s ignored", feature)
          false
        when CB_UNCONFORMABLE
          cb_error("%s does not conform to %s", feature, config_name)
          false
        else # rubocop:disable Lint/DuplicateBranch
          false
        end
      end
    end
  end
end
