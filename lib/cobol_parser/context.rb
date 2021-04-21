# frozen_string_literal: true

require "ostruct"
require_relative "error_helper"
require_relative "reserved_helper"
require_relative "program_helper"
require_relative "tree_helper"

class CobolParser::Context
  include CobolParser::ErrorHelper
  include CobolParser::ReservedHelper
  include CobolParser::ProgramHelper
  include CobolParser::TreeHelper

  FORMAT = {
    FIXED: 0,
    FREE: 1,
  }.freeze

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

  ReplaceListItem = Struct.new(:old_text, :new_text, keyword_init: true)

  # Global variables
  attr_accessor :current_program
  attr_accessor :current_statement
  attr_accessor :current_section
  attr_accessor :current_paragraph
  attr_accessor :functions_are_all
  attr_accessor :non_const_word

  attr_accessor :source_format
  attr_accessor :source_line
  attr_accessor :source_file

  attr_accessor :depend_file
  attr_accessor :depend_list

  attr_accessor :extension_list
  attr_accessor :include_list

  attr_accessor :config_name

  attr_accessor :author_paragraph
  attr_accessor :eject_statement
  attr_accessor :tab_width
  attr_accessor :text_column

  attr_accessor :flag_mfcomment
  attr_accessor :flag_debugging_line
  attr_accessor :flag_fold_copy_lower
  attr_accessor :flag_fold_copy_upper
  attr_accessor :flag_functions_all

  attr_accessor :warn_archaic
  attr_accessor :warn_obsolate
  attr_accessor :warn_column_overflow

  attr_accessor :norestab

  def initialize
    @functions_are_all = false
    @non_const_word = 0

    @source_format = FORMAT[:FIXED]
    @source_line = 0
    @source_file = nil

    init_constants

    @depend_file = nil
    @depend_list = []

    @extension_list = %w[
      .CPY
      .CBL
      .COB
      .cpy
      .cbl
      .cob
    ]
    @extension_list << ""
    @include_list = []

    @config_name = nil

    @author_paragraph = SUPPORT[:OBSOLETE]
    @eject_statement = SUPPORT[:SKIP]
    @tab_width = 8
    @text_column = 72

    @flag_mfcomment = false
    @flag_debugging_line = false
    @flag_fold_copy_lower = false
    @flag_fold_copy_upper = false

    @warn_archaic = SUPPORT[:WARNING]
    @warn_obsolate = SUPPORT[:WARNING]
    @warn_column_overflow = SUPPORT[:WARNING]

    @current_section = nil
    @current_paragraph = nil

    @warningcount = 0
    @errorcount = 0

    @norestab = []
  end

  def fixed_source_format?
    source_format == FORMAT[:FIXED]
  end

  def free_source_format?
    source_format == FORMAT[:FREE]
  end

  def verify(tag, feature)
    case tag
    when SUPPORT[:OK], SUPPORT[:WARNING]
      true
    when SUPPORT[:ARCHAIC]
      warning("%s is archaic in %s", feature, config_name) if warn_archaic
      true
    when SUPPORT[:OBSOLETE]
      warning("%s is obsolete in %s", feature, config_name) if warn_obsolate
      true
    when SUPPORT[:SKIP], SUPPORT[:ERROR]
      false
    when SUPPORT[:IGNORE]
      warning("%s ignored", feature)
      false
    when SUPPORT[:UNCONFORMABLE]
      error("%s does not conform to %s", feature, config_name)
      false
    else # rubocop:disable Lint/DuplicateBranch
      false
    end
  end

  def replace_list_add(replace_list, old_text, new_text)
    replace_list ||= []
    replace_list << ReplaceListItem.new(old_text: old_text, new_text: new_text)
    replace_list
  end
end
