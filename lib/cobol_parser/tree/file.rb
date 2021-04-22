# frozen_string_literal: true

# File
class CobolParser::Tree::File < CobolParser::Tree
  attribute :name # The original name
  attribute :cname # The name used in C
  # SELECT
  attribute :assign # ASSIGN
  attribute :file_status # FILE STATUS
  attribute :sharing # SHARING
  attribute :key # RELATIVE/RECORD KEY
  attribute :alt_key_list # ALTERNATE RECORD KEY
  # FD/SD
  attribute :record # Record descriptor
  attribute :record_depending # RECORD DEPENDING
  attribute :linage # LINAGE
  attribute :linage_ctr # LINAGE COUNTER
  attribute :latfoot # LINAGE FOOTING
  attribute :lattop # LINAGE TOP
  attribute :latbot # LINAGE BOTTOM
  attribute :handler # Error handler
  attribute :handler_prog # Prog where defined
  attribute :record_min # RECORD CONTAINS
  attribute :record_max # RECORD CONTAINS
  attribute :optional # OPTIONAL
  attribute :organization # ORGANIZATION
  attribute :access_mode # ACCESS MODE
  attribute :lock_mode # LOCK MODE
  attribute :same_clause # SAME clause
  attribute :finalized # Is finalized
  attribute :external # Is EXTERNAL
  attribute :special # Special file
  attribute :external_assign # ASSIGN EXTERNAL
  attribute :fileid_assign # ASSIGN DISK
  attribute :global # Is GLOBAL

  module Helper
    def build_file(name)
      CobolParser::Tree::File.new(self, name)
    end

    def validate_file(f, name)
      f.validate(name)
    end

    def finalize_file(f, records)
      f.finalize(records)
    end
  end

  def initialize(cb, name)
    super(cb, category: :UNKNOWN)

    @name = @cb.define(name, self)
    @cname = to_cname(@name)

    @organization = :ORG_SEQUENTIAL
    @ccess_mode = :ACCESS_SEQUENTIAL
    @handler = @cb.standard_error_handler
    @handler_prog = current_program
  end

  # check RECORD/RELATIVE KEY clause
  def validate(name)
    case organization
    when :ORG_INDEXED
      file_error(name, "RECORD KEY") if key.nil?
    when :ORG_RELATIVE
      file_error(name, "RELATIVE KEY") if key.nil? && access_mode != :ACCESS_SEQUENTIAL
    end
  end

  def finalize(records)
    # TODO: tree.c:1611: finalize_file (struct cb_file *f, struct cb_field *records)
    raise NotImplementedError
  end
end
