# frozen_string_literal: true

# Field
class CobolParser::Tree::Field < CobolParser::Tree
  attribute :id # field id
  attribute :storage_id # storage id
  attribute :name # the original name
  attribute :ename # the externalized name
  attribute :size # field size
  attribute :memory_size # memory size
  attribute :offset # byte offset from top (01 field)
  attribute :level # level number
  attribute :occurs_min # OCCURS <max>
  attribute :occurs_max # or OCCURS <min> TO <max>
  attribute :indexes # number of parents who have OCCURS
  attribute :count # reference count
  attribute :occurs_depending # OCCURS ... DEPENDING ON
  attribute :storage
  attribute :usage # USAGE
  attribute :values # VALUE
  attribute :false_88 # 88 FALSE clause
  attribute :index_list # INDEXED BY
  attribute :parent # upper level field (NULL for 01 fields)
  attribute :children # top of lower level fields
  attribute :sister # fields in the same level
  attribute :redefines # REDEFINES
  attribute :rename_thru # RENAMES THRU
  attribute :index_qual # INDEXED BY qualifier
  attribute :file # file name associated in FD section
  attribute :kyes
  # struct cb_key {
  #   int dir;    /* ASCENDING or DESCENDING */
  #   cb_tree key;    /* KEY */
  #   cb_tree ref;    /* reference used in SEARCH ALL */
  #   cb_tree val;    /* value to be compared in SEARCH ALL */
  # } *keys;
  attribute :param_num # CHAINING param number
  attribute :pic # PICTURE
  # screen parameters
  attribute :screen_line
  attribute :screen_column
  attribute :screen_from
  attribute :screen_to
  attribute :screen_foreg
  attribute :screen_backg
  attribute :screen_flag # flags used in SCREEN SECTION
  # flags
  attribute :flag_external # EXTERNAL */
  attribute :flag_blank_zero # BLANK WHEN ZERO */
  attribute :flag_justified # JUSTIFIED RIGHT */
  attribute :flag_sign_leading # SIGN IS LEADING */
  attribute :flag_sign_separate # SIGN IS SEPARATE */
  attribute :flag_synchronized # SYNCHRONIZED */
  attribute :flag_occurs # OCCURS */
  attribute :flag_invalid # is broken */
  attribute :flag_binary_swap # binary byteswap */
  attribute :flag_local # has local scope */
  attribute :flag_base # has memory allocation */
  attribute :flag_field # has been internally cached */
  attribute :flag_item_external # is EXTERNAL */
  attribute :flag_chained # CHAINING item */
  attribute :flag_real_binary # is BINARY-CHAR/SHORT/LONG/DOUBLE */
  attribute :flag_item_based # is BASED */
  attribute :flag_item_78 # is 78 level */
  attribute :flag_any_length # is ANY LENGTH */
  attribute :flag_anylen_done # ANY LENGTH is set up */
  attribute :flag_indexed_by # INDEXED BY item */
  attribute :flag_is_pointer # is POINTER */
  attribute :flag_is_verified # has been verified */
  attribute :flag_is_global # is GLOBAL */
  attribute :flag_is_c_long # is BINARY-C-LONG */
  attribute :flag_is_pdiv_parm # is PROC DIV USING */
  attribute :flag_local_alloced # LOCAL storage is allocated */
  attribute :flag_no_init # no initialize unless used */

  class << self
    def build(cb, name)
      f = new(cb, category: :UNKNOWN)

      f.name = cb.define(name, f)
      f.ename = nil
      f.usage = :DISPLAY
      f.storage = :WORKING
      f.occurs_max = 1

      f
    end

    def add(f, p)
      return p if !f

      t = f
      t = t.sister while t.sister
      t.sister = p

      f
    end

    def build_constant(cb, name, value)
      x = build(cb, name)
      x.category = value.category
      x.storage = :CONSTANT
      x.values = [value]

      x
    end
  end

  def initialize(context, attributes = {})
    super

    return if @id

    @id = @cb.field_id
    cb.field_id += 1
  end

  def validate
    # TODO: field.c:973: cb_validate_field (struct cb_field *f)
  end
end
