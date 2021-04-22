# frozen_string_literal: true

require "fiddle"

# Field
class CobolParser::Tree::Field < CobolParser::Tree
  PIC_DIGITS = [2, 4, 7, 9, 12, 14, 16, 18].freeze

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

  module Helper
    # porting from cobc/tree.c

    def build_field(name)
      CobolParser::Tree::Field.new(self, name)
    end

    def build_implicit_field(name, len)
      x = build_field(name)
      x.pic = build_picture("X(#{len})")
      x.validate

      x
    end

    def build_constant(name, value)
      x = build_field(name)
      x.category = value.category
      x.storage = :CONSTANT
      x.values = @cb.list_init(value)

      x
    end

    def field(x)
      if x.is_a?(CobolParser::Tree::Reference)
        x.ref
      else
        x
      end
    end

    def field_add(f, p)
      return p if !f

      f.add(p)
    end

    # porting from cobc/field.c

    def build_field_tree(level, name, last_field, storage, fn)
      return @cb.error_node if level == @cb.error_node || name == @cb.error_node

      # check the level number
      lv = level.level_from_name
      return @cb.error_node if lv == 0

      # build the field
      f = @cb.build_field(name)
      f.storage = storage
      @last_real_field = last_field
      if lv == 78
        f.level = 1
        f.flag_item_78 = true
        return f
      else
        f.level = lv
      end
      if f.level == 1 && storage == :FILE
        if fn.external
          f.flag_external = true
          @cb.has_external = true
        elsif fn.global
          f.flag_is_global = true
        end
      end
      if last_field && (last_field.level == 77 && f.level != 1 && f.level != 77 && f.level != 66 && f.level != 88)
        @cb.error_x(name, "Level number must begin with 01 or 77")
        return @cb.error_node
      end

      # checks for redefinition
      r = name
      if @cb.warn_redefinition && r.word.count > 1
        if f.level == 1 || f.level == 77
          @cb.redefinition_warning(name, nil)
        else
          r.word.items.each_chain do |l|
            x = l.value
            next unless !x.is_a?(CobolParser::Tree::Field) || x.level == 1 || x.level == 77 ||
                        (f.level == last_field.level && x.parent == last_field.parent)

            @cb.redefinition_warning(name, x)
            break
          end
        end
      end

      last_field = last_field.parent if last_field && last_field.level == 88

      # link the field into the tree
      if f.level == 1 || f.level == 77
        # top level
        @cb.needs_01 = false
        last_field.founder.sister = f if last_field
      elsif !last_field || @cb.needs_01
        # invalid top level
        @cb.error_x(name, "Level number must begin with 01 or 77")
        return @cb.error_node
      elsif f.level == 66
        # TODO: field.c:168:
        raise NotImplementedError
      elsif f.level == 88
        # level 88
        f.parent = last_field
      elsif f.level > last_field.level
        # lower level
        last_field.children = f
        f.parent = last_field
      elsif f.level == last_field.level
        # same level
        last_field.sister = f
        f.parent = last_field.parent
      else
        # upper level
        same_level_flag = false
        last_field.each_parent do |p|
          if p.level == f.level
            last_field = p
            # NOTE: goto same_level;
            last_field.sister = f
            f.parent = last_field.parent
            same_level_flag = true
            break
          end
          break if @cb.relax_level_hierarchy && p.level < f.level
        end
        if !same_level_flag
          if @cb.relax_level_hierarchy
            dummy_fill = @cb.build_filler
            field_fill = @cb.build_field(dummy_fill)
            @cb.warning_x(name, "No previous data item of level %02d", f.level)
            field_fill.level = flevel
            field_fill.storage = storage
            field_fill.children = p.children
            field_fill.parent = p
            p.children.each_sister do |p|
              p.parent = field_fill
            end
            field_fill.parent.children = field_fill
            field_fill.sister = f
            f.parent = field_fill.parent
            last_field = field_fill
          else
            @cb.error_x(name, "No previous data item of level %02d", f.level)
            return @cb.error_node
          end
        end
      end

      # inherit parent's properties
      if f.parent
        f.usage = f.parent.usage
        f.indexes = f.parent.indexes
        f.flag_sign_leading = f.parent.flag_sign_leading
        f.flag_sign_separate = f.parent.flag_sign_separate
        f.flag_is_global = f.parent.flag_is_global
      end

      f
    end

    def validate_field(p)
      p.validate
    end

    attr_accessor :last_real_field

    def clear_real_field
      @last_real_field = nil
    end
  end

  def initialize(cb, name)
    super(cb, category: :UNKNOWN)

    @id = @cb.field_id
    @cb.field_id += 1

    @name = @cb.define(name, self)
    @ename = nil
    @usage = :DISPLAY
    @storage = :WORKING
    @occurs_max = 1
  end

  def add(p)
    t = self
    t = t.sister while t.sister
    t.sister = p

    self
  end

  def validate
    if !validate_field_1
      @flag_invalid = true
      return
    end

    # RXW - Remove
    if flag_item_78
      @flag_is_verified = true
      return
    end

    # setup parameters
    @flag_local = true if storage == :LOCAL || storage == :LINKAGE || flag_item_based
    @flag_base = true if storage == :LINKAGE || flag_item_based
    setup_parameters

    # compute size
    compute_size
    if !redefines
      @memory_size = size * occurs_max
    elsif redefines.memory_size < size * occurs_max
      redefines.memory_size = size * occurs_max
    end

    validate_field_value
    if flag_is_global
      @count += 1
      children.each_sister do |c|
        c.flag_is_global = true
        c.count += 1
      end
    end
    @flag_is_verified = true
  end

  def validate_field_1
    x = self
    # TODO: field.c:308: name = cb_name (x);
    if flag_any_length
      # TODO: field.c:310:
      raise NotImplementedError
    end

    if level == 77
      # TODO: field.c:339:
      raise NotImplementedError
    end

    if flag_external
      # TODO: field.c:346:
      raise NotImplementedError
    end

    if flag_item_based
      # TODO: field.c:363:
      raise NotImplementedError
    end

    if level == 66
      # TODO: field.c:376:
      raise NotImplementedError
    end

    # validate OCCURS
    if flag_occurs
      if (!@cb.verify(@cb.top_level_occurs_clause, "01/77 OCCURS") && (level == 1 || level == 77)) ||
         (level == 66 || level == 88)
        # TODO: level_redundant_error(x, "OCCURS")
        raise NotImplementedError
      end

      index_list.each_chain do |l|
        @cb.field(l.value).flag_is_global = flag_is_global
      end
    end

    # validate OCCURS DEPENDING
    if occurs_depending
      # TODO: field.c:399:
      raise NotImplementedError
    end

    # validate REDEFINES
    if redefines
      # TODO: field.c:448:
    end

    if children
      # group item
      # TODO: field.c:472:

      children.each_sister do |f|
        return false if !f.validate_field_1
      end
    else
      # elementary item

      # validate PICTURE
      need_picture_usages = %i[
        INDEX
        LENGTH
        OBJECT
        POINTER
        PROGRAM_POINTER
        FLOAT
        DOUBLE
        SIGNED_CHAR
        SIGNED_SHORT
        SIGNED_INT
        SIGNED_LONG
        UNSIGNED_CHAR
        UNSIGNED_SHORT
        UNSIGNED_INT
        UNSIGNED_LONG
        PROGRAM
      ]
      need_picture = !need_picture_usages.include?(usage)
      if pic.nil? && need_picture
        if storage == :SCREEN # rubocop:disable Style/GuardClause
          # TODO: field.c:513:
          raise NotImplementedError
        elsif flag_item_78 && values && values.value != @cb.error_node # rubocop:disable Lint/DuplicateBranch
          # TODO: field.c:524
          raise NotImplementedError
        else
          if flag_item_78
            @cb.error_x(x, "Value required for constant item '%s'", name)
          else
            @cb.error_x(x, "PICTURE clause required for '%s'", name)
          end
          return false
        end
      end

      @cb.error_x(x, "'%s' cannot have PICTURE clause", name) if !pic.nil? && !need_picture

      # validate USAGE
      case usage
      when :SIGNED_CHAR
        @usage = :COMP_5
        @pic = @cb.build_picture("S99")
        @flag_real_binary = true
      when :SIGNED_SHORT
        @usage = :COMP_5
        @pic = @cb.build_picture("S9(4)")
        @flag_real_binary = true
      when :SIGNED_INT
        @usage = :COMP_5
        @pic = @cb.build_picture("S9(9)")
        @flag_real_binary = true
      when :SIGNED_LONG
        @usage = :COMP_5
        @pic = @cb.build_picture("S9(18)")
        @flag_real_binary = true
      when :UNSIGNED_CHAR
        @usage = :COMP_5
        @pic = @cb.build_picture("99")
        @flag_real_binary = true
      when :UNSIGNED_SHORT
        @usage = :COMP_5
        @pic = @cb.build_picture("9(4)")
        @flag_real_binary = true
      when :UNSIGNED_INT
        @usage = :COMP_5
        @pic = @cb.build_picture("9(9)")
        @flag_real_binary = true
      when :UNSIGNED_LONG
        @usage = :COMP_5
        @pic = @cb.build_picture("9(18)")
        @flag_real_binary = true
      when :BINARY, :PACKED
        @cb.error_x(x, "'%s' PICTURE clause not compatible with USAGE", name) if pic.category != :NUMERIC
      when :COMP_5, :COMP_X
        if pic && pic.category != :NUMERIC && pic.category != :ALPHANUMERIC
          @cb.error_x(x, "'%s' PICTURE clause not compatible with USAGE", name)
        end
      end

      # validate SIGN

      # validate JUSTIFIED RIGHT
      if flag_justified && !%i[ALPHABETIC ALPHANUMERIC].include?(pic.category)
        @cb.error_x(x, "'%s' cannot have JUSTIFIED RIGHT", name)
      end

      # validate SYNCHRONIZED

      # validate BLANK ZERO
      if flag_blank_zero
        # TODO: field.c:646:
      end

      # validate VALUE
      if values
        # TODO: field.c:683:
      end
    end
  end

  def each_sister
    return enum_for(:each_sister) if !block_given?

    f = self
    while f
      yield f
      f = f.sister
    end
  end

  def each_parent
    return enum_for(:each_parent) if !block_given?

    f = parent
    while f
      yield f
      f = f.parent
    end
  end

  def founder
    f = self
    f = f.parent while f.parent
    f
  end

  def initial_value
    v = values&.value || nil
    case pic.field_category
    when :ALPHABETIC, :ALPHANUMERIC, :ALPHANUMERIC_EDITED
      v&.data.to_s
    when :NUMERIC, :NUMERIC_EDITED
      if pic.scale == 0
        v&.data.to_i
      elsif v && v.scale > 0
        s = v.data.dup
        s[-v.scale] = "." + s[-v.scale]
        s.to_f
      else
        v&.data.to_f
      end
    else
      raise NotImplementedError
    end
  end

  private

  def setup_parameters
    # determine the class
    if children
      # group field
      f = children
      while f
        f.flag_local = flag_local
        f.setup_parameters

        f = f.siter
      end
    else
      # regular field
      case usage
      when :BINARY
        @flag_binary_swap = true if @cb.binary_byteorder == :BIG_ENDIAN
      when :INDEX
        @pic = @cb.build_picture("S9(9)")
      when :LENGTH
        @pic = @cb.build_picture("9(9)")
      when :POINTER, :PROGRAM_POINTER
        @pic = @cb.build_picture("9(10)")
      when :FLOAT
        @pic = @cb.build_picture("S9(7)V9(7)")
      when :DOUBLE
        @pic = @cb.build_picture("S9(9)V9(9)")
      when :COMP_5, :COMP_X
        if pic.category == :ALPHANUMERIC
          s = if pic.size > 8
                "9(36)"
              else
                "9(#{PIC_DIGITS[pic.size - 1]})"
              end
          @pic = @cb.build_picture(s)
        end
        @flag_binary_swap = true if usage == :COMP_X && @cb.binary_byteorder == :BIG_ENDIAN
      end
    end
  end

  def compute_size_children
    result = 0
    children.each_sister do |c|
      if c.redefines
        c.offset = c.redefines.offset
        c.compute_size
        # increase the size if redefinition is larger
        if c.level != 66 && c.size * c.occurs_max > c.redefines.size * c.redefines.occurs_max
          if @cb.larger_redefines_ok
            @cb.warning_x(c, "Size of '%s' larger than size of '%s'", c.name, c.redefines.name)
            result += (c.size * c.occurs_max) - (c.redefines.size * c.redefines.occurs_max)
          else
            @cb.error_x(c, "Size of '%s' larger than size of '%s'", c.name, c.redefines.name)
          end
        end
      else
        c.offset = offset + result
        result += c.compute_size * c.occurs_max

        # word alignment
        if flag_synchronized && @cb.verify(@cb.synchronized_clause, "SYNC")
          align_size = 1
          case c.usage
          when :BINARY, :COMP_5, :COMP_X, :FLOAT, :DOUBLE
            align_size = c.size if c.size == 2 || c.size == 4 || c.size == 8
          when :INDEX, :LENGTH
            align_size = Fiddle::SIZEOF_INT
          when :OBJECT, :POINTER, :PROGRAM_POINTER, :PROGRAM
            align_size = Fiddle::SIZEOF_VOIDP
          end
          if c.offset % align_size != 0
            pad = align_size - (c.offset % align_size)
            c.offset += pad
            result += pad
          end
        end
      end
    end

    result
  end

  def compute_size_elementary_item
    case usage
    when :COMP_X
      return nil if pic.category == :ALPHANUMERIC

      pic_size = pic.size
      if pic_size <= 2
        1
      elsif pic_size <= 4
        2
      elsif pic_size <= 7
        3
      elsif pic_size <= 9
        4
      elsif pic_size <= 12
        5
      elsif pic_size <= 14
        6
      elsif pic_size <= 16
        7
      else
        pic_size <= 18 ? 8 : 16
      end
    when :BINARY, :COMP_5
      pic_size = pic.size
      if pic_size > 18
        @flag_binary_swap = false
        @cb.error_x(self, "'%s' binary field cannot be larger than 18 digits", name)
      end

      case @cb.binary_size
      when :"2_4_8"
        if flag_real_binary && pic_size <= 2
          1
        elsif pic_size <= 4
          2
        elsif pic_size <= 9
          4
        else
          pic_size <= 18 ? 8 : 16
        end
      when :"1_2_4_8"
        if pic_size <= 2
          1
        elsif pic_size <= 4
          2
        elsif pic_size <= 9
          4
        else
          pic_size <= 18 ? 8 : 16
        end
      when :"1__8"
        if pic.have_sign
          if pic_size <= 2
            1
          elsif pic_size <= 4
            2
          elsif pic_size <= 6
            3
          elsif pic_size <= 9
            4
          elsif pic_size <= 11
            5
          elsif pic_size <= 14
            6
          elsif pic_size <= 16
            7
          else
            pic_size <= 18 ? 8 : 16
          end
        elsif pic_size <= 2
          1
        elsif pic_size <= 4
          2
        elsif pic_size <= 7
          3
        elsif pic_size <= 9
          4
        elsif pic_size <= 12
          5
        elsif pic_size <= 14
          6
        elsif pic_size <= 16
          7
        else
          pic_size <= 18 ? 8 : 16
        end
      end
    when :DISPLAY
      pic_size = pic.size
      pic_size += 1 if pic.category == :NUMERIC && pic.have_sign && flag_sign_separate
      pic_size
    when :PACKED
      pic.size / 2 + 1
    when :INDEX, :LENGTH
      Fiddle::SIZEOF_INT
    when :FLOAT
      Fiddle::SIZEOF_FLOAT
    when :DOUBLE
      Fiddle::SIZEOF_DOUBLE
    when :OBJECT, :POINTER, :PROGRAM_POINTER, :PROGRAM
      Fiddle::SIZEOF_VOIDP
    else
      abort
    end
  end

  def compute_size
    if level == 66
      # TODO: field.c:780
    end

    if children
      @size = compute_size_children
    else
      computed = compute_size_elementary_item
      @size = computed if computed
    end

    # the size of redefining field should not be larger than
    # the size of redefined field unless the redefined field
    # is level 01 and non-external
    if redefines&.flag_external &&
       size * occurs_max > redefines.size * redefines.occurs_max
      if @cb.larger_redefines_ok
        @cb.warning_x(self, "Size of '%s' larger than size of '%s'", name, redefines.name)
      else
        @cb.error_x(self, "Size of '%s' larger than size of '%s'", name, redefines.name)
      end
    end

    size
  end

  def validate_field_value
    @cb.validate_move(values.value, self, true) if values

    return if !children

    children.each_sister(&:validate_field_value)
  end
end
