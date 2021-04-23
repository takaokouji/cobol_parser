# frozen_string_literal: true

module CobolParser::TypeCheckHelper
  def emit(x)
    # TODO: current_statement
    # TODO: cb_list_add
    current_statement.body = cb_list_add(current_statement.body, x)
  end

  def cb_emit_list(l)
    # TODO: cb_list_append
    current_statement.body = cb_list_append(current_statement.body, l)
  end

  def build_registers
    # RETURN-CODE
    if current_program.nested_level > 0
      current_program.cb_return_code =
        @cb.build_index(@cb.build_reference("RETURN-CODE"), @cb.constants.zero, false, nil)
      current_program.cb_return_code.flag_is_global = true
    end

    # SORT-RETURN
    current_program.cb_sort_return =
      @cb.build_index(@cb.build_reference("SORT-RETURN"), @cb.constants.zero, false, nil)
    @cb.field(current_program.cb_sort_return).flag_no_init = true

    # NUMBER-OF-CALL-PARAMETERS
    current_program.cb_call_params =
      @cb.build_index(@cb.build_reference("NUMBER-OF-CALL-PARAMETERS"), @cb.constants.zero, false, nil)
    @cb.field(current_program.cb_call_params).flag_no_init = true

    t = Time.now

    # WHEN-COMPILED
    @cb.build_constant(
      @cb.build_reference("WHEN-COMPILED"),
      @cb.build_alphanumeric_literal(t.strftime("%m/%d/%y%H.%M.%S"))
    )

    # FUNCTION WHEN-COMPILED
    @constants.cb_intr_whencomp = @cb.build_alphanumeric_literal(t.strftime("%Y%m%d%H%M%S00%z"))

    # FUNCTION PI
    @constants.cb_intr_pi = @cb.build_numeric_literal(:UNSIGNED, "31415926535897932384626433832795029", 34)

    # FUNCTION E
    @constants.cb_intr_e = @cb.build_numeric_literal(:UNSIGNED, "27182818284590452353602874713526625", 34)
  end

  def build_index(x, values, indexed_by, qual)
    f = @cb.build_field(x)
    f.usage = :INDEX
    f.validate
    f.values = Array(values) if values
    f.index_qual = qual if qual
    indexed_by = false if indexed_by == 0
    f.flag_indexed_by = indexed_by

    current_program.working_storage = @cb.field_add(current_program.working_storage, f)

    x
  end

  def encode_program_id(name)
    name.gsub(/^\d/) { |x| "_%02X" % x.ord }
        .gsub("-", "__")
        .gsub(/[^\w]/) { |x| "_%02X" % x.ord }
  end

  def encode_class_name(name)
    name = "C#{name}" if /^[^A-Z]/ =~ name

    name.gsub("-", "_").split("_").map(&:capitalize).join
  end

  def build_program_id(name, alt_name)
    current_program.orig_source_name = if alt_name
                                         alt_name.data
                                       elsif name.is_a?(CobolParser::Tree::Literal)
                                         name.data
                                       else
                                         name.name
                                       end

    encode_class_name(current_program.orig_source_name)
  end

  def validate_move(src, dst, is_value)
    # TODO: typeck.c:3944: validate_move (cb_tree src, cb_tree dst, size_t is_value)
    # Check only, so this is unnecessary.
  end

  def build_add(v, n, round_opt)
    if v.index? || v.tree_class == :POINTER
      # TODO: @cb.build_binary_op(v, '+', n)
      # TODO: @cb.build_move(@cb.build_binary_op(v, '+', n), v)
      return @cb.build_move(@cb.build_binary_op(v, "+", n), v)
    end

    if v.ref_or_field?
      f = @cb.field(v)
      f.count += 1
    end
    if n.ref_or_field?
      f = @cb.field(n)
      f.count += 1
    end
    if round_opt == @cb.high
      # TODO: @cb.fits_int(n)
      # TODO: @cb.build_optim_add(v, n)
      return @cb.build_optim_add(v, n) if @cb.fits_int(n)

      # TODO: @cb.build_funcall_3("cob_add", v, n, @cb.int0)
      return @cb.build_funcall_3("cob_add", v, n, @cb.int0)
    end
    # build_store_option(v, round_opt)
    opt = build_store_option(v, round_opt)
    if opt == @cb.int0 && @cb.fits_int(n)
      return @cb.build_optim_add(v, n)
    end

    @cb.build_funcall_3("cob_add", v, n, opt)
  end

  # PERFORM statement

  def emit_perform(perform, body)
    return if perform == @cb.error_node

    perform.body = body
    # TODO: @cb.emit(perform)
    @cb.emit(perform)
  end

  def cb_build_perform_once(body)
    return @cb.error_node if body == @cb.error_node

    x = @cb.build_perform(:ONCE)
    x.body = body

    x
  end

  def build_perform_times(times)
    # TODO: @cb.check_integer_value(times)
    return @cb.error_node if @cb.check_integer_value(times) == @cb.error_node

    x = @cb.build_perform(:TIMES)
    x.data = times

    x
  end

  def cb_build_perform_until(condition, varying)
    x = @cb.build_perform(:UNTIL)
    x.test = condition
    x.varying = varying

    x
  end

  def cb_build_perform_forever(body)
    return @cb.error_node if body == @cb.error_node

    x = @cb.build_perform(:FOREVER)
    x.body = body

    x
  end

  def build_perform_exit(label)
    x = @cb.build_perform(:EXIT)
    x.data = label

    x
  end
end
