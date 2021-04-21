# frozen_string_literal: true

module CobolParser::TypeCheckHelper
  def build_registers # rubocop:disable Metrics/AbcSize
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
    if alt_name
      current_program.orig_source_name = alt_name.data
    elsif name.is_a?(CobolParser::Tree::Literal)
      current_program.orig_source_name = name.data
    else
      current_program.orig_source_name = name.name
    end

    encode_class_name(current_program.orig_source_name)
  end
end