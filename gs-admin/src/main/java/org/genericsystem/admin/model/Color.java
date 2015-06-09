package org.genericsystem.admin.model;

import org.genericsystem.admin.model.Color.Blue;
import org.genericsystem.admin.model.Color.Red;
import org.genericsystem.admin.model.Color.White;
import org.genericsystem.admin.model.Color.Yellow;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.value.StringValue;

/**
 * @author Nicolas Feybesse
 *
 */
@SystemGeneric
@Dependencies({ White.class, Red.class, Blue.class, Yellow.class })
@StringValue("Color")
@InstanceValueClassConstraint(String.class)
public class Color {

	@SystemGeneric
	@Meta(Color.class)
	@StringValue("White")
	public static class White {}

	@SystemGeneric
	@Meta(Color.class)
	@StringValue("Red")
	public static class Red {}

	@SystemGeneric
	@Meta(Color.class)
	@StringValue("Blue")
	public static class Blue {}

	@SystemGeneric
	@Meta(Color.class)
	@StringValue("Yellow")
	public static class Yellow {}
}
