package org.genericsystem.admin.model;

import org.genericsystem.admin.model.CarColor.DefaultCarColor;
import org.genericsystem.admin.model.Color.White;
import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.InstanceValueGenerator;
import org.genericsystem.api.core.annotations.constraints.SingularConstraint;
import org.genericsystem.api.core.annotations.value.StringValue;

/**
 * @author Nicolas Feybesse
 *
 */
@SystemGeneric
@Components({ Car.class, Color.class })
@SingularConstraint
@InstanceValueGenerator
@StringValue("CarColor")
@Dependencies(DefaultCarColor.class)
@InstanceValueClassConstraint(String.class)
public class CarColor {

	@SystemGeneric
	@Meta(CarColor.class)
	@StringValue("<Car-White>")
	@Components({ Car.class, White.class })
	public static class DefaultCarColor {
	}

}
