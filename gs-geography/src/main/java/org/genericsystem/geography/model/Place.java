package org.genericsystem.geography.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.geography.model.Place.Latitude;
import org.genericsystem.geography.model.Place.Longitude;

@SystemGeneric
@Dependencies({ Latitude.class, Longitude.class })
public class Place {

	@SystemGeneric
	@Components(Place.class)
	@InstanceValueClassConstraint(Double.class)
	public static class Latitude {
	}

	@SystemGeneric
	@Components(Place.class)
	@InstanceValueClassConstraint(Double.class)
	public static class Longitude {
	}

}
