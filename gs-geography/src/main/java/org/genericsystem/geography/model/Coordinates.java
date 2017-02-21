package org.genericsystem.geography.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.geography.model.Coordinates.Latitude;
import org.genericsystem.geography.model.Coordinates.Longitude;

@SystemGeneric
@Components({ Latitude.class, Longitude.class })
public class Coordinates {

	@SystemGeneric
	public static class Latitude {
	}

	@SystemGeneric
	public static class Longitude {
	}

}
