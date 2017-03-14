package org.genericsystem.geography.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.Supers;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.geography.model.Building.Castle;

@SystemGeneric
@Components(City.class)
@Supers(Place.class)
@Dependencies({ Castle.class, Building.class })
public class Building {

	@SystemGeneric
	@Supers(Building.class)
	@Components(City.class)
	public static class Castle {
	}

	@SystemGeneric
	@Supers(Building.class)
	@Components(City.class)
	public static class Church {
	}

}