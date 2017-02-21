package org.genericsystem.geography.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.geography.model.Subdivision.SubdivisionName;

@SystemGeneric
@Dependencies(SubdivisionName.class)
public class Subdivision {

	@SystemGeneric
	@Components(Subdivision.class)
	public static class SubdivisionName {
	}
}
