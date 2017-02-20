package org.genericsystem.geography.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.geography.model.Continent.ContinentCode;

@SystemGeneric
@Dependencies(ContinentCode.class)
public class Continent {

	// Attribute
	@SystemGeneric
	@Components(Continent.class)
	public static class ContinentCode {

	}

}
