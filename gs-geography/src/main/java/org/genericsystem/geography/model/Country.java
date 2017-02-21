package org.genericsystem.geography.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Supers;
import org.genericsystem.api.core.annotations.SystemGeneric;

@SystemGeneric
@Supers(AdministrativeTerritory.class)
@Components(Continent.class)
public class Country {
}
