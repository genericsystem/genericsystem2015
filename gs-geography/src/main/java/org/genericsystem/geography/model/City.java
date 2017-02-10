package org.genericsystem.geography.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Supers;
import org.genericsystem.api.core.annotations.SystemGeneric;

@SystemGeneric
@Supers(Subdivision.class)
@Components(AdministrativeTerritory.class)
public class City {
}
