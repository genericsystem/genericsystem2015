package org.genericsystem.geography.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.Supers;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.common.Generic;
import org.genericsystem.geography.model.AdministrativeTerritory.Adm1;
import org.genericsystem.geography.model.AdministrativeTerritory.Adm2;
import org.genericsystem.geography.model.AdministrativeTerritory.Adm3;

@SystemGeneric
@Dependencies({ Adm1.class, Adm2.class, Adm3.class })
public class AdministrativeTerritory implements Generic {

	// Inheritance

	// 1st level
	@SystemGeneric
	@Supers(AdministrativeTerritory.class)
	@Components(Country.class)
	public static class Adm1 {
	}

	// 2nd level
	@SystemGeneric
	@Supers(AdministrativeTerritory.class)
	@Components(Adm1.class)
	public static class Adm2 {
	}

	// 3rd level
	@SystemGeneric
	@Supers(AdministrativeTerritory.class)
	@Components(Adm2.class)
	public static class Adm3 {
	}

}
