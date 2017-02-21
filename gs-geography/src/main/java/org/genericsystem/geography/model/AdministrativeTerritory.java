package org.genericsystem.geography.model;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.Supers;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.common.Generic;
import org.genericsystem.geography.model.AdministrativeTerritory.Adm1;
import org.genericsystem.geography.model.AdministrativeTerritory.Adm2;
import org.genericsystem.geography.model.AdministrativeTerritory.Adm3;
import org.genericsystem.geography.model.AdministrativeTerritory.AdmCode;
import org.genericsystem.geography.model.AdministrativeTerritory.AdministrativeTerritoryInstance;

@SystemGeneric
@InstanceClass(AdministrativeTerritoryInstance.class)
@Dependencies({ AdmCode.class, Adm1.class, Adm2.class, Adm3.class, AdministrativeTerritoryInstance.class })
public class AdministrativeTerritory implements Generic {

	// Attribute
	@SystemGeneric
	@Components(AdministrativeTerritory.class)
	public static class AdmCode {
	}

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

	public static class AdministrativeTerritoryInstance implements Generic {
		public String getCode() {
			String str = (String) this.getValue();
			Generic g = this;
			while (g.getBaseComponent() != null) {
				g = g.getBaseComponent();
				str += ", " + g.getValue();
			}
			return str;
		};
	}

}
