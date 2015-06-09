package org.genericsystem.defaults;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.IVertex.SystemProperty;
import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.Supers;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.api.core.annotations.value.AxedPropertyClassValue;
import org.genericsystem.api.core.annotations.value.BooleanValue;
import org.genericsystem.api.core.annotations.value.EngineValue;

public class DefaultConfig {
	@SystemGeneric
	@Meta(MetaAttribute.class)
	@Supers(DefaultRoot.class)
	@Components(DefaultRoot.class)
	@EngineValue
	@Dependencies(SystemMap.class)
	public static class MetaAttribute {
	}

	@SystemGeneric
	@Meta(MetaAttribute.class)
	@Components(DefaultRoot.class)
	@Dependencies({ NoReferentialIntegrityProperty.class, NonHeritableProperty.class, CascadeRemoveProperty.class, org.genericsystem.defaults.constraints.InstanceValueClassConstraint.class, org.genericsystem.defaults.constraints.PropertyConstraint.class,
			org.genericsystem.defaults.constraints.RequiredConstraint.class, org.genericsystem.defaults.constraints.SingularConstraint.class, org.genericsystem.defaults.constraints.UniqueValueConstraint.class, InstanceValueGeneratorProperty.class })
	public static class SystemMap {

	}

	@SystemGeneric
	@Meta(MetaAttribute.class)
	@Supers(SystemMap.class)
	@Components(MetaAttribute.class)
	@Dependencies({ DefaultNoReferentialAxedIntegrityProperty.class })
	@InstanceValueClassConstraint(Boolean.class)
	@PropertyConstraint
	public static class NoReferentialIntegrityProperty implements SystemProperty {
	}

	@SystemGeneric
	@Meta(MetaAttribute.class)
	@Supers(NoReferentialIntegrityProperty.class)
	@Components(MetaAttribute.class)
	@AxedPropertyClassValue(propertyClass = NoReferentialIntegrityProperty.class, pos = ApiStatics.BASE_POSITION)
	@Dependencies({ DefaultValue.class })
	public static class DefaultNoReferentialAxedIntegrityProperty {
	}

	@SystemGeneric
	@Meta(DefaultNoReferentialAxedIntegrityProperty.class)
	@Components(MetaAttribute.class)
	@BooleanValue(true)
	public static class DefaultValue {
	}

	@SystemGeneric
	@Meta(MetaAttribute.class)
	@Supers(SystemMap.class)
	@Components(DefaultRoot.class)
	@InstanceValueClassConstraint(Boolean.class)
	@PropertyConstraint
	public static class NonHeritableProperty implements SystemProperty {

	}

	@SystemGeneric
	@Meta(MetaAttribute.class)
	@Supers(SystemMap.class)
	@Components(MetaAttribute.class)
	@InstanceValueClassConstraint(Boolean.class)
	@PropertyConstraint
	@Deprecated
	public static class CascadeRemoveProperty implements SystemProperty {

	}

	@SystemGeneric
	@Meta(MetaAttribute.class)
	@Supers(SystemMap.class)
	@Components(DefaultRoot.class)
	@InstanceValueClassConstraint(Class.class)
	@PropertyConstraint
	public static class InstanceValueGeneratorProperty implements SystemProperty {

	}

	@SystemGeneric
	@Meta(MetaRelation.class)
	@Supers(MetaAttribute.class)
	@Components({ DefaultRoot.class, DefaultRoot.class })
	@EngineValue
	public static class MetaRelation {
	}

	@SystemGeneric
	@Components(DefaultRoot.class)
	@InstanceValueClassConstraint(Integer.class)
	@PropertyConstraint
	public static class Sequence {

	}

}
