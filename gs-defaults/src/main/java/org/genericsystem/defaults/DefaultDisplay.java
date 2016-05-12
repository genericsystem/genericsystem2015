package org.genericsystem.defaults;

import static org.genericsystem.api.core.ApiStatics.CONCRETE;
import static org.genericsystem.api.core.ApiStatics.META;
import static org.genericsystem.api.core.ApiStatics.SENSOR;
import static org.genericsystem.api.core.ApiStatics.STRUCTURAL;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.IGeneric;

/**
 * @author Nicolas Feybesse
 *
 * @param <T>
 */
public interface DefaultDisplay<T extends DefaultGeneric<T>> extends IGeneric<T> {

	@Override
	default String info() {
		return "(" + getMeta().getValue() + ")" + getSupers() + this + getComponents() + " ";
	}

	@Override
	default String detailedInfo() {
		String s = "\n\n*******************************" + System.identityHashCode(this) + "******************************\n";
		s += " Value       : " + getValue() + "\n";
		s += " Meta        : " + getMeta() + " (" + System.identityHashCode(getMeta()) + ")\n";
		s += " MetaLevel   : " + getMetaLevelString(getLevel()) + "\n";
		s += " Category    : " + getCategoryString(getLevel(), getComponents().size()) + "\n";
		s += " Class       : " + getClass().getName() + "\n";
		s += "**********************************************************************\n";
		for (T superGeneric : getSupers())
			s += " Super       : " + superGeneric + " (" + System.identityHashCode(superGeneric) + ")\n";
		for (T component : getComponents())
			s += " Component   : " + component + " (" + System.identityHashCode(component) + ")\n";
		s += "**********************************************************************\n";
		// s += "**********************************************************************\n";
		// s += "design date : " + new SimpleDateFormat(Statics.LOG_PATTERN).format(new Date(getDesignTs() / Statics.MILLI_TO_NANOSECONDS)) + "\n";
		// s += "birth date  : " + new SimpleDateFormat(Statics.LOG_PATTERN).format(new Date(getBirthTs() / Statics.MILLI_TO_NANOSECONDS)) + "\n";
		// s += "death date  : " + new SimpleDateFormat(Statics.LOG_PATTERN).format(new Date(getDeathTs() / Statics.MILLI_TO_NANOSECONDS)) + "\n";
		// s += "**********************************************************************\n";
		return s;
	}

	public static String getMetaLevelString(int metaLevel) {
		switch (metaLevel) {
		case META:
			return "META";
		case STRUCTURAL:
			return "STRUCTURAL";
		case CONCRETE:
			return "CONCRETE";
		case SENSOR:
			return "SENSOR";
		default:
			return "UNKNOWN";
		}
	}

	public static String getCategoryString(int metaLevel, int dim) {
		switch (metaLevel) {
		case ApiStatics.META:
			switch (dim) {
			case ApiStatics.TYPE_SIZE:
				return "MetaType";
			case ApiStatics.ATTRIBUTE_SIZE:
				return "MetaAttribute";
			case ApiStatics.RELATION_SIZE:
				return "MetaRelation";
			default:
				return "MetaNRelation";
			}
		case ApiStatics.STRUCTURAL:
			switch (dim) {
			case ApiStatics.TYPE_SIZE:
				return "Type";
			case ApiStatics.ATTRIBUTE_SIZE:
				return "Attribute";
			case ApiStatics.RELATION_SIZE:
				return "Relation";
			default:
				return "NRelation";
			}
		case ApiStatics.CONCRETE:
			switch (dim) {
			case ApiStatics.TYPE_SIZE:
				return "Instance";
			case ApiStatics.ATTRIBUTE_SIZE:
				return "Holder";
			case ApiStatics.RELATION_SIZE:
				return "Link";
			default:
				return "NLink";
			}
		default:
			return null;
		}
	}
}
