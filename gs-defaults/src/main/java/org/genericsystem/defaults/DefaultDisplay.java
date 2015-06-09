package org.genericsystem.defaults;

import static org.genericsystem.api.core.ApiStatics.CONCRETE;
import static org.genericsystem.api.core.ApiStatics.META;
import static org.genericsystem.api.core.ApiStatics.SENSOR;
import static org.genericsystem.api.core.ApiStatics.STRUCTURAL;

import java.io.StringWriter;
import java.util.HashMap;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonValue;
import javax.json.JsonWriter;
import javax.json.stream.JsonGenerator;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.IVertex;

public interface DefaultDisplay<T extends DefaultVertex<T>> extends IVertex<T> {

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

	@Override
	default String toPrettyString() {
		StringWriter writer = new StringWriter();
		JsonWriter jsonWriter = Json.createWriterFactory(new HashMap<String, JsonValue>() {
			private static final long serialVersionUID = -8719498570554805477L;
			{
				put(JsonGenerator.PRETTY_PRINTING, JsonValue.TRUE);
			}
		}).createWriter(writer);
		// jsonWriter.write(toPrettyJSon());
		jsonWriter.write(toPrettyJSon());
		jsonWriter.close();
		return writer.toString();
	}

	@Override
	@SuppressWarnings("unchecked")
	default JsonObject toPrettyJSon() {
		JsonObjectBuilder builder = Json.createObjectBuilder();
		builder.add("Value", toString());
		for (T attribute : getAttributes()) {
			JsonArrayBuilder arrayBuilder = Json.createArrayBuilder();
			for (T holder : getHolders(attribute)) {
				if (holder.getComponents().get(0).isSpecializationOf((T) this))
					arrayBuilder.add(holder.toPrettyJSon());
				builder.add(attribute.toString(), arrayBuilder);
			}
		}
		return builder.build();
	}

	@Override
	default JsonObject toJSonId() {
		JsonObjectBuilder builder = Json.createObjectBuilder();
		builder.add("Id", System.identityHashCode(this));
		builder.add("Value", toString());
		builder.add("Meta", System.identityHashCode(getMeta()));
		JsonArrayBuilder arrayBuilder = Json.createArrayBuilder();
		for (T superVertex : getSupers())
			arrayBuilder.add(System.identityHashCode(superVertex));
		builder.add("Supers", arrayBuilder);

		for (T composite : getComponents())
			arrayBuilder.add(System.identityHashCode(composite));
		builder.add("Composites", arrayBuilder);
		return builder.build();
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
