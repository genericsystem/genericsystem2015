package org.genericsystem.distributed.cacheonserver.ui.list;

import java.io.Serializable;
import java.util.Objects;
import java.util.function.Function;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.Transformation2;
import org.genericsystem.distributed.ui.models.GenericModel;

public class TypeListModel extends GenericModel {

	private final ObservableList<GenericModel> subModels;

	public ObservableList<GenericModel> getSubModels() {
		return subModels;
	}

	public TypeListModel(Generic type) {
		super(type);
		this.subModels = new Transformation2<>(type.getObservableSubInstances(), getElementModelBuilder());
	}

	public Function<Generic, GenericModel> getElementModelBuilder() {
		return GenericModel::new;
	}

	public static class TitleTypeListModel extends TypeListModel {

		public TitleTypeListModel(Generic generic) {
			super(generic);
		}

		public ObservableValue<String> getListString() {
			Serializable value = getGeneric().getValue();
			return new ReadOnlyObjectWrapper<>((value instanceof Class ? ((Class<?>) value).getSimpleName() : Objects.toString(value)) + "(s)" + " Management");
		}

	}
}
