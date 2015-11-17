package org.genericsystem.newgui.metacontext;

import java.util.List;
import java.util.Objects;

import javafx.beans.binding.Bindings;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.CocClientEngine;
import org.genericsystem.newgui.context.AbstractContext;
import org.genericsystem.newgui.context.IContext;

public interface IMetaContext {

	public List<IMetaContext> getChildren();

	public Class<? extends IContext> getClazz();

	public IContext buildContext(IContext parent);

	public static class RootMetaContext extends AbstractMetaContext {

		public RootMetaContext(IMetaContext parent) {
			super(parent);
		}

		@Override
		public Class<? extends IContext> getClazz() {
			return RootContext.class;
		}

		public static class SubContext extends AbstractContext {

			public ObservableValue<String> labelTextProperty;

			public SubContext(IContext parent) {
				super(parent);
			}
		}

		public static class RootContext extends AbstractContext {

			public ObjectProperty<CocClientEngine> engineProperty;
			public ObservableValue<String> titleColumnProperty;
			public ObservableList<SubContext> subContextObservableList = FXCollections.observableArrayList();

			public RootContext(IContext parent) {
				super(parent);
			}

			public void initProperty(CocClientEngine engine) {
				engineProperty = new SimpleObjectProperty<>(engine);
				titleColumnProperty = Bindings.createStringBinding(() -> Objects.toString(engineProperty.getValue()), engineProperty);

				engineProperty.getValue().getCurrentCache().getInstancesObservableList(engine).forEach(generic -> {
					SubContext subContext = new SubContext(this);
					ObjectProperty<Generic> genericProperty = new SimpleObjectProperty<>(generic);
					subContext.labelTextProperty = Bindings.createStringBinding(() -> Objects.toString(genericProperty.getValue()), genericProperty);
					subContextObservableList.add(subContext);
					System.out.println(generic.getValue());
				});
			}
		}
	}
}
