package org.genericsystem.newgui.context;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;

public interface IMetaContext {

	public List<IMetaContext> getChildren();

	public Class<? extends IModelContext> getClazz();

	public IModelContext buildContext(IModelContext parent);

	public static abstract class AbstractMetaContext implements IMetaContext {

		private List<IMetaContext> children = new ArrayList<>();

		@Override
		public IModelContext buildContext(IModelContext parent) {
			try {
				return getClazz().getConstructor(IModelContext.class).newInstance(parent);
			} catch (InstantiationException | IllegalAccessException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
				throw new IllegalStateException();
			}
		}

		public AbstractMetaContext(IMetaContext parent) {
			if (parent != null)
				parent.getChildren().add(this);
		}

		@Override
		public List<IMetaContext> getChildren() {
			return this.children;
		}
	}

	// public static class RootMetaContext extends AbstractMetaContext {
	//
	// public RootMetaContext(IMetaContext parent) {
	// super(parent);
	// }
	//
	// @Override
	// public Class<? extends IModelContext> getClazz() {
	// return RootContext.class;
	// }
	//
	// public static class SubContext extends AbstractModelContext {
	//
	// public ObservableValue<String> labelTextProperty;
	//
	// // public ObservableValue<Generic> observableValueGeneric;
	//
	// public SubContext(IModelContext parent) {
	// super(parent);
	// }
	// }
	//
	// public static class RootContext extends AbstractModelContext {
	//
	// public ObjectProperty<CocClientEngine> engineProperty;
	// public ObservableValue<String> titleColumnProperty;
	// public ObservableList<SubContext> subContextObservableList = FXCollections.observableArrayList();
	//
	// public RootContext(IModelContext parent) {
	// super(parent);
	// }
	//
	// public void initProperty(CocClientEngine engine) {
	// engineProperty = new SimpleObjectProperty<>(engine);
	// titleColumnProperty = Bindings.createStringBinding(() -> Objects.toString(engineProperty.getValue()), engineProperty);
	//
	// engineProperty.getValue().getCurrentCache().getInstancesObservableList(engine).forEach(generic -> {
	// SubContext subContext = new SubContext(this);
	// ObjectProperty<Generic> genericProperty = new SimpleObjectProperty<>(generic);
	// subContext.labelTextProperty = Bindings.createStringBinding(() -> Objects.toString(genericProperty.getValue()), genericProperty);
	// subContextObservableList.add(subContext);
	// System.out.println(generic.getValue());
	// });
	// }
	// }
	// }
}
