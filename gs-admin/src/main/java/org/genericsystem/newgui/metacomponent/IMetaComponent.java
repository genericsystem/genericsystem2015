package org.genericsystem.newgui.metacomponent;

import java.util.List;

import org.genericsystem.newgui.component.IComponent;
import org.genericsystem.newgui.component.IComponent.ButtonComponent;
import org.genericsystem.newgui.component.IComponent.TableViewComponent;
import org.genericsystem.newgui.component.IComponent.VBoxComponent;
import org.genericsystem.newgui.metacontext.IMetaContext;

public interface IMetaComponent {
	public IComponent apply(IMetaContext metaContext, IComponent parent);

	public List<IMetaComponent> getChildren();

	public static class TableViewMetaComponent extends AbstractMetaComponent {

		public TableViewMetaComponent(IMetaComponent parent) {
			super(parent);
		}

		@Override
		public IComponent apply(IMetaContext metaContext, IComponent parent) {
			TableViewComponent tableComponent = new TableViewComponent(parent);
			applyChildren(tableComponent, metaContext);
			return tableComponent;
		}
	}

	// public IComponent buildComponent(){
	// return new TableViewComponent(parent)
	// }

	public static class VBoxMetaComponent extends AbstractMetaComponent {
		public VBoxMetaComponent(IMetaComponent parent) {
			super(parent);
		}

		@Override
		public IComponent apply(IMetaContext metaContext, IComponent parent) {
			IComponent thisComponent = new VBoxComponent(parent);
			applyChildren(thisComponent, metaContext);
			// getChildren().forEach(metaComponentChild -> metaComponentChild.apply(metaContext, thisComponent));
			return thisComponent;
		}
	}

	public static class ButtonMetaComponent extends AbstractMetaComponent {
		public ButtonMetaComponent(IMetaComponent parent) {
			super(parent);
		}

		@Override
		public IComponent apply(IMetaContext metaContext, IComponent parent) {
			return new ButtonComponent(parent);
		}
	}

}
