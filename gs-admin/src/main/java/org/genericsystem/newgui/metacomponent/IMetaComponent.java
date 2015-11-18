package org.genericsystem.newgui.metacomponent;

import java.util.List;

import org.genericsystem.newgui.component.IComponent;
import org.genericsystem.newgui.component.IComponent.ButtonComponent;
import org.genericsystem.newgui.component.IComponent.LabelComponent;
import org.genericsystem.newgui.component.IComponent.TableViewComponent;
import org.genericsystem.newgui.component.IComponent.VBoxComponent;
import org.genericsystem.newgui.context.IContext;
import org.genericsystem.newgui.metacontext.IMetaContext.RootMetaContext.RootContext;

public interface IMetaComponent {
	public IComponent apply(IComponent parent, IContext context);

	public List<IMetaComponent> getChildren();

	public IComponent buildComponent(IComponent parent, IContext context);

	public static class LabelMetaComponent extends AbstractMetaComponent {

		public LabelMetaComponent(IMetaComponent parent) {
			super(parent);

		}

		@Override
		public IComponent apply(IComponent parent, IContext context) {
			IComponent comp = buildComponent(parent, context);
			return comp;
		}

		@Override
		public IComponent buildComponent(IComponent parent, IContext context) {
			return new LabelComponent(parent, context);
		}

	}

	public static class TableViewMetaComponent extends AbstractMetaComponent {

		public TableViewMetaComponent(IMetaComponent parent) {
			super(parent);
		}

		@Override
		public IComponent apply(IComponent parent, IContext context) {
			IComponent tableComponent = buildComponent(parent, context);

			applyChildren(tableComponent, context);
			return tableComponent;
		}

		@Override
		public IComponent buildComponent(IComponent parent, IContext context) {
			return new TableViewComponent(parent, context);
		}
	}

	public static class VBoxMetaComponent extends AbstractMetaComponent {
		public VBoxMetaComponent(IMetaComponent parent) {
			super(parent);
		}

		@Override
		public IComponent apply(IComponent parent, IContext context) {
			IComponent thisComponent = buildComponent(parent, context);
			applyChildren(thisComponent, context);
			return thisComponent;
		}

		@Override
		public IComponent buildComponent(IComponent parent, IContext context) {
			VBoxComponent vb = new VBoxComponent(parent, context);
			System.out.println("VBoxMetaComponent::buildComponent ::" + ((RootContext) context).subContextObservableList.size());
			((RootContext) context).subContextObservableList.forEach(sub -> new LabelComponent(vb, sub));
			return vb;
		}
	}

	public static class ButtonMetaComponent extends AbstractMetaComponent {
		public ButtonMetaComponent(IMetaComponent parent) {
			super(parent);
		}

		@Override
		public IComponent apply(IComponent parent, IContext context) {
			return buildComponent(parent, context);
		}

		@Override
		public IComponent buildComponent(IComponent parent, IContext context) {
			return new ButtonComponent(parent, context);
		}
	}

}
