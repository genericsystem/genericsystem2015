package org.genericsystem.distributed.cacheonserver.ui.list;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.components.HtmlH1;
import org.genericsystem.distributed.ui.components.HtmlSection;
import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.GenericModel;
import org.genericsystem.distributed.ui.models.StringModel;

public class TypeSectionHtml<M extends CompositeModel<SUBMODEL>, SUBMODEL extends StringModel> extends HtmlSection<M> {

	public TypeSectionHtml(HtmlElement<?, ?, ?> parent) {
		super(parent);
		addStyleClass("gstable");
	}

	@Override
	protected void initChildren() {
		new InstanceSectionHtml<SUBMODEL>(this).forEach(CompositeModel<SUBMODEL>::getSubModels);
	}

	public static class TitleTypeListHtml<M extends TitleGenericCompositeModel> extends TypeSectionHtml<M, GenericModel> {

		public TitleTypeListHtml(HtmlElement<?, ?, ?> parent) {
			super(parent);
		}

		@Override
		protected void initChildren() {
			new HtmlH1<M>(new HtmlSection<M>(this).addStyleClass("gsrow").addStyleClass("gstitlerow")).bindText(TitleGenericCompositeModel::getString);
			super.initChildren();
		}
	}
}
