package org.genericsystem.distributed.cacheonserver.ui.list;

import org.genericsystem.distributed.cacheonserver.ui.list.TypeListModel.TitleTypeListModel;
import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.components.HtmlH1;
import org.genericsystem.distributed.ui.components.HtmlSection;

public class TypeListHtml<M extends TypeListModel> extends HtmlSection<M> {

	public TypeListHtml(HtmlElement<?, ?, ?> parent) {
		super(parent);
		addStyleClass("gstable");
	}

	@Override
	protected void initChildren() {
		new InstanceElementHtml<>(this).forEach(TypeListModel::getSubModels);
	}

	public static class TitleTypeListHtml<M extends TitleTypeListModel> extends TypeListHtml<M> {

		public TitleTypeListHtml(HtmlElement<?, ?, ?> parent) {
			super(parent);
		}

		@Override
		protected void initChildren() {
			new HtmlH1<TitleTypeListModel>(new HtmlSection<TitleTypeListModel>(this).addStyleClass("gsrow").addStyleClass("gstitlerow")).bindText(TitleTypeListModel::getListString);
			super.initChildren();
		}
	}

	public static class TypeComboboxHtml<M extends TypeListModel> extends TypeListHtml<M> {

		public TypeComboboxHtml(HtmlElement<?, ?, ?> parent) {
			super(parent);
		}

		@Override
		protected void initChildren() {
			new InstanceElementHtml<>(this).forEach(TypeListModel::getSubModels);
		}
	}
}
