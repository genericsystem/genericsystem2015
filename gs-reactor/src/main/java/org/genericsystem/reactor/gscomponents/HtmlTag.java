package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.modelproperties.ActionDefaults;
import org.genericsystem.reactor.modelproperties.GSBuilderDefaults;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;
import org.genericsystem.reactor.modelproperties.StepperDefaults;

import java.util.function.Consumer;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.ReactorStatics;

import io.vertx.core.json.JsonObject;
import javafx.beans.property.Property;

public class HtmlTag {

	public static class HtmlButton extends TagImpl implements ActionDefaults, GSBuilderDefaults, SelectionDefaults {

		@Override
		public String getTag() {
			return "button";
		}

		@Override
		public HtmlDomNode createNode(HtmlDomNode parent, Context modelContext) {
			return new HtmlDomNode(parent, modelContext, this) {

				@Override
				public void handleMessage(JsonObject json) {
					((ActionDefaults) getTag()).getActionProperty(getModelContext()).getValue().accept(new Object());
				}
			};
		}
	}

	public static class HtmlCheckBox extends TagImpl {

		@Override
		public String getTag() {
			return "input";
		}

		@Override
		public HtmlDomNode createNode(HtmlDomNode parent, Context modelContext) {
			return new HtmlDomNode(parent, modelContext, this) {
				{
					HtmlCheckBox.this.addAttribute("type", "checkbox");
				}

				@Override
				public void handleMessage(JsonObject json) {
					getTag().getDomNodeAttributes(getModelContext()).put(ReactorStatics.CHECKED, json.getBoolean(ReactorStatics.CHECKED) ? ReactorStatics.CHECKED : "");
				}
			};
		}
	}

	public static class HtmlDatalist extends TagImpl {

		@Override
		public String getTag() {
			return "datalist";
		}

		@Override
		public HtmlDomNode createNode(HtmlDomNode parent, Context modelContext) {
			return new HtmlDomNode(parent, modelContext, this) {

				@Override
				public void handleMessage(JsonObject json) {
					if (UPDATE.equals(json.getString(MSG_TYPE))) {
						((SelectionDefaults) getTag()).getSelectionIndex(getModelContext()).setValue(json.getInteger(SELECTED_INDEX));
					}
				}
			};
		}
	}

	public static class HtmlDiv extends TagImpl {

		@Override
		public String getTag() {
			return "div";
		}
	}

	public static class HtmlFooter extends TagImpl {

		@Override
		public String getTag() {
			return "footer";
		}
	}

	public static class HtmlH1 extends TagImpl {

		@Override
		public String getTag() {
			return "h1";
		}
	}

	public static class HtmlH2 extends TagImpl {

		@Override
		public String getTag() {
			return "h2";
		}
	}

	public static class HtmlHeader extends TagImpl {

		@Override
		public String getTag() {
			return "header";
		}
	}

	public static class HtmlHyperLink extends TagImpl implements SelectionDefaults, StepperDefaults, ActionDefaults {

		@Override
		public String getTag() {
			return "a";
		}

		@Override
		public HtmlDomNode createNode(HtmlDomNode parent, Context modelContext) {
			return new HtmlDomNode(parent, modelContext, this) {

				@Override
				public void handleMessage(JsonObject json) {
					((ActionDefaults) getTag()).getActionProperty(getModelContext()).getValue().accept(new Object());
				}
			};
		}
	}

	public static class HtmlImg extends TagImpl {

		@Override
		public String getTag() {
			return "img";
		}
	}

	public static class HtmlInputText extends TagImpl implements ActionDefaults {

		@Override
		public String getTag() {
			return "input";
		}

		@Override
		public HtmlDomNode createNode(HtmlDomNode parent, Context modelContext) {
			return new HtmlDomNode(parent, modelContext, this) {

				{
					HtmlInputText.this.addAttribute("type", "HtmlInputText");
				}

				@Override
				public void handleMessage(JsonObject json) {
					super.handleMessage(json);
					if (ADD.equals(json.getString(MSG_TYPE))) {
						Property<Consumer<Object>> action = ((ActionDefaults) getTag()).getActionProperty(getModelContext());
						if (action != null)
							action.getValue().accept(new Object());
					}
					if (UPDATE.equals(json.getString(MSG_TYPE)))
						getTag().getDomNodeAttributes(getModelContext()).put("value", json.getString(TEXT_CONTENT));
				}
			};
		}
	}

	public static class HtmlLabel extends TagImpl {

		@Override
		public String getTag() {
			return "label";
		}

		public static class GSLabelDisplayer extends HtmlLabel {

			public GSLabelDisplayer() {
				bindText();
			}
		}
	}

	public static class HtmlLi extends TagImpl {

		@Override
		public String getTag() {
			return "li";
		}
	}

	public static class HtmlOption extends TagImpl {

		@Override
		public String getTag() {
			return "option";
		}
	}

	public static class HtmlP extends TagImpl {

		@Override
		public String getTag() {
			return "p";
		}
	}

	public static class HtmlRadio extends TagImpl implements ActionDefaults {

		@Override
		public HtmlDomNode createNode(HtmlDomNode parent, Context modelContext) {
			return new HtmlDomNode(parent, modelContext, this) {

				{
					HtmlRadio.this.addAttribute("type", "HtmlRadio");

				}
			};
		}

		@Override
		public String getTag() {
			return "input";
		}
	}

	public static class HtmlSection extends TagImpl {

		@Override
		public String getTag() {
			return "section";
		}
	}

	public static class HtmlSelect extends TagImpl {

		@Override
		public String getTag() {
			return "select";
		}

		@Override
		public HtmlDomNode createNode(HtmlDomNode parent, Context modelContext) {
			return new HtmlDomNode(parent, modelContext, this) {

				@Override
				public void handleMessage(JsonObject json) {
					if (UPDATE.equals(json.getString(MSG_TYPE))) {
						((SelectionDefaults) getTag()).getSelectionIndex(getModelContext()).setValue(json.getInteger(SELECTED_INDEX));
					}
				}
			};
		}
	}

	public static class HtmlSpan extends TagImpl {

		@Override
		public String getTag() {
			return "span";
		}
	}

	public static class HtmlStrong extends TagImpl {

		@Override
		public String getTag() {
			return "strong";
		}
	}

	public static class HtmlUl extends TagImpl {

		@Override
		public String getTag() {
			return "ul";
		}
	}
}
