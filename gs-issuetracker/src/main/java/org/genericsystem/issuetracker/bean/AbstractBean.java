package org.genericsystem.issuetracker.bean;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.mutability.Generic;

public abstract class AbstractBean {

	public ElStringWrapper updateSingleHolder(Generic issue, Generic attribute) {
		return new ElStringWrapper() {

			@Override
			public void setValue(String value) {
				Generic searchedTarget = attribute.getTargetComponent();
				if (searchedTarget == null)
					issue.setHolder(attribute, value);
				else
					issue.setHolder(attribute, null, searchedTarget.getInstance(value));
			}

			@Override
			public String getValue() {
				Generic link = issue.getLinks(attribute).first();
				if (link != null)
					return (link.getTargetComponent() != null) ? Objects.toString(link.getTargetComponent().getValue()) : Objects.toString(link.getValue());
				return null;
			}

			@Override
			public void setValues(List<String> selectedTargets) {
				for (String selectedTarget : selectedTargets)
					setValue(selectedTarget);
				for (Generic link : getTargets(attribute))
					if (!selectedTargets.contains(link.getValue()))
						link.remove();
			}

			private Snapshot<Generic> getTargets(Generic attribute) {
				return () -> issue.getHolders(attribute).stream().map(x -> x.getTargetComponent() != null ? x.getTargetComponent() : x);
			}

			private List<String> getValues(Generic attribute) {
				return getTargets(attribute).stream().map(x -> (String) x.getValue()).collect(Collectors.toList());
			}

			@Override
			public List<String> getValues() {
				return getValues(attribute);
			}
		};
	}

	public ElStringWrapper updateMultiHolder(Generic issue, Generic selectedHolder, Generic attribute) {
		return new ElStringWrapper() {

			@Override
			public void setValue(String value) {
				if (selectedHolder == null)
					issue.setHolder(attribute, value);
				else
					selectedHolder.updateValue(value);
			}

			@Override
			public String getValue() {
				return selectedHolder != null ? Objects.toString(selectedHolder.getValue()) : "";
			}

			@Override
			public List<String> getValues() {
				// TODO Auto-generated method stub
				return null;
			}

			@Override
			public void setValues(List<String> selectedTargets) {
				// TODO Auto-generated method stub

			}
		};

	}

	public interface ElStringWrapper {
		public String getValue();

		public void setValue(String value);

		List<String> getValues();

		void setValues(List<String> selectedTargets);
	}

}
