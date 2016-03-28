package org.genericsystem.defaults.tools;

import java.util.function.Function;
import java.util.stream.Collectors;

import javafx.collections.ListChangeListener.Change;
import javafx.collections.ObservableList;
import javafx.collections.transformation.TransformationList;

/**
 * @author Nicolas Feybesse
 *
 * @param <TARGET>
 * @param <SRC>
 */
public class Transformation<TARGET, SRC> extends TransformationList<TARGET, SRC> {

	private final Function<SRC, TARGET> srcToTarget;

	public Transformation(ObservableList<? extends SRC> source, Function<SRC, TARGET> srcToTarget) {
		super(source);
		this.srcToTarget = srcToTarget;
	}

	@Override
	protected void sourceChanged(Change<? extends SRC> change) {
		// System.out.println("Transformation change : " + change);
		while (change.next()) {
			beginChange();

			if (change.wasPermutated()) {
				assert false;
				nextRemove(change.getFrom(), change.getRemoved().stream().map(srcToTarget::apply).collect(Collectors.toList()));
				nextAdd(change.getFrom(), change.getTo());
			} else {
				if (change.wasRemoved()) {
					nextRemove(change.getFrom(), change.getRemoved().stream().map(srcToTarget::apply).collect(Collectors.toList()));
				}
				if (change.wasAdded())
					nextAdd(change.getFrom(), change.getTo());
			}
			endChange();
		}
	}

	@Override
	public int getSourceIndex(int index) {
		return index;
	}

	@Override
	public TARGET get(int index) {
		SRC f = getSource().get(index);
		return srcToTarget.apply(f);

	}

	@Override
	public int size() {
		return getSource().size();
	}

}
