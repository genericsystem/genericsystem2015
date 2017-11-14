package org.genericsystem.reinforcer;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.genericsystem.reinforcer.Constraint.PositionConstraint;
import org.genericsystem.reinforcer.Constraint.RelationConstraint;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Template2 {

	private final Logger logger = LoggerFactory.getLogger(Template2.class);
	private final String name;
	protected final Set<Labels> confirmedMembers = new HashSet<>();
	protected final Set<Labels> otherMembers = new HashSet<>();
	private List<Constraint> constraints = new ArrayList<>();

	public Template2(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public boolean matches(Labels entry) {
		return constraints.stream().allMatch(constraint -> constraint.check(entry));
	}

	public void addConfirmedMember(Labels labels) {
		confirmedMembers.add(labels);
	}

	public void addLabels(Labels labels) {
		confirmedMembers.add(labels);
	}

	public boolean contains(Labels al) {
		return confirmedMembers.contains(al) || otherMembers.contains(al);
	}

	public boolean contains(Constraint constraint) {
		return constraints.contains(constraint);
	}

	// Returns true iff all the confirmed members verify the constraint.
	public boolean isCompatibleWith(Constraint constraint) {
		return confirmedMembers.stream().allMatch(member -> constraint.check(member));
	}

	public void addConstraint(Constraint constraint) {
		constraints.add(constraint);
	}

	// Finds a constraint that is compatible with the model but not with the item to exclude and adds it to the set of constraints.
	public void addConstraintExcluding(Labels exclude) {
		List<Labels> members = new ArrayList<>(confirmedMembers);
		Set<Constraint> compatibleConstraints = computePossibleAdds(members.get(0));
		for (int i = 1; i < confirmedMembers.size(); i++ ) {
			Set<Constraint> newConstraints = computePossibleAdds(members.get(i));
			compatibleConstraints.retainAll(newConstraints);
		}
		if (compatibleConstraints.isEmpty())
			throw new IllegalStateException("Impossible to specialize template " + name + " to exclude item " + exclude);

		List<Constraint> possibleAdds = compatibleConstraints.stream().filter(constraint -> !constraint.check(exclude)).collect(Collectors.toList());
		if (possibleAdds.isEmpty())
			throw new IllegalStateException("Impossible to specialize template " + name + " to exclude item " + exclude);

		addConstraintFrom(possibleAdds);
	}

	// Adds a constraint that is compatible with the model and with the given item and
	// adds the given item to the confirmed members of the template.
	public void addConstraintIncluding(Labels include) {
		Set<Constraint> possibleAdds = computePossibleAdds(include);
		if (possibleAdds.isEmpty())
			throw new IllegalStateException("Impossible to specialize the model for " + name + " to accomodate the new item.");
		addConstraintFrom(new ArrayList<>(possibleAdds));
		addConfirmedMember(include);
	}

	// Removes a constraint so that the given item matches
	// and adds the given item to the confirmed members of the template.
	public void removeConstraintToInclude(Labels include) {
		Iterator<Constraint> it = constraints.iterator();
		while (it.hasNext()) {
			Constraint constraint = it.next();
			if (!constraint.check(include))
				it.remove();
		}
		addConfirmedMember(include);
	}

	// Picks a constraint in the given list and adds it to this template’s constraints.
	public void addConstraintFrom(List<Constraint> constraints) {
		List<RelationConstraint> relC = constraints.stream().filter(c -> c instanceof RelationConstraint).map(c -> (RelationConstraint) c).collect(Collectors.toList());
		if (!relC.isEmpty())
			addConstraint(relC.get(0));
		else {
			List<PositionConstraint> posC = constraints.stream().filter(c -> c instanceof PositionConstraint).map(c -> (PositionConstraint) c).collect(Collectors.toList());
			if (!posC.isEmpty())
				addConstraint(posC.get(0));
			else
				addConstraint(constraints.get(0));
		}
	}

	// Returns a set of constraints verified by the item but not present in the template.
	public Set<Constraint> computePossibleAdds(Labels item) {
		return item.getConstraints().stream().filter(constraint -> !contains(constraint) && isCompatibleWith(constraint)).collect(Collectors.toSet());
	}

	public boolean hasConstraints() {
		return !constraints.isEmpty();
	}

	@Override
	public String toString() {
		return "\nTemplate " + name + "\nConstraints: " + constraints;
	}
}
