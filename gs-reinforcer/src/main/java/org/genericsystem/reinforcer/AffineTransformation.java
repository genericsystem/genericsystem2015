package org.genericsystem.reinforcer;

import java.util.Collection;

import org.genericsystem.reinforcer.Template3.Match;
import org.genericsystem.reinforcer.tools.GSRect;

// Represents only transformations corresponding to non-proportional scaling + translation
public class AffineTransformation {
	protected final double xScale;
	protected final double xShift;
	protected final double yScale;
	protected final double yShift;

	public AffineTransformation(Match match) {
		GSRect rect1 = match.source.getRect();
		GSRect rect2 = match.match.getRect();
		double[] xParams = solve(rect1.getX(), rect2.getX(), rect1.getX() + rect1.getWidth(), rect2.getX() + rect2.getWidth());
		double[] yParams = solve(rect1.getY(), rect2.getY(), rect1.getY() + rect1.getHeight(), rect2.getY() + rect2.getHeight());
		xScale = xParams[0];
		xShift = xParams[1];
		yScale = yParams[0];
		yShift = yParams[1];
	}

	public AffineTransformation(Collection<Match> matches) {
		if (matches.isEmpty())
			throw new IllegalStateException("Can not compute transformation from an empty collection of matches.");
		double xScale = 0, xShift = 0, yScale = 0, yShift = 0;
		int count = 0;
		for (Match match : matches) {
			count++;
			AffineTransformation matchTransform = new AffineTransformation(match);
			xScale = ((count - 1) * xScale + matchTransform.xScale) / count;
			xShift = ((count - 1) * xShift + matchTransform.xShift) / count;
			yScale = ((count - 1) * yScale + matchTransform.yScale) / count;
			yShift = ((count - 1) * yShift + matchTransform.yShift) / count;
		}
		this.xScale = xScale;
		this.xShift = xShift;
		this.yScale = yScale;
		this.yShift = yShift;
	}

	// Solves xt = a xs + b, returns a and b.
	private double[] solve(double xs1, double xt1, double xs2, double xt2) {
		double a, b;
		if (xs1 == xs2)
			throw new IllegalStateException("The given points must be distinct.");
		a = (xt2 - xt1) / (xs2 - xs1);
		b = xt1 - a * xs1;
		return new double[] { a, b };
	}

	public Labels transform(Labels labels) {
		Labels transformed = new Labels();
		for(Label label : labels)
			transformed.addLabel(label.affineTransform(xScale, xShift, yScale, yShift));
		return transformed;
	}

	// Returns the (squared) distance between this transformation and the transformation defined by the given match.
	public double computeError(Match match) {
		AffineTransformation matchTransform = new AffineTransformation(match);
		return square(xScale - matchTransform.xScale) + square(xShift - matchTransform.xShift) +
				square(yScale - matchTransform.yScale) + square(yShift - matchTransform.yShift);
	}

	private double square(double x) {
		return x * x;
	}
}