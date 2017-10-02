package org.genericsystem.layout;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

public class Ransac<DATA> {

	private final List<DATA> datas;
	private final int n;
	private final int k;
	private Model<DATA> bestModel;
	private Map<Integer, DATA> bestDataMap;
	private double bestError = Double.MAX_VALUE;
	private double t;
	private int d;
	private final Function<Collection<DATA>, Model<DATA>> modelProvider;

	public Ransac(List<DATA> datas, Function<Collection<DATA>, Model<DATA>> modelProvider, int n, int k, double t, int d) {
		if (n > datas.size())
			throw new IllegalStateException("n parameter must be inferior or equal to data size");
		if (d > datas.size())
			throw new IllegalStateException("d parameter must be inferior or equal to data size");
		if (n > d)
			throw new IllegalStateException("d parameter must be superior or equal to n parameter");
		assert n < datas.size();
		assert n <= d;
		this.datas = datas;
		this.t = t;
		this.n = n;
		this.k = k;
		this.d = d;
		this.modelProvider = modelProvider;

	}

	public void compute() {
		for (int i = 0; i < k; i++) {
			Map<Integer, DATA> randomDataMap = new HashMap<>();
			for (int j = 0; j < n;) {
				int random = Double.valueOf(Math.floor(Math.random() * datas.size())).intValue();
				if (randomDataMap.put(random, datas.get(random)) == null)
					j++;
			}
			Model<DATA> possibleModel = modelProvider.apply(randomDataMap.values());
			for (int pt = 0; pt < datas.size(); pt++)
				if (!randomDataMap.containsKey(pt))
					if (possibleModel.computeError(datas.get(pt)) < t)
						randomDataMap.put(pt, datas.get(pt));

			if (randomDataMap.size() >= d) {
				possibleModel = modelProvider.apply(randomDataMap.values());
				double erreur = possibleModel.computeGlobalError(randomDataMap.values());
				if (erreur < bestError) {
					bestModel = possibleModel;
					bestDataMap = randomDataMap;
					bestError = erreur;
					System.out.println("Improved error : " + bestError);
				}
			}
		}
		if (bestModel == null)
			throw new IllegalStateException("Unable to find a good model. Please, check your parameters n = " + n + ", t = " + t + ", d = " + d);
	}

	public Model<DATA> getBestModel() {
		return bestModel;
	}

	public double getBestError() {
		return bestError;
	}

	public Map<Integer, DATA> getBestDataSet() {
		return bestDataMap;
	}

	public static interface Model<DATA> {

		public double computeError(DATA data);

		public default double computeGlobalError(Collection<DATA> datas) {
			double error = 0;
			for (DATA data : datas)
				error += computeError(data);
			return error;
		}

		public Object[] getParams();
	}

	public static void main(String[] args) {
		Function<Collection<Double>, Model<Double>> modelProvider = datas -> {
			double mean = 0;
			for (Double data : datas)
				mean += data;
			mean /= datas.size();
			double meanParam = mean;
			return new Model<Double>() {

				@Override
				public double computeError(Double data) {
					return Math.abs(data - meanParam);
				}

				@Override
				public Object[] getParams() {
					return new Object[] { meanParam };
				}
			};
		};
		List<Double> datas = Arrays.asList(1d, 1d, 2d, 3d, 1d, 2d, 1d, 1d);

		Ransac<Double> ransac = new Ransac<>(datas, modelProvider, 4, 100, 0.1, datas.size() / 2);
		ransac.compute();
		System.out.println("Result : " + ransac.getBestModel().getParams()[0]);

	}
}
