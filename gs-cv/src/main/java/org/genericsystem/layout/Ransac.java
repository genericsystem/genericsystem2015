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
	private Map<Integer, DATA> bestDataSet;
	private double bestError = Double.MAX_VALUE;
	private double t;
	private int d;
	private final Function<Collection<DATA>, Model<DATA>> modelProvider;

	public Ransac(List<DATA> datas, Function<Collection<DATA>, Model<DATA>> modelProvider, int n, int k, double t, int d) {
		assert n < datas.size();
		this.datas = datas;
		this.n = n;
		this.k = k;
		this.d = d;
		this.modelProvider = modelProvider;
	}

	public void compute() {
		for (int i = 0; i < k; i++) {
			Map<Integer, DATA> randomDataSet = new HashMap<>();
			for (int j = 0; j <= n;) {
				int random = Double.valueOf(Math.floor(Math.random() * datas.size())).intValue();
				if (randomDataSet.put(random, datas.get(random)) == null)
					j++;
				System.out.println("randomDataSet : " + randomDataSet);
			}
			System.out.println("randomDataSet2 : " + randomDataSet);
			Model<DATA> possibleModel = modelProvider.apply(randomDataSet.values());
			for (int pt = 0; pt < datas.size(); pt++) {
				if (!randomDataSet.keySet().contains(pt)) {
					if (possibleModel.computeError(datas.get(pt)) < t) {
						randomDataSet.put(pt, datas.get(pt));
					}

				}
			}
			System.out.println("randomDataSet3 : " + randomDataSet);
			if (randomDataSet.size() >= d) {
				possibleModel = modelProvider.apply(randomDataSet.values());
				double erreur = possibleModel.computeGlobalError(randomDataSet.values());
				if (erreur < bestError) {
					bestModel = possibleModel;
					bestDataSet = randomDataSet;
					bestError = erreur;
				}
			}

		}
	}

	public Model<DATA> getBestModel() {
		return bestModel;
	}

	public double getBestError() {
		return bestError;
	}

	public Map<Integer, DATA> getBestDataSet() {
		return bestDataSet;
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
