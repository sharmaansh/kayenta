package com.netflix.kayenta.judge.stats

import com.netflix.kayenta.judge.Metric
import com.netflix.kayenta.judge.stats.DescriptiveStatistics._
import org.apache.commons.math.util.FastMath


object EffectSizes {

  /**
    * Mean Ratio
    * Measures the difference between the mean values as a ratio (experiment/control)
    */
  def meanRatio(control: Metric, experiment: Metric): Double = {
    mean(experiment)/mean(control)
  }

  /**
    * Mean Ratio
    * Measures the difference between the mean values as a ratio (experiment/control)
    */
  def meanRatio(control: MetricStatistics, experiment: MetricStatistics): Double = {
    experiment.mean/control.mean
  }

  /**
    * Cohen's d (Pooled Standard Deviation)
    * Cohen's d is an effect size used to indicate the standardized difference between two means
    */
  def cohenD(control: Metric, experiment: Metric): Double = {
    cohenD(summary(control), summary(experiment))
  }

  /**
    * Cohen's d (Pooled Standard Deviation)
    * Cohen's d is an effect size used to indicate the standardized difference between two means
    */
  def cohenD(control: MetricStatistics, experiment: MetricStatistics): Double = {
    val pooledStd = FastMath.sqrt(((experiment.count - 1) * FastMath.pow(experiment.std, 2) + (control.count - 1) * FastMath.pow(control.std, 2)) / (control.count + experiment.count - 2))
    FastMath.abs(experiment.mean - control.mean) / pooledStd
  }

}
