package com.whitepages.sampleSvc

import com.codahale.metrics.MetricRegistry

object SampleMonitor {

  def ext(metrics: MetricRegistry): PartialFunction[Any, Unit] = {
    val testCounter = metrics.counter("sample.testcmd")

    {
      case ("test", "testcmd") =>
        testCounter.inc(2)
    }
  }
}
