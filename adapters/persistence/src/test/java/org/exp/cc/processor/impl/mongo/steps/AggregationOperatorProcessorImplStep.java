package org.exp.cc.processor.impl.mongo.steps;

import org.exp.cc.model.AggregationCriteria;
import org.exp.cc.processor.AggregationOperatorProcessor;
import org.exp.cc.processor.ComparisonOperatorProcessor;
import org.exp.cc.processor.impl.mongo.AggregationOperatorProcessorImpl;
import org.exp.cc.test.ThrowableStep;
import org.springframework.data.mongodb.core.aggregation.Aggregation;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Aggregation operation processor implementation step.
 */
public class AggregationOperatorProcessorImplStep extends ThrowableStep<AggregationOperatorProcessorImplStep> {
    private AggregationOperatorProcessor aggregationOperatorProcessor;
    private Aggregation aggregation;

    /**
     * Step with no setup.
     * @return this instance
     */
    public AggregationOperatorProcessorImplStep givenIHaveNoSetup() {
        return this;
    }

    /**
     * Step to setup AggreagtionOperatorProcessorImpl.
     * @param comparisonOperatorProcessor comparison operator processor
     * @return this instance
     */
    public AggregationOperatorProcessorImplStep givenISetupAggregationOperatorProcessorImpl(final ComparisonOperatorProcessor comparisonOperatorProcessor) {
        this.aggregationOperatorProcessor = new AggregationOperatorProcessorImpl(comparisonOperatorProcessor);
        return this;
    }

    /**
     * Step to call AggregationOperatorProcessor generateAggregation.
     * @param aggregationCriteria aggregation criteria
     * @return this instance
     */
    public AggregationOperatorProcessorImplStep whenIGenerateAggregation(final AggregationCriteria aggregationCriteria) {
        this.aggregation = this.aggregationOperatorProcessor.generateAggregation(aggregationCriteria);
        return this;
    }

    /**
     * Step to assert expected aggregation.
     * @param aggregation aggregation
     * @return this instance
     */
    public AggregationOperatorProcessorImplStep thenGeneratedAggregationShouldBe(final Aggregation aggregation) {
        assertThat(this.aggregation.toString()).isEqualTo(aggregation.toString());
        return this;
    }
}
