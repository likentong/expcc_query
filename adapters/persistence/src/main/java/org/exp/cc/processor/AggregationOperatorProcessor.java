package org.exp.cc.processor;

import org.exp.cc.model.AggregationCriteria;
import org.springframework.data.mongodb.core.aggregation.Aggregation;

/**
 * Aggregation operator processor.
 */
public interface AggregationOperatorProcessor {

    /**
     * Generate mongodb aggregation based on aggregation criteria.
     * @param aggregationCriteria aggregation criteria
     * @return aggregation
     */
    Aggregation generateAggregation(AggregationCriteria aggregationCriteria);

}
