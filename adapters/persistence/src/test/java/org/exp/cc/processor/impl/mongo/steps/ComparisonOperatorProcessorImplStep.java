package org.exp.cc.processor.impl.mongo.steps;

import org.exp.cc.model.persistence.QueryFields;
import org.exp.cc.processor.ComparisonOperatorProcessor;
import org.exp.cc.processor.impl.mongo.ComparisonOperatorProcessorImpl;
import org.exp.cc.test.ThrowableStep;
import org.springframework.data.mongodb.core.query.Criteria;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

/**
 * Comparison operator processor implementation step.
 */
public class ComparisonOperatorProcessorImplStep extends ThrowableStep<ComparisonOperatorProcessorImplStep> {
    private Criteria[] criteria;
    private ComparisonOperatorProcessor comparisonOperatorProcessor;

    public ComparisonOperatorProcessorImplStep() {
        this.comparisonOperatorProcessor = new ComparisonOperatorProcessorImpl();
    }

    /**
     * Step with no setup.
     * @return this instance
     */
    public ComparisonOperatorProcessorImplStep givenIHaveNoSetup() {
        return this;
    }

    /**
     * Step to call ComparisonOperatorProcessor generateCriteria.
     * @param queryFields query fields
     * @return this instance
     */
    public ComparisonOperatorProcessorImplStep whenIGenerateCriteria(final QueryFields queryFields) {
        this.criteria = this.comparisonOperatorProcessor.generateCriteria(queryFields);
        return this;
    }

    /**
     * Step to assert expected criteria.
     * @param criteria expected criteria
     * @return this instance
     */
    public ComparisonOperatorProcessorImplStep thenCriteriaShouldBe(final Criteria[] criteria) {
        assertThat(this.criteria).isEqualTo(criteria);
        return this;
    }
}
