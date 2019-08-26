package org.exp.cc.processor.impl.mongo.steps;

import org.exp.cc.model.persistence.QueryCriteria;
import org.exp.cc.model.persistence.QueryFields;
import org.exp.cc.processor.ComparisonOperatorProcessor;
import org.exp.cc.processor.LogicalOperatorProcessor;
import org.exp.cc.processor.impl.mongo.LogicalOperatorProcessorImpl;
import org.exp.cc.test.ThrowableStep;
import org.springframework.data.mongodb.core.query.Criteria;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Logical operator processor implementation step.
 */
public class LogicalOperatorProcessorImplStep extends ThrowableStep<LogicalOperatorProcessorImplStep> {
    private LogicalOperatorProcessor logicalOperatorProcessor;
    private ComparisonOperatorProcessor comparisonOperatorProcessor;
    private Criteria criteria;

    public LogicalOperatorProcessorImplStep() {
        this.comparisonOperatorProcessor = mock(ComparisonOperatorProcessor.class);
        this.logicalOperatorProcessor = new LogicalOperatorProcessorImpl(this.comparisonOperatorProcessor);
    }

    /**
     * Step with no setup.
     * @return this instance
     */
    public LogicalOperatorProcessorImplStep givenIHaveNoSetup() {
        return this;
    }

    /**
     * Step to mock ComparisonOperator generateCriteria.
     * @param queryFields query fields
     * @param criteria criteria
     * @return this instance
     */
    public LogicalOperatorProcessorImplStep givenIMockComparisonOperatorGenerateCriteria(final QueryFields queryFields, final Criteria[] criteria) {
        when(this.comparisonOperatorProcessor.generateCriteria(eq(queryFields))).thenReturn(criteria);
        return this;
    }

    /**
     * Step to call LogicalOperatorProcessor generateCriteria.
     * @param queryCriteria query criteria
     * @return this instance
     */
    public LogicalOperatorProcessorImplStep whenIGenerateCriteria(final QueryCriteria queryCriteria) {
        this.criteria = this.logicalOperatorProcessor.generateCriteria(queryCriteria);
        return this;
    }

    /**
     * Step to assert expected criteria.
     * @param criteria criteria
     * @return this instance
     */
    public LogicalOperatorProcessorImplStep thenCriteriaShouldBe(final Criteria criteria) {
        assertThat(this.criteria).isEqualTo(criteria);
        return this;
    }

}
