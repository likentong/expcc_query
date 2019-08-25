package org.exp.cc.datastore.dao.person.impl.steps;

import org.exp.cc.constant.PersistenceConstant;
import org.exp.cc.datastore.dao.DataStoreDAO;
import org.exp.cc.datastore.dao.person.PersonDAO;
import org.exp.cc.datastore.dao.person.impl.PersonDAOImpl;
import org.exp.cc.model.AggregationCriteria;
import org.exp.cc.test.ThrowableStep;
import org.mockito.ArgumentCaptor;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * PersonDAO implementation step.
 */
public class PersonDAOImplStep extends ThrowableStep<PersonDAOImplStep> {
    private DataStoreDAO dataStoreDAO;
    private List<Map<String, Object>> results;
    private PersonDAO personDAO;
    private ArgumentCaptor<AggregationCriteria> aggregationCriteriaArgumentCaptor;
    private ArgumentCaptor<String> queryStringArgumentCaptor;

    public PersonDAOImplStep() {
        this.dataStoreDAO = mock(DataStoreDAO.class);
        this.personDAO = new PersonDAOImpl(this.dataStoreDAO);

        this.aggregationCriteriaArgumentCaptor = ArgumentCaptor.forClass(AggregationCriteria.class);
        this.queryStringArgumentCaptor = ArgumentCaptor.forClass(String.class);
    }

    /**
     * Step with no setup.
     * @return this instance
     */
    public PersonDAOImplStep givenIHaveNoSetup() {
        return this;
    }

    /**
     * Step to mock DataStoreDAO aggregateData method to return results.
     * @param dataStoreResults data store result
     * @return this instance
     */
    public PersonDAOImplStep givenIMockDataStoreDAOAggregateData(final List<Map<String, Object>> dataStoreResults) {
        when(this.dataStoreDAO.aggregateData(eq(PersistenceConstant.Entity.PERSON), any(AggregationCriteria.class))).thenReturn(dataStoreResults);
        return this;
    }

    /**
     * Step to mock DataStoreDAO query data to return results.
     * @param dataStoreResults data store results.
     * @return this instance
     */
    public PersonDAOImplStep givenIMockDataStoreDAOQueryData(final List<Map<String, Object>> dataStoreResults) {
        when(this.dataStoreDAO.queryData(eq(PersistenceConstant.Entity.PERSON), anyString(), eq(Collections.emptyList()))).thenReturn(dataStoreResults);
        return this;
    }

    /**
     * Step to call PersonDAO getPersonCountByDemographicId.
     * @param id ids
     * @return this instance
     */
    public PersonDAOImplStep whenIGetPersonCountByDemographicId(final Set<Object> id) {
        this.results = this.personDAO.getPersonCountByDemographicId(id);
        return this;
    }

    /**
     * Step to call PersonDAO getPersonByDemographicId.
     * @param id ids
     * @return this instance
     */
    public PersonDAOImplStep whenIGetPersonByDemographicId(final Set<Object> id) {
        this.results = this.personDAO.getPersonByDemographicId(id);
        return this;
    }

    /**
     * Step to assert expected results.
     * @param results expected results.
     * @return this instance
     */
    public PersonDAOImplStep thenResultsShouldBe(final List<Map<String, Object>> results) {
        assertThat(this.results).isEqualTo(results);
        return this;
    }

    /**
     * Step to assert AggregationCriteria.
     * @param aggregationCriteria expected aggregation criteria
     * @return this instance
     */
    public PersonDAOImplStep thenAggregationCriteriaShouldBe(final AggregationCriteria aggregationCriteria) {
        verify(this.dataStoreDAO).aggregateData(eq(PersistenceConstant.Entity.PERSON), this.aggregationCriteriaArgumentCaptor.capture());
        assertThat(this.aggregationCriteriaArgumentCaptor.getValue()).isEqualTo(aggregationCriteria);
        return this;
    }

    /**
     * Step to assert query string.
     * @param queryString expected query string
     * @return this instance
     */
    public PersonDAOImplStep thenQueryStringShouldBe(final String queryString) {
        verify(this.dataStoreDAO).queryData(eq(PersistenceConstant.Entity.PERSON), this.queryStringArgumentCaptor.capture(), eq(Collections.emptyList()));
        assertThat(this.queryStringArgumentCaptor.getValue()).isEqualTo(queryString);
        return this;
    }

}
