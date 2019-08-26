package org.exp.cc.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.ImmutableMap;
import org.exp.cc.model.controller.Response;
import org.exp.cc.model.service.Result;
import org.exp.cc.service.PersonService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.util.Lists.newArrayList;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Person controller test.
 */
public class PersonControllerTest {
    private MockMvc mockMvc;
    private PersonService personService;
    private ObjectMapper mapper;

    public PersonControllerTest() {
        this.mapper = new ObjectMapper();
    }

    /**
     * Test Initialization.
     */
    @BeforeEach
    public void init() {
        this.personService = mock(PersonService.class);
        this.mockMvc = MockMvcBuilders.standaloneSetup(new PersonController(this.personService)).build();
    }

    /**
     * getPerson with valid demographic id, should return response.
     * @throws Exception exception
     */
    @Test
    public void getPerson_WithValidDemographicId_ShouldReturnsResponse() throws Exception {
        final List<Map<String, Object>> data = newArrayList(
                ImmutableMap.of("ID", 1, "idPerson", 1, "data", "test1"),
                ImmutableMap.of("ID", 2, "idPerson", 2, "data", "test2")
        );
        final Map<String, Object> summary = ImmutableMap.of("record_count", 2);

        when(this.personService.getPerson(anySet())).thenReturn(new Result(data, summary));

        final MvcResult mvcResult = this.mockMvc.perform(get("/person")
                .param("demographic_id", "1, 2"))
                .andExpect(status().is(200))
                .andReturn();

        final Response actualResponse = this.mapper.readValue(mvcResult.getResponse().getContentAsString(), Response.class);
        assertThat(actualResponse).isEqualTo(new Response(data, summary));
    }

    /**
     * addPersonToQueue with valid person, should success with http status 204.
     * @throws Exception exception
     */
    @Test
    public void addPersonToQueue_WithValidPerson_SuccessWithNoContent() throws Exception {
        final Map<String, Object> person = ImmutableMap.of("ID", 1, "idPerson", 1, "data", "testing");

        this.mockMvc.perform(post("/person/queue")
                .contentType(MediaType.APPLICATION_JSON)
                .content(this.mapper.writeValueAsString(person))
        ).andExpect(status().is(204));

        verify(this.personService).sendToPersonQueue(person);
    }
}
