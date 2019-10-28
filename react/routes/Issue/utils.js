export default function transform(data) {
  const {
    issueTypeId, 
    assigneeId, 
    statusId, 
    issueIds, 
    quickFilterIds, 
    createStartDate, 
    createEndDate, 
    contents,
    component,
    epic,
    label,
    reporterIds,
    sprint,
    summary,
    version,
  } = data;
  return {
    advancedSearchArgs: {
      issueTypeId,
      reporterIds,
      statusId,
    },     
    otherArgs: {
      assigneeId,
      issueIds,
      component,
      epic,
      label,
      sprint,
      summary,
      version,
    },
    searchArgs: {
      createStartDate,
      createEndDate,           
    },
    quickFilterIds,
    contents,
  };
}
